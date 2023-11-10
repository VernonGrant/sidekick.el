;;; sidekick.el --- Provides information about a symbol inside a side window  -*- lexical-binding: t -*-

;; Copyright (C) 2022 by Vernon Grant.

;; Author: Vernon Grant <vernon@ruppell.io>
;; Version: 1.1.0
;; Package-Requires: ((emacs "26.1"))
;; Keywords: search, references, grep, package, sidekick
;; Homepage: https://github.com/VernonGrant/sidekick.el

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Sidekick is a Emacs package that's aim is to provide general information about a
;; symbol inside a single window.

;;; Code:

(require 'project)

;;;;;;;;;;;;;;;;;;;;
;; Customizations ;;
;;;;;;;;;;;;;;;;;;;;

(defgroup sidekick-search nil
    "Sidekick search settings."
    :group 'convenience
    :prefix "sidekick-search-")

(defcustom sidekick-search-minimum-symbol-length 2
    "The minimum symbol / term length in order for Sidekick to update."
    :group 'sidekick-search
    :type 'integer)

(defcustom sidekick-search-case-insensitive t
    "Should sidekick searches be case insensitve."
    :group 'sidekick-search
    :type 'boolean)

(defcustom sidekick-search-file-limit 10000
    "Limit the number of files to process during project wide matching search."
    :group 'sidekick-search
    :type 'integer)

(defgroup sidekick-window nil
    "Sidekick window settings."
    :group 'convenience
    :prefix "sidekick-window-")

(defcustom sidekick-window-hide-footer nil
    "Remove the Sidekick footer branding."
    :group 'sidekick-window
    :type 'boolean)

;;;;;;;;;;;;;;;;;;;;;;;
;; Private Variables ;;
;;;;;;;;;;;;;;;;;;;;;;;

(defvar sidekick--mode-file-associations
    `(("c++-mode"        . ("cpp" "h" "hh"))
         ("c-mode"          . ("c" "cc" "h" "hh"))
         ("cperl-mode"      . ("pl" "PL"))
         ("css-mode"        . ("css"))
         ("scss-mode"       . ("css" "sass" "scss"))
         ("emacs-lisp-mode" . ("el" "emacs"))
         ("go-mode"         . ("go"))
         ("java-mode"       . ("java"))
         ("js-mode"         . ("js" "es" "es6"))
         ("json-mode"       . ("json"))
         ("markdown-mode"   . ("md"))
         ("text-mode"       . ("txt"))
         ("php-mode"        . ("php" "phtml" "twig" "blade"))
         ("phps-mode"       . ("php" "phtml" "twig" "blade"))
         ("python-mode"     . ("py"))
         ("ruby-mode"       . ("rb"))
         ("rust-mode"       . ("rs"))
         ("restclient-mode" . ("el"))
         ("org-mode"        . ("org"))
         ("typescript-mode" . ("ts"))
         ("web-mode"        . ("html" "blade" "twig"))
         ("nxml-mode"       . ("xml" "xml.dist"))
         ("yaml-mode"       . ("yml")))
    "Association List holding major mode file associations.")

(defconst sidekick--buffer-name "*sidekick*"
    "The default Sidekick buffer name.")

(defconst sidekick--match-line-number-regexp "^[0-9]+:"
    "Regexp used to extract line numbers from grep results.")

(defconst sidekick--match-file-path-regexp "^[\/|\.\/].*"
    "Regexp used to extract file paths from grep results.")

(defvar sidekick--state-updating nil
    "Is non-nil when an update is in process.")

(defvar sidekick--state-clean t
    "Is non-nil if no update calls were made.")

(defvar sidekick--state-mode-str nil
    "The mode name of the current operation.")

(defvar sidekick--state-project-dir nil
    "The project directory of the current operation.")

(defvar sidekick--state-symbol nil
    "The symbol of the current operation, includes text properties.")

(defvar sidekick--state-symbol-str nil
    "The symbol or term string of the current operation.")

(defvar sidekick--state-buffer-name nil
    "The buffer name of the current operation.")

(defvar sidekick--state-buffer-file-name nil
    "The buffer file name of the current operation.")

;;;;;;;;;;;;;;;;;;;
;; Sidekick Mode ;;
;;;;;;;;;;;;;;;;;;;

(defvar sidekick-mode-hook nil
    "List of functions to call on sidekick mode activation.")

(defvar sidekick-mode-local-map
    (let ((map (make-sparse-keymap)))
        (define-key map "q" 'sidekick-quit)
        (define-key map "g" 'sidekick-refresh)
        (define-key map "n" 'sidekick-next-match)
        (define-key map "p" 'sidekick-previous-match)
        (define-key map (kbd "RET") 'sidekick-open-match)
        map)
    "The local key map used for `sidekick-mode'.")

(defvar sidekick-mode-fonts
    `(
         ;; Headings.
         ("^-\\{3\\}|\s?" . 'font-lock-comment-face)
         ("^-\\{3\\}|\s.*\\(|-+\\)" (1 font-lock-comment-face))
         ("^-\\{3\\}|\s\\(.*\\)\s|-+" (1 font-lock-function-name-face))

         ;; Sub headings.
         ("^-\\{3\\}|\s\\(.*\\)" (1 font-lock-function-name-face))

         ;; File path.
         ("^[\/|\.\/].*" . 'compilation-info)

         ;; Line numbers.
         ("^[\s0-9]+" . 'compilation-line-number)

         ;; Footer sections.
         ("^-\\{3\\}.*" . font-lock-comment-face)
         ("^[-\s]\\{3\\}+\s+[-].*" . font-lock-comment-face)
         ("^[\\*\s]\\{2\\}[\\*\s]+[\\*]" . font-lock-comment-face)
         ("^[\\*\s]+[\s]+\\([a-zA-Z]+[:]\s[0-9\\.]+\\)" (1 font-lock-keyword-face)))
    "Defines the font lock defaults for `sidekick-mode'.")

(defun sidekick--deconstruct()
    "Clears Sidekick mode related variables."
    (setq buffer-read-only nil)
    (kill-all-local-variables))

(define-derived-mode sidekick-mode fundamental-mode "Sidekick"
    "Enables sidekick mode. Only used inside the Sidekick panel."
    (progn
        (setq buffer-read-only t)
        (use-local-map sidekick-mode-local-map)
        (display-line-numbers-mode 0)
        (toggle-truncate-lines 1)
        (run-hooks 'sidekick-mode-hook)
        (setq font-lock-defaults '(sidekick-mode-fonts t))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Sidekick Keymap Interactions ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TODO: Better optimize this function.
(defun sidekick--match-get-line-and-path()
    "Get's a match's line number and file path, inside of Sidekick Window."
    (let ((match-line-num nil)
             (match-file-path nil))

        (let ((cur-line (thing-at-point 'line t)))
            (when (string-match sidekick--match-line-number-regexp cur-line)
                (setq match-line-num (string-to-number (match-string 0 cur-line)))

                ;; NOTE: Optimize this in future release.
                ;; Find and extract the match's file path.
                (save-excursion
                    ;; Will move up until a file path is found.
                    (unless (string-match sidekick--match-file-path-regexp cur-line)
                        (while (and (> (string-width (thing-at-point 'line t)) 0)
                                   (> (line-number-at-pos (point)) 1))
                            (forward-line -1))
                        (forward-line))

                    (when (string-match sidekick--match-file-path-regexp (thing-at-point 'line t))
                        (setq match-file-path  (buffer-substring-no-properties
                                                   (line-beginning-position)
                                                   (line-end-position)))))

                (list match-line-num match-file-path)))))

(defun sidekick--match-setup-buffer(match-file-path)
    "Find the match's file and create it's buffer.

Will create the buffer for the provided file path, returns the
match's buffer on success and nil otherwise.

MATCH-FILE-PATH The file path of a match."
        (let ((match-buffer nil))
            (if match-file-path
                (setq match-buffer (find-file-noselect match-file-path nil nil))
                (setq match-buffer (find-file-noselect sidekick--state-buffer-file-name nil nil)))
            (unless match-buffer
                (message "Sidekick: Match file or buffer could not be found."))
            match-buffer))

(defun sidekick--match-buffer-operations(match-buffer match-line-num)
    "Perform default match highlighting, centering operations.

Will perform a series of default buffer operations that makes the
match more visible.

MATCH-BUFFER The match's buffer.
MATCH-LINE-NUM The match's line number."
    ;; If line number is non-nil, find match.
    (when match-line-num
        (with-selected-window (get-buffer-window match-buffer)
            (progn
                (goto-char (point-min))
                (forward-line (1- match-line-num))
                (re-search-forward (regexp-quote sidekick--state-symbol-str) nil t)
                (re-search-backward (regexp-quote sidekick--state-symbol-str) nil t)
                (recenter))))

    ;; If line number is nil, then this must be a file path so just go to the
    ;; beginning of file.
    (unless match-line-num
        (with-selected-window (get-buffer-window match-buffer)
            (progn (goto-char (point-min))))))

(defun sidekick-quit()
    "Closes the sidekick window and kill it's buffer."
    (interactive)
    ;; NOTE: Might need to do some cleanup here.
    (quit-window)
    (kill-buffer sidekick--buffer-name))

(defun sidekick-previous-match()
    "Displays the previous match, creating and opening it's buffer."
    (interactive)
    (when (string= (buffer-name) sidekick--buffer-name)
        (forward-line -1)
        (let ((match-line-and-path (sidekick--match-get-line-and-path)))
            (let ((match-buffer (sidekick--match-setup-buffer (nth 1 match-line-and-path)))
                     (match-line-num (nth 0 match-line-and-path)))

                ;; Display the match's buffer.
                (display-buffer-use-some-window match-buffer `())

                ;; Perform default match operations.
                (when match-line-num
                    (sidekick--match-buffer-operations
                        match-buffer
                        match-line-num))))))

(defun sidekick-next-match()
    "Displays the next match, creating and opening it's buffer."
    (interactive)
    (when (string= (buffer-name) sidekick--buffer-name)
        (forward-line 1)
        (let ((match-line-and-path (sidekick--match-get-line-and-path)))
            (let ((match-buffer (sidekick--match-setup-buffer (nth 1 match-line-and-path)))
                     (match-line-num (nth 0 match-line-and-path)))

                (message (buffer-file-name match-buffer))
                (message (int-to-string match-line-num))

                ;; Display the match's buffer.
                (display-buffer-use-some-window match-buffer `())

                ;; Perform default match operations.
                (when match-line-num
                    (sidekick--match-buffer-operations
                        match-buffer
                        match-line-num))))))

(defun sidekick-refresh()
    "Re-runs the previous operations, refreshing the results."
    (interactive)
    (unless sidekick--state-clean
        (sidekick--trigger-update
            sidekick--state-symbol
            sidekick--state-buffer-file-name
            sidekick--state-mode-str)
        (message "Sidekick: refreshed!")))

(defun sidekick-open-match()
    "Go's directly to the match's symbol, creating a buffer if needed."
    (interactive)
    (when (string= (buffer-name) sidekick--buffer-name)
        (let ((match-line-and-path (sidekick--match-get-line-and-path)))
            (print match-line-and-path)
            (let ((match-buffer (sidekick--match-setup-buffer (nth 1 match-line-and-path)))
                     (match-line-num (nth 0 match-line-and-path)))

                ;; (switch-to-buffer match-buffer nil t)
                (switch-to-buffer-other-window match-buffer)

                ;; Go to line number.
                (sidekick--match-buffer-operations match-buffer match-line-num)))))

;;;;;;;;;;;;;;;;;;;;;;;;
;; Sidekick Utilities ;;
;;;;;;;;;;;;;;;;;;;;;;;;

(defun sidekick--get-project-root-path()
    "Get the root path to the current project.
Returns the project path if found, otherwise nil."
    (let ((dir default-directory))
        (let ((project-root (or (project-root (project-current))
                                (locate-dominating-file dir ".sidekick")
                                (when (fboundp 'projectile-project-root)
                                    (projectile-project-root))
                                (locate-dominating-file dir ".projectile")
                                (locate-dominating-file dir ".git"))))

            (when (not project-root)
                (message "Sidekick: Project root directory not found!"))

            (if (string-suffix-p "/" project-root)
                (substring project-root 0 -1)
                project-root))))

(defun sidekick--get-window-width()
    "Get the Sidekick window width in columns."
    (let ((sidekick-win (nth 0 (get-buffer-window-list sidekick--buffer-name))))
        (if sidekick-win
            (window-width sidekick-win) (+ 80))))

(defun sidekick--get-mode-file-glob-pattern(mode-str buffer-fn)
    "Get the supported file glob pattern based on the mode and buffer file name.

Attempts to extract the mode's file globs from
`sidekick--mode-file-associations'.  If found, returns the glob
pattern, else returns the buffer file's extension, both in string
format.

MODE-STR The major mode name.
BUFFER-FN The buffers file name."
    (let ((iterator 0)
             (glob-pat ""))
        (while (< iterator (length sidekick--mode-file-associations))
            (let ((mode (nth iterator sidekick--mode-file-associations)))
                (when (string= (car mode) mode-str)
                    ;; If the globs pattern is empty, just use the current
                    ;; buffers extension.
                    (if (= (length (cdr mode)) 0)
                        (setq glob-pat (concat "*." (file-name-extension buffer-fn nil)))
                        (setq glob-pat (cdr mode)))
                    (setq iterator (length sidekick--mode-file-associations))))
            (setq iterator (+ iterator 1)))
        glob-pat))

(defun sidekick--handle-window-creation(sidekick-buf)
    "Handles the creation of the Sidekick window.

Only create the Sidekick window if it does not already exist.

SIDEKICK-BUF The sidekick buffer, will handle nil values."
    (message (get-buffer-window sidekick-buf t))
    (unless (get-buffer-window sidekick-buf t)
        ;; Define default window properties.
        (let ((buf-window-alist `(
                                     (dedicated . t)
                                     ;; (inhibit-same-window . t)
                                     ;; (slot . -1)
                                     ;; (side . left)
                                     ;; (window-width . 0.25)
                                     ;; (window-parameters . ((no-delete-other-windows . t)))
                                     )))

            ;; Set custom window properties.
            ;; (setf (alist-get 'window-width buf-window-alist) sidekick-window-width)
            ;; (setf (alist-get 'side buf-window-alist) sidekick-window-side)
            ;; (display-buffer-in-side-window sidekick-buf buf-window-alist)
            ;; Maybe we need to check if the buffer is visable?
            ;; (display-buffer-reuse-window sidekick-buf buf-window-alist)
            (display-buffer-same-window sidekick-buf buf-window-alist)
            )))

(defun sidekick-set-file-associations(mode-str extensions)
    "Add new, or update existing major mode file associations.

If a mode already exists, it's glob pattern get's replaced.  If
no matching mode was found, it will be added to the
`sidekick--mode-file-associations' list.

MODE-STR The mode name as a string.
EXTENSIONS The glob pattern as a string."
    (let ((mode-alist (assoc mode-str sidekick--mode-file-associations)))
        (if mode-alist
            ;; Update existing alist if the mode does exist.
            (setcdr (assoc (car mode-alist) sidekick--mode-file-associations) extensions)
            ;; Push new alist if the mode does not exist.
            (push (cons mode-str extensions) sidekick--mode-file-associations))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Sidekick User Interface ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun sidekick-draw-section-heading(heading)
    "Draws a heading separator inside the active buffer.

HEADING The heading text label."
    (let ((iterator 0)
             (heading-fnl (concat "---| " heading " |")))
        (insert heading-fnl)
        (while (< iterator
                   (- (sidekick--get-window-width) (string-width heading-fnl)))
            (progn
                (setq iterator (+ iterator 1))
                (insert "-"))))
    (insert "\n\n"))

(defun sidekick-draw-section-sub-heading(heading)
    "Draws a sub heading separator inside the active buffer.

HEADING The heading text label."
    (insert (concat "---| "  heading))
    (insert "\n\n"))

(defun sidekick-draw-separator()
    "Draws a separator inside the active buffer."
    (let ((iterator 0))
        (while (< iterator (sidekick--get-window-width))
            (progn
                (setq iterator (+ iterator 1))
                (insert "-"))))
    (insert "\n"))

(defun sidekick-draw-text-centerd(text)
    "Centers text inside the Sidekick window.

The text will be centered based on the column with of the
Sidekick window.  The text can contain line breaks but trailing
spaces will not be trimmed.

TEXT The text to be centered."
    (let ((text-lns (split-string text "\n"))
             (text-ln-max 0)
             (iterator 0))

        ;; Find the longest line.
        (while (< iterator (length text-lns))
            (when (> (string-width (nth iterator text-lns)) text-ln-max)
                (setq text-ln-max (string-width (nth iterator text-lns))))
            (setq iterator (+ iterator 1)))

        ;; Add padding to lines and print.
        (setq iterator 0)
        (let ((padding (/ (- (sidekick--get-window-width) text-ln-max) 2))
                 (padding-str ""))
            ;; Generate padding string.
            (while (< iterator padding)
                (setq padding-str (concat padding-str " "))
                (setq iterator (+ iterator 1)))

            ;; Insert each line, with generated padding.
            (setq iterator 0)
            (while (< iterator (length text-lns))
                (insert (concat padding-str (nth iterator text-lns) "\n"))
                (setq iterator (+ iterator 1))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Sidekick Update Functions ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun sidekick--update-footer()
    "Draws the logo and version number at the bottom of the Sidekick buffer."
    (unless sidekick-window-hide-footer
        (sidekick-draw-separator)
        (let ((logo-str (concat
                            "------------ ----    ----\n"
                            "************ ****   ****\n"
                            "----         ----  ----\n"
                            "************ *********    Version: 1.0.0\n"
                            "------------ ---------    --------------\n"
                            "       ***** ****  ****\n"
                            "------------ ----   ----\n"
                            "************ ****    ****")))
            (sidekick-draw-text-centerd logo-str))
        (sidekick-draw-separator)))

(defun sidekick--escape-shell-command-string(command-string)
    "Escape special characters inside the provided shell command string.
COMMAND-STRING The shell command."
    (replace-regexp-in-string "\\$" "\\$" command-string nil t))

(defun sidekick--update-eldoc(symbol-str buffer-fn project-dir mode-str)
    "Find all files containing the symbol, or term.

Will use grep to search the current projects for files
containing a symbol.  These files must match the mode's glob
pattern.

SYMBOL-STR The symbol without text properties, string format.
BUFFER-FN The buffers files name.
PROJECT-DIR The projects root directory path.
MODE-STR The mode name as a string."
    (sidekick-draw-section-heading "Documentation")

    ;; TODO: Check if eldoc is enabled first.

    ;; Insert eldoc buffer output.
    (let ((doc-buf (eldoc-doc-buffer)))
        (when doc-buf
            (insert-buffer-substring doc-buf)))
    (insert "\n\n"))

(defun sidekick--update-project-matches(symbol-str buffer-fn project-dir mode-str)
    " Finds symbol in all associated files.

Will use grep to search the current projects for files containing
a symbol. The files that are searched depends on the file
extensions associated with the current mode.

SYMBOL-STR The symbol without text properties, string format.
BUFFER-FN The buffers files name.
PROJECT-DIR The projects root directory path.
MODE-STR The mode name as a string."
    (sidekick-draw-section-heading "Matches in associated files")

    ;; - Get the list of files that contains the search terms.
    (let* ((grep-cmd (mapconcat 'identity (list
                                              "grep"
                                              "--recursive"

                                              ;; Make make searches case insensitive.
                                              (when sidekick-search-case-insensitive
                                                  "--ignore-case")

                                              ;; Use mmap(2) instead of read(2)
                                              ;; to read input, which can result
                                              ;; in better performance under
                                              ;; some circumstances but can
                                              ;; cause undefined behaviour.
                                              "--mmap"

                                              ;; Interpret pattern as fixed strings.
                                              "--fixed-strings"

                                              ;; Treat files as text.
                                              "--text"

                                              ;; Only return file names.
                                              "--files-with-matches"

                                              ;; Adds zero byte after each file
                                              ;; name, required to create file
                                              ;; list.
                                              "--null"

                                              ;; Exclude binary files.
                                              "--binary-files=without-match"

                                              ;; TODO: Implement excluded
                                              ;; directories option.
                                              "--exclude-dir='.git'"
                                              ;; "--exclude-dir='vendor'"

                                              ;; Let's build our include paths.
                                              (mapconcat
                                                  (lambda(s) (format "--include='*.%s'" s))
                                                  (cdr (assoc mode-str sidekick--mode-file-associations)) " ")

                                              ;; The search string.
                                              ;; TODO: Escape single quotes here.
                                              (concat "'" symbol-str "'")

                                              ;; The project directory.
                                              project-dir) " "))
              (results (shell-command-to-string grep-cmd))
              (resulting-files (if (length< results 1)
                                   nil
                                   (split-string results "\0")))
              (string-not-empty-p (lambda (s) (not (string-empty-p s))))

              ;; TODO: Add file limit option.
              ;; Lets set a limit for now.
              (files (take sidekick-search-file-limit (seq-filter string-not-empty-p resulting-files)))
              (grouped-files '()))

        ;; Group files based on their extensions.
        (dolist (file files)
            (let ((file-ext (file-name-extension file)))
                (if (assoc file-ext grouped-files)
                    (push file (cdr (assoc file-ext grouped-files)))
                    (push (cons file-ext (list file)) grouped-files))))

        ;; NOTE: Maybe we need to sort out list.
        (print grouped-files)
        (print grep-cmd)

        ;; No we create a single buffer for each file type.
        (dolist (group grouped-files)
            (let* ((ext (car group))
                      (ext-files (cdr group))
                      (ext-files-count (length ext-files))
                      (ext-files-count-str (if (= ext-files-count 1)
                                               (concat " (" (int-to-string ext-files-count) " File)")
                                               (concat " (" (int-to-string ext-files-count) " Files)")))
                      (match-mode-p (lambda (r k) (if (or (not r) (not k)) nil (string-match r (concat "." k)))))
                      (mode (cdr (assoc ext auto-mode-alist match-mode-p)))
                      (mode-name-p (lambda (s) (capitalize (string-join (butlast (string-split s "-")) " ")))))

                ;; Insert mode heading.
                (goto-char (point-max))
                (if mode
                    (sidekick-draw-section-sub-heading (concat (funcall mode-name-p (symbol-name mode)) ext-files-count-str))
                    (sidekick-draw-section-sub-heading ext))

                ;; Performance is trash, think of better options.
                (dolist (file ext-files)
                    (insert (with-temp-buffer
                                ;; Make sure the search results are case insensitive (keep lines).
                                (when sidekick-search-case-insensitive
                                    (setq-default search-upper-case nil))

                                ;; Insert our file contents.
                                (insert-file-contents file)

                                ;; Delete un-wanted whitespace.
                                (delete-whitespace-rectangle (point-min) (point-max))

                                ;; Insert line numbers
                                (rectangle-number-lines (point-min) (point-max) 1 "%s: ")

                                ;; Remove lines that are of no interest.
                                (keep-lines symbol-str (point-min) (point-max))

                                ;; Insert file path.
                                (goto-char (point-min))
                                (insert file "\n")

                                ;; Insure empty lines.
                                (goto-char (point-max))
                                (delete-blank-lines)
                                (buffer-string)))
                    (insert "\n"))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Sidekick Functionality ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun sidekick--update(symbol symbol-str buffer-fn project-dir mode-str)
    "Update the Sidekick panel with symbol related information.

This function handle the entire Sidekick update procedure.

SYMBOL The raw symbol from `thing-at-point'.
SYMBOL-STR The symbol without text properties, string format.
BUFFER-FN The buffers files name.
PROJECT-DIR The projects root directory path.
MODE-STR The mode name as a string."
    ;; Remove clean state.
    (setq sidekick--state-clean nil)

    ;; Allows us to ignore other update calls.
    (setq sidekick--state-updating t)

    ;; Keep copy of previous argument states.
    (setq sidekick--state-buffer-name buffer-fn)
    (setq sidekick--state-buffer-file-name buffer-fn)
    (setq sidekick--state-symbol symbol)
    (setq sidekick--state-symbol-str symbol-str)
    (setq sidekick--state-mode-str mode-str)
    (setq sidekick--state-project-dir project-dir)

    (let ((sidekick-buf (get-buffer sidekick--buffer-name)))
        ;; If there's a buffer, we need to clear it.
        (when sidekick-buf
            (with-current-buffer sidekick--buffer-name
                (progn (setq buffer-read-only nil) (erase-buffer))))

        ;; If there's no buffer, create one.
        (unless sidekick-buf
            (setq sidekick-buf (get-buffer-create sidekick--buffer-name)))

        ;; Handle window creation.
        ;; (sidekick--handle-window-creation sidekick-buf)
        )

    ;; Perform sidekick buffer operations.
    (with-current-buffer sidekick--buffer-name
        (progn
            (sidekick--deconstruct)
            (erase-buffer)
            ;; TODO: Check if eldoc is enabled first.

            ;; TODO: Add xref.

            ;; (sidekick--update-symbol-occur
            ;;     symbol-str buffer-fn project-dir mode-str)

            ;; (sidekick--update-symbol-references
            ;;     symbol-str buffer-fn project-dir mode-str)

            ;; TODO: add more features here.
            ;; TODO: Maybe remove files.
            ;; (sidekick--update-symbol-files
            ;;     symbol-str buffer-fn project-dir mode-str)

            ;; Show documentation.
            ;; (sidekick--update-eldoc
            ;;     symbol-str buffer-fn project-dir mode-str)

            ;; Grep solution.
            ;; (sidekick--update-grep-symbol
            ;;     symbol-str buffer-fn project-dir mode-str)

            ;; (type-of (thing-at-point 'symbol))
            ;; (message (type-of symbol-str))

            ;; Find matching symbols.
            (sidekick--update-project-matches
                symbol-str
                buffer-fn
                project-dir
                mode-str)

            (sidekick--update-footer)
            (goto-char (point-min))
            (sidekick-mode)
            (highlight-regexp (regexp-quote symbol-str) 'match)
            ))

    ;; If we're not inside the Sidekick window, and take focus is non-nil,
    ;; select it.
    (unless (string= (buffer-name) sidekick--buffer-name)
        (switch-to-buffer-other-window sidekick--buffer-name))

    ;; Enable future update calls.
    (setq sidekick--state-updating nil))

;;;;;;;;;;;;;;;;;;;;;;;
;; Sidekick Triggers ;;
;;;;;;;;;;;;;;;;;;;;;;;

(defun sidekick--should-update(symbol buffer-fn project-dir mode-str)
    "Determines whether or not the sidekick buffer should be updated.

It will return non-nil on success and nil when Sidekick update
can not be performed.  In order for Sidekick to update the
following conditions apply:

1. Buffer must have an associated file.  2. Project directory
needs to be successfully detected.  3. The grep executable
must be available.  4. Sidekick should not already be in the
state of updating.  5. The symbol length must be more then the
minimum specified with `sidekick-search-minimum-symbol-length'.
6. The buffers mode name needs to be inside the
`sidekick--mode-file-associations' list.

SYMBOL The raw symbol from `thing-at-point'.
BUFFER-FN The buffers files name.
PROJECT-DIR The projects root directory path.
MODE-STR The mode name as a string."
    (and
        symbol
        buffer-fn
        project-dir
        ;; (sidekick--get-grep-executable-path) ;; TODO: check for grep path here.
        (not sidekick--state-updating)
        (> (string-width symbol) (if (< sidekick-search-minimum-symbol-length 2) 2
                                     sidekick-search-minimum-symbol-length))
        (length> (cdr (assoc mode-str sidekick--mode-file-associations)) 1)))

(defun sidekick--trigger-update(symbol buffer-fn mode-str)
    "Triggers the Sidekick update procedure.

Wrapper function for `sidekick--update', makes sure that all
prerequisites specified in `sidekick--should-update' has been
met.

SYMBOL The raw symbol from `thing-at-point'.
BUFFER-FN The buffers files name.
MODE-STR The mode name as a string."
    (let ((project-dir (sidekick--get-project-root-path)))
        (when (sidekick--should-update symbol buffer-fn project-dir mode-str)
            (sidekick--update
                symbol
                (substring-no-properties symbol)
                buffer-fn
                project-dir
                mode-str))))

(defun sidekick-focus()
    "Will focus on the Sidekick window, if visible."
    (interactive)
    (let ((sidekick-window (get-buffer-window sidekick--buffer-name)))
        (if sidekick-window
            (select-window sidekick-window)
            (message "Sidekick: window not visible."))))

(defun sidekick-at-point()
    "Takes the symbol at point and triggers a Sidekick update."
    (interactive)
    (sidekick--trigger-update
        (thing-at-point 'symbol)
        (buffer-file-name)
        (symbol-name major-mode)))

(defun sidekick-search-for-literal()
    "Input a literal string and triggers a Sidekick update."
    (interactive)
    (let ((string-lit (read-string "Search (Literal): ")))
        (sidekick--trigger-update
            string-lit
            (buffer-file-name)
            (symbol-name major-mode))))

(provide 'sidekick)

;;; sidekick.el ends here
