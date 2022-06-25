;;; sidekick.el --- A coding assistance panel  -*- lexical-binding: t -*-

;; Copyright (C) 2022 by Vernon Grant.

;; Author: Vernon Grant <vernon@ruppell.io>
;; Version: 1.0.0
;; Package-Requires: ((emacs "26.1"))
;; Keywords: extensions, lisp
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

;; Sidekick is a Emacs extension that's aim is to provide useful information about
;; a symbol inside a single window.

;;; Code:

;; TODO: Escape string for regexUse symbol as literal.

;;;;;;;;;;;;;;;;;;;;
;; Customizations ;;
;;;;;;;;;;;;;;;;;;;;

(defgroup sidekick-search nil
    "Sidekick search settings."
    :group 'convenience
    :prefix "sidekick-search-")

(defcustom sidekick-search-minimum-symbol-length 2
    "The minimum symbol / term length in order for Sidekick to
update."
    :group 'sidekick-search
    :type 'integer)

(defgroup sidekick-window nil
    "Sidekick window settings."
    :group 'convenience
    :prefix "sidekick-window-")

(defcustom sidekick-window-take-focus nil
    " If non-nil, automatically select the sidekick window after
every update."
    :group 'sidekick-window
    :type 'boolean)

(defcustom sidekick-window-width 0.225
    "The width of the Sidekick window in normalized percentage."
    :group 'sidekick-window
    :type 'float)

(defcustom sidekick-window-side 'right
  "The Sidekick window position, left or right."
  :type '(choice
          (const :tag "Left" 'left)
          (const :tag "Right" 'right)))

(defcustom sidekick-window-hide-footer nil
    "Remove the Sidekick footer branding."
    :group 'sidekick-window
    :type 'boolean)

;;;;;;;;;;;;;;;;;;;;;;;
;; Private Variables ;;
;;;;;;;;;;;;;;;;;;;;;;;

(defvar sidekick--mode-file-associations
    `(
         ("c++-mode"        . "*.{cpp,h,hh}")
         ("c-mode"          . "*.{c,cc,h,hh}")
         ("cperl-mode"      . "*.{pl,PL}")
         ("css-mode"        . "*.{css,sass,scss}")
         ("emacs-lisp-mode" . "*.{el,emacs}")
         ("go-mode"         . "*.go")
         ("java-mode"       . "*.java")
         ("js-mode"         . "*.{js,es,es6}")
         ("json-mode"       . "*.json")
         ("markdown-mode"   . "*.md")
         ("php-mode"        . "*.{php,phtml,twig}")
         ("python-mode"     . "*.py")
         ("ruby-mode"       . "*.rb")
         ("rust-mode"       . "*.rs")
         ("typescript-mode" . "*.ts")
         ;; Empty string, signifies using the current file's extension if
         ;; available.
         ("web-mode"        . "")
         ("xml-mode"        . "*.xml")
         ("yaml-mode"       . "*.yml"))
    "Association List holding major mode file associations.")

(defconst sidekick--buffer-name "*sidekick*"
    "The default Sidekick buffer name.")

(defconst sidekick--match-line-number-regexp "^[0-9]+"
    "Regexp used to extract line numbers from ripgrep results.")

(defconst sidekick--match-file-path-regexp "^\\.\\/.+"
    "Regexp used to extract file paths from ripgrep results.")

(defvar sidekick--state-updating nil
    "Is non-nil when an update is in process.")

(defvar sidekick--state-clean t
    "Is non-nil if no update calls were made.")

(defvar sidekick--state-mode-name nil
    "The mode name of the current operation.")

(defvar sidekick--state-project-dir nil
    "The project directory of the current operation.")

(defvar sidekick--state-symbol-str nil
    "The symbol or term string of the current operation.")

(defvar sidekick--state-buffer-name nil
    "The buffer name of the current operation.")

(defvar sidekick--state-buffer-file-name nil
    "The buffer file name of the current operation.")

(defvar sidekick--state-previous-buffer-window nil
    "Holds the previous buffer's window, so focus can be toggled.")

;;;;;;;;;;;;;;;;;;;
;; Sidekick Mode ;;
;;;;;;;;;;;;;;;;;;;

(defvar sidekick-mode-hook nil
    "List of functions to call on sidekick mode activation.")

(defvar sidekick-mode-local-map
    (let ((map (make-sparse-keymap)))
        (define-key map "q" 'sidekick-quit)
        (define-key map "g" 'sidekick-refresh)
        (define-key map "n" 'sidekick-open-next-match)
        (define-key map "p" 'sidekick-open-previous-match)
        (define-key map (kbd "RET") 'sidekick-open-match)
        map)
    "The local key map used for sidekick-mode.")

;; TODO: fix syntax issues.
(defvar sidekick-mode-fonts
    '(
         ;; Headings.
         ("^-\\{2\\}|\s?" . 'font-lock-comment-face)
         ("^-\\{2\\}|\s.*\\(|-+\\)" (1 font-lock-comment-face))
         ("^-\\{2\\}|\s\\(.*\\)\s|-+" (1 font-lock-function-name-face))

         ;; File path.
         ("^\\./.*" . 'xref-file-header)

         ;; Line numbers.
         ("^[0-9]+:" . 'xref-line-number)

         ;; Footer sections.
         ("^-\\{3\\}+" . font-lock-comment-face)
         ("^[-\s]\\{2\\}+\s+[-]+" . font-lock-comment-face)
         ("^[*\s]\\{2\\}+\s+[*]+" . font-lock-comment-face)
         ("^[*\s]+[\s]+\\([a-zA-Z]+[:]\s[0-9\\.]+\\)" (1 font-lock-keyword-face)))
    "Defines the font lock defaults for sidekick-mode.")

(defun sidekick--deconstruct()
    "Clears Sidekick mode related variables."
    (setq buffer-read-only nil)
    (kill-all-local-variables))

(define-derived-mode sidekick-mode fundamental-mode "Sidekick"
    "Enables sidekick mode. Only used inside the Sidekick panel."
    (progn
        (setq buffer-read-only t)
        (setq font-lock-defaults '(sidekick-mode-fonts))
        (setq mode-line-format nil)
        (use-local-map sidekick-mode-local-map)
        (display-line-numbers-mode 0)
        (run-hooks 'sidekick-mode-hook)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Sidekick Keymap Interactions ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun sidekick--match-get-line-and-path()
    "Get's the line number and file path of a match inside of the
Sidekick Window."
    (let ((match-line-num nil)
             (match-file-path nil))
        ;; Extract the line number of the match.
        (when (string-match sidekick--match-line-number-regexp (thing-at-point 'line t))
            (setq match-line-num
                (string-to-number (match-string 0 (thing-at-point 'line t)))))

        ;; NOTE: Optimize this in future release.
        ;; Find and extract the match's file path.
        (save-excursion
            ;; Will move up until a file path is found.
            (unless (string-match sidekick--match-file-path-regexp (thing-at-point 'line t))
                (while (and (> (string-width (thing-at-point 'line t)) 0)
                           (> (line-number-at-pos (point)) 1))
                    (previous-line))
                (next-line))

            (when (string-match sidekick--match-file-path-regexp (thing-at-point 'line t))
                (setq match-file-path  (buffer-substring-no-properties
                                           (line-beginning-position)
                                           (line-end-position)))))
        (list match-line-num match-file-path)))

(defun sidekick--match-setup-buffer(match-file-path)
    "Finds the match's file and sets up it's buffer in the background."
    (interactive)
        (let ((match-buffer nil))
            (if match-file-path
                (setq match-buffer (find-file-noselect match-file-path nil nil))
                (setq match-buffer (find-file-noselect sidekick--state-buffer-file-name nil nil)))

            (unless match-buffer
                (message "Sidekick: Match file or buffer could not be found."))
            match-buffer))

(defun sidekick--match-buffer-operations(match-buffer match-line-num)
    "Perform default match highlighting, centering operations."
    ;; If line number is non-nil, find match.
    (when match-line-num
        (with-selected-window (get-buffer-window match-buffer)
            (progn
                (beginning-of-line)
                (goto-line match-line-num)
                (re-search-forward sidekick--state-symbol-str nil t)
                (re-search-backward sidekick--state-symbol-str nil t)
                (recenter))))

    ;; If line number is nil, then this must be a file path so just go to the
    ;; beginning of file.
    (unless match-line-num
        (with-selected-window (get-buffer-window match-buffer)
            (progn (beginning-of-buffer)))))

(defun sidekick-quit()
    "Closes the sidekick window and kills it's buffer."
    (interactive)
    ;; NOTE: Might need to do some cleanup here.
    (quit-window)
    (kill-buffer sidekick--buffer-name))

(defun sidekick-open-previous-match()
    "Displays the previous match, creating and opening it's buffer."
    (interactive)
    (when (string= (buffer-name) sidekick--buffer-name)
        (previous-line)
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

(defun sidekick-open-next-match()
    "Displays the next match, creating and opening it's buffer."
    (interactive)
    (when (string= (buffer-name) sidekick--buffer-name)
        (next-line)
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

(defun sidekick-refresh()
    "Reruns the previous operations, refreshing the results."
    (interactive)
    (unless sidekick--state-clean
        (sidekick--trigger-update
            sidekick-state-symbol
            sidekick--state-buffer-file-name
            sidekick--state-mode-name)
        (message "Sidekick: refreshed!")))

(defun sidekick-open-match()
    "Go's directly to the match's symbol, creating a buffer if needed."
    (interactive)
    (when (string= (buffer-name) sidekick--buffer-name)
        (let ((match-line-and-path (sidekick--match-get-line-and-path)))
            (let ((match-buffer (sidekick--match-setup-buffer (nth 1 match-line-and-path)))
                     (match-line-num (nth 0 match-line-and-path)))
                ;; Display the matches buffer.
                (switch-to-buffer match-buffer)

                ;; Go to line number.
                (sidekick--match-buffer-operations match-buffer match-line-num)))))

;;;;;;;;;;;;;;;;;;;;;;;;
;; Sidekick Utilities ;;
;;;;;;;;;;;;;;;;;;;;;;;;

(defun sidekick--get-rg-executable-path()
    "Get the Ripgrep (rg) executable path."
    ;; TODO: Implement operating system specific solutions.
    (let ((rg-exe (executable-find "rg")))
        (if (not rg-exe)
            (progn
                (error "Sidekick: Ripgrep (rg) executable could not be found.") nil)
            rg-exe)))

(defun sidekick--get-project-root-path()
    "Get the root path to the current project."
    (let ((dir default-directory))
        (let ((project-root (or (locate-dominating-file dir ".sidekick")
                                (locate-dominating-file dir ".projectile")
                                (locate-dominating-file dir ".git"))))
            (if project-root
                project-root
                (progn (error "Sidekick: Project root directory not found.") nil)))))

(defun sidekick--get-window-width()
    "Get the Sidekick window width in columns."
    (let ((sidekick-win (nth 0 (get-buffer-window-list sidekick--buffer-name))))
        (if sidekick-win
            (window-width sidekick-win) (+ 80))))

(defun sidekick--extract-supported-modes()
    "Extracts all supported modes from sidekick--mode-file-associations."
    (let ((iterator 0)
             (modes-list nil))
        (while (< iterator (length sidekick--mode-file-associations))
            (let ((mode (nth iterator sidekick--mode-file-associations)))
                (add-to-list 'modes-list (car mode) t))
            (setq iterator (+ iterator 1)))
        modes-list))

(defun sidekick--get-mode-file-glob-pattern(mode-name buffer-fn)
    "Returns the glob pattern for a supported major mode."
    (let ((iterator 0)
             (glob-pat ""))
        (while (< iterator (length sidekick--mode-file-associations))
            (let ((mode (nth iterator sidekick--mode-file-associations)))
                (when (string= (car mode) mode-name)
                    ;; If the globs pattern is empty, just use the current
                    ;; buffers extension.
                    (if (= (length (cdr mode)) 0)
                        (setq glob-pat (concat "*." (file-name-extension buffer-fn nil)))
                        (setq glob-pat (cdr mode)))
                    (setq iterator (length sidekick--mode-file-associations))))
            (setq iterator (+ iterator 1)))
        glob-pat))

(defun sidekick--handle-window-creation(buf)
    "Handles the creation of the Sidekick window."
    (unless (get-buffer-window buf t)
        ;; Define default window properties.
        (let ((buf-window-alist `(
                                     (dedicated . t)
                                     (slot . -1)
                                     (side . left)
                                     (window-width . 0.25)
                                     (window-parameters . ((no-delete-other-windows . t))))))

            ;; Set custom window properties.
            (setf (alist-get 'window-width buf-window-alist) sidekick-window-width)
            (setf (alist-get 'side buf-window-alist) sidekick-window-side)
            (display-buffer-in-side-window buf buf-window-alist))))

(defun sidekick-set-file-associations(mode-name globs)
    "Add new, or update existing major mode file associations."
    (let ((mode-alist (assoc mode-name sidekick--mode-file-associations)))
        (if mode-alist
            ;; Update existing alist if the mode does exist.
            (setcdr (assoc (car mode-alist) sidekick--mode-file-associations) globs)
            ;; Push new alist if the mode does not exist.
            (push (list mode-name globs) sidekick--mode-file-associations))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Sidekick User Interface ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun sidekick-draw-section-heading(heading)
    "Draws a heading separator inside the active buffer."
    (let ((iterator 0)
             (heading-fnl (concat "--| " heading " |")))
        (insert heading-fnl)
        (while (< iterator
                   (- (sidekick--get-window-width) (string-width heading-fnl)))
            (progn
                (setq iterator (+ iterator 1))
                (insert "-"))))
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
    "Centers text inside the Sidekick window."
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
        (let ((padding (- (sidekick--get-window-width) text-ln-max))
                 (padding-str ""))

            ;; Generate padding string.
            (dotimes (number (/ padding 2))
                (setq padding-str (concat padding-str " ")))

            (while (< iterator (length text-lns))
                (insert (concat padding-str (nth iterator text-lns) "\n"))
                (setq iterator (+ iterator 1))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Sidekick Update Functions ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun sidekick--update-footer(symbol symbol-str buffer-fn project-dir mode-name)
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

(defun sidekick--get-ripgrep-output-string(args symbol-str path mode-name buffer-fn)
    "Runs ripgrep and returns a string containing the output."
    (let ((symbol-lit (concat "'" symbol-str "'"))
             (glob-pat (concat
                           "--glob="
                           (sidekick--get-mode-file-glob-pattern mode-name buffer-fn))))
        (let ((rg-cmd (mapconcat 'identity
                          (list
                              (sidekick--get-rg-executable-path)
                              args glob-pat symbol-lit path) " ")))
            (shell-command-to-string rg-cmd))))

(defun sidekick--update-symbol-occur(symbol symbol-str buffer-fn project-dir mode-name)
    "Shows all occurrences of symbol, or term in the current active buffer."
    (sidekick-draw-section-heading "In Buffer")
    (cd project-dir)
    (insert (sidekick--get-ripgrep-output-string
                "-n --no-heading --no-filename --trim"
                symbol-str
                buffer-fn
                mode-name
                buffer-fn))
    (insert "\n"))

(defun sidekick--update-symbol-references(symbol symbol-str buffer-fn project-dir mode-name)
    "Shows all references to a symbol, or term inside a project directory."
    ;; TODO: Exclude the current buffers file.
    (sidekick-draw-section-heading "In Project")
    (cd project-dir)
    (insert (sidekick--get-ripgrep-output-string
                "-n --heading --trim"
                symbol-str
                "./"
                mode-name
                buffer-fn))
    (insert "\n"))

(defun sidekick--update-symbol-files(symbol symbol-str buffer-fn project-dir mode-name)
    "Find all files containing the symbol, or term."
    (sidekick-draw-section-heading "Files")
    (cd project-dir)
    (insert (sidekick--get-ripgrep-output-string
                "-l"
                symbol-str
                "./"
                mode-name
                buffer-fn))
    (insert "\n"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Sidekick Functionality ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun sidekick--update(symbol symbol-str buffer-fn project-dir mode-name)
    "Updates the Sidekick panel with symbol, or term related information."
    ;; Remove clean state.
    (setq sidekick--state-clean nil)
    ;; Allows us to ignore other update calls.
    (setq sidekick--state-updating t)
    ;; Set state, previous buffer.
    (setq sidekick--state-buffer-name buffer-fn)
    (setq sidekick--state-buffer-file-name buffer-fn)
    ;; Set state, symbol.
    (setq sidekick-state-symbol symbol-str)
    (setq sidekick--state-symbol-str symbol-str)
    ;; Set state project and mode-name.
    (setq sidekick--state-mode-name mode-name)
    (setq sidekick--state-project-dir project-dir)

    (let ((sidekick-buf (get-buffer sidekick--buffer-name)))
        ;; If there is a buffer, we need to clear it.
        (when sidekick-buf
            (with-current-buffer sidekick--buffer-name
                (progn (setq buffer-read-only nil) (erase-buffer))))

        ;; If there's no buffer, create one.
        (unless sidekick-buf
            (setq sidekick-buf (get-buffer-create sidekick--buffer-name)))

        ;; Handle window creation.
        (sidekick--handle-window-creation sidekick-buf))

    ;; Perform sidekick buffer operations.
    (with-current-buffer sidekick--buffer-name
        (progn
            (sidekick--deconstruct)
            (erase-buffer)
            (sidekick--update-symbol-occur
                symbol symbol-str buffer-fn project-dir mode-name)
            (sidekick--update-symbol-references
                symbol symbol-str buffer-fn project-dir mode-name)
            (sidekick--update-symbol-files
                symbol symbol-str buffer-fn project-dir mode-name)
            (sidekick--update-footer
                symbol symbol-str buffer-fn project-dir mode-name)
            (goto-char (point-min))
            (sidekick-mode)
            (highlight-regexp symbol 'highlight)))

    ;; If we're not inside the Sidekick window, and take focus is non-nil,
    ;; select it.
    (unless (string= (buffer-name) sidekick--buffer-name)
        (when sidekick-window-take-focus
            (switch-to-buffer-other-window sidekick--buffer-name)))

    ;; Enable future update calls.
    (setq sidekick--state-updating nil))

;;;;;;;;;;;;;;;;;;;;;;;
;; Sidekick Triggers ;;
;;;;;;;;;;;;;;;;;;;;;;;

(defun sidekick--should-update(symbol buffer-fn project-dir mode-name)
    "Determines whether or not the sidekick buffer should be updated."
    (and
        symbol
        buffer-fn
        project-dir
        (sidekick--get-rg-executable-path)
        (not sidekick--state-updating)
        ;; Clamps the symbol length to a minimum of two.
        (> (string-width symbol) (if (< sidekick-search-minimum-symbol-length 2) 2
        sidekick-search-minimum-symbol-length))
        (member mode-name (sidekick--extract-supported-modes))))

(defun sidekick--trigger-update(symbol buffer-fn mode-name)
    "Triggers the Sidekick update procedure."
    (let ((project-dir (sidekick--get-project-root-path)))
        (when (sidekick--should-update symbol buffer-fn project-dir mode-name)
            (sidekick--update
                symbol
                (substring-no-properties symbol)
                buffer-fn
                project-dir
                mode-name))))

(defun sidekick-focus()
    "Will focus on the Sidekick window, if visible."
    (interactive)
    (let ((sidekick-window (get-buffer-window sidekick--buffer-name)))
        (if sidekick-window
            (select-window sidekick-window)
            (message "Sidekick: window not visible."))))

(defun sidekick-focus-toggle()
    "Will toggle between sidekick window and previous buffer
window, if visible."
    (interactive)
    (if (string= (buffer-name) sidekick--buffer-name)
        (if sidekick--state-previous-buffer-window
            (select-window sidekick--state-previous-buffer-window)
            (other-window))
        (progn
            (setq sidekick--state-previous-buffer-window
                (get-buffer-window (buffer-name)))
            (sidekick-focus))))

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
