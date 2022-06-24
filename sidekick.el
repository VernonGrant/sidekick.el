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

;;;;;;;;;;;;;;;;;;;
;; Sidekick Mode ;;
;;;;;;;;;;;;;;;;;;;

;; TODO: Add method that will focus on Sidekick window if available.
;; TODO: Add all missing documentation.
;; TODO: Implement defcustom where needed.
;; TODO: Add case insensitive option.
;; TODO: Escape regexp characters from search string.
;; TODO: Highlight or flash marching term.

;; User options.
(defvar sidekick-min-symbol-length 2 "The minimum allowed symbol length.")
(defvar sidekick-take-focus nil)
(defvar sidekick-window-width 0.25 "The width of the sidekick window in fractional percentage.")
(defvar sidekick-window-side 'right "The Sidekick window's position, left or right.")
(defvar sidekick-hide-footer nil)

;; Internal variables.
(defvar sidekick-mode-hook nil "*List of functions to call when entering Sidekick mode.")

;; Temp:
(setq sidekick-min-symbol-length 2)
(setq sidekick-window-side 'right)
(setq sidekick-window-width 0.2)
(setq sidekick-take-focus t)
(setq sidekick-hide-footer nil)

(defvar sidekick-mode-file-associations `(
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
                                             ("web-mode"        . "")
                                             ("xml-mode"        . "*.xml")
                                             ("yaml-mode"       . "*.yml")))

;; Private variables
(defconst sidekick-buffer-name "*sidekick*")
(defconst sidekick-match-line-number-reg "^[0-9]+")
(defconst sidekick-match-file-path-reg "^\\.\\/.+")

;; State keeping.
(defvar sidekick-state-updating nil "non-nil when sidekick is updating.")
(defvar sidekick-state-clean t)
(defvar sidekick-state-mode-name nil)
(defvar sidekick-state-project-dir nil)
(defvar sidekick-state-symbol-str nil)
(defvar sidekick-state-buffer-name nil)
(defvar sidekick-state-buffer-fn nil)

;; Mode keymap.
(defvar sidekick-mode-map
    (let ((map (make-sparse-keymap)))
        (define-key map "q" 'sidekick-quit)
        (define-key map "g" 'sidekick-refresh)
        (define-key map "n" 'sidekick-open-next-match)
        (define-key map "p" 'sidekick-open-previous-match)
        (define-key map (kbd "RET") 'sidekick-open-match)
        (define-key map "o" 'sidekick-open-match-other-window)
        map))

(setq sidekick-highlights
    '(
         ;; Heading sections.
         ("^-\\{2\\}|\s?" . 'font-lock-comment-face)
         ("^-\\{2\\}|\s.*\\(|-+\\)" (1 font-lock-comment-face))
         ("^-\\{2\\}|\s\\(.*\\)\s|-+" (1 font-lock-function-name-face))

         ;; Footer sections.
         ("^-\\{3\\}+" . font-lock-comment-face)
         ("^[-\s]\\{2\\}+\s+[-]+" . font-lock-comment-face)
         ("^[*\s]\\{2\\}+\s+[*]+" . font-lock-comment-face)
         ("^[*\s]+[\s]+\\([a-zA-Z]+[:]\s[0-9\\.]+\\)" (1 font-lock-keyword-face))

         ;; File path.
         ("^\\./.*" . 'xref-file-header)

         ;; Line numbers.
         ("^[0-9]+:" . 'xref-line-number)))

(defun sidekick--deconstruct()
    "Clears Sidekick mode related variables."
    (setq buffer-read-only nil)
    (kill-all-local-variables))

(define-derived-mode sidekick-mode fundamental-mode "Sidekick"
    "Enables sidekick mode. Only used inside the auto generated Sidekick panel."
    ;; TODO: Setup mode hooks.
    (progn
        (setq buffer-read-only t)
        (setq font-lock-defaults '(sidekick-highlights))
        (display-line-numbers-mode 0)
        (use-local-map sidekick-mode-map)
        (run-hooks 'sidekick-mode-hook)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Sidekick Keymap Interactions ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun sidekick--match-get-line-and-path()
    "Get's the line number and file path of a match inside of the Sidekick Window."
    (let ((match-line-num nil)
             (match-file-path nil))
        ;; get the match's line number.
        (when (string-match sidekick-match-line-number-reg (thing-at-point 'line t))
            (setq match-line-num
                (string-to-number (match-string 0 (thing-at-point 'line t)))))

        ;; get the match's file path.
        (save-excursion
            ;; NOTE: This can be optimized.
            ;; If not on a file path, move up until one is found.
            (unless (string-match sidekick-match-file-path-reg (thing-at-point 'line t))
                (while (and (> (string-width (thing-at-point 'line t)) 0)
                           (> (line-number-at-pos (point)) 1))
                    (previous-line))
                (next-line))

            (when (string-match sidekick-match-file-path-reg (thing-at-point 'line t))
                (setq match-file-path  (buffer-substring-no-properties
                                           (line-beginning-position)
                                           (line-end-position)))))

        (list match-line-num match-file-path)))

(defun sidekick--match-setup-buffer(match-file-path)
    "Sets up a matches buffer in the background."
    (interactive)
        (let ((match-buffer nil))
            (if match-file-path
                (setq match-buffer (find-file-noselect match-file-path nil nil))
                (setq match-buffer (find-file-noselect sidekick-state-buffer-fn nil nil)))

            (unless match-buffer
                (error "Sidekick: Match buffer setup failed."))
            match-buffer))

(defun sidekick--match-buffer-operations(match-buffer match-line-num)
    "Handles operations that take place inside the match's buffer."
    ;; If line number is non-nil, find match and position cursor.
    (when match-line-num
        (with-selected-window (get-buffer-window match-buffer)
            (progn
                (goto-line match-line-num)
                ;; TODO: Escape string for regexUse symbol as literal
                (re-search-forward sidekick-state-symbol-str nil t)
                (re-search-backward sidekick-state-symbol-str nil t)
                (recenter-top-bottom 0))))

    ;; If line number is nil, just go to the beginning of file.
    (unless match-line-num
        (with-selected-window (get-buffer-window match-buffer)
            (progn (beginning-of-buffer)))))

(defun sidekick-quit()
    "Closes the sidekick window and kills it's related buffer."
    (interactive)
    ;; NOTE: Might need to do some cleanup here.
    (quit-window)
    (kill-buffer sidekick-buffer-name))

(defun sidekick-open-previous-match()
    "Displays the previous match, opening it's related buffer and
moving to the matching symbol."
    (interactive)
    (when (string= (symbol-name major-mode) "sidekick-mode")
        ;; Go to previous line.
        (previous-line)

        (let ((match-line-and-path (sidekick--match-get-line-and-path)))
            (let ((match-buffer (sidekick--match-setup-buffer (nth 1 match-line-and-path)))
                     (match-line-num (nth 0 match-line-and-path)))
                ;; Display the matches buffer.
                (display-buffer-use-some-window match-buffer `())

                ;; Handle match buffer operations.
                (when match-line-num
                    (sidekick--match-buffer-operations match-buffer match-line-num))))))

(defun sidekick-open-next-match()
    "Displays the next match, opening it's related buffer and
moving to the matching symbol."
    (interactive)
    (when (string= (symbol-name major-mode) "sidekick-mode")
        ;; Go to next line.
        (next-line)
        (let ((match-line-and-path (sidekick--match-get-line-and-path)))
            (let ((match-buffer (sidekick--match-setup-buffer (nth 1 match-line-and-path)))
                     (match-line-num (nth 0 match-line-and-path)))

                ;; Display the matches buffer.
                (display-buffer-use-some-window match-buffer `())

                ;; Handle match buffer operations.
                (when match-line-num
                    (sidekick--match-buffer-operations match-buffer match-line-num))))))

(defun sidekick-refresh()
    "Reruns the previous Sidekick operations, refreshing the results."
    (interactive)
    (unless sidekick-state-clean
        (sidekick--trigger-update
            sidekick-state-symbol
            sidekick-state-buffer-fn
            sidekick-state-mode-name)
        (message "Sidekick: refreshed!")))

(defun sidekick-open-match()
    "Go's directly to the match's symbol, creating a buffer if needed."
    (interactive)
    (when (string= (symbol-name major-mode) "sidekick-mode")
        (let ((match-line-and-path (sidekick--match-get-line-and-path)))
            (let ((match-buffer (sidekick--match-setup-buffer (nth 1 match-line-and-path)))
                     (match-line-num (nth 0 match-line-and-path)))
                ;; Display the matches buffer.
                (switch-to-buffer match-buffer)

                ;; Go to line number.
                (sidekick--match-buffer-operations match-buffer match-line-num)))))

(defun sidekick-open-match-other-window()
    "Creates the match's buffer in other window and go's to the
symbol if given, else go's to beginning of file."
    (interactive)
    (when (string= (symbol-name major-mode) "sidekick-mode")
        (let ((match-line-and-path (sidekick--match-get-line-and-path)))
            (let ((match-buffer (sidekick--match-setup-buffer (nth 1 match-line-and-path)))
                     (match-line-num (nth 0 match-line-and-path)))
                ;; Display the matches buffer.
                (switch-to-buffer-other-window match-buffer)

                ;; Go to line number.
                (sidekick--match-buffer-operations match-buffer match-line-num)))))

;;;;;;;;;;;;;;;;;;;;;;;;
;; Sidekick Utilities ;;
;;;;;;;;;;;;;;;;;;;;;;;;

(defun sidekick--get-rg-executable-path()
    "Get the Ripgrep (rg) executable path."
    (let ((rg-exe (executable-find "rg")))
        (if (not rg-exe)
            (progn
                (error "Sidekick: Ripgrep (rg) executable could not be found.") nil)
            rg-exe)))

(defun sidekick--get-project-root-path()
    "Get the root path to the current project."
    (let ((dir default-directory))
        (let ((project-root (or
                                (locate-dominating-file dir ".sidekick")
                                (locate-dominating-file dir ".projectile")
                                (locate-dominating-file dir ".git"))))
        (if (not project-root)
            (progn
                (error "Sidekick: Project root directory not found.") nil)
            project-root))))

(defun sidekick--get-window-width()
    "Get the Sidekick window with in columns."
    (let ((sidekick-win (nth 0 (get-buffer-window-list sidekick-buffer-name))))
        (if sidekick-win
            (window-width sidekick-win) (+ 80))))

(defun sidekick--extract-supported-modes()
    "Extracts all supported modes from the mode, file associations variable."
    (let ((iterator 0)
             (modes-list nil))
        (while (< iterator (length sidekick-mode-file-associations))
            (let ((mode (nth iterator sidekick-mode-file-associations)))
                (add-to-list 'modes-list (car mode) t))
            (setq iterator (+ iterator 1)))
        modes-list))

(defun sidekick--get-mode-file-glob-pattern(mode-name buffer-fn)
    "Returns the glob pattern for a supported major mode."
    (let ((iterator 0)
             (glob-pat ""))
        (while (< iterator (length sidekick-mode-file-associations))
            (let ((mode (nth iterator sidekick-mode-file-associations)))
                (when (string= (car mode) mode-name)
                    ;; If the globs pattern is empty, just use the current
                    ;; buffers extension.
                    (if (= (length (cdr mode)) 0)
                        (setq glob-pat (concat "*." (file-name-extension buffer-fn nil)))
                        (setq glob-pat (cdr mode)))
                    (setq iterator (length sidekick-mode-file-associations))))
            (setq iterator (+ iterator 1)))
        glob-pat))

(defun sidekick--handle-window-creation(buf)
    "Handles the creation of the Sidekick window or frame"
    (unless (get-buffer-window buf t)
        (let ((buf-window-alist `( (dedicated . t)
                                     (slot . 0)
                                     (side . left)
                                     (window-width . 0.25)
                                     (window-parameters . ((no-delete-other-windows . t))))))

            ;; Create a window.
            (setf (alist-get 'window-width buf-window-alist) sidekick-window-width)
            (setf (alist-get 'side buf-window-alist) sidekick-window-side)
            (display-buffer-in-side-window buf buf-window-alist))))

(defun sidekick-set-file-associations(mode-name globs)
    "Set"
    (let ((mode-alist (assoc mode-name sidekick-mode-file-associations)))
        ;; Push new alist if the mode does not exist.
        (unless mode-alist
            (push (list mode-name globs) sidekick-mode-file-associations))
        ;; Update existing alist if the mode does exist.
        (when mode-alist
            (setcdr (assoc (car mode-alist) sidekick-mode-file-associations) globs))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Sidekick User Interface ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun sidekick-draw-section-heading(heading)
    "Draws a new heading separator inside the active buffer."
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
    "Draws a new heading separator inside the active buffer."
    (let ((iterator 0))
        (while (< iterator (sidekick--get-window-width))
            (progn
                (setq iterator (+ iterator 1))
                (insert "-"))))
    (insert "\n"))

(defun sidekick-draw-text-centerd(text)
    "Center a string inside the Sidekick window."
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
    (unless sidekick-hide-footer
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
    "Runs Ripgrep and returns a string containing the output."
    (let ((symbol-lit (concat "'" symbol-str "'"))
             (glob-pat (concat
                           "--glob="
                           (sidekick--get-mode-file-glob-pattern mode-name buffer-fn))))
        (let ((rg-cmd (mapconcat 'identity
                          (list
                              (sidekick--get-rg-executable-path)
                              args
                              glob-pat symbol-lit path) " ")))
            (shell-command-to-string rg-cmd))))

(defun sidekick--update-symbol-occur(symbol symbol-str buffer-fn project-dir mode-name)
    "Shows all occurrences of symbol in the current active buffer."
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
    "Shows all references to a symbol inside a project directory."
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
    "Find all files containing the symbol."
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
    "Updates the Sidekick panel with symbol related information."
    ;; Pause future update calls.
    (setq sidekick-state-updating t)

    ;; Remove clean state.
    (setq sidekick-state-clean nil)

    ;; Set state, previous buffer.
    (setq sidekick-state-buffer-name buffer-fn)
    (setq sidekick-state-buffer-fn buffer-fn)

    ;; Set state, symbol.
    (setq sidekick-state-symbol symbol-str)
    (setq sidekick-state-symbol-str symbol-str)

    ;; Set state project and mode-name.
    (setq sidekick-state-mode-name mode-name)
    (setq sidekick-state-project-dir project-dir)

    (let ((sidekick-buf (get-buffer sidekick-buffer-name)))
        ;; If there is a buffer, we need to clear it.
        (when sidekick-buf
            (with-current-buffer sidekick-buffer-name
                (progn
                    (setq buffer-read-only nil)
                    (erase-buffer))))

        ;; If there's no buffer, create one.
        (unless sidekick-buf
            (setq sidekick-buf (get-buffer-create sidekick-buffer-name)))

        ;; Handle window creation,
        (sidekick--handle-window-creation sidekick-buf))

    ;; Perform sidekick buffer operations.
    (with-current-buffer sidekick-buffer-name
        (progn
            (sidekick--deconstruct)
            (erase-buffer)
            (sidekick--update-symbol-occur
                symbol
                symbol-str
                buffer-fn
                project-dir
                mode-name)
            (sidekick--update-symbol-references
                symbol
                symbol-str
                buffer-fn
                project-dir
                mode-name)
            (sidekick--update-symbol-files
                symbol
                symbol-str
                buffer-fn
                project-dir
                mode-name)
            (sidekick--update-footer
                symbol
                symbol-str
                buffer-fn
                project-dir
                mode-name)
            (goto-char 0)
            (sidekick-mode)
            (highlight-regexp symbol 'highlight)))

    ;; If we're not inside Sidekick, and take focus is true, focus on Sidekick.
    (unless (string= (buffer-name) sidekick-buffer-name)
        (when sidekick-take-focus
            (switch-to-buffer-other-window sidekick-buffer-name)))

    ;; Enable future update calls.
    (setq sidekick-state-updating nil))

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
        (not sidekick-state-updating)
        ;; Clamps the symbol length to a minimum of two.
        (> (string-width symbol) (if (< sidekick-min-symbol-length 2) 2
        sidekick-min-symbol-length))
        (member mode-name (sidekick--extract-supported-modes))))

(defun sidekick--trigger-update(symbol buffer-fn mode-name)
    "Triggers the Sidekick update procedure."
    (let ((project-dir (sidekick--get-project-root-path)))
        (when (sidekick--should-update symbol buffer-fn project-dir mode-name)
            (sidekick--update symbol (substring-no-properties symbol)
                buffer-fn
                project-dir
                mode-name))))

(defun sidekick-focus()
    "Will focus on the Sidekick window, if available."
    (interactive)
    (let ((sidekick-window (get-buffer-window sidekick-buffer-name)))
        (when sidekick-window
            (select-window sidekick-window))))

(defun sidekick-at-point()
    "Uses the symbol at point as the symbol and updates the Sidekick results."
    (interactive)
    (sidekick--trigger-update
        (thing-at-point 'symbol)
        (buffer-file-name)
        (symbol-name major-mode)))

(defun sidekick-search-for-literal()
    "Search for a literal string."
    (interactive)
    (let ((string-lit (read-string "Search (Literal): ")))
        (sidekick--trigger-update
            string-lit
            (buffer-file-name)
            (symbol-name major-mode))))

(provide 'sidekick)

;;; sidekick.el ends here
