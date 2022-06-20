;;; sidekick.el --- A coding assistance panel  -*- lexical-binding: t -*-

;; Copyright (C) 2022 by Vernon Grant.

;; Author: Vernon Grant <vernon@ruppell.io>
;; Version: 0.0.0
;; Package-Requires: ((emacs "25"))
;; Keywords: extensions, lisp
;; Homepage: https://github.com/VernonGrant/sidekick

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

;; A coding assistance panel.

;;; Code:

;;;;;;;;;;;;;;;;;;;
;; Sidekick Mode ;;
;;;;;;;;;;;;;;;;;;;

(defvar sidekick-mode-hook nil "*List of functions to call when entering Sidekick mode.")

(defvar sidekick-updating nil "non-nil when sidekick is updating.")

(defvar sidekick-min-symbol-length 2 "The minimum allowed symbol length.")

(defvar sidekick-auto-focus-post-update nil)

;; TODO: Add all default supported modes.
(defvar sidekick-mode-file-associations `(
                                             ("php-mode"        . "*.{php,phtml,twig}")
                                             ("js-mode"         . "*.{js,es,es6}")
                                             ("c-mode"          . "*.{c,cc,h,hh}")
                                             ("c++-mode"        . "*.{cpp,h,hh}")
                                             ("css-mode"        . "*.{css,sass,scss}")
                                             ("web-mode"        . "*.{html}")
                                             ("markdown-mode"   . "*.{md}")
                                             ("emacs-lisp-mode" . "*.{el,emacs}")))

(defconst sidekick-buffer-name "*sidekick*")

(defvar sidekick-mode-map
    (let ((map (make-sparse-keymap)))
        (define-key map "q" 'quit-window)
        (define-key map (kbd "M-n") 'sidekick-jump-forward)
        (define-key map (kbd "M-p") 'sidekick-jump-backward)
        ;; TODO: Implement refresh.
        ;; TODO: Implement movement between headings.
        ;; TODO: Implement preview of files and line numbers.
        map))

;; TODO: test regexps.
;; TODO: Implement custom faces.
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

(defun sidekick--construct()
    ""
    (setq buffer-read-only t)
    (setq font-lock-defaults '(sidekick-highlights))
    (display-line-numbers-mode 0)
    (use-local-map sidekick-mode-map)
    (run-hooks 'sidekick-mode-hook))

(defun sidekick--deconstruct()
    ""
    (setq buffer-read-only nil)
    (kill-all-local-variables))

(define-derived-mode sidekick-mode fundamental-mode "Sidekick"
    ""
    ;; TODO: Setup mode hooks.
    (sidekick--construct))

;;;;;;;;;;;;;;;;;;;;;;;
;; Sidekick Bindings ;;
;;;;;;;;;;;;;;;;;;;;;;;

(defun sidekick-jump-backward()
    (interactive)
    (re-search-backward "^[.-]+[|/]" nil t))

(defun sidekick-jump-forward()
    (interactive)
    (re-search-forward "^[.-]+[|/]" nil t))

;;;;;;;;;;;;;;;;;;;;;;;;
;; Sidekick Utilities ;;
;;;;;;;;;;;;;;;;;;;;;;;;

(defun sidekick--get-project-root-path()
    "Get the root path to the current project."
    ;; TODO: Document me:
    ;; TODO: Decide the precedence of project root directory indicators.
    ;; TODO: Report when some project root can not be found.
    (let ((dir default-directory))
        (or
            (locate-dominating-file dir ".sidekick")
            (locate-dominating-file dir ".projectile")
            (locate-dominating-file dir ".git"))))

;; TODO: implement this.
(defun sidekick--get-rg-path()
    "Get the path of rg")

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

(defun sidekick--get-mode-file-glob-pattern(mode-name)
    "Returns the glob pattern for a supported major mode."
    (let ((iterator 0)
             (glob-pat ""))
        (while (< iterator (length sidekick-mode-file-associations))
            (let ((mode (nth iterator sidekick-mode-file-associations)))
                (when (string= (car mode) mode-name)
                    (setq glob-pat (cdr mode))
                    (setq iterator (length sidekick-mode-file-associations))
                    ))
            (setq iterator (+ iterator 1)))
        glob-pat))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Sidekick User Interface ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun sidekick-draw-section-heading(heading)
    "Draws a new heading separator inside the active buffer."
    (let ((iterator 0)
             (heading-fnl (concat "--| " heading " |")))
        (insert heading-fnl)
        (while (< iterator (- (sidekick--get-window-width)
                               (string-width heading-fnl)))
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

;; TODO: implement tests.
(defun sidekick-draw-text-centerd(text)
    "Center a string inside the sidekick window."
    (let ((text-lns (split-string text "\n"))
             (text-ln-max 0)
             (iterator 0))

        ;; Find the longest line.
        (while (< iterator (length text-lns))
            (when (> (string-width (nth iterator text-lns)) text-ln-max)
                (setq text-ln-max (string-width (nth iterator text-lns))))
            (setq iterator (+ iterator 1)))

        ;; Reset iterator.
        (setq iterator 0)

        ;; Add padding  lines and print.
        (let ((padding (- (sidekick--get-window-width) text-ln-max))
                 (padding-str ""))

            ;; Generate padding string.
            (dotimes (number (/ padding 2))
                (setq padding-str (concat padding-str " ")))

            (while (< iterator (length text-lns))
                (insert (concat padding-str (nth iterator text-lns) "\n"))
                ;; (when (not (= (+ iterator 1) (length text-lns)))
                ;; (insert "\n"))
                (setq iterator (+ iterator 1))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Sidekick Symbol References ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TODO: improve window properties.
(defun sidekick--update-footer(symbol symbol-str buffer-fn project-dir mode-name)
    "Draws the logo and version number at the bottom of the Sidekick buffer."
    (sidekick-draw-separator)
    (let ((logo-str (concat
                        "------------ ----    ----\n"
                        "************ ****   ****\n"
                        "----         ----  ----\n"
                        "************ *********    Version: 0.0.1\n"
                        "------------ ---------    --------------\n"
                        "       ***** ****  ****\n"
                        "------------ ----   ----\n"
                        "************ ****    ****")))
        (sidekick-draw-text-centerd logo-str))
    (sidekick-draw-separator))

;; TODO: Combine these methods into one external function.
;; TODO: for C and C++ search header files to. Have mode / file extension
;; associations and generate a glob pattern for each.
(defun sidekick--update-symbol-occur(symbol symbol-str buffer-fn project-dir mode-name)
    "Shows all occurrences of symbol in the current active buffer."
    (cd project-dir)
    (sidekick-draw-section-heading "In Buffer")
    (let ((file-glob (concat "--glob=" (sidekick--get-mode-file-glob-pattern mode-name))))
        (let ((command (mapconcat
                           'identity
                           (list "rg -n --no-heading --no-filename --trim" (concat "'" symbol-str "'") buffer-fn) " ")))
            (insert (shell-command-to-string command))
            (insert "\n"))))

(defun sidekick--update-symbol-references(symbol symbol-str buffer-fn project-dir mode-name)
    "Shows all references to a symbol inside a project directory."
    (cd project-dir)
    (sidekick-draw-section-heading "In Project")
    (let ((file-glob (concat "--glob=" (sidekick--get-mode-file-glob-pattern mode-name))))
        (let ((command (mapconcat
                           'identity
                           (list "rg -n --trim --heading" file-glob (concat "'" symbol-str "'") "./") " ")))
            (insert (shell-command-to-string command))
            (insert "\n"))))

(defun sidekick--update-symbol-files(symbol symbol-str buffer-fn project-dir mode-name)
    "Find all files containing the symbol."
    ;; Count the number of files.
    (let ((file-glob (concat "--glob=" (sidekick--get-mode-file-glob-pattern mode-name))))

        ;; Change the directory to the project root before running grep.
        (cd project-dir)
        (sidekick-draw-section-heading "Files")

        ;; Run grep and output results into sidekick buffer.
        (let ((command (mapconcat
                           'identity
                           (list "rg -l" file-glob (concat "'" symbol-str "'") "./") " ")))
            (insert (shell-command-to-string command))
            (insert "\n"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Sidekick Functionality ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun sidekick--update(symbol symbol-str buffer-fn project-dir mode-name)
    "Updates the Sidekick panel with symbol related information."
    ;; Pause future update calls.
    (setq sidekick-updating t)

    ;; Destroy buffer if it already exists.
    (when (get-buffer sidekick-buffer-name)
        (kill-buffer sidekick-buffer-name))

    ;; Create and setup buffer window.
    (display-buffer-in-side-window
        (get-buffer-create sidekick-buffer-name)
        `(
             (side . right)
             (slot . 0)
             (window-width . 0.25)
             ;; TODO: optimize the display of the sidekick window.
             ;; SEE: https://www.gnu.org/software/emacs/manual/html_node/elisp/Window-Parameters.html
             (window-parameters . (
                                      ;;(no-other-window . t)
                                      ;; (mode-line-format . "")
                                      (no-delete-other-windows . t)))
             ))

    ;; Perform sidekick buffer operations.
    (with-current-buffer sidekick-buffer-name
        (progn
            (sidekick--deconstruct)
            (erase-buffer)
            (sidekick--update-symbol-occur symbol symbol-str buffer-fn project-dir mode-name)
            (sidekick--update-symbol-references symbol symbol-str buffer-fn project-dir mode-name)
            (sidekick--update-symbol-files symbol symbol-str buffer-fn project-dir mode-name)
            (sidekick--update-footer symbol symbol-str buffer-fn project-dir mode-name)
            (goto-char 0)
            (sidekick-mode)
            (highlight-regexp symbol 'highlight)))

    ;; TODO: implement focus option.
    (when sidekick-auto-focus-post-update
        (switch-to-buffer-other-window sidekick-buffer-name))

    ;; Enable future update calls.
    (setq sidekick-updating nil))

;;;;;;;;;;;;;;;;;;;;;;;
;; Sidekick Triggers ;;
;;;;;;;;;;;;;;;;;;;;;;;

(defun sidekick--should-update(symbol buffer-fn project-dir mode-name)
    "Determines whether or not the sidekick buffer should be updated."
    ;; Note: The symbol at this stage, might have text properties.
    ;; TODO: What if the buffer has no file or extension.
    (and
        symbol
        buffer-fn
        project-dir
        (not sidekick-updating)
        (> (string-width symbol) sidekick-min-symbol-length)
        (member mode-name (sidekick--extract-supported-modes))
        ))

(defun sidekick--trigger-update()
    "Gets called every 'sidekick-update-timeout' seconds."
    (let ((symbol (thing-at-point 'symbol))
             ;; TODO: What if the buffer has no file or extension.
             (buffer-fn (or (buffer-file-name) (buffer-name)))
             (project-dir (sidekick--get-project-root-path))
             (mode-name (symbol-name major-mode)))
        (when (sidekick--should-update symbol buffer-fn project-dir mode-name)
            (sidekick--update
                symbol
                (substring-no-properties symbol)
                buffer-fn
                project-dir
                mode-name))))

(defun sidekick-at-point()
    "Trigger sidekick panel update."
    (interactive)
    (sidekick--trigger-update))

;;;;;;;;;;;;;
;; Testing ;;
;;;;;;;;;;;;;

(defun sidekick--testing()
    (interactive)
    (print (sidekick--get-mode-file-glob-pattern "c-mode"))
    ;; (print (symbol-name major-mode))
    (print (sidekick--extract-supported-modes))
    ;; (isearch-backward-regexp &optional NOT-REGEXP NO-RECURSIVE-EDIT)
    ;; (print (sidekick--get-window-width))
    ;; (print (window-width))
    ;; (highlight-regexp "In Buffer" 'highlight)
    ;; (print (thing-at-point 'symbol))
    ;; (print (or (buffer-file-name) (buffer-name)))
    ;; (print (sidekick--get-project-root-path))
    ;; (print (not sidekick-updating))
    ;; (print (> (string-width (thing-at-point 'symbol)) sidekick-min-symbol-length))
    ;; (print (member (symbol-name major-mode) (sidekick--extract-supported-modes))
    )

(global-set-key (kbd "C-c t") 'sidekick--testing)

(provide 'sidekick)

;;; sidekick.el ends here
