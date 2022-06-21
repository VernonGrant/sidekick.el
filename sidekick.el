;;; sidekick.el --- A coding assistance panel  -*- lexical-binding: t -*-

;; Copyright (C) 2022 by Vernon Grant.

;; Author: Vernon Grant <vernon@ruppell.io>
;; Version: 1.0.0
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

;; Sidekick is a Emacs extension that's aim is to provide useful information about
;; a symbol inside a single panel.

;;; Code:

;; TODO: Implement output with limit.
;; TODO: improve window properties.
;; TODO: Buffers must have a file association.
;; TODO: Add option to ask for custom string input.
;; TODO: Add option for case insensitive searches.
;; TODO: Add all default supported modes.
;; TODO: test regexps.
;; TODO: Implement custom faces.
;; TODO: Keymap, implement refresh.
;; TODO: Keymap, implement movement between headings.
;; TODO: Keymap, implement preview of files and line numbers.

;;;;;;;;;;;;;;;;;;;
;; Sidekick Mode ;;
;;;;;;;;;;;;;;;;;;;

(defvar sidekick-mode-hook nil "*List of functions to call when entering Sidekick mode.")

(defvar sidekick-updating nil "non-nil when sidekick is updating.")

(defvar sidekick-min-symbol-length 2 "The minimum allowed symbol length.")

(defvar sidekick-auto-focus-post-update t)

(defvar sidekick-mode-file-associations `(
                                             ;; Leaving globs empty, make
                                             ;; Sidekick use the current buffers
                                             ;; extension.
                                             ("cperl-mode"      . "*.{pl,PL}")
                                             ("python-mode"     . "*.py")
                                             ("php-mode"        . "*.{php,phtml,twig}")
                                             ("js-mode"         . "*.{js,es,es6}")
                                             ("typescript-mode" . "*.ts")
                                             ("java-mode"       . "*.java")
                                             ("json-mode"       . "*.json")
                                             ("yaml-mode"       . "*.yml")
                                             ("xml-mode"        . "*.xml")
                                             ("c-mode"          . "*.{c,cc,h,hh}")
                                             ("c++-mode"        . "*.{cpp,h,hh}")
                                             ("css-mode"        . "*.{css,sass,scss}")
                                             ("web-mode"        . "")
                                             ("markdown-mode"   . "*.md")
                                             ("emacs-lisp-mode" . "*.{el,emacs}")))

(defconst sidekick-buffer-name "*sidekick*")

(defvar sidekick-activated-in-buffer-fn nil "Only used for Sidekick key bindings.")

(defvar sidekick-mode-map
    (let ((map (make-sparse-keymap)))
        (define-key map "q" 'quit-window)
        (define-key map "o" 'sidekick-preview-other-window)
;;        (define-key map (kbd "M-n") 'sidekick-jump-forward)
;;        (define-key map (kbd "M-p") 'sidekick-jump-backward)
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

(defun sidekick-preview-other-window()
    (interactive)
    (when (string= (symbol-name major-mode) "sidekick-mode")
        (save-excursion
            ;; Find out if it has a file association.
            ;; If true;
            ;; open file center that line.
            ;; go back to sidekick and move cursor down to next match.
            (beginning-of-line)
            (print sidekick-activated-in-buffer-fn)
            (print (string (char-after)))
            (print (thing-at-point 'symbol))

            ;;(print (thing-at-point 'line))
         )
        (print "Ready to go")))

;; (defun sidekick-jump-backward()
;;     (interactive)
;;     (when (string= (symbol-name major-mode) "sidekick-mode")
;;         (print "Ready to go")
;;         (re-search-backward "^[.-]+[|/]" nil t))
;;     )

;; (defun sidekick-jump-forward()
;;     (interactive)
;;     (when (string= (symbol-name major-mode) "sidekick-mode")
;;         (print "Ready to go")
;;         (re-search-forward "^[.-]+[|/]" nil t))
;;     )

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
                    ;; If the globs is empty, just use the current buffers
                    ;; extension.
                    (if (= (length (cdr mode)) 0)
                        (setq glob-pat (concat "*." (file-name-extension buffer-fn nil)))
                        (setq glob-pat (cdr mode)))
                    (setq iterator (length sidekick-mode-file-associations))))
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Sidekick Symbol References ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun sidekick--update-footer(symbol symbol-str buffer-fn project-dir mode-name)
    "Draws the logo and version number at the bottom of the Sidekick buffer."
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
    (sidekick-draw-separator))

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
    (setq sidekick-updating t)

    ;; Set the activated in buffer name.
    (setq sidekick-activated-in-buffer-fn buffer-fn)

    ;; Destroy buffer if it already exists.
    (when (get-buffer sidekick-buffer-name)
        (kill-buffer sidekick-buffer-name))

    ;; SEE: https://www.gnu.org/software/emacs/manual/html_node/elisp/Window-Parameters.html
    ;; Create and setup buffer window.
    (display-buffer-in-side-window
        (get-buffer-create sidekick-buffer-name)
        `(
             (side . right)
             (slot . 0)
             (window-width . 0.25)
             (window-parameters . (
                                      ;; TODO: Optimize the window.
                                      ;; (no-other-window . t)
                                      ;; (mode-line-format . "")
                                      (no-delete-other-windows . t)))))

    ;; Perform sidekick buffer operations.
    (with-current-buffer sidekick-buffer-name
        (progn
            (sidekick--deconstruct)
            (erase-buffer)
            (sidekick--update-symbol-occur symbol symbol-str
                buffer-fn project-dir mode-name)
            (sidekick--update-symbol-references symbol symbol-str
                buffer-fn project-dir mode-name)
            (sidekick--update-symbol-files symbol symbol-str
                buffer-fn project-dir mode-name)
            (sidekick--update-footer symbol symbol-str
                buffer-fn project-dir mode-name)
            (goto-char 0)
            (sidekick-mode)
            (highlight-regexp symbol 'highlight)))

    ;; If auto focus enabled, take focus.
    (when sidekick-auto-focus-post-update
        (switch-to-buffer-other-window sidekick-buffer-name))

    ;; Enable future update calls.
    (setq sidekick-updating nil))

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
        buffer-file-name
        (not sidekick-updating)
        (> (string-width symbol) sidekick-min-symbol-length)
        (member mode-name (sidekick--extract-supported-modes))))

(defun sidekick--trigger-update(symbol)
    "Gets called every 'sidekick-update-timeout' seconds."
    (let ((buffer-fn (buffer-file-name))
             (project-dir (sidekick--get-project-root-path))
             (mode-name (symbol-name major-mode)))
        (when (sidekick--should-update symbol buffer-fn project-dir mode-name)
            (sidekick--update symbol (substring-no-properties symbol)
                buffer-fn
                project-dir
                mode-name))))

(defun sidekick-at-point()
    "Trigger sidekick panel update."
    (interactive)
    (sidekick--trigger-update (thing-at-point 'symbol)))

(defun sidekick-search-for-symbol()
    "Search for a symbol using the given string literal"
    (interactive)
    (let ((string-lit (read-string "Search (String Literal): ")))
        (sidekick--trigger-update string-lit)))

(provide 'sidekick)

;;; sidekick.el ends here
