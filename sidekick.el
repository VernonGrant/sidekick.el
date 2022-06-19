;;; sidekick.el --- A coding assistance panel  -*- lexical-binding: t -*-

;; Copyright (C) 2009-2022  Free Software Foundation, Inc.

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

(require 'timer)

(defvar sidekick-mode-hook nil "*List of functions to call when entering Sidekick mode.")

(defvar sidekick-updating nil "non-nil when sidekick is updating.")

(defvar sidekick-min-symbol-length 2 "The minimum allowed symbol length.")

(defvar sidekick-auto-focus-post-update nil)

(defvar sidekick-supported-modes (list
								  "php-mode"
								  "js-mode"
								  "css-mode"
								  "web-mode"
								  "c-mode"
								  "c++-mode"
								  "emacs-lisp-mode") "A list of all supported sidekick modes.")

(defconst sidekick-buffer-name "*sidekick*")

(defvar sidekick-mode-map
  (let ((map (make-sparse-keymap)))
	(define-key map "q" 'quit-window)
	;; TODO: Implement movement between headings.
    map))

 ;; '(font-lock-builtin-face ((t (:bold t :foreground "PaleGreen"))))
 ;; '(font-lock-comment-face ((t (:foreground "tomato3"))))
 ;; '(font-lock-constant-face ((t (:foreground "Aquamarine"))))
 ;; '(font-lock-doc-string-face ((t (:foreground "LightSalmon3"))))
 ;; '(font-lock-function-name-face ((t (:foreground "SteelBlue1"))))
 ;; '(font-lock-keyword-face ((t (:foreground "cyan1"))))
 ;; '(font-lock-reference-face ((t (:foreground "LightSalmon2"))))
 ;; '(font-lock-string-face ((t (:foreground "LightSalmon3"))))
 ;; '(font-lock-type-face ((t (:foreground "PaleGreen3"))))
 ;; '(font-lock-variable-name-face ((t (:foreground "khaki1"))))
 ;; '(font-lock-warning-face ((t (:bold t :foreground "IndianRed"))))
 ;; '(font-lock-preprocessor-face ((t (:foreground "SkyBlue3"))))

(setq sidekick-highlights
      '(
		("^-\\{2\\}|\s?" . 'font-lock-comment-face)
		("^-\\{2\\}|\s.*\\(|-+\\)" (1 font-lock-comment-face))
		("^-\\{2\\}|\s\\(.*\\)\s|-+" (1 font-lock-keyword-face))))

(defun sidekick--construct()
  ""
  (setq buffer-read-only t)
  (setq font-lock-defaults '(sidekick-highlights))
  (use-local-map sidekick-mode-map)
  (run-hooks 'sidekick-mode-hook))

(defun sidekick--deconstruct()
  ""
  (setq buffer-read-only nil)
  (kill-all-local-variables))


(define-derived-mode sidekick-mode fundamental-mode "Sidekick"
  ""
  ;; TODO: Add syntax highlighting.
  ;; TODO: Setup mode hooks.
  (sidekick--construct))

;;;;;;;;;;;;;;;;;;;;;;
;; Sidekick Helpers ;;
;;;;;;;;;;;;;;;;;;;;;;

(defun sidekick--get-project-root-path()
  "Get the root path to the current project."
  ;; TODO: Report when some project root can not be found.
  (let ((dir default-directory))
	(or
	  (locate-dominating-file dir ".git")
	  (locate-dominating-file dir ".sidekick")
	  (locate-dominating-file dir ".projectile"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Sidekick User Interface ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun sidekick-draw-section-heading(heading)
  "Draws a new heading separator inside the active buffer."
  (insert (concat "--| " heading " |"))
  (let ((iterator 0))
	(while (< iterator (- (window-width) (string-width heading)))
	  (progn
		(setq iterator (+ iterator 1))
		(insert "-"))))
  (insert "\n\n"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Sidekick Symbol References ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun sidekick--update-header(symbol symbol-str buffer-fn project-dir)
  ""
  (insert "")
  )

(defun sidekick--update-notice(symbol symbol-str buffer-fn project-dir)
  ""
  (sidekick-draw-section-heading
   (mapconcat
	'identity
	(list  "Updated" (format-time-string "%H:%M:%S") symbol) " - ")))

(defun sidekick--update-symbol-occur(symbol symbol-str buffer-fn project-dir)
  ""
  (cd project-dir)
  (sidekick-draw-section-heading "Current Buffer")
  (let ((file-glob (concat "--glob=\*" (file-name-extension buffer-fn t))))
	(let ((command (mapconcat
					'identity
					(list "rg -n --no-heading --no-filename --trim" (concat "'" symbol-str "'") buffer-fn) " ")))
	  (insert (shell-command-to-string command))
	  (insert "\n\n")))
  )

(defun sidekick--update-symbol-references(symbol symbol-str buffer-fn project-dir)
  ""
  (cd project-dir)
  (sidekick-draw-section-heading "Project")
  (let ((file-glob (concat "--glob=\*" (file-name-extension buffer-fn t))))
	(let ((command (mapconcat
					'identity
					(list "rg -n --trim --heading" file-glob (concat "'" symbol-str "'") "./") " ")))
	  (insert (shell-command-to-string command))
	  (insert "\n\n")
	  ))

  )

;; TODO: Files.
(defun sidekick--update-symbol-files(symbol symbol-str buffer-fn project-dir)
  "Find all files containing the symbol."
  ;; Count the number of files.
  (let ((file-glob (concat "--glob=\*" (file-name-extension buffer-fn t))))

	;; Change the directory to the project root before running grep.
	(cd project-dir)
	(sidekick-draw-section-heading "Files")

	;; Run grep and output results into sidekick buffer.
	(let ((command (mapconcat
					'identity
					(list "rg -l" file-glob (concat "'" symbol-str "'") "./") " ")))
	  (insert (shell-command-to-string command))
	  (insert "\n\n"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Sidekick Functionality ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun sidekick--update(symbol symbol-str buffer-fn project-dir)
  ""
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
	 (window-width . 0.2)
	 ;; TODO: optimize the display of the sidekick window.
	 ;; (window-parameters . (
	 ;; 					   ;;(no-other-window . t)
	 ;; 					   (no-delete-other-windows . t)))
	 ))

  ;; Perform sidekick buffer operations.
  (with-current-buffer sidekick-buffer-name
	(progn
	  (sidekick--deconstruct)
	  (erase-buffer)
	  (sidekick--update-header symbol symbol-str buffer-fn project-dir)
	  (sidekick--update-symbol-occur symbol symbol-str buffer-fn project-dir)
	  (sidekick--update-symbol-references symbol symbol-str buffer-fn project-dir)
	  (sidekick--update-symbol-files symbol symbol-str buffer-fn project-dir)
	  (goto-char 0)
	  (sidekick-mode)))

  ;; TODO: implement focus option.
  (when sidekick-auto-focus-post-update
	(switch-to-buffer-other-window sidekick-buffer-name))

  ;; Enable future update calls.
  (setq sidekick-updating nil))

;;;;;;;;;;;;;;;;;;;;;;;
;; Sidekick Triggers ;;
;;;;;;;;;;;;;;;;;;;;;;;

(defun sidekick--should-update(symbol buffer-fn project-dir)
  "Determines whether or not the sidekick buffer should be updated."
  ;; Note: The symbol at this stage, might have text properties.
  ;; TODO: What if the buffer has no file or extension.
  (and
   symbol
   buffer-fn
   project-dir
   (not sidekick-updating)
   (> (string-width symbol) sidekick-min-symbol-length)
   (member (symbol-name major-mode) sidekick-supported-modes)))

(defun sidekick--trigger-update()
  "Gets called every 'sidekick-update-timeout' seconds."
  (let ((symbol (thing-at-point 'symbol))
		;; TODO: What if the buffer has no file or extension.
		(buffer-fn (or (buffer-file-name) (buffer-name)))
		(project-dir (sidekick--get-project-root-path)))
	(when (sidekick--should-update symbol buffer-fn project-dir)
	  (sidekick--update
	   symbol
	   (substring-no-properties symbol)
	   buffer-fn
	   project-dir))))

(defun sidekick-at-point()
  "Trigger sidekick panel update."
  (interactive)
  (sidekick--trigger-update))

;;;;;;;;;;;;;
;; Testing ;;
;;;;;;;;;;;;;

(defun sidekick--testing()
  ""
  (interactive)
  (print (thing-at-point 'symbol))
  (print (or (buffer-file-name) (buffer-name)))
  (print (sidekick--get-project-root-path))
  (print (not sidekick-updating))
  (print (> (string-width (thing-at-point 'symbol)) sidekick-min-symbol-length))
  (print (member (symbol-name major-mode) sidekick-supported-modes))
  )

(global-set-key (kbd "C-c t") 'sidekick--testing)

(provide 'sidekick)

;;; sidekick.el ends here
