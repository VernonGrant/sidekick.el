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

(defvar sidekick-update-timeout 3 "*The amount of seconds before sidekick should update.")

(defvar sidekick-mode-hook nil "*List of functions to call when entering Sidekick mode.")

(defvar sidekick-previous-symbol nil "The previously checked symbol.")

(defvar sidekick-min-symbol-length 2 "The minimum allowed symbol length.")

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

(defun sidekick--construct()
  ""
  (setq buffer-read-only t)
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
  (insert heading)
  (let ((iterator 0))
	(while (< iterator (window-width))
	  (progn
		(setq iterator (+ iterator 1))
		(insert "-"))))
  (insert "\n"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Sidekick Symbol References ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun sidekick--handle-references(symbol-str buffer-fn)
  "Find all files containing the symbol."
  ;; Count the number of files.
  (let ((project-dir (sidekick--get-project-root-path))
		(file-glob (concat "--glob=\*" (file-name-extension buffer-fn t))))

	;; Change the directory to the project root before running grep.
	(cd project-dir)

	;; Run grep and output results into sidekick buffer.
	(let ((heading (mapconcat
					'identity
					(list "Files [" symbol-str "]:\n") " "))
		  (command (mapconcat
					'identity
					(list "rg -l" file-glob symbol-str "./") " "))
		  )

	  (sidekick-draw-section-heading heading)
	  (insert (shell-command-to-string command)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Sidekick Functionality ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun sidekick--update(symbol-str buffer-fn)
  ;; Destroy buffer if it already exists.
  (when (get-buffer sidekick-buffer-name)
	(kill-buffer sidekick-buffer-name))

  ;; Create new buffer.
  (display-buffer-in-side-window
   (get-buffer-create sidekick-buffer-name)
   `(
	 (side . right)
	 (slot . 0)
	 (window-width . 0.2)
	 ;; (window-parameters . (
	 ;; 					   ;;(no-other-window . t)
	 ;; 					   (no-delete-other-windows . t)))
	 ))

  ;; Render sidekick buffer.
  (with-current-buffer sidekick-buffer-name
	(progn
	  (sidekick--deconstruct)
	  (erase-buffer)
	  (sidekick--handle-references symbol-str buffer-fn)
	  (goto-char 0)
	  (sidekick-mode))))

;;;;;;;;;;;;;;;;;;;;;;;
;; Sidekick Triggers ;;
;;;;;;;;;;;;;;;;;;;;;;;

(defun sidekick--should-update(symbol buffer-fn)
  "Determines whether or not the sidekick buffer should be updated."
  ;; Note: The symbol at this stage, might have text properties.
  (and
   symbol
   buffer-fn
   (> (string-width symbol) sidekick-min-symbol-length)
   (member (symbol-name major-mode) sidekick-supported-modes)
   (not (string= sidekick-previous-symbol symbol))))

(defun sidekick--trigger-update()
  "Gets called every 'sidekick-update-timeout' seconds."
  (let ((symbol (thing-at-point 'symbol))
		(buffer-fn (buffer-file-name)))
	(when (sidekick--should-update symbol buffer-fn)
	  (setq sidekick-previous-symbol (substring-no-properties symbol))
	  (sidekick--update (substring-no-properties symbol) buffer-fn))))

(run-with-timer 0 sidekick-update-timeout 'sidekick--trigger-update)

(provide 'sidekick)

;;; sidekick.el ends here
