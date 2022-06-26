;;; sidekick-evil.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2022 Aryslan Yakshibaev
;;
;; Author: Aryslan Yakshibaev <yak.aryslan.1999@gmail.com>
;; Maintainer: Aryslan Yakshibaev <yak.aryslan.1999@gmail.com>
;; Created: June 26, 2022
;; Modified: June 26, 2022
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/Consoleaf/sidekick-evil
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:

(require 'evil)
(require 'sidekick)

(evil-define-state sidekick
  "Sidekick state"
  :cursor 'box
  :enable (motion))

(evil-set-initial-state 'sidekick-mode 'sidekick)

(define-key evil-sidekick-state-map (kbd "q") #'sidekick-quit)
(define-key evil-sidekick-state-map "g" #'sidekick-refresh)
(define-key evil-sidekick-state-map "n" #'sidekick-open-next-match)
(define-key evil-sidekick-state-map "N" #'sidekick-open-previous-match)
(define-key evil-sidekick-state-map (kbd "RET") #'sidekick-open-match)

(provide 'sidekick-evil)
;;; sidekick-evil.el ends here
