;;; init.el -*- lexical-binding: t; -*-

(load "~/.emacs.d/lisp/core")
(load "~/.emacs.d/lisp/load-pkgs")

(switch-to-buffer "*scratch*")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-agenda-files
   '("/Users/chillen/org/20230907094607-ramp_onboarding.org" "/Users/chillen/org/20230905163559-ramp.org" "/Users/chillen/org/20230906103129-retool.org" "/Users/chillen/org/20230907094055-ramp_30_60_90.org" "/Users/chillen/org/inbox.org") nil nil "Customized with use-package org"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-code ((t (:background "#303030"))))
 '(org-level-1 ((t (:height 1.15))))
 '(org-level-2 ((t (:height 1.1))))
 '(org-level-3 ((t (:heihgt 1.05)))))
