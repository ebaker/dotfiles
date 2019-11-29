;; @ebaker org head[1-4] colors
(custom-set-variables '(spacemacs-theme-custom-colors
                      '(
                        (head1 . "#b48ead")
                        (head2 . "#a7a6d4")
                        (head3 . "#bfebbf")
                        (head4 . "#f0dfaf")
                        )))
(setq org-level-color-stars-only t)


;; @ebaker org-mode setyp

;; org-directory
(setq org-directory "~/org")

(setq org-agenda-files (list (concat org-directory "/people.org")
                             (concat org-directory "/todo.org")
                             ;; (concat org-directory "/yara.org")
                             (concat org-directory "/emacs.org")
                             (concat org-directory "/org.org")

                             ;; @ebaker TODO - should capture files be hidden from day agenda view
                             (concat org-directory "/capture-emacs.org")
                             (concat org-directory "/capture-drafts.org")
                             (concat org-directory "/capture-beorg.org")
                             ))

(setq org-archive-location (concat org-directory "/archives/%s_archive::"))

;; TODO remove or fix
(setq org-todo-keyword-faces
      '(("TODO" . (:foreground "#ff39a3" :weight bold))
        ("NEXT" . "#E35DBF")
        ("DONE" . (:foreground "#A2FF38" :weight bold))
        ))


(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-todo ((t (:foreground "#ff39a3" :weight bold))))
 ;; '(org-agenda-clocking ((t (:background "red2"))) t)
 ;; '(org-agenda-done ((t (:foreground "ForestGreen"))))
 '(org-done ((t (:foreground "#A2FF38" :weight bold))))
 )

;; TODO idk
(make-face 'eliot-todo-face)
(set-face-foreground 'eliot-todo-face "#ff39a3")
;; (make-face 'eliot-done-face)
;; (set-face-foreground 'eliot-done-face "A2FF38")
(font-lock-add-keywords nil
                        '(("TODO" . 'eliot-todo-face)))
;; (font-lock-add-keywords 'org-mode
;;                         '(("DONE" . 'eliot-done-face)))

(provide 'eliot-org-variables)
