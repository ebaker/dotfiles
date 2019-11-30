(setq org-level-color-stars-only t)


;; @ebaker org-mode setyp

(setq org-startup-indented t) ; Enable `org-indent-mode' by default
(add-hook 'org-mode-hook #'visual-line-mode)

;; org-directory
(setq org-directory "~/org")

(setq org-agenda-files (list
			(concat org-directory "/todo.org")
			(concat org-directory "/notes.org")
			;; (concat org-directory "/yara.org")
			(concat org-directory "/people.org")
			(concat org-directory "/emacs.org")
			(concat org-directory "/org.org")
			(concat org-directory "/habits.org")

			;; @ebaker TODO - should capture files be hidden from day agenda view
			(concat org-directory "/capture-emacs.org")
			(concat org-directory "/capture-drafts.org")
			(concat org-directory "/capture-beorg.org")
			))

;; @ebaker - agenda show five days starting yesterday - https://emacs.stackexchange.com/questions/12517/how-do-i-make-the-timespan-shown-by-org-agenda-start-yesterday
(setq org-agenda-start-day "-1d")
(setq org-agenda-span 5)
(setq org-agenda-start-on-weekday nil)

(setq org-archive-location (concat org-directory "/archives/%s_archive::"))

;; TODO remove or fix
(setq org-todo-keyword-faces
      '(("TODO" . (:foreground "#ff39a3" :weight bold))
	("NEXT" . "#E35DBF")
	("DONE" . (:foreground "#A2FF38" :weight bold))
	))


;; (custom-set-faces
;;  ;; custom-set-faces was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  '(org-todo ((t (:foreground "#ff39a3" :weight bold))))
;;  ;; '(org-agenda-clocking ((t (:background "red2"))) t)
;;  ;; '(org-agenda-done ((t (:foreground "ForestGreen"))))
;;  '(org-done ((t (:foreground "#A2FF38" :weight bold))))
;;  )

;; TODO idk
(make-face 'eliot-todo-face)
(set-face-foreground 'eliot-todo-face "#ff39a3")
;; (make-face 'eliot-done-face)
;; (set-face-foreground 'eliot-done-face "A2FF38")
(font-lock-add-keywords nil
			'(("TODO" . 'eliot-todo-face)))
;; (font-lock-add-keywords 'org-mode
;;                         '(("DONE" . 'eliot-done-face)))


(with-eval-after-load 'org-agenda
  (define-key org-agenda-mode-map "e" 'org-agenda-week-view)
  (define-key org-agenda-mode-map "S" 'org-save-all-org-buffers)
  (define-key org-agenda-mode-map "s" 'org-agenda-schedule)
  (define-key org-agenda-mode-map "w" 'org-agenda-refile)
  (define-key org-agenda-mode-map "f" 'org-agenda-set-effort)
  (define-key org-agenda-mode-map "n" 'org-agenda-later)
  (define-key org-agenda-mode-map "p" 'org-agenda-earlier)
  (define-key org-agenda-mode-map "x" 'org-agenda-bulk-mark)
)
(with-eval-after-load 'org

  ;; @ebaker - custom todo keywords
  ;; https://emacs.stackexchange.com/questions/31466/all-todos-how-to-set-different-colors-for-different-categories
  (setq org-todo-keywords
	'(
	  (sequence "TODO" "|" "DONE")
	  ;; (sequence "TODO" "NEXT" "|" "DONE")
	  ))

  ;; @ebaker - TODO does this work or need debugging?
  ;; https://www.computerhope.com/cgi-bin/htmlcolor.pl?c=FF39A3

  (setq org-tag-faces
	'(("TODO" . (:foreground "#ff39a3" :weight bold))
	  ("DONE" . (:foreground "#A2FF38" :weight bold))
	  ))

  ;; @ebaker - import org-archive-subtree-hierarchical
  ;; https://github.com/ebaker/el/org-archive-subtree-hierarchical.el
  (require 'org-archive-subtree-hierarchical)


  ;; @ebaker - custom bullets
  ;; (setq org-bullets-bullet-list '("◉" "◇" "○" "▻" "⌑"))
  ;; (setq org-bullets-bullet-list '("▣" "▢" "￭" "⌑" "▪" "▫"))
  ;; (setq org-bullets-bullet-list '("▢" "￭" "￮" "●" "▣"))
  ;; (setq org-bullets-bullet-list '("▢" "⬡" "◇" "￮" "▻"))

  ;; @ebaker - folding symbol
  (setq org-ellipsis " v ")
  ;; others - ▼ ▽ ▾ ▿ ﹀ v

  ;; @ebaker - custom cature templates
  ;; TODO - source?
  (setq org-capture-templates
	'(("n" "Notes" entry (file "~/org/capture-emacs.org")
	   "* %^{Description} %^g %?
ded: %U")
	  ("t" "Todo" entry (file "~/org/capture-emacs.org")
	   "* TODO %^{Description} %^g %?
:PROPERTIES:
:CREATED: %U
:END:"))
	)

  ;; @ebaker - org-refile
  ;; @ebaker - TODO source?
  ;; Targets include this file and any file contributing to the agenda - up to 4 levels deep
  ;; (setq org-refile-targets (quote ((nil :maxlevel . 2)
  ;;                                  (org-agenda-files :maxlevel . 2))))
  ;; (setq org-refile-targets (quote ((org-agenda-files :maxlevel . 2))))

  (setq org-refile-targets (quote (("~/org/todo.org" :maxlevel . 1)
				   ("~/org/notes.org" :maxlevel . 2)
				    ;; (concat org-directory "/yara.org" :maxlevel . 1)
				   ("~/org/people.org" :maxlevel . 1)
				   ("~/org/maybe.org" :maxlevel . 1)

				   ("~/org/emacs.org" :maxlevel . 1)
				   ("~/org/org.org" :maxlevel . 1)
				   ("~/org/habits.org" :maxlevel . 1)
				   ("~/org/computer.org" :maxlevel . 1)
				   )))

  (setq org-completion-use-ido nil)

  ;; Refile to the Top Level
  (setq org-refile-use-outline-path 'file)
  (setq org-outline-path-complete-in-steps nil)
  (setq org-refile-allow-creating-parent-nodes 'confirm)

  ;; @ebaker agenda commands
  ;; - https://stackoverflow.com/questions/17182954/write-and-call-function-from-agenda-org-mode
  (setq org-agenda-custom-commands
	'(("r" "[r]efile list for captured tasks/notes" tags "+refile")
	  ;; match those tagged with :inbox:, are not scheduled, are not DONE.
	  ("u" "[u]nscheduled tasks" tags "-refile-SCHEDULED={.+}/!-DONE")
	  ("D" "[D]one tasks list" tags "+TODO=\"DONE\"-crypt")
	)
      )

  ;; @ebaker - TODO update colors
  ;; from http://pragmaticemacs.com/emacs/org-mode-basics-vi-a-simple-todo-list/
  ;; set priority range from A to C with default A
  (setq org-highest-priority ?A)
  (setq org-lowest-priority ?C)
  ;; (setq org-default-priority ?A)

  ;;set colours for priorities
  (setq org-priority-faces '((?A . (:foreground "#FF9538"))
			     (?B . (:foreground "#FFB638"))
			     (?C . (:foreground "#FFD738"))))


  ;; TODO consider a refile & schedule function
  (defun eliot/org-agenda-refile ()
    (interactive)
    (org-agenda-refile))

;; @ebaker - re-map org-meta-return
(org-defkey org-mode-map [(meta return)] 'org-meta-return)

(setq hl-todo-mode nil)

(defun tnez/src-cleanup ()
  (indent-region (point-min) (point-max)))

(add-hook 'org-babel-post-tangle-hook 'tnez/src-cleanup)

;; @ebaker org-temp
;; (require 'org-tempo)


)

;; @ebaker - reveal.js
;; (require 'ox-reveal)


(provide 'eliot-org)
