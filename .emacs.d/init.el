;; Use a hook so the message doesn't get clobbered by other messages.
(add-hook 'emacs-startup-hook
	  (lambda ()
	    (message "Emacs ready in %s with %d garbage collections."
		     (format "%.2f seconds"
			     (float-time
			      (time-subtract after-init-time before-init-time)))
		     gcs-done)))


;; heavily insprired by https://blog.jft.rocks/emacs/emacs-from-scratch.html

;; Minimal UI
(scroll-bar-mode -1)
(tool-bar-mode   -1)
(tooltip-mode    -1)
(menu-bar-mode   -1)

;; check this
;; (add-to-list 'default-frame-alist '(font . "Inconsolata-dz for Powerline"))
;; (add-to-list 'default-frame-alist '(font . "Space Mono"))

;; https://emacs.stackexchange.com/questions/16818/cocoa-emacs-24-5-font-issues-inconsolata-dz
(add-to-list 'default-frame-alist '(font . "InconsolataDZ for Powerline"))
(add-to-list 'default-frame-alist '(height . 44))
(add-to-list 'default-frame-alist '(width . 100))


;; Make startup faster by reducing the frequency of garbage
;; collection.  The default is 800 kilobytes.  Measured in bytes.
(setq gc-cons-threshold (* 50 1000 1000))

(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")

;; super
(setq mac-command-modifier 'super
      mac-option-modifier  'meta
      )

(push "~/.emacs.d/elisp/" load-path)


;; @ebaker - comment-or-uncomment-region-or-line
(require 'comment-or-uncomment-region-or-line)

;; @ebaker - global custom hotkeys
(global-set-key (kbd "s-/") 'comment-or-uncomment-region-or-line)
(global-set-key (kbd "s-o") 'other-window)
(global-set-key (kbd "s-0") 'delete-window)
(global-set-key (kbd "s-1") 'delete-other-windows)
(global-set-key (kbd "s-2") 'split-window-below)
(global-set-key (kbd "s-3") 'split-window-right)
(global-set-key (kbd "s-b") 'helm-buffers-list)
(global-set-key (kbd "s-k") 'ido-kill-buffer)
(global-set-key (kbd "s-a") 'org-agenda)

;; @ebaker - remove keybinding eyebrowse
(assq-delete-all 'eyebrowse-mode minor-mode-map-alist)

;; from https://sam217pa.github.io/2016/09/02/how-to-build-your-own-spacemacs/

(setq delete-old-versions -1 )		; delete excess backup versions silently
(setq version-control t )		; use version control
(setq vc-make-backup-files t )		; make backups file even when in version controlled dir
(setq backup-directory-alist `(("." . "~/.emacs.d/backups")) ) ; which directory to put backups file
(setq vc-follow-symlinks t )				       ; don't ask for confirmation when opening symlinked file
(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-save-list/" t)) ) ;transform backups file name
(setq inhibit-startup-screen t )	; inhibit useless and old-school startup screen
(setq ring-bell-function 'ignore )	; silent bell when you make a mistake
(setq coding-system-for-read 'utf-8 )	; use utf-8 by default
(setq coding-system-for-write 'utf-8 )
(setq sentence-end-double-space nil)	; sentence SHOULD end with only a point.
;; (setq default-fill-column 140)		; toggle wrapping text at the 80th character
(setq initial-scratch-message "") ; print a default message in the empty scratch buffer opened at startup

;; line numbers
;; (global-display-line-numbers-mode)

;; https://emacs.stackexchange.com/questions/28736/emacs-pointcursor-movement-lag/28746
(setq auto-window-vscroll nil)

;; remembers minibuffer history between sessions
(save-place-mode t)

;; cursor at the same place it was at before
(savehist-mode t)

;; Show matching parens
(setq show-paren-delay 0)
(show-paren-mode 1)

;; Package configs
(require 'package)
(setq package-enable-at-startup nil)
(setq package-archives '(("org"   . "http://orgmode.org/elpa/")
			 ("melpa" . "https://melpa.org/packages/")
			 ("gnu"   . "https://elpa.gnu.org/packages/")
			 ))
(package-initialize)

;; Bootstrap `use-package`
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)


;; Defer Packages you don’t need Immediately with Idle Timers
(use-package recentf
  ;; Loads after 1 second of idle time.
  :defer 1)

(use-package uniquify
  ;; Less important than recentf.
  :defer 2)

(defun ebaker/emacsify-evil-mode ()
  "Remove Evil Normal state bindings and add some Emacs bindings in Evil Normal state."

  ;; remove all keybindings from insert-state keymap
  (setcdr evil-insert-state-map nil)
  ;; but [escape] should switch back to normal state
  (define-key evil-insert-state-map [escape] 'evil-normal-state)
  (define-key evil-normal-state-map (kbd "C-e") 'evil-end-of-line)
  (define-key evil-normal-state-map (kbd "C-u") 'evil-scroll-up)
  (define-key evil-normal-state-map (kbd "[ m") 'beginning-of-defun)
  (define-key evil-normal-state-map (kbd "] m") 'end-of-defun)
  (define-key evil-normal-state-map (kbd "k") 'evil-previous-visual-line)
  (define-key evil-normal-state-map (kbd "j") 'evil-next-visual-line)
  (define-key evil-normal-state-map (kbd "C-p") 'evil-previous-visual-line)
  (define-key evil-normal-state-map (kbd "C-n") 'evil-next-visual-line)
  (define-key evil-normal-state-map (kbd "C-f") 'evil-forward-char)
  (define-key evil-normal-state-map (kbd "C-n") 'evil-backward-char))

;; Vim mode
(use-package evil
  :ensure t
  :defer 1
  :init
  (setq evil-disable-insert-state-bindings t)
  (setq evil-want-C-u-scroll t)
  :config
  (evil-mode 1)
  (ebaker/emacsify-evil-mode)

  ;; @ebaker - enter normal state after saving
  (add-hook 'after-save-hook #'evil-normal-state))

;;
(defun ebaker/evilify-org-agenda-mode ()
  "Org Agenda use some Evil keybindings."

  (define-key org-agenda-mode-map "j" 'org-agenda-next-line)
  (define-key org-agenda-mode-map "k" 'org-agenda-previous-line)
  ;; (define-key org-agenda-mode-map "gj" 'org-agenda-next-item)
  ;; (define-key org-agenda-mode-map "gk" 'org-agenda-previous-item)
  (define-key org-agenda-mode-map "l" 'forward-char)
  (define-key org-agenda-mode-map "h" 'backward-char)
  (define-key org-agenda-mode-map (kbd "C-j") 'org-agenda-next-item)
  (define-key org-agenda-mode-map (kbd "C-k") 'org-agenda-previous-item)
  (define-key org-agenda-mode-map (kbd "[") 'org-agenda-earlier)
  (define-key org-agenda-mode-map (kbd "]") 'org-agenda-later)
  (define-key org-agenda-mode-map (kbd "f") 'org-agenda-earlier)
  (define-key org-agenda-mode-map (kbd "b") 'org-agenda-later)
  (define-key org-agenda-mode-map (kbd "m") 'org-agenda-bulk-toggle)
  (define-key org-agenda-mode-map (kbd "~") 'org-agenda-bulk-toggle-all)
  (define-key org-agenda-mode-map (kbd "*") 'org-agenda-bulk-mark-all)
  (define-key org-agenda-mode-map (kbd "%") 'org-agenda-bulk-mark-regexp)
  ;; (define-key org-agenda-mode-map (kbd "M") 'org-agenda-bulk-remove-all-marks)
  )


(use-package org
  :ensure t
  :defer 1
  :config
  (add-hook 'org-agenda-mode-hook #'ebaker/evilify-org-agenda-mode)

  (require 'eliot-org)
  )

;; ore pretty bullets
(use-package org-bullets
  :ensure t
  :defer 2
  :after (org)
  :hook (org-mode . org-bullets-mode))

;; Theme
(use-package doom-themes
  :ensure t
  :config
  (load-theme 'doom-one t))

;; ivy
(use-package ivy :demand
  :ensure t
  :defer 1
  :config
  (setq ivy-use-virtual-buffers t
	ivy-count-format "%d/%d ")
  (ivy-mode 1))

;; Which Key
(use-package which-key
  :ensure t
  :defer 1
  :init
  (setq which-key-separator " ")
  (setq which-key-prefix-prefix "+")
  :config
  (which-key-mode 1))

;; Custom keybinding
(use-package general
  :ensure t
  :defer 1
  :config (general-define-key
  :states '(normal visual insert emacs)
  :prefix "SPC"
  :non-normal-prefix "M-SPC"
  ;; "/"   '(counsel-rg :which-key "ripgrep") ; You'll need counsel package for this
  "TAB" '(switch-to-prev-buffer :which-key "previous buffer")
  ;; "SPC" '(helm-M-x :which-key "M-x")
  ;; "pf"  '(helm-find-files :which-key "find files")
  ;; Buffers
  ;; "bb"  '(helm-buffers-list :which-key "buffers list")
  "b" '(:ignore t :which-key "buffer")
  "bb" '(ivy-switch-buffer)
  ;; Window
  "w" '(:ignore t :which-key "windows")
  "wl"  '(windmove-right :which-key "move right")
  "wh"  '(windmove-left :which-key "move left")
  "wk"  '(windmove-up :which-key "move up")
  "wj"  '(windmove-down :which-key "move bottom")
  "w3"  '(split-window-right :which-key "split right")
  "w2"  '(split-window-below :which-key "split bottom")
  "wx"  '(delete-window :which-key "delete window")
  ;; Others
  "a" '(:ignore t :which-key "applications")
  "at"  '(ansi-term :which-key "open terminal")
))


;; Fancy titlebar for MacOS
(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearance . dark)) ;; light
(setq ns-use-proxy-icon  nil)
(setq frame-title-format nil)

;; @ebaker - review - very slow
;; (let ((path (shell-command-to-string ". ~/.zshrc; echo -n $PATH")))
;;   (setenv "PATH" path)
;;   (setq exec-path
;;	(append
;;	 (split-string-and-unquote path ":")
;;	 exec-path)))

;; @ebaker - org


;; ranger
;; (use-package ranger
;;   :ensure t
;;   :defer 2
;;   :commands (ranger)
;;   :bind (("C-x d" . deer))
;;   :config
;;   (setq ranger-cleanup-eagerly t))

;; Projectile
(use-package projectile
  :ensure t
  :defer 2
  :init
  (setq projectile-require-project-root nil)
  :config
  (projectile-mode 1))

    ;; Integration with `projectile'
    (with-eval-after-load 'projectile
      (setq projectile-completion-system 'ivy))


;; All The Icons
(use-package all-the-icons :ensure t)

;; NeoTree
(use-package neotree
  :ensure t
  :defer 2
  :init
  (setq neo-theme (if (display-graphic-p) 'icons 'arrow)))

;; Flycheck
(use-package flycheck
  :ensure t
  :defer 2
  :init (global-flycheck-mode))

;; flycheck - official flycheck-pos-tip
;; (use-package flycheck-pos-tip
;;   :ensure t
;;   :init (with-eval-after-load 'flycheck
;;	  (flycheck-pos-tip-mode)))

;; flycheck-popup-tip - https://github.com/flycheck/flycheck-popup-tip
(use-package flycheck-popup-tip
  :ensure t
   :defer 2
   :init
   (with-eval-after-load 'flycheck '(add-hook 'flycheck-mode-hook 'flycheck-popup-tip-mode)))

(with-eval-after-load 'flycheck (flycheck-popup-tip-mode))

;; https://alhassy.github.io/init/

;; Make it very easy to see the line with the cursor.
(global-hl-line-mode t)

;; Clean up any accidental trailing whitespace and in other places,
;; upon save.
(add-hook 'before-save-hook 'whitespace-cleanup)

;; (use-package diminish)

;; ;; Let's hide some markers.
;; (diminish 'eldoc-mode)
;; (diminish 'org-indent-mode)
;; (diminish 'subword-mode)

;; https://emacs.stackexchange.com/questions/20946/generate-mouse-2-event-from-macbook-trackpad
(defun my-flyspell-mode-hook ()
  ;; Do things when flyspell enters of leaves flyspell mode.
  ;; Added manually
  ;;
  ;; Magic Mouse Fixes
  (if flyspell-mode (progn
       (define-key flyspell-mouse-map [down-mouse-3] #'flyspell-correct-word)
       (define-key flyspell-mouse-map [mouse-3] #'undefined))
    nil)
  ;; End my-flyspell-mode-hook
  )

(use-package flyspell
  :defer 2
  :init
  (add-hook 'flyspell-mode-hook 'my-flyspell-mode-hook)
  :hook
  ((text-mode . flyspell-mode))

  )

(setq ispell-program-name "/usr/local/bin/aspell")
(setq ispell-dictionary "en_US") ;; set the default dictionary

;; (diminish 'flyspell-mode) ;; Don't show it in the modeline.
;; https://github.com/d12frosted/flyspell-correct/issues/30
;; (use-package flyspell-correct-popup
;;   :bind (("C-M-;" . flyspell-correct-wrapper)
;;	 (:map popup-menu-keymap
;;	      ("TAB" . popup-next)
;;	      ("S-TAB" . popup-previous)))
;;   :init
;;   (setq flyspell-correct-interface #'flyspell-correct-popup))



;; (global-font-lock-mode t)
;; (custom-set-faces
;;  ;; custom-set-faces was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  '(org-done ((t (:foreground "#A2FF38" :weight bold))))
;;  '(org-todo ((t (:foreground "#ff39a3" :weight bold)))))

;; LSP
(use-package lsp-mode
  :ensure t
  :defer 2
  :init
  (add-hook 'prog-major-mode #'lsp-prog-major-mode-enable))

(use-package lsp-ui
  :ensure t
  :defer 2
  :init
  (add-hook 'lsp-mode-hook 'lsp-ui-mode))

;; Company mode
(use-package company
  :ensure t
  :init
  (setq company-minimum-prefix-length 3)
  (setq company-auto-complete nil)
  (setq company-idle-delay 0)
  (setq company-require-match 'never)
  (setq company-frontends
	'(company-pseudo-tooltip-unless-just-one-frontend
	  company-preview-frontend
	  company-echo-metadata-frontend))
  (setq tab-always-indent 'complete)
  (defvar completion-at-point-functions-saved nil)
  :config
  (global-company-mode 1)
  (define-key company-active-map (kbd "TAB") 'company-complete-common-or-cycle)
  (define-key company-active-map (kbd "<tab>") 'company-complete-common-or-cycle)
  (define-key company-active-map (kbd "S-TAB") 'company-select-previous)
  (define-key company-active-map (kbd "<backtab>") 'company-select-previous)
  (define-key company-mode-map [remap indent-for-tab-command] 'company-indent-for-tab-command)
  (defun company-indent-for-tab-command (&optional arg)
    (interactive "P")
    (let ((completion-at-point-functions-saved completion-at-point-functions)
	  (completion-at-point-functions '(company-complete-common-wrapper)))
      (indent-for-tab-command arg)))

(defun company-complete-common-wrapper ()
  (let ((completion-at-point-functions completion-at-point-functions-saved))
    (company-complete-common))))

(use-package company-lsp
  :ensure t
  :defer 2
  :init
  (push 'company-lsp company-backends))

;; Powerline
(use-package spaceline
  :ensure t
  :init
  (setq powerline-default-separator 'utf-8)
  :config
  (spaceline-emacs-theme)
  (spaceline-toggle-minor-modes-off)
  (spaceline-toggle-buffer-size-off)
  (spaceline-toggle-evil-state-on))

;;;;;;;;;;;;;;;;;;;;;;;
;; Language Supports ;;
;;;;;;;;;;;;;;;;;;;;;;;

;; elisp
(use-package highlight-defined
  :ensure t
  :defer 2
  :config
  (add-hook 'emacs-lisp-mode-hook 'highlight-defined-mode)
  (add-to-list 'auto-mode-alist '("\\.el\\'" . display-line-number-mode))
  )

;; JavaScript
(use-package js2-mode
  :ensure t
  :defer 2
  :init
  (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
  (add-to-list 'auto-mode-alist '("\\.js\\'" . display-line-number-mode))
  )

(use-package tern
  :ensure t
  :defer 2)

 ;; Typescript
;; (use-package typescript-mode
;;   :ensure t
;;   :defer 2
;;   :init
;;   (add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-mode)))

(add-hook 'js2-mode-hook 'lsp)

;; ;; Dashboard
;; (use-package dashboard
;;   :ensure t
;;   :config
;;   (dashboard-setup-startup-hook))

;; ;; Set the title
;; (setq dashboard-banner-logo-title "Welcome to Emacs Dashboard")
;; ;; Set the banner
;; (setq dashboard-startup-banner 3)
;; ;; Value can be
;; ;; 'official which displays the official emacs logo
;; ;; 'logo which displays an alternative emacs logo
;; ;; 1, 2 or 3 which displays one of the text banners
;; ;; "path/to/your/image.png" which displays whatever image you would prefer

;; ;; Content is not centered by default. To center, set
;; (setq dashboard-center-content t)

;; ;; To disable shortcut "jump" indicators for each section, set
;; (setq dashboard-show-shortcuts nil)

;; ;; go to dashboard on startup

;; (setq initial-buffer-choice (lambda () (get-buffer "*dashboard*")))


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("7dc3fe8fadb914563790a3fbe587fd455626442f66da333ea4de2c455feefb98" "fa1fa0bc00fc80f5466cfd6b595e4a010d0c1953b7f135fd2658ca93ff8c8a17" "423435c7b0e6c0942f16519fa9e17793da940184a50201a4d932eafe4c94c92d" default)))
 '(package-selected-packages
   (quote
    (org-bullets org ivy undo-tree gnu-elpa-keyring-update esup flyspell-correct-popup page-break-lines counsel doom-themes evil use-package)))
 '(spacemacs-theme-custom-colors
   (quote
    ((head1 . "#b48ead")
     (head2 . "#a7a6d4")
     (head3 . "#bfebbf")
     (head4 . "#f0dfaf")))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-done ((t (:foreground "#A2FF38" :weight bold))))
 '(org-todo ((t (:foreground "#ff39a3" :weight bold)))))


;; Make gc pauses faster by decreasing the threshold.
(setq gc-cons-threshold (* 2 1000 1000))