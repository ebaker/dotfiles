;; bug fix gnutls
(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")

;; heavily insprired by https://blog.jft.rocks/emacs/emacs-from-scratch.html

;; Minimal UI
(scroll-bar-mode -1)
(tool-bar-mode   -1)
(tooltip-mode    -1)
(menu-bar-mode   -1)

;; Package configs
(require 'package)
(setq package-enable-at-startup nil)
(setq package-archives '(("org"   . "http://orgmode.org/elpa/")
                          ("melpa" . "https://melpa.org/packages/")
                          ("gnu"   . "https://elpa.gnu.org/packages/")))

(package-initialize)

;; Bootstrap `use-package`
(unless package-archive-contents
  (package-refresh-contents))

;; Initialize use-package on non-linux platforms
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

;; command-log-mode shows hotkeys used in realtime
(use-package command-log-mode
  :defer 2)

;; Use a hook so the message doesn't get clobbered by other messages.
(add-hook 'emacs-startup-hook
  (lambda ()
    (message "Emacs ready in %s with %d garbage collections."
      (format "%.2f seconds"
        (float-time
          (time-subtract after-init-time before-init-time)))
      gcs-done)))

;; Set default font
(set-face-attribute 'default nil
  :family "Inconsolata for Powerline"
  :height 180
  :weight 'normal
  :width 'normal)

;; https://emacs.stackexchange.com/questions/16818/cocoa-emacs-24-5-font-issues-inconsolata-dz
(add-to-list 'default-frame-alist '(height . 44))
(add-to-list 'default-frame-alist '(width . 100))


;; Make startup faster by reducing the frequency of garbage
;; collection.  The default is 800 kilobytes.  Measured in bytes.
(setq gc-cons-threshold (* 50 1000 1000))

;;
;; emacs setup
;;

;; Garbage-collect on focus-out, Emacs /should/ feel snappier.
(add-hook 'focus-out-hook #'garbage-collect)

;; super & meta on mac
(setq mac-command-modifier 'super
  mac-option-modifier  'meta)

;; spaces for tabs
(setq-default indent-tabs-mode nil)

;; line numbers
(column-number-mode)
(global-display-line-numbers-mode t)

;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
                 term-mode-hook
                 shell-mode-hook
                 eshell-mode-hook
                 treemacs-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; emacs config hotkey
(defun find-config ()
  "Edit init.el"
  (interactive)
  (find-file "~/.emacs.d/init.el"))

;; make confirmation prompts shorter
(defalias 'yes-or-no-p 'y-or-n-p)

;; save buffers only once on quit
;; https://emacs.stackexchange.com/questions/13662/a-confirmation-after-c-x-c-c-before-exiting-emacs
(defun my-save-buffers-kill-emacs (&optional arg)
  "Offer to save each buffer(once only), then kill this Emacs process.
With prefix ARG, silently save all file-visiting buffers, then kill."
  (interactive "P")
  (save-some-buffers arg t)
  (and (or (not (fboundp 'process-list))
         ;; process-list is not defined on MSDOS.
         (let ((processes (process-list))
                active)
           (while processes
             (and (memq (process-status (car processes)) '(run stop open listen))
               (process-query-on-exit-flag (car processes))
               (setq active t))
             (setq processes (cdr processes)))
           (or (not active)
             (progn (list-processes t)
               (y-or-n-p "Active processes exist; kill them and exit anyway? ")))))
    ;; Query the user for other things, perhaps.
    (run-hook-with-args-until-failure 'kill-emacs-query-functions)
    (or (null confirm-kill-emacs)
      (funcall confirm-kill-emacs "Really exit Emacs? "))
    (kill-emacs)))

;; load-path
(push "~/.emacs.d/elisp/" load-path)

(defun ebaker/other-window-reverse ()
  (interactive)
  (other-window -1))

;; @ebaker - comment-or-uncomment-region-or-line
(require 'comment-or-uncomment-region-or-line)

;; Make escape quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; @ebaker - global super custom hotkeys
(global-set-key (kbd "s-/") 'comment-or-uncomment-region-or-line)
(global-set-key (kbd "s-'") 'indent-region)
(global-set-key (kbd "s-o") 'other-window)
(global-set-key (kbd "s-O") 'ebaker/other-window-reverse)
(global-set-key (kbd "s-0") 'delete-window)
(global-set-key (kbd "s-1") 'delete-other-windows)
(global-set-key (kbd "s-2") 'split-window-below)
(global-set-key (kbd "s-3") 'split-window-right)
(global-set-key (kbd "s-b") 'ido-switch-buffer)
(global-set-key (kbd "s-k") 'ido-kill-buffer)
(global-set-key (kbd "s-a") 'org-agenda)
(global-set-key (kbd "s-r") 'revert-buffer)
(global-set-key (kbd "C-x i") 'find-config)
(global-set-key (kbd "C-x C-c") 'my-save-buffers-kill-emacs)
(global-set-key (kbd "C-x C-r") 'counsel-recentf)
(global-set-key (kbd "C-M-u") 'universal-argument)

;; @ebaker - remove keybinding eyebrowse
(assq-delete-all 'eyebrowse-mode minor-mode-map-alist)

;; from https://sam217pa.github.io/2016/09/02/how-to-build-your-own-spacemacs/

(setq delete-old-versions -1 )		; delete excess backup versions silently
(setq version-control t )		; use version control
(setq vc-make-backup-files t )		; make backups file even when in version controlled dir
(setq backup-directory-alist `(("." . "~/.emacs.d/backups")) ) ; which directory to put backups file
(setq vc-follow-symlinks t )                       ; don't ask for confirmation when opening symlinked file
(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-save-list/" t)) ) ;transform backups file name
(setq inhibit-startup-screen t )	; inhibit useless and old-school startup screen
(setq ring-bell-function 'ignore )	; silent bell when you make a mistake
(setq coding-system-for-read 'utf-8 )	; use utf-8 by default
(setq coding-system-for-write 'utf-8 )
(setq sentence-end-double-space nil)	; sentence SHOULD end with only a point.
;; (setq default-fill-column 140)		; toggle wrapping text at the 80th character
(setq initial-scratch-message ";; *scratch*\n\n") ; print a default message in the empty scratch buffer opened at startup

;; https://emacs.stackexchange.com/questions/28736/emacs-pointcursor-movement-lag/28746
(setq auto-window-vscroll nil)

;; custom.el
(setq custom-file (make-temp-file "emacs-custom"))
(setq-default custom-file (expand-file-name ".custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))
;; (setq custom-file (make-temp-file "emacs-custom")) ; custom.el - don't persist changes

;; Make it very easy to see the line with the cursor.
(global-hl-line-mode t)

;; Clean up any accidental trailing whitespace and in other places,
;; upon save.
(add-hook 'before-save-hook 'whitespace-cleanup)

;; remembers minibuffer history between sessions
(save-place-mode t)

;; cursor at the same place it was at before
(savehist-mode t)

;; Show matching parens
(setq show-paren-delay 0)
(show-paren-mode 1)

;; (global-font-lock-mode t)

;; environment variables
(use-package exec-path-from-shell
  :config
  (exec-path-from-shell-initialize))

;; Defer Packages you don’t need Immediately with Idle Timers
(use-package recentf
  :ensure nil
  ;; Loads after 1 second of idle time.
  :defer 1)

;; undo-tree
(use-package undo-tree
  ;; :diminish undo-tree-mode:
  :init
  (global-undo-tree-mode 1))

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
  (define-key evil-normal-state-map (kbd "C-n") 'evil-backward-char)
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join))

;; Vim mode
(use-package evil
  :defer 1
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-disable-insert-state-bindings t)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump nil)
  (setq evil-undo-system 'undo-tree)
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

(use-package evil-collection
  :config
  (evil-collection-init 'magit)
  (evil-collection-init 'dired))

;; Orgmode

(use-package org
  :defer 1
  :config
  (add-hook 'org-agenda-mode-hook #'ebaker/evilify-org-agenda-mode)
  (require 'eliot-org))

;; https://emacs.stackexchange.com/questions/22405/after-executing-org-narrow-to-subtree-how-do-i-move-between-subtrees-of-the-sam
(defun my/org-narrow-forward ()
  "Move to the next subtree at same level, and narrow to it."
  (interactive)
  (widen)
  (org-forward-heading-same-level 1)
  (org-narrow-to-subtree))

;; ore pretty bullets
(use-package org-bullets
  :defer 2
  :after (org)
  :hook (org-mode . org-bullets-mode))

;; Theme
(use-package doom-themes
  :config
  (load-theme 'doom-tomorrow-day t))
;; (load-theme 'doom-one t))

;; ivy
(use-package ivy
  :defer 1
  :diminish
  :bind (("C-s" . swiper)
          :map ivy-minibuffer-map
          ("TAB" . ivy-alt-done)
          ("C-l" . ivy-alt-done)
          ("C-j" . ivy-next-line)
          ("C-k" . ivy-previous-line)
          :map ivy-switch-buffer-map
          ("C-k" . ivy-previous-line)
          ("C-l" . ivy-done)
          ("C-d" . ivy-switch-buffer-kill)
          :map ivy-reverse-i-search-map
          ("C-k" . ivy-previous-line)
          ("C-d" . ivy-reverse-i-search-kill))
  :config
  (setq ivy-use-virtual-buffers t
    ivy-count-format "%d/%d "
    ivy-initial-inputs-alist nil)
  (ivy-mode 1))

(use-package ivy-rich
  :init
  (ivy-rich-mode 1))

(use-package counsel
  :bind (("M-x" . counsel-M-x)
          ;; ("C-x b" . counsel-ibuffer)
          ;; ("C-x C-f" . counsel-find-file)
          :map minibuffer-local-map
          ("C-r" . 'counsel-minibuffer-history)))

(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . helpful-function)
  ([remap describe-symbol] . helpful-symbol)
  ([remap describe-variable] . helpful-variable)
  ([remap describe-command] . helpful-command)
  ([remap describe-key] . helpful-key))

;; Which Key
(use-package which-key
  :init
  (setq which-key-separator " ")
  (setq which-key-prefix-prefix "+")
  :diminish which-key-mode
  :config
  (which-key-mode)
  (setq which-key-idle-delay 0.05))

(use-package expand-region
  :bind ("C-=" . er/expand-region))

;; Custom keybinding
(use-package general
  :defer 1
  :init
  (setq general-override-states '(insert
                                   emacs
                                   hybrid
                                   normal
                                   visual
                                   motion
                                   operator
                                   replace))
  :config
  (general-create-definer ebaker/leader-keys
    :keymaps '(normal visual insert emacs)
    :prefix "SPC"
    :global-prefix "M-SPC")
  (ebaker/leader-keys
    "/"   '(counsel-rg :which-key "ripgrep") ; You'll need counsel package for this
    "TAB" '(switch-to-prev-buffer :which-key "previous buffer")
    "SPC" '(counsel-M-x :which-key "M-x")

    ;; Buffers
    "b" '(:ignore t :which-key "buffer")
    "bb" '(ivy-switch-buffer)

    ;; undo tree
    "u" '(undo-tree-visualize :which-key u)

    ;; describe-
    "d" '(:ignore t :which-key "describe-")
    "dv" '(describe-variable :which-key "describe-variable")
    "df" '(describe-function :which-key "describe-function")
    "dk" '(describe-key :which-key "describe-key")

    ;; eval-
    "e" '(:ignore t :which-key "eval-")
    "eb" '(eval-buffer :which-key "eval-buffer")
    "ex" '(eval-expression :which-key "eval-expression")
    "er" '(eval-region :which-key "eval-region")

    ;; Window
    "w" '(:ignore t :which-key "windows")
    "wl"  '(windmove-right :which-key "move right")
    "wh"  '(windmove-left :which-key "move left")
    "wk"  '(windmove-up :which-key "move up")
    "wj"  '(windmove-down :which-key "move bottom")
    "w3"  '(split-window-right :which-key "split right")
    "w2"  '(split-window-below :which-key "split bottom")
    "wx"  '(delete-window :which-key "delete window")

    ;; Toggles
    "t"  '(:ignore t :which-key "toggles")

    ;; Others
    "a" '(:ignore t :which-key "applications")
    "at"  '(ansi-term :which-key "open terminal")))


;; Fancy titlebar for MacOS
(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearance . dark)) ;; light
(setq ns-use-proxy-icon  nil)
(setq frame-title-format nil)


;; All The Icons
(use-package all-the-icons)

;; treemacs
(use-package treemacs
  :config
  (setq treemacs-show-cursor t)
  (global-set-key [f8] 'treemacs))

(use-package treemacs-all-the-icons
  :config
  (treemacs-load-theme "all-the-icons"))

(use-package treemacs-evil)

;; vertical-center-mode
(require 'vertical-center-mode)
(ebaker/leader-keys
  "tc" 'vertical-center-mode)

;;
;; Diminish
;;
;; https://alhassy.github.io/init/

;; eldoc diminish working
;; (use-package diminish :ensure t
;;   :config
;;   (eval-after-load "eldoc" '(diminish 'eldoc-mode)))

;; (use-package delight
;;   :ensure t
;;   :config
;;   (delight '((eldoc-mode nil "eldoc"))))

;; ;; ;; Let's hide some markers.
;; ;; (diminish 'org-indent-mode)
;; ;; (diminish 'subword-mode)

;; rainbow delimiters
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package hydra)

(defhydra hydra-text-scale (:timeout 4)
  "scale text"
  ("j" text-scale-increase "in")
  ("k" text-scale-decrease "out")
  ("f" nil "finished" :exit t))

(ebaker/leader-keys
  "ts" '(hydra-text-scale/body :which-key "scale text"))

;; Git
(use-package magit
  :bind ("C-M-;" . magit-status)
  :commands (magit-status magit-get-current-branch)
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(ebaker/leader-keys
  "g"   '(:ignore t :which-key "git")
  "gs"  'magit-status
  "gd"  'magit-diff-unstaged
  "go"  'magit-branch-or-checkout
  "gl"   '(:ignore t :which-key "log")
  "glc" 'magit-log-current
  "glf" 'magit-log-buffer-file
  "gb"  'magit-branch
  "gP"  'magit-push-current
  "gp"  'magit-pull-branch
  "gf"  'magit-fetch
  "gF"  'magit-fetch-all
  "gr"  'magit-rebase)

;;
;; Flyspell
;;

;; https://emacs.stackexchange.com/questions/20946/generate-mouse-2-event-from-macbook-trackpad
(defun flyspell-mouse-2-macbook ()
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
  :ensure nil
  :init
  (setq ispell-program-name "/usr/local/bin/aspell")
  (setq ispell-dictionary "en_US") ;; set the default dictionary
  :diminish flyspell-mode: ;; Don't show it in the modeline.
  :hook
  ((text-mode . flyspell-mode)
    (flyspell-mode . flyspell-mouse-2-macbook)))

;; https://github.com/d12frosted/flyspell-correct/issues/30
;; (use-package flyspell-correct-popup
;;   :bind (("C-M-;" . flyspell-correct-wrapper)
;;	 (:map popup-menu-keymap
;;        ("TAB" . popup-next)
;;        ("S-TAB" . popup-previous)))
;;   :init
;;   (setq flyspell-correct-interface #'flyspell-correct-popup))

;; LSP
;; (require 'eliot-lsp)

;;
;; Company mode
;;

(use-package company
  :init
  (setq company-minimum-prefix-length 2)
  (setq company-auto-complete nil)
  (setq company-idle-delay 0.0)
  (setq company-require-match 'never)
  (setq company-frontends
    '(
       company-pseudo-tooltip-unless-just-one-frontend
       company-preview-frontend
       company-echo-metadata-frontend
       )
    )
  (setq tab-always-indent 'complete)
  (defvar completion-at-point-functions-saved nil)
  :config
  (global-company-mode 1)
  ;; (define-key company-active-map (kbd "TAB") 'company-complete-common-or-cycle)
  ;; (define-key company-active-map (kbd "<tab>") 'company-complete-common-or-cycle)
  ;; (define-key company-active-map (kbd "S-TAB") 'company-select-previous)
  ;; (define-key company-active-map (kbd "<backtab>") 'company-select-previous)
  (define-key company-active-map (kbd "<down>") 'company-complete-common-or-cycle)
  (define-key company-active-map (kbd "<up>") 'company-select-previous)
  (define-key company-active-map (kbd "<tab>") 'company-complete-selection)
  (define-key company-active-map (kbd "C-n") 'company-complete-common-or-cycle)
  (define-key company-active-map (kbd "C-p") 'company-select-previous)
  (define-key company-active-map (kbd "<tab>") 'company-complete-selection)
  (define-key company-active-map (kbd "<right>") 'company-complete-selection)
  (define-key company-active-map (kbd "TAB") 'company-complete-selection)
  (define-key company-active-map (kbd "S-TAB") 'company-abort)
  (define-key company-active-map (kbd "<backtab>") 'company-abort)
  (define-key company-active-map (kbd "ESC") 'company-abort)

  ;; prevent company from completing on its own when we type regular characters
  ;; ("SPC" . company--my-insert-spc)
  ;; ("."   . company--my-insert-dot)

  ;;   (define-key company-mode-map [remap indent-for-tab-command] 'company-indent-for-tab-command)
  ;;   (defun company-indent-for-tab-command (&optional arg)
  ;;     (interactive "P")
  ;;     (let ((completion-at-point-functions-saved completion-at-point-functions)
  ;;    (completion-at-point-functions '(company-complete-common-wrapper)))
  ;;       (indent-for-tab-command arg)))

  ;; (defun company-complete-common-wrapper ()
  ;;   (let ((completion-at-point-functions completion-at-point-functions-saved))
  ;;     (company-complete-common)))
  )

;; lsp
(use-package lsp-mode
  ;; :config
  ;; (lsp-headerline-breadcrumb-enable t)
  :hook (((js2-mode rjsx-mode) . lsp)
          (lsp-mode . lsp-enable-which-key-integration))
  :bind (([s-mouse-1] . xref-find-definitions)
          (:map lsp-mode-map
          ("TAB" . completion-at-point)))
  :commands lsp)

;; lsp general
(ebaker/leader-keys
  "l"  '(:ignore t :which-key "lsp")
  "ld" 'xref-find-definitions
  "lr" 'xref-find-references
  "lb" 'xref-pop-marker-stack
  "ln" 'lsp-ui-find-next-reference
  "lp" 'lsp-ui-find-prev-reference
  "ls" 'counsel-imenu
  "le" 'lsp-ui-flycheck-list
  "lS" 'lsp-ui-sideline-mode
  "lX" 'lsp-execute-code-action)

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :config
  ;; (setq lsp-ui-sideline-enable t)
  ;; (setq lsp-ui-sideline-show-hover nil)
  ;; (setq lsp-ui-doc-position 'bottom)
  (lsp-ui-doc-show))

;;; Elementary textual completion backend.
(setq company-backends
  ;; (add-to-list 'company-backends 'company-dabbrev)
  (add-to-list 'company-backends 'company-lsp))

;; Company UI
;; (use-package company-box
;;   :ensure t
;;   :hook (company-mode . company-box-mode))
;; (use-package company-posframe
;;   :ensure t
;;   :config (company-posframe-mode 1))
;; (use-package posframe
;;   :ensure t)
;; (use-package company-quickhelp
;;   :ensure t)

;; (use-package eldoc-box
;; :ensure t)
;; (use-package company-quickhelp
;;   :ensure t)

;; eglot
;; (use-package eglot
;;   :ensure t
;;   )


;; Yasnippet
;; (require 'eliot-yasnippet)

;;
;; Powerline
;;

;; spaceline
;; (use-package spaceline
;;   :ensure t
;;   :init
;;   (setq powerline-default-separator 'utf-8)
;;   :config
;;   (spaceline-emacs-theme)
;;   (spaceline-toggle-minor-modes-off)
;;   (spaceline-toggle-buffer-size-off)
;;   (spaceline-toggle-evil-state-on))

;; doom-modeline
(use-package doom-modeline
  :defer t
  :custom((doom-modeline-height 15))
  :hook (after-init . doom-modeline-init))

;; xterm-colors
(use-package xterm-color
  :config
  (setq compilation-environment '("TERM=xterm-256color"))
(defun my/advice-compilation-filter (f proc string)
  (funcall f proc (xterm-color-filter string)))
(advice-add 'compilation-filter :around #'my/advice-compilation-filter))

;;;;;;;;;;;;;;;;;;;;;;;
;; Language Supports ;;
;;;;;;;;;;;;;;;;;;;;;;;

;; editorconfig
(use-package editorconfig
  :config
  (editorconfig-mode 1))

;; smartparens
(use-package smartparens
  :hook (prog-mode . smartparens-mode))

;; elisp
(setq lisp-indent-offset 2)

(use-package highlight-defined
  :defer 2
  :config
  (add-hook 'emacs-lisp-mode-hook 'highlight-defined-mode)
  (add-hook 'emacs-lisp-mode-hook 'display-line-numbers-mode))

(add-hook 'emacs-lisp-mode-hook #'eldoc-mode)

;; YAML
(use-package yaml-mode
  :mode "\\.ya?ml\\'")

;; JavaScript
;; npm i -g typescript typescrypt-language-server
(use-package js2-mode
  :defer 2
  :init
  (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
  :config
  (add-hook 'js2-mode 'display-line-numbers-mode)
  (setq js2-mode-show-parse-errors nil)
  (setq js2-mode-show-strict-warnings nil))

(add-hook 'js2-mode
  (lambda ()
    (setq-local eldoc-documentation-function #'ignore)))

(use-package tern)

;; (use-package company-tern
;;   :ensure t
;;   :config
;; (add-to-list 'company-backends 'company-tern))

;; flymake
;; https://emacs.stackexchange.com/questions/36363/how-to-change-flycheck-symbol-like-spacemacs
(define-fringe-bitmap 'flymake-fringe-bitmap-ball
  (vector #b00000000
    #b00000000
    #b00000000
    #b00000000
    #b00000000
    #b00111000
    #b01111100
    #b11111110
    #b11111110
    #b01111100
    #b00111000
    #b00000000
    #b00000000
    #b00000000
    #b00000000
    #b00000000
    #b00000000))

(define-fringe-bitmap 'flymake-fringe-bitmap-ball-medium
  (vector #b00000000
    #b00000000
    #b00000000
    #b00000000
    #b00000000
    #b00000000
    #b00111000
    #b01111100
    #b01111100
    #b00111000
    #b00000000
    #b00000000
    #b00000000
    #b00000000
    #b00000000
    #b00000000
    #b00000000))

(use-package flymake-diagnostic-at-point
  :after flymake
  :config
  (add-hook 'flymake-mode-hook #'flymake-diagnostic-at-point-mode))

(use-package flymake-eslint
  :config
  (setq flymake-diagnostic-at-point-timer-delay 0.3)
  (add-hook 'js2-mode-hook ; or whatever the mode-hook is for your mode of choice
    (lambda ()
      (flymake-eslint-enable)))
  )

;; Typescript
;; (use-package typescript-mode
;;   :ensure t
;;   :defer 2
;;   :init
;;   (add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-mode)))

(use-package prettier-js
  :config
  (add-hook 'js2-mode-hook 'prettier-js-mode))

(use-package json-mode
  :mode ("\\.json\\'"))

(use-package rjsx-mode
  :mode ("/\\(components\\|containers\\|src\\|pages\\)/.*\\.js[x]?\\'")
  ;; (("/\\(containers\\)/[^/]*\\.js" . rjsx-mode)
  ;;  ("/\\(components\\)/[^/]*\\.js" . rjsx-mode)
  ;;  ("\\.jsx\\'" . rjsx-mode))
  ;; :config
  ;; (add-to-list 'lsp-language-id-configuration '(rjsx-mode . "javascript"))
  )

(use-package add-node-modules-path
  :hook ((js2-mode . add-node-modules-path)
          (rjsx-mode . add-node-modules-path)))

;; PHP
(use-package php-mode
  :mode ("\\.php\\'" . php-mode))

;; Docker
(use-package dockerfile-mode
  :mode
  ("Dockerfile\\(-.*\\)?\\'" . dockerfile-mode))

;; Projectile
(use-package projectile
  :defer 2
  :diminish projectile-mode
  :config (projectile-mode)
  :custom ((projectile-completion-system 'ivy))
  :bind-keymap
  ("C-c p" . projectile-command-map)
  ("C-M-p" . projectile-command-map)
  :bind(("s-F" . projectile-ripgrep))
  :init
  ;; NOTE: Set this to the folder where you keep your Git repos!
  (setq projectile-require-project-root nil)
  (when (file-directory-p "~/r/tawkify")
    (setq projectile-project-search-path '("~/r/tawkify")))
  (setq projectile-switch-project-action #'projectile-dired))

(use-package counsel-projectile
  :after projectile
  :bind (("s-p" . counsel-projectile-find-file))
  :config
  (counsel-projectile-mode))
  (counsel-projectile-mode)
  (define-key projectile-mode-map [remap projectile-ripgrep] nil))

(ebaker/leader-keys
  "p" '(:ignore t :which-key "projectile")
  "pf"  'counsel-projectile-find-file
  "ps"  '(counsel-projectile-switch-project :which-key "[s]witch project")
  "pr"  'counsel-projectile-rg
  "pF"  'projectile-ripgrep
  "pp"  'counsel-projectile
  "pc"  'projectile-compile-project
  "pP"  'projectile-test-project
  "pd"  'projectile-dired)

(use-package counsel-projectile
  :defer 2
  :config (counsel-projectile-mode))


(setq delete-by-moving-to-trash t)
(when (string= system-type "darwin")
  (setq dired-use-ls-dired nil)
  (defun system-move-file-to-trash (file)
    "Use \"trash\" to move FILE to the system trash.
When using Homebrew, install it using \"brew install trash\"."
    (call-process (executable-find "trash")
      nil 0 nil
      file)))

(use-package dired
  :ensure nil
  :commands (dired dired-jump)
  :bind (("C-x C-j" . dired-jump))
  :custom ((dired-listing-switches "-aFl"))
  :config
  (evil-collection-define-key 'normal 'dired-mode-map
    "h" 'dired-single-up-directory
    "l" 'dired-single-buffer))

(use-package dired-single)

;; (use-package all-the-icons-dired
;; :hook (dired-mode . all-the-icons-dired-mode))

(use-package dired-hide-dotfiles
  :hook (dired-mode . dired-hide-dotfiles-mode)
  :config
  (evil-collection-define-key 'normal 'dired-mode-map
    "H" 'dired-hide-dotfiles-mode))

;; Dashboard
;; (require 'eliot-dashboard)

;; Make gc pauses faster by decreasing the threshold.
(setq gc-cons-threshold (* 2 1000 1000))
(global-eldoc-mode -1)
