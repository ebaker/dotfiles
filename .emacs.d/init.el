;; -*- lexical-binding: t; -*-

;; bug fix gnutls
(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")


;; heavily insprired by https://blog.jft.rocks/emacs/emacs-from-scratch.html

;; Minimal UI
(scroll-bar-mode -1)
(tool-bar-mode   -1)
(tooltip-mode    -1)
(menu-bar-mode   -1)

;; mac
(setq mac-pass-command-to-system nil)

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
  ;; :family "Inconsolata for Powerline"
  :family "Fira Mono for Powerline"
  :height 180
  :weight 'normal
  :width 'normal)

;; emojis emacs 27+
;; https://github.crookster.org/emacs27-from-homebrew-on-macos-with-emoji/
(set-fontset-font
  "fontset-default" 'unicode "Apple Color Emoji" nil 'prepend)
(set-fontset-font
  t 'symbol (font-spec :family "Apple Color Emoji") nil 'prepend)

;; https://emacs.stackexchange.com/questions/16818/cocoa-emacs-24-5-font-issues-inconsolata-dz
(add-to-list 'default-frame-alist '(height . 44))
(add-to-list 'default-frame-alist '(width . 100))


;; Make startup faster by reducing the frequency of garbage
;; collection.  The default is 800 kilobytes.  Measured in bytes.
(setq gc-cons-threshold (* 50 1000 1000))

;; lsp-mode performance
(setq read-process-output-max (* 1024 1024))
(setq gc-cons-threshold 100000000)

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

(delete-selection-mode t)

;; line numbers
(column-number-mode)
(global-display-line-numbers-mode t)

;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
                 term-mode-hook
                 shell-mode-hook
                 eshell-mode-hook
                 vterm-mode-hook
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

;; cut, copy, paste, save, save as, undo, redo
(global-set-key (kbd "s-x") 'kill-region)
(global-set-key (kbd "s-c") 'kill-ring-save)
(global-set-key (kbd "s-v") 'yank)
(global-set-key (kbd "s-s") 'save-buffer)
(global-set-key (kbd "s-S") 'write-file)
(global-set-key (kbd "s-z") 'evil-undo)
(global-set-key (kbd "s-Z") 'evil-redo)

;; @ebaker - global super custom hotkeys
(global-set-key (kbd "s-/") 'comment-or-uncomment-region-or-line)
(global-set-key (kbd "s-'") 'indent-region)
(global-set-key (kbd "s-o") 'other-window)
(global-set-key (kbd "s-O") 'ebaker/other-window-reverse)
(global-set-key (kbd "s-0") 'delete-window)
(global-set-key (kbd "s-1") 'delete-other-windows)
(global-set-key (kbd "s-2") 'split-window-below)
(global-set-key (kbd "s-3") 'split-window-right)
;; (global-set-key (kbd "s-b") 'ido-switch-buffer)
(global-set-key (kbd "s-b") 'persp-ivy-switch-buffer)
(global-set-key (kbd "s-k") 'ido-kill-buffer)
(global-set-key (kbd "s-a") 'org-agenda)
(global-set-key (kbd "s-r") 'revert-buffer)
(global-set-key (kbd "C-x i") 'find-config)
(global-set-key (kbd "C-x C-c") 'my-save-buffers-kill-emacs)
(global-set-key (kbd "C-x C-r") 'counsel-recentf)
(global-set-key (kbd "C-M-u") 'universal-argument)
(global-set-key (kbd "C-M-n") 'persp-next)
(global-set-key (kbd "C-M-p") 'persp-prev)
(global-set-key (kbd "<f2>") 'vterm-toggle)
(global-set-key (kbd "C-<f2>") 'vterm-toggle-cd)

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

;; https://www.gnu.org/software/emacs/manual/html_node/elisp/File-Locks.html
(setq lock-file-name-transforms
      '(("\\`/.*/\\([^/]+\\)\\'" "~/.emacs.d/lockfiles/\\1" t)))

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
  (global-undo-tree-mode 1)
  :config
  (setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo"))))

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
  (define-key evil-normal-state-map (kbd "C-b") 'evil-backward-char)
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)
  (define-key evil-normal-state-map "gd" 'xref-find-definitions)
  (define-key evil-normal-state-map "gD" 'xref-find-references)
  (define-key evil-normal-state-map "gb" 'xref-pop-marker-stack)
  (define-key evil-normal-state-map (kbd "M-.") 'xref-find-definitions)
  (define-key evil-insert-state-map (kbd "M-.") 'xref-find-definitions))

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
  (evil-collection-init 'dired)
  (evil-collection-init 'vterm))

(use-package evil-surround
  :config
  (global-evil-surround-mode 1))

;; Orgmode

(use-package org
  :demand t
  :config
  (add-hook 'org-agenda-mode-hook #'ebaker/evilify-org-agenda-mode)
  ;; (require 'eliot-org)
  (setq org-refile-targets '((nil :maxlevel . 1)
                             (org-agenda-files :maxlevel . 1)))

  (setq org-outline-path-complete-in-steps nil)
  (setq org-refile-use-outline-path t)
  (setq org-agenda-span 'day)
  (require 'eliot-roam))



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

;; org-mode prettify
(setq-default prettify-symbols-alist '(("#+BEGIN_SRC" . "↦") ;; "†"
                                       ("#+END_SRC" . "⇤")
                                       ("#+begin_src" . "↦")
                                       ("#+end_src" .  "⇤")
                                       ;; (">=" . "≥")
                                        ;; ("=>" . "⇨")
                                        ))
(setq prettify-symbols-unprettify-at-point 'right-edge)
(add-hook 'org-mode-hook 'prettify-symbols-mode)

;; org-clip
(use-package org-cliplink
  :after (org))

;; Theme
(use-package doom-themes
  :config
  ;; (load-theme 'doom-tomorrow-day t))
  ;; (load-theme 'doom-spacegrey))
  ;; (load-theme 'doom-mono-light t))
  ;; (load-theme 'doom-opera-light t))
  ;; (load-theme 'doom-1337))
  ;; (load-theme 'doom-tomorrow-night))
  ;; (load-theme 'doom-one t))
  (load-theme 'doom-solarized-light t))
  ;; (load-theme 'doom-molokai t))
  ;; (load-theme 'doom-Iosvkem t))


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

(use-package counsel
  :bind (("M-x" . counsel-M-x)
          ;; ("C-x b" . counsel-ibuffer)
          ;; ("C-x C-f" . counsel-find-file)
          :map minibuffer-local-map
          ("C-r" . 'counsel-minibuffer-history)))

(use-package ivy-rich
  :init
  (ivy-rich-mode 1))

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
    :states '(normal visual motion)
    :keymaps 'override
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
    "at"  '(ansi-term :which-key "open terminal")

    ;; org-mode
    "o"   '(:ignore t :which-key "org mode")

    "oi"  '(:ignore t :which-key "insert")
    "oil" '(org-insert-link :which-key "insert link")

    "on"  '(org-toggle-narrow-to-subtree :which-key "toggle narrow")

    "os"  '(dw/counsel-rg-org-files :which-key "search notes")

    "oa"  '(org-agenda :which-key "status")
    "ot"  '(org-todo-list :which-key "todos")
    "oc"  '(org-capture t :which-key "capture")
    "ox"  '(org-export-dispatch t :which-key "export")))


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

;; forge
;; (use-package forge
;;   :after magit)

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
  :hook (((typescript-mode) . lsp) ;; ((js2-mode rjsx-mode). lsp)
          ((rustic-mode) . lsp)
          (lsp-mode . lsp-enable-which-key-integration))
  :bind (([s-mouse-1] . xref-find-definitions)
          (:map lsp-mode-map
          ("TAB" . completion-at-point)))
  :commands lsp
  :custom
  (lsp-rust-analyzer-cargo-watch-command "clippy")
  (lsp-eldoc-render-all t)
  (lsp-rust-analyzer-server-display-inlay-hints t)
  (setq lsp-idle-delay 0.100))

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
  (setq lsp-ui-doc-position 'bottom)
  (setq lsp-headerline-breadcrumb-enable-diagnostics nil)
  (lsp-ui-doc-show))

;;; Elementary textual completion backend.
;; (setq company-backends
  ;; (add-to-list 'company-backends 'company-dabbrev))

(use-package vterm)

(use-package vterm-toggle
  :init (setenv "TERM" "xterm")
  :bind (:map vterm-mode-map
          ("C-<return>" . vterm-toggle-insert-cd)
          ("<f2>" . vterm-toggle)))

(ebaker/leader-keys
  "tv"  '(:ignore t :which-key "vterm")
  "tvv" 'vterm-toggle
  "tvn" 'vterm-toggle-forward
  "tvp" 'vterm-toggle-backward)

;; UTF-8 support
;; (prefer-coding-system       'utf-8)
;; (set-default-coding-systems 'utf-8)
;; (set-terminal-coding-system 'utf-8)
;; (set-keyboard-coding-system 'utf-8)
;; (setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))

;; (require 'xterm-color)

;; (setq comint-output-filter-functions
;;       (remove 'ansi-color-process-output comint-output-filter-functions))

;; (add-hook 'shell-mode-hook
;;           (lambda ()
;;             ;; Disable font-locking in this buffer to improve performance
;;             (font-lock-mode -1)
;;             ;; Prevent font-locking from being re-enabled in this buffer
;;             (make-local-variable 'font-lock-function)
;;             (setq font-lock-function (lambda (_) nil))
;;             (add-hook 'comint-preoutput-filter-functions 'xterm-color-filter nil t)))

;; (setenv "TERM" "xterm-256color")

;; Company UI
(use-package company-box
  :hook (company-mode . company-box-mode))
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
(use-package yasnippet
  :hook (prog-mode . yas-minor-mode)
  :config
  (yas-reload-all))
;; Add yasnippet support for all company backends
;; https://github.com/syl20bnr/spacemacs/pull/179
(defvar company-mode/enable-yas t
  "Enable yasnippet for all backends.")

(defun company-mode/backend-with-yas (backend)
  (if (or (not company-mode/enable-yas) (and (listp backend) (member 'company-yasnippet backend)))
      backend
    (append (if (consp backend) backend (list backend))
            '(:with company-yasnippet))))

(setq company-backends (mapcar #'company-mode/backend-with-yas company-backends))

(use-package js-react-redux-yasnippets)
(ebaker/leader-keys
  "y"  '(:ignore t :which-key "yasnippet")
  "ya" 'yas-insert-snippet)
(setq lsp-completion-provider :none)
(setq lsp-completion-provider :capf)

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
  :custom(doom-modeline-height 15)
  :hook (after-init . doom-modeline-init))

;; ;; xterm-colors
;; (use-package xterm-color
;;   :config
;;   (setq compilation-environment '("TERM=xterm-256color"))
;; (defun my/advice-compilation-filter (f proc string)
;;   (funcall f proc (xterm-color-filter string)))
;; (advice-add 'compilation-filter :around #'my/advice-compilation-filter))

;;;;;;;;;;;;;;;;;;;;;;;
;; Language Supports ;;
;;;;;;;;;;;;;;;;;;;;;;;

;; editorconfig
(use-package editorconfig
  :config
  (editorconfig-mode 1)
  (setq  web-mode-markup-indent-offset 2
       web-mode-css-indent-offset 2
       web-mode-code-indent-offset 2
       web-mode-indent-style 2
       web-mode-block-padding 2
       web-mode-style-padding 2
       web-mode-script-padding 2
       js2-basic-offset 2
       js-indent-level 2
       css-indent-offset 2))

;; smartparens
(use-package smartparens
  :hook (prog-mode . smartparens-mode)
  :config
  (sp-local-pair 'prog-mode "{" nil :post-handlers '((indent-between-pair "RET")))
(sp-local-pair 'prog-mode "[" nil :post-handlers '((indent-between-pair "RET")))
(sp-local-pair 'prog-mode "(" nil :post-handlers '((indent-between-pair "RET"))))

(defun indent-between-pair (&rest _ignored)
  (newline)
  (indent-according-to-mode)
  (forward-line -1)
  (indent-according-to-mode))



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
;; (use-package js2-mode
;;   :defer 2
;;   :init
;;   (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
;;   :config
;;   (add-hook 'js2-mode 'display-line-numbers-mode)
;;   (setq js2-mode-show-parse-errors nil)
;;   (setq js2-mode-show-strict-warnings nil))

;; (add-hook 'js2-mode
;;   (lambda ()
;;     (setq-local eldoc-documentation-function #'ignore)))

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

(use-package typescript-mode
  :ensure t
  :init
  (define-derived-mode typescript-tsx-mode typescript-mode "tsx")
  :config
  (setq typescript-indent-level 2)
  (add-hook 'typescript-mode #'subword-mode)
  (add-to-list 'auto-mode-alist '("\\.[j|t]sx\\'" . typescript-tsx-mode))
  (add-to-list 'auto-mode-alist '("\\.[j|t]s\\'" . typescript-mode))
  )

(use-package tree-sitter
  :ensure t
  :hook ((typescript-mode . tree-sitter-hl-mode)
   (typescript-tsx-mode . tree-sitter-hl-mode)))

(use-package tree-sitter-langs
  :ensure t
  :after tree-sitter
  :config
  (tree-sitter-require 'tsx)
  (add-to-list 'tree-sitter-major-mode-language-alist '(typescript-tsx-mode . tsx)))

;; https://github.com/felipeochoa/rjsx-mode/issues/71
;; https://gist.github.com/rangeoshun/67cb17392c523579bc6cbd758b2315c1

(use-package emmet-mode
  :commands emmet-mode
  :hook
  (typescript-mode)
  :bind("C-;" . emmet-expand-line)
  :config
  (setq emmet-indent-after-insert nil)
  ;; (setq emmet-indentation 2)
  (setq emmet-self-closing-tag-style " /")
  (setq emmet-expand-jsx-className? t)
  (add-to-list 'emmet-jsx-major-modes 'typescript-mode))


;; (defun my-web-mode-hook ()
;;   (setq web-mode-enable-auto-pairing nil))

;; (add-hook 'web-mode-hook  'my-web-mode-hook)

;; (defun sp-web-mode-is-code-context (id action context)
;;   (and (eq action 'insert)
;;        (not (or (get-text-property (point) 'part-side)
;;                 (get-text-property (point) 'block-side)))))

;; (sp-local-pair 'web-mode "<" nil :when '(sp-web-mode-is-code-context))

;; (use-package prettier-js
;;   :config
;;   (add-hook 'typescript-mode-hook 'prettier-js-mode))

;; (use-package eslint-fix
;;   :config
;;   (add-hook 'typescript-mode-hook 'eslint-fix-auto-mode))

(use-package eslintd-fix
  :config (setq eslintd-fix-executable "/Users/eliot/.volta/bin/eslint_d")
  :hook ((typescript-mode . eslintd-fix-mode)
         (json-mode . eslintd-fix-mode)))

(use-package json-mode
  :mode ("\\.json\\'"))

;; (use-package rjsx-mode
;;   :mode ("/\\(components\\|containers\\|src\\|pages\\)/.*\\.[j|t]s[x]?\\'" . rjsx-mode)
;;   ;; (("/\\(containers\\)/[^/]*\\.js" . rjsx-mode)
;;   ;;  ("/\\(components\\)/[^/]*\\.js" . rjsx-mode)
;;    ("\\.ts[x]?\\'" . rjsx-mode)
;;   ;; :config
;;   ;; (add-to-list 'lsp-language-id-configuration '(rjsx-mode . "javascript"))
;;   )

(use-package add-node-modules-path
  :hook (;; (js2-mode . add-node-modules-path)
          ;; (rjsx-mode . add-node-modules-path)
          (typescript-mode . add-node-modules-path)))

;; graphql
(use-package graphql-mode)

;; import-js
;; (use-package import-js
  ;; :config
  ;; (global-set-key (kbd "s-i") 'import-js-import))

;; rust
(use-package rustic
  :ensure
  :mode ("\\.rs\\'" . rustic-mode)
  :bind (:map rustic-mode-map
              ("M-j" . lsp-ui-imenu)
              ("M-?" . lsp-find-references)
              ("C-c C-c l" . flycheck-list-errors)
              ("C-c C-c a" . lsp-execute-code-action)
              ("C-c C-c r" . lsp-rename)
              ("C-c C-c q" . lsp-workspace-restart)
              ("C-c C-c Q" . lsp-workspace-shutdown)
              ("C-c C-c s" . lsp-rust-analyzer-status))
  :config
  ;; uncomment for less flashiness
  ;; (setq lsp-eldoc-hook nil)
  ;; (setq lsp-enable-symbol-highlighting nil)
  ;; (setq lsp-signature-auto-activate nil)

  ;; comment to disable rustfmt on save
  (setq rustic-format-on-save t)
  (add-hook 'rustic-mode-hook 'rk/rustic-mode-hook))

(defun rk/rustic-mode-hook ()
  ;; so that run C-c C-c C-r works without having to confirm, but don't try to
  ;; save rust buffers that are not file visiting. Once
  ;; https://github.com/brotzeit/rustic/issues/253 has been resolved this should
  ;; no longer be necessary.
  (when buffer-file-name
    (setq-local buffer-save-without-query t)))

;; PHP
(use-package php-mode
  :mode ("\\.php\\'" . php-mode))

;; dotenv
(use-package dotenv-mode
  :mode ("\\.env\\..*\\'" . dotenv-mode))

;; Docker
(use-package dockerfile-mode
  :mode
  ("Dockerfile\\(-.*\\)?\\'" . dockerfile-mode))

;; ;; ripgrep
(use-package ripgrep)
;; (use-package rg)
;; (setq ripgrep--base-arguments '("--line-number" "--with-filename"))

(use-package perspective
  :bind (("C-M-k" . persp-switch)
         ("C-M-n" . persp-next)
         ("C-M-p" . persp-prev)
         ("C-x k" . persp-kill-buffer*))
  ;; :custom
  ;; (persp-initial-frame-name "Main")
  :config
  ;; Running `persp-mode' multiple times resets the perspective list...
  ;; (setq persp-state-default-file "~/.emacs-persp-default"')
  (customize-set-variable persp-sort 'created)
  (unless (equal persp-mode t)
    (persp-mode)))

;; Projectile
(use-package projectile
  :defer 2
  :diminish projectile-mode
  :config (projectile-mode)
  :custom ((projectile-completion-system 'ivy))
  :bind-keymap
  ("C-c C-p" . projectile-command-map)
  ;; ("C-M-p" . projectile-command-map)
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

(use-package treemacs-projectile
  :after (treemacs projectile))

(use-package treemacs-perspective
  :after (treemacs perspective) ;;or perspective vs. persp-mode
  :ensure t
  :config (treemacs-set-scope-type 'Perspectives))

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
;; (global-eldoc-mode -1)
