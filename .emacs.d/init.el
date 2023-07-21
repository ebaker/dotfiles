;; -*- lexical-binding: t; eval: (save-place-local-mode -1); eval: (outline-hide-sublevels 1); -*-



;;; Initialization
;; heavily inspired by https://blog.jft.rocks/emacs/emacs-from-scratch.html

;; bug fix gnutls
(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")

;; Make startup faster by reducing the frequency of garbage
;; collection.  The default is 800 kilobytes.  Measured in bytes.
(setq gc-cons-threshold (* 50 1000 1000))

;; lsp-mode performance
(setq read-process-output-max (* 1024 1024))
(setq gc-cons-threshold 100000000)


;;; Packaging

;; Package configs
(require 'package)
(setq package-enable-at-startup nil)
(setq package-archives '(("org"   . "http://orgmode.org/elpa/")
                          ("melpa" . "https://melpa.org/packages/")
                          ("jcs-elpa" . "https://jcs-emacs.github.io/jcs-elpa/packages/")
                          ("gnu"   . "https://elpa.gnu.org/packages/")
                          ("gnu-devel" . "https://elpa.gnu.org/devel/")))

(package-initialize)

;; Bootstrap `use-package`
(unless package-archive-contents
  (package-refresh-contents))

;; Initialize use-package on non-linux platforms
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

;; Install and load `quelpa-use-package'.
(setq quelpa-update-melpa-p nil)
(package-install 'quelpa-use-package)
(require 'quelpa-use-package)

;; ;; Bootstrap straight.el
;; (defvar bootstrap-version)
;; (let ((bootstrap-file
;;       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
;;       (bootstrap-version 5))
;;   (unless (file-exists-p bootstrap-file)
;;     (with-current-buffer
;;         (url-retrieve-synchronously
;;         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
;;         'silent 'inhibit-cookies)
;;       (goto-char (point-max))
;;       (eval-print-last-sexp)))
;;   (load bootstrap-file nil 'nomessage))

;; ;; Always use straight to install on systems other than Linux
;; (setq straight-use-package-by-default (not (eq system-type 'gnu/linux)))

;; ;; Use straight.el for use-package expressions
;; (straight-use-package 'use-package)

;; ;; Load the helper package for commands like `straight-x-clean-unused-repos'
;; (require 'straight-x)

;;; UI

;; Minimal UI
(scroll-bar-mode -1)
(tool-bar-mode   -1)
(tooltip-mode    -1)
(menu-bar-mode   -1)

;; Fancy titlebar for MacOS
(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearance . dark)) ;; light
(setq ns-use-proxy-icon  nil)
(setq frame-title-format nil)

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
(add-to-list 'default-frame-alist '(width . 110))

;;; System

;; mac
(setq mac-pass-command-to-system nil)

;; super & meta on mac
(setq mac-command-modifier 'super
  mac-option-modifier  'meta)

;;; Defaults

;; Garbage-collect on focus-out, Emacs /should/ feel snappier.
(add-hook 'focus-out-hook #'garbage-collect)

;; spaces for tabs
(setq-default indent-tabs-mode nil)

(delete-selection-mode t)

;; line numbers
(column-number-mode)
(global-display-line-numbers-mode t)
(setq-default display-line-numbers-width 3)

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

;;; Global Keybindings

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
(global-set-key (kbd "s-'") 'indent-region)
(global-set-key (kbd "s-o") 'other-window)
(global-set-key (kbd "s-O") 'ebaker/other-window-reverse)
(global-set-key (kbd "s-i") 'window-swap-states)
(global-set-key (kbd "s-0") 'delete-window)
(global-set-key (kbd "s-1") 'delete-other-windows)
(global-set-key (kbd "s-2") 'split-window-below)
(global-set-key (kbd "s-3") 'split-window-right)
;; (global-set-key (kbd "s-b") 'ido-switch-buffer)
;; (global-set-key (kbd "s-b") 'persp-ivy-switch-buffer)
(global-set-key (kbd "s-b") 'consult-buffer)
(global-set-key (kbd "s-k") 'ido-kill-buffer)
(global-set-key (kbd "s-a") 'org-agenda)
(global-set-key (kbd "s-r") 'revert-buffer)
(global-set-key (kbd "C-x i") 'find-config)
(global-set-key (kbd "C-x C-c") 'my-save-buffers-kill-emacs)
;; (global-set-key (kbd "C-x C-r") 'counsel-recentf)
(global-set-key (kbd "C-x C-r") 'consult-recent-file)
(global-set-key (kbd "C-M-u") 'universal-argument)
(global-set-key (kbd "C-M-n") 'persp-next)
(global-set-key (kbd "C-M-p") 'persp-prev)
(global-set-key (kbd "<f2>") 'vterm-toggle)
(global-set-key (kbd "C-<f2>") 'vterm-toggle-cd)

;;; Usability

;;;; environment variables
(use-package exec-path-from-shell
  :config
  (exec-path-from-shell-initialize))

;;;; recent files
(use-package recentf
  :ensure nil
  ;; :straight nil
  ;; Loads after 1 second of idle time.
  :hook (after-init . recentf-mode))

;;;; minibuffer history
(use-package savehist
  :config
  (setq history-length 25)
  (savehist-mode 1))

  ;; Individual history elements can be configured separately
  ;;(put 'minibuffer-history 'history-length 25)
  ;;(put 'evil-ex-history 'history-length 50)
  ;;(put 'kill-ring 'history-length 25))

;;;; undo-tree
(use-package undo-tree
  ;; :diminish undo-tree-mode:
  :init
  (global-undo-tree-mode 1)
  :config
  (setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo"))))

;;;; outli
(use-package outli
  :ensure nil
  :quelpa (cape-yasnippet :fetcher github :repo "elken/cape-yasnippet")
  ;; :after lispy ; only if you use lispy; it also sets speed keys on headers!
  :bind (:map outli-mode-map ; convenience key to get back to containing heading
        ("C-c C-p" . (lambda () (interactive) (outline-back-to-heading))))
  :custom ((outli-default-nobar t))
  :hook ((emacs-lisp-mode) . outli-mode)) ; or whichever modes you prefer

;;;; Ripgrep
(use-package ripgrep)
;; (use-package rg)
;; (setq ripgrep--base-arguments '("--line-number" "--with-filename"))

;;; Evil

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

(use-package evil-collection
  :config
  (evil-collection-init 'magit)
  (evil-collection-init 'dired)
  (evil-collection-init 'vterm))

(use-package evil-surround
  :config
  (global-evil-surround-mode 1))

;;; Org-mode

;;;; Location for celestial calculations
(setq calendar-location-name "San Francisco, CA")
(setq calendar-latitude 37.773972)
(setq calendar-longitude -122.431297)

;;;; Orgmode

(use-package org
  :demand t
  :config
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

;;;; Agenda

(use-package org-agenda
  :ensure nil
  ;; :straight nil
  :after (org)
  :bind
  (:map org-agenda-mode-map
    ;; evilify org-agenda-mode
    ("j" . org-agenda-next-line)
    ("k" . org-agenda-previous-line)
    ("<C-j>" . org-agenda-next-item)
    ("<C-k>" . org-agenda-previous-item)
    ("l" . forward-char)
    ("h" . backward-char)
    ("s" . org-agenda-schedule)
    ("S" . org-save-all-org-buffers)
    ("[" . org-agenda-earlier)
    ("]" . org-agenda-later)
    ("f" . org-agenda-earlier)
    ("b" . org-agenda-later)
    ("m" . org-agenda-bulk-toggle)
    ("~" . org-agenda-bulk-toggle-all)
    ("*" . org-agenda-bulk-mark-all)
    ("%" . org-agenda-bulk-mark-regexp)))

;;;; Chef

(use-package org-chef
  :ensure t)

;;;; Git Auto-commit

(use-package git-auto-commit-mode)

;;;; Bullets
(use-package org-bullets
  :defer 2
  :after (org)
  :hook (org-mode . org-bullets-mode))

;;;; Prettify
(setq-default prettify-symbols-alist '(("#+BEGIN_SRC" . "↦") ;; "†"
                                       ("#+END_SRC" . "⇤")
                                       ("#+begin_src" . "↦")
                                       ("#+end_src" .  "⇤")
                                       ;; (">=" . "≥")
                                        ;; ("=>" . "⇨")
                                        ))
(setq prettify-symbols-unprettify-at-point 'right-edge)
(add-hook 'org-mode-hook 'prettify-symbols-mode)

;;;; Active Babel languages
(org-babel-do-load-languages
 'org-babel-load-languages
 '((chatgpt-shell . t)
   (emacs-lisp . t)))

;;;; Cliplink
(use-package org-cliplink
  :after (org))

;;; Themes
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

;; https://emacs.stackexchange.com/questions/24088/make-a-function-to-toggle-themes/44626#44626
(defvar quick-switch-themes
  (let ((themes-list (list 'doom-solarized-light
                       'doom-one
                       'doom-tomorrow-day
                       'doom-Iosvkem
                       )))
    (nconc themes-list themes-list))
  "A circular list of themes to keep switching between.
Make sure that the currently enabled theme is at the head of this
list always.

A nil value implies no custom theme should be enabled.")

(defun quick-switch-themes* ()
  "Switch between to commonly used faces in Emacs.
One for writing code and the other for reading articles."
  (interactive)
  (if-let* ((next-theme (cadr quick-switch-themes)))
      (progn (when-let* ((current-theme (car quick-switch-themes)))
               (disable-theme (car quick-switch-themes)))
             (load-theme next-theme t)
             (message "Loaded theme: %s" next-theme))
    ;; Always have the dark mode-line theme
    (mapc #'disable-theme (delq 'smart-mode-line-dark custom-enabled-themes)))
  (setq quick-switch-themes (cdr quick-switch-themes)))

;;; Keybinding

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
    ;; "/"   '(counsel-rg :which-key "ripgrep")
    "/"   '(consult-ripgrep :which-key "ripgrep")
    "TAB" '(switch-to-prev-buffer :which-key "previous buffer")
    ;; "SPC" '(counsel-M-x :which-key "M-x")
    "SPC" '(execute-extended-command :which-key "M-x")

    ;; Buffers
    "b" '(:ignore t :which-key "buffer")
    "bi" '(ivy-switch-buffer)
    "bb" '(consult-buffer)

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
    "tt" '(quick-switch-themes* :which-key "switch theme")

    ;; Others
    "a" '(:ignore t :which-key "applications")
    "at"  '(ansi-term :which-key "open terminal")

    ;; org-mode
    "o"   '(:ignore t :which-key "org mode")

    "oa"  '(:ignore t :which-key "agenda")
    "oaa" '(org-agenda :which-key "status")
    "oat" '(org-todo-list :which-key "todos")

    "oi"  '(:ignore t :which-key "insert")
    "oil" '(org-insert-link :which-key "insert link")

    "os"  '(org-schedule :which-key "schedule")
    "ot"  '(org-todo :which-key "todo")

    "oc"  '(org-capture t :which-key "capture")
    "of"  '(dw/counsel-rg-org-files :which-key "find in notes")
    "on"  '(org-toggle-narrow-to-subtree :which-key "toggle narrow")
    "ox"  '(org-export-dispatch t :which-key "export")))

;;; Completion

;;;; ivy
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
  ;; (ivy-mode 1)
  )

(use-package counsel
  ;; :bind (("M-x" . counsel-M-x)
  ;;         ;; ("C-x b" . counsel-ibuffer)
  ;;         ;; ("C-x C-f" . counsel-find-file)
  ;;         :map minibuffer-local-map
  ;;         ("C-r" . 'counsel-minibuffer-history))
  )

(use-package ivy-rich
  ;; :init
  ;; (ivy-rich-mode 1)
  )

;;;; vertico
(defun dw/minibuffer-backward-kill (arg)
  "When minibuffer is completing a file name delete up to parent
folder, otherwise delete a word"
  (interactive "p")
  (if minibuffer-completing-file-name
      ;; Borrowed from https://github.com/raxod502/selectrum/issues/498#issuecomment-803283608
      (if (string-match-p "/." (minibuffer-contents))
          (zap-up-to-char (- arg) ?/)
        (delete-minibuffer-contents))
      (backward-kill-word arg)))

(use-package vertico
  :bind (:map vertico-map
         ;; ("C-j" . vertico-next)
         ;; ("C-k" . vertico-previous)
         ;; ("C-f" . vertico-exit)
         ("C-M-j" . vertico-exit-input)
         ("M-TAB" . minibuffer-complete)
         ("s-<return>" . minibuffer-force-complete-and-exit)
         :map minibuffer-local-map
         ("M-h" . dw/minibuffer-backward-kill))
  :custom
  (vertico-cycle t)
  :init
  (vertico-mode)
  )

;; Consult
(defun dw/get-project-root ()
  (when (fboundp 'projectile-project-root)
    (projectile-project-root)))

;; Configure directory extension.
(use-package vertico-directory
  :after vertico
  :ensure nil
  ;; More convenient directory navigation commands
  :bind (:map vertico-map
              ("RET" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word))
  ;; Tidy shadowed file names
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

(use-package consult
  :demand t
  ;; :bind (("C-s" . consult-line)
  ;;        ("C-M-l" . consult-imenu)
  ;;        ("C-M-j" . persp-switch-to-buffer*)
  ;;        :map minibuffer-local-map
  ;;        ("C-r" . consult-history))
  :custom
  (consult-project-root-function #'dw/get-project-root)
  (completion-in-region-function #'consult-completion-in-region))

;; https://github.com/minad/consult/wiki#shorten-recent-files-in-consult-buffer
(defun my-consult--source-recentf-items ()
  (let ((ht (consult--buffer-file-hash))
        file-name-handler-alist ;; No Tramp slowdown please.
        items)
    (dolist (file recentf-list (nreverse items))
      ;; Emacs 29 abbreviates file paths by default, see
      ;; `recentf-filename-handlers'.
      (unless (eq (aref file 0) ?/)
        (setq file (expand-file-name file)))
      (unless (gethash file ht)
        (push (propertize
               (file-name-nondirectory file)
               'multi-category `(file . ,file))
              items)))))

(plist-put consult--source-recent-file
           :items #'my-consult--source-recentf-items)

;; Marginalia
(use-package marginalia
  :after vertico
  :custom
  (marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil))
  :init
  (marginalia-mode))

;;;; Yasnippet
;; (require 'eliot-yasnippet)
(use-package yasnippet
  ;; :hook (prog-mode . yas-minor-mode)
  :ensure nil
  :quelpa (yasnippet :fetcher github :repo "joaotavora/yasnippet")
  :bind (:map yas-minor-mode-map
         ("TAB" . nil)
         ("<tab>" . nil))
  :config
  (yas-reload-all)
  (yas-global-mode))
(use-package yasnippet-snippets)
(require 'yasnippet-snippets)
(use-package js-react-redux-yasnippets)
(ebaker/leader-keys
  "y"  '(:ignore t :which-key "yasnippet")
  "ya" 'yas-insert-snippet)
;; (setq lsp-completion-provider :capf)
;; ;; Add yasnippet support for all company backends
;; ;; https://github.com/syl20bnr/spacemacs/pull/179
;; (defvar company-mode/enable-yas t
;;   "Enable yasnippet for all backends.")

;;;; Corfu

(use-package corfu
  ;; Optional customizations
  :custom
  ;; (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  ;; (corfu-auto t)                 ;; Enable auto completion
  (corfu-separator ?\s)          ;; Orderless field separator
  ;; (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
  ;; (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
  ;; (corfu-preview-current nil)    ;; Disable current candidate preview
  ;; (corfu-preselect 'prompt)      ;; Preselect the prompt
  ;; (corfu-on-exact-match nil)     ;; Configure handling of exact matches
  ;; (corfu-scroll-margin 5)        ;; Use scroll margin
  ;; (corfu-auto-delay 0.1)
  ;; (corfu-auto-prefix 1)

  ;; Enable Corfu only for certain modes.
  ;; :hook ((prog-mode . corfu-mode)
  ;;        (shell-mode . corfu-mode)
  ;;        (eshell-mode . corfu-mode))

  ;; Recommended: Enable Corfu globally.
  ;; This is recommended since Dabbrev can be used globally (M-/).
  ;; See also `corfu-exclude-modes'.
  :init
  ;; (setq corfu-min-width 40)
  ;; (setq corfu-max-width corfu-min-width)       ; Always have the same width
  (global-corfu-mode)
  :general (:keymaps 'corfu-map :states 'insert
  "C-n" #'corfu-next
  "C-p" #'corfu-previous
  "<escape>" #'corfu-quit
  "<tab>" #'corfu-insert
  "s-<return>" #'corfu-insert
  "M-d" #'corfu-popupinfo-documentation
  "M-l" #'corfu-popupinfo-location)
  :config
  ;; https://github.com/minad/corfu/issues/12
  (evil-make-overriding-map corfu-map)
  (advice-add 'corfu--setup :after 'evil-normalize-keymaps)
  (advice-add 'corfu--teardown :after 'evil-normalize-keymaps)
  )

(use-package corfu-popupinfo
  :after corfu
  :ensure nil
  ;; :straight nil
  :init
  (corfu-popupinfo-mode)
  :general (:keymaps 'corfu-map
            ;; Scroll in the documentation window
            "M-n" #'corfu-popupinfo-scroll-up
            "M-p" #'corfu-popupinfo-scroll-down)
  :config
  (setq corfu-popupinfo-delay 0.1))

;; Use Dabbrev with Corfu!
(use-package dabbrev
  ;; Swap M-/ and C-M-/
  :bind (("M-/" . dabbrev-completion)
         ("C-M-/" . dabbrev-expand))
  ;; Other useful Dabbrev configurations.
  :custom
  (dabbrev-ignored-buffer-regexps '("\\.\\(?:pdf\\|jpe?g\\|png\\)\\'")))

(use-package corfu-candidate-overlay
  ;; :straight (:type git
  ;;            :repo "https://code.bsdgeek.org/adam/corfu-candidate-overlay"
  ;;            :files (:defaults "*.el"))
  :after corfu
  :config
  ;; enable corfu-candidate-overlay mode globally
  ;; this relies on having corfu-auto set to nil
  (corfu-candidate-overlay-mode +1)
  ;; bind Ctrl + TAB to trigger the completion popup of corfu
  (global-set-key (kbd "C-<tab>") 'completion-at-point)
  ;; bind Ctrl + Shift + Tab to trigger completion of the first candidate
  ;; (keybing <iso-lefttab> may not work for your keyboard model)
  (global-set-key (kbd "C-S-<tab>") 'corfu-candidate-overlay-complete-at-point))

;;;; Orderless

(use-package orderless
  :demand t
  :config
  (setq completion-styles '(orderless flex)
        completion-category-overrides '((eglot (styles . (orderless flex))))))

;;;; kind-icon
(use-package kind-icon
  :after corfu
  :custom
  (kind-icon-default-face 'corfu-default)
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

;; ;; Use Company backends as Capfs.
;; (require 'company-yasnippet)
;; (add-to-list completion-at-point-functions
;;   (mapcar #'cape-company-to-capf
;;     (list #'company-files #'company-ispell #'company-dabbrev)))

;;;; Cape

;; (require 'cape-yasnippet)
(use-package cape-yasnippet
  :ensure nil
  ;; :straight (:host github :repo "elken/cape-yasnippet")
  :quelpa (cape-yasnippet :fetcher github :repo "elken/cape-yasnippet")
  :after yasnippet
  ;; :hook ((prog-mode . yas-setup-capf)
  ;;        (text-mode . yas-setup-capf)
  ;;        ;; (lsp-mode  . yas-setup-capf)
  ;;         ;; (sly-mode  . yas-setup-capf)
  ;;         )
  :bind (("C-c y" . cape-yasnippet)
         ("M-+"   . yas-insert-snippet))
  :config
  ;; (defun yas-setup-capf ()
  ;;   (setq-local completion-at-point-functions
  ;;               (cons 'cape-yasnippet
  ;;                     completion-at-point-functions)))
  ;; (push 'cape-yasnippet completion-at-point-functions)
  (setq cape-yasnippet-lookup-by 'key))

;; Add extensions
(use-package cape
  :after cape-yasnippet
  ;; Bind dedicated completion commands
  ;; Alternative prefix keys: C-c p, M-p, M-+, ...
  :bind (("C-c p p" . completion-at-point) ;; capf
         ("C-c p t" . complete-tag)        ;; etags
         ("C-c p d" . cape-dabbrev)        ;; or dabbrev-completion
         ("C-c p h" . cape-history)
         ("C-c p f" . cape-file)
         ("C-c p k" . cape-keyword)
         ("C-c p s" . cape-symbol)
         ("C-c p a" . cape-abbrev)
         ("C-c p l" . cape-line)
         ("C-c p w" . cape-dict)
         ("C-c p \\" . cape-tex)
         ("C-c p _" . cape-tex)
         ("C-c p ^" . cape-tex)
         ("C-c p &" . cape-sgml)
         ("C-c p r" . cape-rfc1345))
  :init
  ;; Add `completion-at-point-functions', used by `completion-at-point'.
  ;; NOTE: The order matters!
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-elisp-block)
  ;;(add-to-list 'completion-at-point-functions #'cape-history)
  ;;(add-to-list 'completion-at-point-functions #'cape-keyword)
  ;;(add-to-list 'completion-at-point-functions #'cape-tex)
  ;;(add-to-list 'completion-at-point-functions #'cape-sgml)
  ;;(add-to-list 'completion-at-point-functions #'cape-rfc1345)
  ;;(add-to-list 'completion-at-point-functions #'cape-abbrev)
  ;;(add-to-list 'completion-at-point-functions #'cape-dict)
  ;;(add-to-list 'completion-at-point-functions #'cape-symbol)
  ;;(add-to-list 'completion-at-point-functions #'cape-line)
  (add-to-list 'completion-at-point-functions #'cape-yasnippet)

  ;; ;; Option 2: Undo the Eglot modification of completion-category-defaults
  ;; (with-eval-after-load 'eglot
  ;;  (setq completion-category-defaults nil))

;; Enable cache busting, depending on if your server returns
;; sufficiently many candidates in the first place.
  (advice-add 'eglot-completion-at-point :around #'cape-wrap-buster)
  )


;;;; Org-block-capf


(use-package org-block-capf
  :ensure nil
  :quelpa (org-block-capf :fetcher github :repo "xenodium/org-block-capf")
  :after yasnippet
  ;; :custom
  ;; (company-org-block-edit-style 'auto) ;; 'auto, 'prompt, or 'inline
  ;; :hook ((org-mode . org-block-capf-add-to-completion-at-point-functions))
  :config
  (add-hook 'org-mode-hook #'org-block-capf-add-to-completion-at-point-functions)
  )

;;;; TAB cycle

;; A few more useful configurations...
(use-package emacs
  :init
  ;; TAB cycle if there are only few candidates
  (setq completion-cycle-threshold 1)

  ;; Emacs 28: Hide commands in M-x which do not apply to the current mode.
  ;; Corfu commands are hidden, since they are not supposed to be used via M-x.
  ;; (setq read-extended-command-predicate
  ;;       #'command-completion-default-include-p)

  ;; Enable indentation+completion using the TAB key.
  ;; `completion-at-point' is often bound to M-TAB.
  (setq tab-always-indent 'complete))

;;; Help

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


;;; Icons
(use-package all-the-icons)

(use-package nerd-icons
  ;; :custom
  ;; The Nerd Font you want to use in GUI
  ;; "Symbols Nerd Font Mono" is the default and is recommended
  ;; but you can use any other Nerd Font if you want
  ;; (nerd-icons-font-family "Symbols Nerd Font Mono")
  )

;;; Diminish
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

;;; Editing

;; vertical-center-mode
(require 'vertical-center-mode)
(ebaker/leader-keys
  "tc" 'vertical-center-mode)

;; rainbow delimiters
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;;; Hydra

(use-package hydra)

(defhydra hydra-text-scale (:timeout 4)
  "scale text"
  ("j" text-scale-increase "in")
  ("k" text-scale-decrease "out")
  ("f" nil "finished" :exit t))

(ebaker/leader-keys
  "ts" '(hydra-text-scale/body :which-key "scale text"))

;;; Git

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

;;; Flyspell

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
  ;; :straight nil
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


;;; LSP

;; lsp general
(ebaker/leader-keys
  "l"  '(:ignore t :which-key "lsp")
  "ld" 'xref-find-definitions
  "lr" 'xref-find-references
  "lb" 'xref-pop-marker-stack
  "ln" 'lsp-ui-find-next-reference
  "lp" 'lsp-ui-find-prev-reference
  ;; "ls" 'counsel-imenu
  "ls" 'consult-imenu
  "le" 'lsp-ui-flycheck-list
  "lS" 'lsp-ui-sideline-mode
  "lX" 'lsp-execute-code-action)

(defun my/eglot-capf ()
(setq-local completion-at-point-functions
            (list (cape-super-capf
                   #'eglot-completion-at-point
                   #'cape-yasnippet
                    ;; (cape-company-to-capf #'company-yasnippet)
                   #'cape-file))))

(defun my-corfu-combined-sort (candidates)
  "Sort CANDIDATES using both display-sort-function and corfu-sort-function."
  (let ((candidates
         (let ((display-sort-func (corfu--metadata-get 'display-sort-function)))
           (if display-sort-func
               (funcall display-sort-func candidates)
             candidates))))
    (if corfu-sort-function
        (funcall corfu-sort-function candidates)
      candidates)))

;; ;; https://gist.github.com/gsj987/64d48bf49a374c96421ad20df886e947
(use-package eglot
  :ensure t
  :defer 3
  :hook
  ((js-mode
    typescript-mode
    typescript-tsx-mode) . eglot-ensure)
  :config
  (cl-pushnew '((js-mode typescript-mode typescript-tsx-mode) . ("typescript-language-server" "--stdio")) eglot-server-programs :test #'equal)
  (add-hook 'eglot-managed-mode-hook #'my/eglot-capf)
  (setq corfu-sort-override-function #'my-corfu-combined-sort))

;;; Modeline

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
  :hook (after-init . doom-modeline-mode))


;;; Languages

;;;; editorconfig
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

;;;; smartparens
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

;;;; smart-comment
(use-package smart-comment
  :bind ("s-/" . smart-comment))
;; related: https://github.com/redguardtoo/evil-nerd-commenter

;;;; elisp
(setq lisp-indent-offset 2)

(use-package highlight-defined
  :defer 2
  :config
  (add-hook 'emacs-lisp-mode-hook 'highlight-defined-mode)
  (add-hook 'emacs-lisp-mode-hook 'display-line-numbers-mode))

(add-hook 'emacs-lisp-mode-hook #'eldoc-mode)

;;;; YAML
(use-package yaml-mode
  :mode "\\.ya?ml\\'")

;;;; flymake

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

;;;; JavaScript
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

;; (use-package tern)

;; (use-package company-tern
;;   :ensure t
;;   :config
;; (add-to-list 'company-backends 'company-tern))

;; import-js
;; (use-package import-js
  ;; :config
  ;; (global-set-key (kbd "s-i") 'import-js-import))

;; needed by yasnippet
(use-package js2-mode)

;;;; TypeScript

(use-package typescript-mode
  :ensure t
  :init
  (define-derived-mode typescript-tsx-mode typescript-mode "tsx")
  :config
  (setq typescript-indent-level 2)
  (add-hook 'typescript-mode #'subword-mode)
  (add-to-list 'auto-mode-alist '("\\.[t]sx\\'" . typescript-tsx-mode))
  (add-to-list 'auto-mode-alist '("\\.[t]s\\'" . typescript-mode))
  )

;;;; Treesitter

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

;;;; Emmet

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

;;;; JS/TS Formatting

(use-package eslintd-fix
  :config (setq eslintd-fix-executable "/Users/eliot/.volta/bin/eslint_d")
  :hook ((typescript-mode . eslintd-fix-mode)
          (json-mode . eslintd-fix-mode)))

;; (use-package prettier-js
;;   :config
;;   (add-hook 'typescript-mode-hook 'prettier-js-mode))

;; (use-package eslint-fix
;;   :config
;;   (add-hook 'typescript-mode-hook 'eslint-fix-auto-mode))

;;;; JSON

(use-package json-mode
  :mode ("\\.json\\'"))

;;;; RJSX

;; (use-package rjsx-mode
;;   :mode ("/\\(components\\|containers\\|src\\|pages\\)/.*\\.[j|t]s[x]?\\'" . rjsx-mode)
;;   ;; (("/\\(containers\\)/[^/]*\\.js" . rjsx-mode)
;;   ;;  ("/\\(components\\)/[^/]*\\.js" . rjsx-mode)
;;    ("\\.ts[x]?\\'" . rjsx-mode)
;;   ;; :config
;;   ;; (add-to-list 'lsp-language-id-configuration '(rjsx-mode . "javascript"))
;;   )

;;;; Nodejs

(use-package add-node-modules-path
  :hook (;; (js2-mode . add-node-modules-path)
          ;; (rjsx-mode . add-node-modules-path)
          (typescript-mode . add-node-modules-path)))

;;;; GraphQL

(use-package graphql-mode)



;;;; Rust
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

;;;; PHP
(use-package php-mode
  :mode ("\\.php\\'" . php-mode))

;;;; dotenv
(use-package dotenv-mode
  :mode ("\\.env\\..*\\'" . dotenv-mode))

;;;; Docker
(use-package dockerfile-mode
  :mode
  ("Dockerfile\\(-.*\\)?\\'" . dockerfile-mode))

;;;; Markdown
(use-package markdown-mode
  :ensure t
  :mode ("README\\.md\\'" . gfm-mode)
  :init (setq markdown-command "multimarkdown")
  :bind (:map markdown-mode-map
          (("C-c C-e" . markdown-do)
           ("M-<up>" . markdown-move-up)
           ("M-<down>" . markdown-move-down))))

;;; Perspective
(use-package perspective
  :bind (("C-M-k" . persp-switch)
         ("C-M-n" . persp-next)
         ("C-M-p" . persp-prev)
         ("C-x k" . persp-kill-buffer*))
  :custom
  (persp-mode-prefix-key (kbd "C-c M-p"))
  ;; (persp-initial-frame-name "Main")
  :config
  ;; Running `persp-mode' multiple times resets the perspective list...
  ;; (setq persp-state-default-file "~/.emacs-persp-default"')
  (customize-set-variable persp-sort 'created)
  ;; ;; consult
  ;; (consult-customize consult--source-buffer :hidden t :default nil)
  ;; (add-to-list 'consult-buffer-sources persp-consult-source)
  (unless (equal persp-mode t)
    (persp-mode)))

;;; Projectile
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

;;; Treemacs
(use-package treemacs
  :config
  (setq treemacs-show-cursor t)
  (global-set-key [f8] 'treemacs))

(use-package treemacs-all-the-icons
  :config
  (treemacs-load-theme "all-the-icons"))

(use-package treemacs-evil)


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

;;; Dired

(use-package dired
  :ensure nil
  ;; :straight nil
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

;; Elementary textual completion backend.
;; (setq company-backends
;; (add-to-list 'company-backends 'company-dabbrev))

;;; Terminal

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

;; ;; xterm-colors
;; (use-package xterm-color
;;   :config
;;   (setq compilation-environment '("TERM=xterm-256color"))
;; (defun my/advice-compilation-filter (f proc string)
;;   (funcall f proc (xterm-color-filter string)))
;; (advice-add 'compilation-filter :around #'my/advice-compilation-filter))

;;; AI
(defvar my-openai-key (shell-command-to-string "$SHELL -c 'echo -n $CHATGPT_OPENAI_KEY'"))

;;;; Org-AI

(use-package org-ai
  :ensure
  :commands (org-ai-mode)
  :custom
  (org-ai-openai-api-token my-openai-key)
  :init
  (add-hook 'org-mode-hook #'org-ai-mode)
  :config
  ;; if you are on the gpt-4 beta:
  ;; (setq org-ai-default-chat-model "gpt-4")
  ;; if you are using yasnippet and want `ai` snippets
  (org-ai-install-yasnippets))
(setq org-startup-with-inline-images t)

;;;; Whisper
(use-package whisper
  :load-path "~/.emacs.d/elisp/whisper.el"
  :bind ("C-s-r" . whisper-run)
  :config
  (setq whisper-install-directory "~/.emacs.d/bin/"
        whisper-model "base"
        whisper-language "en"
    whisper--ffmpeg-input-device ":0"
    whisper-translate nil))

(use-package greader :ensure)
(require 'whisper)
(require 'org-ai-talk)

;; macOS speech settings, optional
(setq org-ai-talk-say-words-per-minute 210)
(setq org-ai-talk-say-voice "Karen")

;;;; CodeGPT

(use-package codegpt :ensure t)
(setq openai-key my-openai-key)

;;;; ChatGPT-Shell

(use-package chatgpt-shell
  :ensure t
  :custom
  ((chatgpt-shell-openai-key
    (lambda ()
      (auth-source-pass-get 'secret openai-key)))))
(setq chatgpt-shell-openai-key my-openai-key)

;; (require 'ob-chatgpt-shell)

;;; Dashboard
;; (require 'eliot-dashboard)

;;; Optimization

;; Make gc pauses faster by decreasing the threshold.
(setq gc-cons-threshold (* 2 1000 1000))
;; (global-eldoc-mode -1)
