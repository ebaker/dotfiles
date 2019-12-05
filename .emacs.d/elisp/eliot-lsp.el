;;
;; LSP
;;
;; lsp-mode
(use-package lsp-mode
  :ensure t
  :init
  (setq lsp-prefer-flymake t)
  ;; (setq lsp-session-file (concat doom-etc-dir "lsp-session"))

  ;; Don't prompt the user for the project root every time we open a new
  ;; lsp-worthy file, instead, try to guess it with projectile.
  (setq lsp-auto-guess-root t)

  ;; Auto-kill LSP server once you've killed the last buffer associated with its
  ;; project.
  (setq lsp-keep-workspace-alive nil)
  :config
  (add-hook 'prog-major-mode #'lsp-prog-major-mode-enable)
  (custom-set-variables '(lsp-eldoc-hook nil))
  )

;; lsp-ui
(use-package lsp-ui
  :ensure t
  :init
  (setq	lsp-ui-flycheck-enable nil
	;; lsp-ui-sideline-enable nil
	;; lsp-ui-doc-enable nil
	;; lsp-ui-imenu-enable nil
	;; ;; lsp-ui-sideline-ignore-duplicate t
	;; ;;	lsp-ui-doc-max-height 8
	;; ;;	lsp-ui-doc-max-width 35
		)
  :config
  (add-hook 'lsp-mode-hook 'lsp-ui-mode)
  ;; (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
  ;; (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references)
  )

;; Company LSP

(use-package company-lsp
  :ensure t
  ;; :defer 2
  :init
  (push 'company-lsp company-backends)
  :config
  (setq company-lsp-cache-candidates 'auto))

(provide 'eliot-lsp)
