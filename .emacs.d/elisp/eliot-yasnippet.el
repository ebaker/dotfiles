;;
;; Snippets
;;

;; yasnippet - Yet another snippet extension program
(use-package yasnippet
  :ensure t
  :diminish yas-minor-mode
  :config
    (yas-global-mode 1)
    ;; respect the spacing in my snippet declarations
    (setq yas-indent-line 'fixed)
)

;; Nice “interface” to said program
(use-package yankpad
  ;; :if company-mode ;; load & initialise only if company-mode is defined
  :demand t
  :ensure t
  :init
    ;; Location of templates
    (setq yankpad-file "~/.emacs.d/yankpad.org")
    (setq yankpad-category "Category: Default")
  :config
    ;; If you want to complete snippets using company-mode
    ;; (add-to-list 'company-backends #'company-yankpad)
    ;; If you want to expand snippets with hippie-expand
    (add-to-list 'hippie-expand-try-functions-list #'yankpad-expand)
    ;; Load the snippet templates -- useful after yankpad is altered
    ;; (add-hook 'after-init-hook 'yankpad-reload)
)

;;
;; Add yasnippet support for all company backends
;; https://emacs.stackexchange.com/a/10520/10352
;;
(defvar company-mode/enable-yas t
  "There can only be one main completition backend, so let's
   enable yasnippet/yankpad as a secondary for all completion backends.")

(defun company-mode/backend-with-yas (backend)
  (if (or (not company-mode/enable-yas)
	  (and (listp backend) (member 'company-yankpad backend)))
      backend
    (append (if (consp backend) backend (list backend))
	    '(:with company-yankpad))))

(setq company-backends (mapcar #'company-mode/backend-with-yas company-backends))


(provide 'eliot-yasnippet)
