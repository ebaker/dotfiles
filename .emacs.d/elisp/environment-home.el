;; @ebaker - gpg
(epa-file-enable)

;; @ebaker - org-crypt
(require 'org-crypt)
(org-crypt-use-before-save-magic)

;; GPG key to use for encryption
;; Either the Key ID or set to nil to use symmetric encryption. ;; (setq org-crypt-key nil)
(setq org-crypt-key "32B6647BAF80C3120C4767B8EC29AE3FF8C03F83")

;; org Global Tags
(setq org-tag-alist '(("crypt" . ?c)))
