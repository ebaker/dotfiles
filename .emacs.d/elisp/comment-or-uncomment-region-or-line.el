;; @ebaker - comment-or-uncomment-region or line
;; https://github.com/al3x/emacs/blob/master/utilities/comment-or-uncomment-region-or-line.el
(defun comment-or-uncomment-region-or-line ()
  "Comments or uncomments the region or the current line if there's no active region."
  (interactive)
  (let (beg end)
    (if (region-active-p)
        (setq beg (region-beginning) end (region-end))
      (setq beg (line-beginning-position) end (line-end-position)))
    (comment-or-uncomment-region beg end)))

(provide 'comment-or-uncomment-region-or-line)
