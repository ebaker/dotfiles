(require 'exwm-randr)

(defvar efs/polybar-process nil
  "Holds the process of the running Polybar instance, if any")

(defun efs/kill-panel ()
  (interactive)
  (when efs/polybar-process
    (ignore-errors
      (kill-process efs/polybar-process)))
  (setq efs/polybar-process nil))

(defun efs/start-panel ()
  (interactive)
  (efs/kill-panel)
  (setq efs/polybar-process (start-process-shell-command "polybar" nil "polybar panel")))

(setq exwm-input-global-keys
      `(
        ;; Bind "s-r" to exit char-mode and fullscreen mode.
        ([?\s-f] . exwm-reset)
        ;; Bind "s-w" to switch workspace interactively.
        ([?\s-w] . exwm-workspace-switch)
        ;; Bind "s-0" to "s-9" to switch to a workspace by its index.
        ,@(mapcar (lambda (i)
                    `(,(kbd (format "s-%d" i)) .
                      (lambda ()
                        (interactive)
                        (exwm-workspace-switch-create ,i))))
                  (number-sequence 0 9))
        ;; Bind "s-&" to launch applications ('M-&' also works if the output
        ;; buffer does not bother you).
        ([?\s-&] . (lambda (command)
         (interactive (list (read-shell-command "$ ")))
         (start-process-shell-command command nil command)))
        ;; Bind "s-<f2>" to "slock", a simple X display locker.
        ([s-f2] . (lambda ()
        (interactive)
        (start-process "" nil "/usr/bin/slock")))))

(use-package exwm
  :init
  (efs/start-panel)
  :config
  (require 'exwm-randr)
  (setq exwm-randr-workspace-output-plist '(1 "DVI-D-0"))
(add-hook 'exwm-randr-screen-change-hook
          (lambda ()
            (start-process-shell-command
             "xrandr" nil "xrandr --output DVI-D-0 -r 59.95 --mode 2560x1440")))
  (exwm-randr-enable)
  ;; (setq exwm-systemtray-height 16)
  ;; (setq exwm-input-global-keys `(,(kbd "s-&") .
  ;;                              (lambda (command)
  ;;                                (interactive (list (read-shell-command "$ ")))
  ;;                                (start-process-shell-command command nil command))))
  (exwm-init))


(provide 'eliot-exwm)
