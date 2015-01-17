(defun init/evil-leader-group (prefix name)
  "Creates an evil leader group"
  (let ((command (intern (concat "group:" name))))
    (define-prefix-command command)
    (evil-leader/set-key prefix command)))

(provide 'init-keybindings)
