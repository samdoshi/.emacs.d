(defun init/alternate-buffer ()
  "Switch back and forth between current and last buffer."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) t)))

(defun init/edit-emacs-README.org ()
  "Edit ~/.emacs.d/README.org"
  (interactive)
  (find-file "~/.emacs.d/README.org"))

(provide 'init-utils)
