(defun init/alternate-buffer ()
  "Switch back and forth between current and last buffer."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) t)))

(defun init/edit-emacs-README.org ()
  "Edit ~/.emacs.d/README.org"
  (interactive)
  (find-file "~/.emacs.d/README.org"))

(defun init/hashed-host-slug ()
  "Return a hashed version of the top-level domain name"
  (md5 (concat "UGXp4Adb.p8m;baTN8ybKxebV"
               (car (split-string (downcase (system-name))
                                  "\\.")))))

(provide 'init-utils)

