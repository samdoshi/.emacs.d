(require 'projectile)
(require 'helm-files)

(defun init/projectile-eshell ()
  (interactive)
  (if (projectile-project-p)
      (let* ((dir (projectile-project-root))
             (projectile-require-project-root nil)
             (helm-ff-default-directory (file-name-directory (projectile-expand-root dir))))
        (helm-ff-switch-to-eshell dir))
    (message "Not in project")))

(provide 'init-projectile)
