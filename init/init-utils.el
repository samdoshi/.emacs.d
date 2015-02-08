(require 's)

(defun init/alternate-buffer ()
  "Switch back and forth between current and last (non-visible) buffer"
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) nil)))

(defun init/change-frame-font-height (delta)
  (let ((current-height (face-attribute 'default :height)))
    (set-face-attribute 'default
                        (selected-frame)
                        :height (+ current-height delta))))

(defun init/edit-emacs-README.org ()
  "Edit ~/.emacs.d/README.org"
  (interactive)
  (find-file "~/.emacs.d/README.org"))

(defun init/hashed-host-slug ()
  "Return a hashed version of the top-level domain name"
  (md5 (concat "UGXp4Adb.p8m;baTN8ybKxebV"
               (car (split-string (downcase (system-name))
                                  "\\.")))))

(defun init/initial-scratch-message ()
  (let* ((scratch-buffer (get-buffer "*scratch*"))
         (fortune (shell-command-to-string "cowsay $(fortune -s -n 300)"))
         (fortune (s-lines fortune))
         (fortune (mapconcat #'(lambda (x) (s-trim-right (concat ";;  " x))) fortune "\n"))
         (fortune (concat fortune "\n\n")))
    fortune))

(defun init/kill-buffer-named (name)
  (let ((buffer (get-buffer name)))
    (if buffer
        (kill-buffer buffer))))

(defun init/kill-compilation-buffer ()
  (interactive)
  (init/kill-buffer-named "*compilation*"))

(defun init/small-font-buffer ()
  (text-scale-decrease 2))

(defun init/split-window-and-focus ()
  (interactive)
  (split-window-vertically)
  (other-window 1))

(defun init/vsplit-window-and-focus ()
  (interactive)
  (split-window-horizontally)
  (other-window 1))

(provide 'init-utils)

