;; https://redd.it/3kqt6e
(setq-default gc-cons-threshold 100000000)

(let ((file-name-handler-alist nil))
  (require 'cask)
  (cask-initialize)

  (require 'ob-tangle)
  (org-babel-load-file
   (expand-file-name "README.org" user-emacs-directory)))
