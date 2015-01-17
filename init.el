(require 'cask)
(cask-initialize)

(require 'ob-tangle)
(org-babel-load-file
  (expand-file-name "README.org" user-emacs-directory))
