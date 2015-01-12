(require 'cask)
(cask-initialize)

(require 'ob-tangle)
(org-babel-load-file
  (expand-file-name "emacs-init.org" user-emacs-directory))
