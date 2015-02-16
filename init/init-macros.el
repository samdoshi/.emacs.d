(require 'evil)

(defmacro init|evilify (map &rest body)
  "Add `hjkl' navigation, search and visual state to MAP and set additional
bindings contained in BODY."
  `(evil-add-hjkl-bindings ,map 'emacs
    "/" 'evil-search-forward
    "n" ',(lookup-key evil-motion-state-map "n")
    "N" ',(lookup-key evil-motion-state-map "N")
    "v" 'evil-visual-char
    "V" 'evil-visual-line
    (kbd "C-v") 'evil-visual-block
    "y" 'evil-yank
    ,@body))

(provide 'init-macros)
