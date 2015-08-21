(require 'hydra)
(require 'evil-numbers)

(defhydra init/hydra-numbers ()
  "numbers"
  ("a" evil-numbers/inc-at-pt "increase")
  ("x" evil-numbers/dec-at-pt "decrease"))

(provide 'init-hydra)
