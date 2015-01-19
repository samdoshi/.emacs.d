(require 'evil)
(require 'powerline)

(defun init/evil-state-name (state)
  "evil-state to string"
  (symbol-name state))

(defun init/powerline-evil-state-name ()
  "Format evil-state for use on LHS of powerline"
  (format " %-9s" (upcase (init/evil-state-name evil-state))))

(defun init/evil-state-face-name (state)
  "Symbol for a given evil state"
  (intern (format "init-evil-state-%s-face" (symbol-name state))))

(defun init/evil-state-face (state fg bg)
  "Define a face for the given STATE and background COLOR."
  (eval `(defface ,(init/evil-state-face-name state) '((t ()))
           ,(format "%s state face." (symbol-name state))
           :group 'init-theme))
  (set-face-attribute (init/evil-state-face-name state) nil
                      :foreground fg
                      :background bg
                      :weight 'bold
                      :box (face-attribute 'mode-line :box)
                      :inherit 'mode-line))

(defun init/powerline-evil-state-face ()
  (init/evil-state-face-name evil-state))

(defun init/mode-line-prepare-left ()
  (let* ((active (powerline-selected-window-active))
         (line-face (if active 'mode-line 'mode-line-inactive))
         (face1 (if active 'powerline-active1 'powerline-inactive1))
         (face2 (if active 'powerline-active2 'powerline-inactive2))
         (state-face (if active (init/powerline-evil-state-face) 'mode-line-inactive)))
    (list
     (powerline-raw (init/powerline-evil-state-name) state-face)
     (powerline-raw "%*" line-face)
     (powerline-raw " " line-face)
     (powerline-buffer-size line-face)
     (powerline-raw " " line-face)
     (powerline-buffer-id line-face)
     (powerline-raw " " line-face)
     (powerline-vc line-face)
     (powerline-raw " " line-face)
     (powerline-major-mode face1)
     (powerline-process face1)
     (powerline-raw " " face1)
     (powerline-minor-modes face1)
     (powerline-narrow face1)
     (powerline-raw " " face1))))

(defun init/mode-line-prepare-right ()
  (let* ((active (powerline-selected-window-active))
         (line-face (if active 'mode-line 'mode-line-inactive))
         (face1 (if active 'powerline-active1 'powerline-inactive1))
         (face2 (if active 'powerline-active2 'powerline-inactive2)))
    (list
     (powerline-raw "%4l" face1 'r)
     (powerline-raw ":" face1)
     (powerline-raw "%3c" face1 'r)
     (powerline-raw " " face1))))

(defun init/mode-line-prepare ()
  "This is the powerline fun that get's evaluated each time"
  (let* ((active (powerline-selected-window-active))
         (face2 (if active 'powerline-active2 'powerline-inactive2))
         (lhs (init/mode-line-prepare-left))
         (rhs (init/mode-line-prepare-right)))
    (concat (powerline-render lhs)
            (powerline-fill face2 (powerline-width rhs))
            (powerline-render rhs))))

(defun init/powerline-theme ()
  "Set up powerline"
  (setq-default powerline-default-separator 'wave)
  (setq-default powerline-height 17)
  (setq-default mode-line-format
                '("%e" (:eval (init/mode-line-prepare)))))

(defun init/common-theme ()
  "Common initialisation by all themes"
  (init/powerline-theme)
  (powerline-reset))

(defconst init/solarized-base03    "#002b36")
(defconst init/solarized-base02    "#073642")
(defconst init/solarized-base01    "#586e75")
(defconst init/solarized-base00    "#657b83")
(defconst init/solarized-base0     "#839496")
(defconst init/solarized-base1     "#93a1a1")
(defconst init/solarized-base2     "#eee8d5")
(defconst init/solarized-base3     "#fdf6e3")
(defconst init/solarized-yellow    "#b58900")
(defconst init/solarized-orange    "#cb4b16")
(defconst init/solarized-red       "#dc322f")
(defconst init/solarized-magenta   "#d33682")
(defconst init/solarized-violet    "#6c71c4")
(defconst init/solarized-blue      "#268bd2")
(defconst init/solarized-cyan      "#2aa198")
(defconst init/solarized-green     "#859900")

(defun init/solarized-dark-theme ()
  (interactive)
  (load-theme 'solarized-dark t)
  (set-face-attribute 'mode-line nil
                      :overline nil
                      :underline nil
                      :box nil)
  (set-face-attribute 'linum nil
                      :foreground init/solarized-base01
                      :height 0.8)
  (init/evil-state-face 'normal  init/solarized-base02 init/solarized-blue)
  (init/evil-state-face 'insert  init/solarized-base2  init/solarized-green)
  (init/evil-state-face 'visual  init/solarized-base2  init/solarized-orange)
  (init/evil-state-face 'replace init/solarized-base2  init/solarized-magenta)
  (init/evil-state-face 'emacs   init/solarized-base2  init/solarized-base03)
  (init/evil-state-face 'motion  init/solarized-base02 init/solarized-blue)
  (init/common-theme))

(defun init/solarized-light-theme ()
  (interactive)
  (load-theme 'solarized-light t)
  (init/common-theme))

(provide 'init-theme)

