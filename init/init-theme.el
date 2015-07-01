(require 'evil)
(require 'powerline)

(defun init/evil-state-name (state)
  "evil-state to string"
  (if (eq 'evilified state)
      "evilify"
    (symbol-name state)))

(defun init/current-evil-state-name ()
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

(defun init/current-evil-state-face ()
  "Get the face corresponding to the current evil state for use by powerline"
  (init/evil-state-face-name evil-state))

(defun init/mode-line-prepare-left ()
  (let* ((active (powerline-selected-window-active))
         (line-face (if active 'mode-line 'mode-line-inactive))
         (face1 (if active 'powerline-active1 'powerline-inactive1))
         (face2 (if active 'powerline-active2 'powerline-inactive2))
         (state-face (if active (init/current-evil-state-face) 'powerline-inactive1))
         (projectile-p (bound-and-true-p projectile-mode))
         (separator-left (intern (format "powerline-%s-%s"
                                         powerline-default-separator
                                         (car powerline-default-separator-dir))))
         (separator-right (intern (format "powerline-%s-%s"
                                          powerline-default-separator
                                          (cdr powerline-default-separator-dir)))))
    (append
     (list
      (powerline-raw (init/current-evil-state-name) state-face)
      (funcall separator-left state-face face1)
      (powerline-raw " " face1))
     (when (and active projectile-p)
       (list (powerline-raw (projectile-project-name) face1)
             (powerline-raw  " " face1)))
     (list
      (powerline-buffer-id face1)
      (powerline-raw "%*" face1)
      (powerline-raw " " face1)
      (powerline-major-mode face1)
      (powerline-raw " " face1)
      (funcall separator-left face1 face2)
      (powerline-vc face2)
      (powerline-raw " " face2)
      (powerline-process face2)
      (powerline-narrow face2)
      (funcall separator-left face2 line-face)
      (powerline-raw " " line-face)))))

(defun init/mode-line-prepare-right ()
  (let* ((active (powerline-selected-window-active))
         (line-face (if active 'mode-line 'mode-line-inactive))
         (face1 (if active 'powerline-active1 'powerline-inactive1))
         (face2 (if active 'powerline-active2 'powerline-inactive2))
         (state-face (if active (init/current-evil-state-face) 'powerline-inactive1))
         (separator-left (intern (format "powerline-%s-%s"
                                         powerline-default-separator
                                         (car powerline-default-separator-dir))))
         (separator-right (intern (format "powerline-%s-%s"
                                          powerline-default-separator
                                          (cdr powerline-default-separator-dir)))))
    (if active
        (list
         (funcall separator-right line-face face2)
         (powerline-raw " " face2)
         (powerline-minor-modes face2)
         (powerline-raw " " face2)
         (funcall separator-right face2 state-face)
         (powerline-raw " " state-face)
         (powerline-buffer-size state-face)
         (powerline-raw "%4l" state-face 'r)
         (powerline-raw ":" state-face)
         (powerline-raw "%3c" state-face 'r)
         (powerline-raw " " state-face))
      (list
         (funcall separator-right line-face state-face)
         (powerline-raw " " state-face)
         (powerline-buffer-size state-face)
         (powerline-raw "%4l" state-face 'r)
         (powerline-raw ":" state-face)
         (powerline-raw "%3c" state-face 'r)
         (powerline-raw " " state-face)))))

(defun init/mode-line-prepare ()
  "This is the powerline fun that get's evaluated each time"
  (let* ((active (powerline-selected-window-active))
         (line-face (if active 'mode-line 'mode-line-inactive))
         (lhs (init/mode-line-prepare-left))
         (rhs (init/mode-line-prepare-right)))
    (concat (powerline-render lhs)
            (powerline-fill line-face (powerline-width rhs))
            (powerline-render rhs))))

(defun init/powerline-theme ()
  "Set up powerline"
  (setq-default powerline-default-separator nil)
  (setq-default mode-line-format
                '("%e" (:eval (init/mode-line-prepare)))))

(defun init/common-theme ()
  "Common initialisation by all themes"
  (init/powerline-theme)
  (powerline-reset))

(defun init/set-font (font-name)
  (set-face-attribute 'default nil :family font-name)
  (set-face-attribute 'default nil :weight 'regular)
  (powerline-reset))

(defun init/set-font-menlo ()
  (interactive)
  (init/set-font "Menlo"))

(defun init/set-font-consolas ()
  (interactive)
  (init/set-font "Consolas"))

(defun init/set-font-source-code-pro ()
  (interactive)
  (init/set-font "Source Code Pro"))

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

(defun init/solarized-common ()
  (set-face-attribute 'mode-line nil
                      :overline nil
                      :underline nil
                      :box nil)
  (set-face-attribute 'mode-line-inactive nil
                      :overline nil
                      :underline nil
                      :box nil)
  (set-face-attribute 'mode-line-buffer-id nil
                      :foreground init/solarized-orange)
  (set-face-attribute 'helm-source-header nil
                      :height 1.15))

(defun init/solarized-dark-theme ()
  (interactive)
  (load-theme 'solarized-dark t)
  (init/solarized-common)
  (set-face-attribute 'powerline-active1 nil
                      :foreground init/solarized-base1
                      :background init/solarized-base02)
  (set-face-attribute 'powerline-inactive1 nil
                      :foreground init/solarized-base1
                      :background init/solarized-base02)
  (set-face-attribute 'powerline-active2 nil
                      :foreground init/solarized-base03
                      :background init/solarized-base0)
  (set-face-attribute 'powerline-inactive2 nil
                      :foreground init/solarized-base0
                      :background init/solarized-base03)
  (set-face-attribute 'linum nil
                      :foreground init/solarized-base01
                      :underline nil
                      :slant 'normal
                      :height 100)
  (set-face-attribute 'linum-highlight-face nil
                      :foreground init/solarized-base02
                      :background init/solarized-base01
                      :underline nil
                      :slant 'normal
                      :height 100)
  (init/evil-state-face 'normal    init/solarized-base02 init/solarized-blue)
  (init/evil-state-face 'insert    init/solarized-base2  init/solarized-green)
  (init/evil-state-face 'visual    init/solarized-base2  init/solarized-orange)
  (init/evil-state-face 'replace   init/solarized-base2  init/solarized-magenta)
  (init/evil-state-face 'emacs     init/solarized-base2  init/solarized-base03)
  (init/evil-state-face 'evilified init/solarized-base02 init/solarized-blue)
  (init/evil-state-face 'motion    init/solarized-base02 init/solarized-blue)
  (init/common-theme))

(defun init/solarized-light-theme ()
  (interactive)
  (load-theme 'solarized-light t)
  (init/solarized-common)
  (set-face-attribute 'powerline-active1 nil
                      :foreground init/solarized-base01
                      :background init/solarized-base2)
  (set-face-attribute 'powerline-inactive1 nil
                      :foreground init/solarized-base1
                      :background init/solarized-base2)
  (set-face-attribute 'powerline-active2 nil
                      :foreground init/solarized-base01
                      :background init/solarized-base3)
  (set-face-attribute 'powerline-inactive2 nil
                      :foreground init/solarized-base1
                      :background init/solarized-base2)
  (set-face-attribute 'linum nil
                      :foreground init/solarized-base1
                      :underline nil
                      :slant 'normal
                      :height 100)
  (set-face-attribute 'linum-highlight-face nil
                      :foreground init/solarized-base2
                      :background init/solarized-base1
                      :underline nil
                      :slant 'normal
                      :height 100)
  (init/evil-state-face 'normal    init/solarized-base2 init/solarized-blue)
  (init/evil-state-face 'insert    init/solarized-base3 init/solarized-green)
  (init/evil-state-face 'visual    init/solarized-base3 init/solarized-orange)
  (init/evil-state-face 'replace   init/solarized-base3 init/solarized-magenta)
  (init/evil-state-face 'emacs     init/solarized-base3 init/solarized-base00)
  (init/evil-state-face 'evilified init/solarized-base2 init/solarized-blue)
  (init/evil-state-face 'motion    init/solarized-base2 init/solarized-blue)
  (init/common-theme))

(provide 'init-theme)

