;; superseded by init-helm-deft

(require 'f)
(require 'helm)

;; http://wikemacs.org/wiki/How_to_write_helm_extensions

(defun init/is-note-file (file)
  "Used to filter which files are valid notes"
  (let ((ext (f-ext file)))
    (or
     (equal ext "org")
     (equal ext "md"))))

(defvar init/cached-note-files nil)

(defun init/refresh-note-files ()
  "Reload the list of notes"
  (setq init/cached-note-files
        (mapcar (lambda (c)
                  (cons (f-filename c) c))
                (f-files "~/Dropbox/Notes" 'init/is-note-file))))

(defvar init/helm-source-notes
      '((name . "Notes")
        (candidates . init/cached-note-files)
        (action . (lambda (candidate)
                    (find-file candidate)))))

(defun init/helm-notes ()
  "Helm interface for my notes"
  (interactive)
  (init/refresh-note-files)
  (helm :sources '(init/helm-source-notes)))

(provide 'init-notes)
