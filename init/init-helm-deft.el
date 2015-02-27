;;; helm-deft.el --- helm module for grepping note files over directories

;; Copyright (C) 2014 Derek Feichtinger
;; Copyright (C) 2015 Sam Doshi

;; Author: Derek Feichtinger <derek.feichtinger@psi.ch>
;; Keywords: convenience
;; Homepage: https://github.com/dfeich/helm-deft
;; Version: TODO

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

(require 'helm)
(require 'helm-grep)
(require 'helm-files)
(require 'f)

(defconst init/helm-deft-dir-list '("~/Dropbox/Notes"))

(defconst init/helm-deft-extension "org")

(defvar init/helm-deft-file-list ""
  "variable to store the list of candidate files")

(defvar init/helm-source-deft-fn
  '((name . "File Names")
    (init . (lambda ()
              (progn (setq init/helm-deft-file-list (init/helm-deft-fname-search))
                     (with-current-buffer (helm-candidate-buffer 'local)
                       (insert (mapconcat 'identity
                                          init/helm-deft-file-list "\n"))))))
    (candidates-in-buffer)
    (match-part . (lambda (c) (helm-basename c)))
    (type . file)
    (candidate-transformer . (lambda (c) (sort (helm-highlight-files c)
                                               (lambda (a b)
                                                 (string< (downcase (car a))
                                                          (downcase (car b)))))))))

(defvar init/helm-source-deft-filegrep
  '((name . "File Contents")
    (candidates-process . init/helm-deft-fgrep-search)
    (action . helm-grep-action)
    (requires-pattern)
    (filter-one-by-one . helm-grep-filter-one-by-one)
    (cleanup . (lambda () (when (get-buffer "*helm-deft-proc*")
                            (let ((kill-buffer-query-functions nil))
                              (kill-buffer "*helm-deft-proc*")))))))

(defun init/helm-deft-rotate-searchkeys ()
  "rotate the words of the search pattern in the helm minibuffer"
  (interactive)
  (helm-log "Executing init/helm-deft-rotate-searchkeys")
  (let ((patlst (split-string helm-pattern "  *")))
    (when (and (>= (length patlst) 1)
               (> (length (car patlst)) 0))
      (delete-minibuffer-contents)
      (insert (mapconcat #'identity
                         (append (cdr patlst) (list (car patlst)))
                         " "))
      (helm-update))))

(defvar init/helm-deft-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map helm-map)
    (define-key map (kbd "C-r") 'init/helm-deft-rotate-searchkeys)
    (delq nil map)))

(defun init/helm-deft-fname-search ()
  "search all preconfigured directories for matching files and return the filenames as a list"
  (cl-loop for dir in init/helm-deft-dir-list
           collect (f--files dir (equal (f-ext it) init/helm-deft-extension) t)
           into reslst
           finally (return (apply #'append reslst))))

(defun init/helm-deft-build-cmd (ptrnstr filelst)
  "Builds a grep command where PTRNSTR may contain multiple search patterns
separated by spaces. The first pattern will be used to retrieve matching lines.
All other patterns will be used to pre-select files with matching lines.
FILELST is a list of file paths"
  (let* ((ptrnlst (remove "" (reverse (split-string ptrnstr "  *"))))
         (firstp (pop ptrnlst))
         (filelst (mapconcat 'identity filelst " "))
         (innercmd (if ptrnlst
                       (cl-labels ((build-inner-cmd
                                    (ptrnlst filelst)
                                    (let ((pattern (pop ptrnlst)))
                                      (if ptrnlst
                                          (format "$(grep -Elie \"%s\" %s)" pattern
                                                  (build-inner-cmd ptrnlst filelst))
                                        (format "$(grep -Elie \"%s\" %s)"
                                                pattern filelst)))))
                         (build-inner-cmd ptrnlst filelst))
                     filelst)))
    (format "grep -EHine \"%s\" %s" firstp innercmd)))

(defun init/helm-deft-fgrep-search ()
  "greps for the helm search pattern in the configuration defined file list"
  (let* ((shcmd (init/helm-deft-build-cmd helm-pattern init/helm-deft-file-list)))
    (helm-log "grep command: %s" shcmd)
    (start-process-shell-command "helm-deft-proc" "*helm-deft-proc*"
                                 shcmd)))

(defun init/helm-deft ()
  "Preconfigured `helm' module for locating note files where either the
filename or the file contents match the query string. Inspired by the
emacs `deft' extension"
  (interactive)
  (helm :sources '(init/helm-source-deft-fn init/helm-source-deft-filegrep)
        :keymap init/helm-deft-map))

(provide 'init-helm-deft)

