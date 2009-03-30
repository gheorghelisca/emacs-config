;;; company-files.el --- a company-mode completion back-end for file names
;;
;; Copyright (C) 2009 Nikolaj Schumacher
;;
;; This file is part of company 0.2.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(require 'company)
(eval-when-compile (require 'cl))

(defun company-files-directory-files (dir prefix)
  (ignore-errors
    (if (equal prefix "")
        (directory-files dir nil "\\`[^.]\\|\\`.[^.]")
      (file-name-all-completions prefix dir))))

(defun company-files-grab-existing-name ()
  ;; Grab file names with spaces, only when they include quotes.
  (let ((file (or (company-grab "\"\\(~?/[^\"\n]*\\)" 1)
                  (company-grab "\'\\(~?/[^\'\n]*\\)" 1)
                  (company-grab "[ \t\n]\\(~?/[^ \t\n]*\\)" 1)))
        dir)
    (and file
         (setq dir (file-name-directory file))
         (file-exists-p dir)
         (file-name-all-completions (file-name-nondirectory file) dir)
         file)))

(defvar company-files-completion-cache nil)

(defun company-files-complete (prefix)
  (let* ((dir (file-name-directory prefix))
         (file (file-name-nondirectory prefix))
         candidates)
    (setq company-files-completion-cache nil)
    (unless (equal dir (car company-files-completion-cache))
      (dolist (file (company-files-directory-files dir file))
        (setq file (concat dir file))
        (push file candidates)
        (when (file-directory-p file)
          ;; Add one level of children.
          (dolist (child (company-files-directory-files file ""))
            (push (concat file child) candidates))))
      (setq company-files-completion-cache (cons dir (nreverse candidates))))
    (cdr company-files-completion-cache)))

(defun company-files (command &optional arg &rest ignored)
  "a `company-mode' completion back-end existing file names."
  (case command
    ('prefix (company-files-grab-existing-name))
    ('candidates (company-files-complete arg))
    ('location (cons (dired-noselect
                      (file-name-directory (directory-file-name arg))) 1))
    ('sorted t)
    ('no-cache t)))

(provide 'company-files)
;;; company-files.el ends here
