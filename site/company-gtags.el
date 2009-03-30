;;; company-gtags.el --- a company-mode completion back-end for GNU Global
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

(defcustom company-gtags-gnu-global-program-name
  (or (locate-file "global" exec-path exec-suffixes 'file-executable-p)
      "global")
  "*"
  :type 'string
  :group 'company)

(defvar company-gtags-symbol-regexp
  "\\_<[A-Za-z_][A-Za-z_0-9]*\\_>")

(defvar company-gtags-modes '(c-mode c++-mode jde-mode java-mode php-mode))

(defvar company-gtags-available 'unknown)
(make-variable-buffer-local 'company-gtags-available)

(defun company-gtags-available ()
  (when (eq company-gtags-available 'unknown)
    (condition-case err
        (setq company-gtags-available
              (= 0 (call-process company-gtags-gnu-global-program-name
                                 nil nil nil "-c" "WHATEVER")))
      (error
       (message "Company: GNU Global not found")
       (setq-default company-gtags-available nil))))
  company-gtags-available)

(defun company-gtags-fetch-tags (prefix)
  (with-temp-buffer
    (let (tags)
      (when (= 0 (call-process "global" nil (list (current-buffer) nil)
                               nil "-c" prefix))
        (goto-char (point-min))
        (while (looking-at company-gtags-symbol-regexp)
          (push (match-string-no-properties 0) tags)
          (forward-line)))
      (nreverse tags))))

(defun company-gtags-location (tag)
  (with-temp-buffer
    (when (= 0 (call-process "global" nil (list (current-buffer) nil)
                               nil "-x" tag))
        (goto-char (point-min))
        (when (looking-at (concat (regexp-quote tag)
                                  "[ \t]+\\([[:digit:]]+\\)"
                                  "[ \t]+\\([^ \t]+\\)"))
          (cons (expand-file-name (match-string 2))
                (string-to-number (match-string 1)))))))

(defun company-gtags (command &optional arg &rest ignored)
  "A `company-mode' completion back-end for GNU Global."
  (case command
    ('prefix (and (memq major-mode company-gtags-modes)
                  (not (company-in-string-or-comment))
                  (company-gtags-available)
               (or (company-grab company-gtags-symbol-regexp) "")))
    ('candidates (company-gtags-fetch-tags arg))
    ('sorted t)
    ('location (company-gtags-location arg))))

(provide 'company-gtags)
;;; company-gtags.el ends here


