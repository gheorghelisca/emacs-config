;;; external-abook.el --- Enable the use of external address books from within Emacs
;;
;; Copyright (C) 2008 pmade inc. (Peter Jones pjones@pmade.com)
;;
;; Permission is hereby granted, free of charge, to any person obtaining
;; a copy of this software and associated documentation files (the
;; "Software"), to deal in the Software without restriction, including
;; without limitation the rights to use, copy, modify, merge, publish,
;; distribute, sublicense, and/or sell copies of the Software, and to
;; permit persons to whom the Software is furnished to do so, subject to
;; the following conditions:
;;
;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
;; LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
;; OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
;; WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
;;
;; Commentary:
;;
;; This package allows Emacs to use any of the external address book
;; access tools that already work with the Mutt MUA external query
;; mechanism.  http://www.mutt.org/doc/manual/manual-4.html#queryq
;;
;; Usage:
;;
;; (require 'external-abook)
;; (setq external-abook-command "my-query-script '%s'")
;;
;; Bind `external-abook-try-expand' to a key binding of your choice.
;; It will expand the text before point into an email address from
;; your external address book.
;;
;; Git Repository:
;;
;; git clone git://pmade.com/elisp

(defvar external-abook-command nil
  "The command line tool to use for searching the external address book.

Specify a command to run, with its options.  For example:

  (setq external-abook-command \"mutt_ldap_query.pl '%s'\")

See http://www.mutt.org/doc/manual/manual-4.html#query for more information.")

(defun external-abook-search (query)
  "Search the address book using the given query."
  (let ((results (shell-command-to-string (format external-abook-command query))))
    (external-abook-parse (cdr (split-string results "\n" t)))))

(defun external-abook-parse (strings)
  "Parse the search results found in the given list of strings."
  (if (null strings) nil
    (let ((elements (split-string (car strings) "\t+" t))
          (others (external-abook-parse (cdr strings))))
      (append others (list (nreverse (external-abook-strip elements)))))))

(defun external-abook-strip (elements)
  (mapcar (lambda (s) (replace-regexp-in-string "[ \t]+$" "" s)) elements))

(defun external-abook-make-string (address)
  "Create a valid email address string from the given address."
  (cond ((null address)
         nil)
        ((eql (length address) 1)
         (format "%s" (car address)))
        (t
         (apply 'format "%s <%s>" address))))

(defun external-abook-completing-read (message choices)
  "Call ido-completing-read if available."
  (completing-read message choices))

(defun external-abook-single-result (query results)
  "Narrow the results down to a result formatted with external-abook-make-string."
  (cond
   ((null results) nil)
   ((= 1 (length results)) (external-abook-make-string (car results)))
   (t (external-abook-make-string (assoc (external-abook-completing-read (format "Select Name (%s): " query) results) results)))))

(defun external-abook-bounds ()
  "Find text before point that should be used to search with."
  (let* ((end (point))
         (bol (save-excursion (beginning-of-line) (point)))
         (start (save-excursion (search-backward-regexp "[ :,]" bol t))))
    (if start (cons (1+ start) end) (cons bol end))))

(defun external-abook-try-expand ()
  "Attempt to expand the text behind point from the external address book.

This function is useful when bound to a key, for example, in Gnus
message mode.  When called, it will search the external address
book for entries that match the text behind point.  If a match is
found, that text will be replaced with the matching email
address."
  (interactive)
  (let* ((bounds (external-abook-bounds))
         (query (and bounds (buffer-substring (car bounds) (cdr bounds))))
         (results (and query (external-abook-search query)))
         (email (external-abook-single-result query results)))
    (if email
        (progn
          (delete-region (car bounds) (cdr bounds))
          (insert email))
      (message "No match."))))

(provide 'external-abook)
