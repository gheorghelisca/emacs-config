
(require 'ldap)
(require 'eudc)
(require 'external-abook)
(require 'post)

(setq external-abook-command "~/.mutt/mutt-addresses-search '%s'")

(setq eudc-default-return-attributes nil
      eudc-strict-return-matches nil)

(setq ldap-ldapsearch-args (quote ("-tt" "-LLL" "-x")))
(setq eudc-inline-query-format '((name)
                                 (firstname)
                                 (firstname name)
                                 (email)
                                 ))
(setq eudc-inline-expansion-format '("%s %s <%s>" firstname name email))
(setq eudc-multiple-match-handling-method 'select)

(setq ldap-host-parameters-alist
;;       (quote (("ldap1.in.tum.de" base "ou=addressbook,dc=your_dc_here,dc=fr"
;;                binddn "cn=admin,dc=your_dc_here,dc=fr"
;;                passwd "your_password")))
      (quote (("ldap1.in.tum.de" base "ou=Personen,ou=IN,o=TUM,c=DE"
               binddn ""))))

(eudc-set-server "ldap1.in.tum.de" 'ldap t)
(setq eudc-server-hotlist '(("ldap1.in.tum.de" . ldap)))
(setq eudc-inline-expansion-servers 'hotlist)


(defun enz-eudc-expand-inline()
  (interactive)
  (move-end-of-line 1)
  (insert "*")
  (unless (condition-case nil
              (eudc-expand-inline)
            (error nil))
    (backward-delete-char-untabify 1)))

;; Adds some hooks

(eval-after-load "message"
  '(progn
     (define-key post-mode-map (kbd "TAB") 'enz-eudc-expand-inline)
     (define-key post-mode-map "\C-x\t" 'external-abook-try-expand)))

(eval-after-load "sendmail"
  '(progn
     (define-key post-mode-map (kbd "TAB") 'enz-eudc-expand-inline)
     (define-key post-mode-map "\C-x\t" 'external-abook-try-expand)))

(eval-after-load "post"
  '(progn
     (define-key post-mode-map (kbd "TAB") 'enz-eudc-expand-inline)
     (define-key post-mode-map "\C-x\t" 'external-abook-try-expand)))

(provide 'mail-addons)
