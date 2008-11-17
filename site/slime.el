;;;  -*- Mode: Emacs-Lisp -*-

(defvar lisp-*directory* (expand-file-name "/usr/local/lehrstuhl/DIR/lisp/"))

(defvar slime-*directory* "~/work/lisp/site/slime-cvs")

;; get prog info
(defun inf9-get-prog-info (program)
  "Get some information by calling program"
  (remove ?\n (shell-command-to-string program)))

;; get debian name
(defun inf9-get-debian-version-name ()
  "Get debian version name"
  (let ( (version-name (inf9-get-prog-info "cat /etc/debian_version | cut -b -3")) )
    (cond ( (string= version-name "3.0") "woody" )
          ( (string= version-name "3.1") "sarge" )
          ( (string= version-name "4.0") "etch"  )
          ( t                            "lenny" ))))

;; host specific variables
(defvar *inf9-hostname* (inf9-get-prog-info "hostname"))
(defvar *inf9-architecture* (inf9-get-prog-info "dpkg --print-architecture"))
(defvar *inf9-debian-version-name* (inf9-get-debian-version-name))

; cmucl
(defvar cmucl-*version* "cmucl-19c")
(defvar cmucl-*directory* (expand-file-name (concat lisp-*directory* "implementations/" cmucl-*version* "/")))
(defvar cmucl-core-*directory* (expand-file-name (concat lisp-*directory* "images/" cmucl-*version* "/")))
(defvar cmucl-core-*extension* (concat "-" *inf9-debian-version-name*))
; allegro
(defvar acl-*version* (concat "acl-8.0-" *inf9-architecture*))
(defvar acl-*directory* (expand-file-name (concat lisp-*directory* "implementations/" acl-*version* "/")))
(defvar acl-core-*directory* (expand-file-name (concat lisp-*directory* "images/" acl-*version* "/")))
(defvar acl-core-*extension* (concat "-" *inf9-debian-version-name*))

(add-to-list 'load-path slime-*directory*)

(setq inferior-lisp-program (concat acl-*directory* "alisp"))
(require 'slime)
(slime-setup)

; variable settings
(setq slime-multiprocessing t)
(setq slime-startup-animation nil)

; cmucl
(defun cmucl ()
  "Inferior CMU Common LISP -- without x."
  (interactive)
  (let ( (inferior-lisp-program (concatenate 'string cmucl-*directory* "bin/lisp"
                                             " -core " cmucl-*directory* "lib/cmucl/lib/lisp.core")) )
    (slime)))

(defun cmucl-base ()
  "Inferior CMU Common LISP -- with base packages."
  (interactive)
  (let ( (inferior-lisp-program (concatenate 'string cmucl-*directory* "bin/lisp"
                                             " -core " cmucl-core-*directory* "cmucl-base" cmucl-core-*extension* ".core"))
         (slime-multiprocessing nil) )
    (slime)))

(defun cmucl-rpl ()
  "Inferior CMU Common LISP -- with NISP and RPL."
  (interactive)
  (let ( (inferior-lisp-program (concatenate 'string cmucl-*directory* "bin/lisp"
                                             " -core " cmucl-core-*directory* "cmucl-rpl" cmucl-core-*extension* ".core"))
         (slime-multiprocessing nil) )
    (slime)))

; allegro
(defun acl ()
  "Inferior Allegro"
  (interactive)
  (let ( (inferior-lisp-program (concatenate 'string acl-*directory* "alisp")) )
    (slime)))

(defun acl-base ()
  "Inferior Allegro -- with NISP and RPL"
  (interactive)
  (let ( (inferior-lisp-program (concatenate 'string acl-*directory* "alisp"
                                             " -I " acl-core-*directory* "acl-base" acl-core-*extension* ".core")) )
    (slime)))

(defun acl-rpl ()
  "Inferior Allegro -- with NISP and RPL"
  (interactive)
  (let ( (inferior-lisp-program (concatenate 'string acl-*directory* "alisp"
                                             " -I " acl-core-*directory* "acl-rpl" acl-core-*extension* ".core")) )
    (slime)))

(defun acl-base-no-clim ()
  "Inferior Allegro -- with NISP and RPL"
  (interactive)
  (let ( (inferior-lisp-program (concatenate 'string acl-*directory* "alisp"
                                             " -I " acl-core-*directory* "acl-base-no-clim" acl-core-*extension* ".core")) )
    (slime)))

(defun acl-rpl-no-clim ()
  "Inferior Allegro -- with NISP and RPL"
  (interactive)
  (let ( (inferior-lisp-program (concatenate 'string acl-*directory* "alisp"
                                             " -I " acl-core-*directory* "acl-rpl-no-clim" acl-core-*extension* ".core")) )
    (slime)))

(defun acl-awbs-prakt ()
  "Inferior Allegro -- for Praktikum"
  (interactive)
  (let ( (inferior-lisp-program (concatenate 'string acl-*directory* "alisp"
                                             " -I " acl-core-*directory* "acl-rpl-awbs-praktikum.core")) )
    (slime)))

; Key definitions
;(global-set-key [(meta l)] 'cmucl-rpl)
(global-set-key [(ctrl l)] 'acl-rpl)
;(global-set-key [(meta f)]
;                '(lambda ()
;                   (interactive)
;                   (insert "(ext:quit)")))
(global-set-key [(ctrl f)]
                '(lambda ()
                   (interactive)
                   (slime-quit-lisp)))
(global-set-key [f9] 'other-window)
(global-set-key [f11] 'delete-other-windows)

