;-*-Mode:Emacs-Lisp-*-

;; Public emacs site
(add-to-list 'load-path "~/.emacs.d/site")
(add-to-list 'load-path "~/.emacs.d/site/org-mode")
(add-to-list 'load-path "~/work/lisp/site/slime")

;; start emacs server for emacsclient
(server-start)

;; slime
(require 'slime)

;; use cool ldap-search and mutt aliases and addressbook for composing mails
(require 'mail-addons)

;; Emacs should always ask for confirmation on exit
(setq confirm-kill-emacs 'yes-or-no-p)

;; other settings
(require 'paren)
(show-paren-mode 1)
(global-set-key '[delete] 'delete-char)

;; disable iconification bindings
(global-unset-key "\C-z")
(global-unset-key "\C-x\C-z")

(setq minibuffer-max-depth nil)
(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(TeX-PDF-mode t)
 '(backup-directory-alist (quote ((".*" . "~/.emacs.d/emacs-file-backups"))))
 '(c-basic-offset (quote set-from-style))
 '(column-number-mode t)
 '(compilation-scroll-output t)
 '(compile-command "make ")
 '(desktop-save-mode nil)
 '(diary-file "~/.emacs.d/diary")
 '(ecb-tip-of-the-day nil)
 '(egg-buffer-hide-section-type-on-start (quote ((egg-status-buffer-mode . :diff) (egg-commit-buffer-mode . :diff))))
 '(gnuserv-program (concat exec-directory "/gnuserv") t)
 '(ido-use-filename-at-point (quote guess))
 '(ido-use-url-at-point t)
 '(indent-tabs-mode nil)
 '(ispell-local-dictionary "american")
 '(kept-new-versions 3)
 '(kept-old-versions 3)
 '(line-number-mode t)
 '(load-home-init-file t t)
 '(mark-diary-entries-in-calendar t)
 '(menu-bar-mode nil)
 '(next-line-add-newlines nil)
 '(paren-mode (quote paren) nil (paren))
 '(pc-select-meta-moves-sexps t)
 '(pc-select-selection-keys-only t)
 '(pc-selection-mode t nil (pc-select))
 '(safe-local-variable-values (quote ((TeX-PDF . t) (readtable . nisp) (readtable . :nisp) (Package . NISP) (Syntax . Common-Lisp) (Package . SAX) (Encoding . utf-8) (Syntax . COMMON-LISP) (Package . CL-PPCRE) (package . rune-dom) (readtable . runes) (Syntax . ANSI-Common-Lisp) (Base . 10))))
 '(tool-bar-mode nil)
 '(transient-mark-mode t)
 '(version-control t)
 '(view-diary-entries-initially t)
 '(whitespace-check-leading-whitespace nil)
 '(whitespace-modes (quote (ada-mode asm-mode autoconf-mode awk-mode c-mode c++-mode cc-mode change-log-mode cperl-mode electric-nroff-mode emacs-lisp-mode f90-mode fortran-mode html-mode html3-mode java-mode jde-mode ksh-mode nil LaTeX-mode lisp-mode m4-mode makefile-mode modula-2-mode nroff-mode objc-mode pascal-mode perl-mode prolog-mode python-mode scheme-mode sgml-mode sh-mode shell-script-mode simula-mode tcl-mode tex-mode texinfo-mode vrml-mode xml-mode))))

(autoload 'c++-mode  "cc-mode" "C++ Editing Mode" t)
(autoload 'c-mode    "cc-mode" "C Editing Mode"   t)
(autoload 'objc-mode "cc-mode" "Objective C Editing Mode" t)
(autoload 'text-mode "indented-text-mode" "Indented Text Editing Mode" t)
(autoload 'xrdb-mode "xrdb-mode" "Mode for editing X resource files" t)
(setq auto-mode-alist
      (append '(("\\.C$"       . c++-mode)
                ("\\.cc$"      . c++-mode)
                ("\\.c$"       . c-mode)
                ("\\.h$"       . c++-mode)
                ("\\.i$"       . c++-mode)
                ("\\.ii$"      . c++-mode)
                ("\\.m$"       . objc-mode)
                ("\\.pl$"      . prolog-mode)
                ("\\.sql$"     . c-mode)
                ("\\.sh$"      . shell-script-mode)
                ("\\.mak$"     . makefile-mode)
                ("\\.GNU$"     . makefile-mode)
                ("makefile$"   . makefile-mode)
                ("Makefile$"   . makefile-mode)
                ("Imakefile$"  . makefile-mode)
                ("\\.Xdefaults$"    . xrdb-mode)
                ("\\.Xenvironment$" . xrdb-mode)
                ("\\.Xresources$"   . xrdb-mode)
                ("*.\\.ad$"         . xrdb-mode)
                ("\\.[eE]?[pP][sS]$" . ps-mode)
                ("\\.nsp"      . lisp-mode)
                ("\\.asd"      . lisp-mode)
                ("\\.vimpulse" . lisp-mode)
                ("\\.cl$"      . lisp-mode)
                ("\\.launch"   . xml-mode)
                ) auto-mode-alist))

(setq default-tab-width 4)
(setq initial-major-mode 'text-mode)
(setq default-major-mode 'text-mode)
(setq scroll-step 1)
(setq scroll-conservatively 5)
(global-font-lock-mode t)               ;colorize all buffers

;; Search highlighting
;; highlight during query
(setq query-replace-highlight t)
;; highlight incremental search
(setq search-highlight t)

(require 'ido)
(ido-mode 'both)

;; C/C++ indentation config
(require 'cc-mode)
(setq c-basic-offset 2)
(setq c-default-style
      '((java-mode . "java") (other . "ellemtel")))
(setq c-offsets-alist '((arglist-cont-nonempty . +)))
(define-key c-mode-base-map "\C-c\C-c" 'recompile)

;; SHIFT-Arrow for moving through windows
(require 'windmove)
;; (windmove-default-keybindings)
(setq windmove-wrap-around t)
(global-set-key "\M-\S-n" 'windmove-down)
(global-set-key "\M-\S-p" 'windmove-up)
(global-set-key "\M-\S-f" 'windmove-right)
(global-set-key "\M-\S-b" 'windmove-left)

;; [ and ] should be handled paranthesis-like in lisp files.
(modify-syntax-entry ?\[ "(]  " lisp-mode-syntax-table)
(modify-syntax-entry ?\] ")[  " lisp-mode-syntax-table)

(slime-setup '(slime-fancy slime-asdf))
;; (slime-highlight-edits-init)
(setq slime-complete-symbol-function 'slime-fuzzy-complete-symbol)

(setq slime-multiprocessing t)
(add-hook 'slime-mode-hook
  (lambda ()
    (put 'make-instance 'common-lisp-indent-function '(4 &rest 2))
    (put 'with-failure-handling 'common-lisp-indent-function '((&whole 4 &rest (&whole 1 1 2)) &body))

    (define-key slime-mode-map "\r" 'newline-and-indent)
    (define-key slime-mode-map [tab] 'slime-fuzzy-indent-and-complete-symbol)))

;; use internal w3m browser (used in particular for clhs lookup)
(setq browse-url-browser-function (lambda (url &optional new-window)
                                    (when (one-window-p)
                                      (split-window))
                                    (other-window 1)
                                    (w3m url new-window nil)))

;; sbcl
(defun sbcl ()
  "Inferior SBCL"
  (interactive)
  (let ( (inferior-lisp-program "/usr/bin/sbcl") )
    (slime)))

;; sbcl git dev version
(defun sbcl-dev ()
  "Inferior SBCL"
  (interactive)
  (let ( (inferior-lisp-program "sbcl") )
    (slime)))

(defun sbcl-ros ()
  "Inferior SBCL, in ROS environment."
  (interactive)
  (let ( (inferior-lisp-program "/home/moesenle/work/ros/scripts/sbcl-ros.sh") )
    (slime)))

(global-set-key "\C-cl" 'sbcl-dev)
(global-set-key "\C-cf"
                '(lambda ()
                  (interactive)
                  (slime-quit-lisp)))

;; paredit mode
(require 'paredit)
(add-hook 'emacs-lisp-mode-hook (lambda ()
                                  (paredit-mode +1)))
(add-hook 'lisp-mode-hook (lambda ()
                            (paredit-mode +1)))
(add-hook 'inferior-lisp-mode-hook (lambda ()
                                     (paredit-mode +1)))

;; Mouse wheel
(autoload 'mwheel-install "mwheel" "Enable mouse wheel support.") (mwheel-install)

;; Ignore .svn stuff in grep-find
(setq grep-find-command "find . -type f -not -name \"*.svn-base\" -and -not -name \"*.tmp\" -print0 | xargs -0 -e grep -i -n -s -F ")

;; Load auctex
(load "auctex")

(put 'downcase-region 'disabled nil)

;; Flyspell mode
(dolist (hook '(text-mode-hook))
  (add-hook hook (lambda () (flyspell-mode 1))))
(dolist (hook '(change-log-mode-hook log-edit-mode-hook))
  (add-hook hook (lambda () (flyspell-mode -1))))

;;(setq flyspell-default-dictionary "american")

;; Load cool git frontend
(require 'egg)

;; Set ispell default dictionary
(ispell-change-dictionary "american")

;; winner-mode to remember window config
(require 'winner)
(winner-mode)

;; numbered windows
(require 'window-number)
(window-number-mode)

;; ;; delete trailing whitespaces in all lines before saving
;; (add-hook 'write-file-hooks 'delete-trailing-whitespace)

;; ;; delete whitespaces and lines > 1 at end of file before saving
;; (add-hook 'write-file-hooks 'nuke-trailing-whitespace)

;;(whitespace-global-mode)

(defun mutt ()
  (interactive)
  (cd "~")
  (ansi-term "/usr/bin/mutt" "mutt"))

(put 'upcase-region 'disabled nil)

;; Load rosemacs
(add-to-list 'load-path "~/work/ros/ros/tools/rosemacs")

(require 'rosemacs)
(invoke-rosemacs)
(global-set-key "\C-x\C-r" ros-keymap)

(defvar *current-tramp-path* nil)
(defun connect-to-host (path)
  (setq *current-tramp-path* path)
  (setq slime-translate-from-lisp-filename-function
    (lambda (f)
      (concat *current-tramp-path* f)))
  (setq slime-translate-to-lisp-filename-function
    (lambda (f)
      (substring f (length *current-tramp-path*))))
  (slime-connect "localhost" 4005))
 
(defun slime-leela ()
  (interactive)
  (connect-to-host "/ssh:demo@leela:"))
 
(defun leela-homedir ()
  (interactive)
  (find-file (concat "/ssh:demo@leela:" "/home/demo/")))

;; kill-ring <-> x11
(setq x-select-enable-clipboard t)
(setq interprogram-paste-function 'x-cut-buffer-or-selection-value)

(require 'org)
(setq org-ditaa-jar-path "~/.emacs.d/bin/ditaa.jar")

(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 )
