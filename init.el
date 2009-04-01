;-*-Mode:Emacs-Lisp-*-

;; Public emacs site
(add-to-list 'load-path "/usr/share/emacs/site-lisp/emacs-color-themes")
(add-to-list 'load-path "~/local/private/all/share/emacs/site-lisp")
(add-to-list 'load-path "~/.emacs.d/site")
;(add-to-list 'load-path "~/work/list/sbcl/site/slime-2.0-cvs")

;; start emacs server for emacsclient
(server-start)

;; use cool ldap-search when writing mails
(load "~/.emacs.d/site/mail-addons.el")

;; slime
(setq slime-bind-keys nil)
(if (file-readable-p "/usr/local/lehrstuhl/DIR/lisp/config-host/slime")
  (load "/usr/local/lehrstuhl/DIR/lisp/config-host/slime"))

;; Emacs should always ask for confirmation on exit
(setq confirm-kill-emacs 'yes-or-no-p)

;; other settings
(require 'paren)
(show-paren-mode 1)
(global-set-key '[delete] 'delete-char)

;; disable iconification bindings
(global-unset-key "\C-z")
(global-unset-key "\C-x\C-z")

(ffap-bindings)

(setq minibuffer-max-depth nil)
(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(c-basic-offset (quote set-from-style))
 '(column-number-mode t)
 '(diary-file "~/.emacs.d/diary")
 '(ecb-tip-of-the-day nil)
 '(gnuserv-program (concat exec-directory "/gnuserv"))
 '(ispell-local-dictionary "american")
 '(line-number-mode t)
 '(load-home-init-file t t)
 '(mark-diary-entries-in-calendar t)
 '(menu-bar-mode t)
 '(next-line-add-newlines nil)
 '(paren-mode (quote paren) nil (paren))
 '(pc-select-meta-moves-sexps t)
 '(pc-select-selection-keys-only t)
 '(pc-selection-mode t nil (pc-select))
 '(safe-local-variable-values (quote ((readtable . nisp) (readtable . :nisp) (Package . NISP) (Syntax . Common-Lisp) (Package . SAX) (Encoding . utf-8) (Syntax . COMMON-LISP) (Package . CL-PPCRE) (package . rune-dom) (readtable . runes) (Syntax . ANSI-Common-Lisp) (Base . 10))))
 '(tool-bar-mode nil)
 '(transient-mark-mode t)
 '(view-diary-entries-initially t)
 '(whitespace-check-leading-whitespace nil)
 '(whitespace-modes (quote (ada-mode asm-mode autoconf-mode awk-mode c-mode c++-mode cc-mode change-log-mode cperl-mode electric-nroff-mode emacs-lisp-mode f90-mode fortran-mode html-mode html3-mode java-mode jde-mode ksh-mode nil LaTeX-mode lisp-mode m4-mode makefile-mode modula-2-mode nroff-mode objc-mode pascal-mode perl-mode prolog-mode python-mode scheme-mode sgml-mode sh-mode shell-script-mode simula-mode tcl-mode tex-mode texinfo-mode vrml-mode xml-mode))))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(default ((t (:stipple nil :background "black" :foreground "#c0c0c0" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 90 :width normal :family "adobe-courier"))))
 '(secondary-selection ((t (:background "paleturquoise" :foreground "black"))))
 '(slime-highlight-edits-face ((((class color) (background dark)) (:background "gray15")))))

(require 'color-theme)
(color-theme-hober)
;; The color theme seems to overwrite some face configurations.
;; We set them manually
(set-face-foreground 'secondary-selection "black")

;;; Set the right font for new frames
(add-to-list 'default-frame-alist '(font . "-Adobe-Courier-Medium-R-Normal--12-120-75-75-M-70-ISO8859-1"))

(autoload 'c++-mode  "cc-mode" "C++ Editing Mode" t)
(autoload 'c-mode    "cc-mode" "C Editing Mode"   t)
(autoload 'objc-mode "cc-mode" "Objective C Editing Mode" t)
(autoload 'text-mode "indented-text-mode" "Indented Text Editing Mode" t)
(autoload 'xrdb-mode "xrdb-mode" "Mode for editing X resource files" t)
(autoload 'ps-mode "ps-mode" "Major mode for editing PostScript" t)
(setq auto-mode-alist
  (append '(("\\.C$"       . c++-mode)
      ("\\.cc$"      . c++-mode)
      ("\\.c$"       . c-mode)
      ("\\.h$"       . c++-mode)
      ("\\.i$"       . c++-mode)
      ("\\.ii$"      . c++-mode)
      ("\\.m$"       . objc-mode)
      ("\\.pl$"      . perl-mode)
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
      ) auto-mode-alist))

(setq default-tab-width 4)
(setq initial-major-mode 'text-mode)
(setq default-major-mode 'text-mode)
(setq scroll-step 1)
(setq scroll-conservatively 5)
(global-font-lock-mode t)               ;colorize all buffers
(setq-default indent-tabs-mode nil)

;; Search highlighting
;; highlight during query
(setq query-replace-highlight t)
;; highlight incremental search
(setq search-highlight t)

;;(setq viper-mode nil)                ; enable Viper at load time
;;(require 'viper)                   ; load Viper

;;(require 'vimpulse)                ; load Vimpulse
;;(require 'redo)                    ; redo

;; vim like movement in compilation/grep buffers
;;(add-hook 'compilation-mode-hook 'vimove-mode)

(require 'rect-mark)
;;(setq woman-use-own-frame nil)     ; don't create new frame for manpages
;;(setq woman-use-topic-at-point t)  ; don't prompt upon K key (manpage display)

;; ecb
;; (require 'ecb-autoloads)

;; Vim-like motion
;;(require 'vimove)

;; C/C++ indentation config
(require 'cc-mode)
(setq c-basic-offset 4)
(setq c-default-style
      '((java-mode . "java") (other . "ellemtel")))
(setq c-offsets-alist '((arglist-cont-nonempty . +)))
(define-key c-mode-base-map "\C-c\C-c" 'recompile)

;; SHIFT-Arrow for moving through windows
(windmove-default-keybindings)

;; [ and ] should be handled paranthesis-like in lisp files.
(modify-syntax-entry ?\[ "(]  " lisp-mode-syntax-table)
(modify-syntax-entry ?\] ")[  " lisp-mode-syntax-table)

;; Some additional slime config
(add-to-list 'load-path (concat slime-*directory* "contrib/"))

(slime-setup '(slime-fancy slime-asdf))
;; (slime-highlight-edits-init)
(setq slime-complete-symbol-function 'slime-fuzzy-complete-symbol)

(setq slime-multiprocessing t)
(add-hook 'slime-mode-hook
  (lambda ()
    ;;; adjust lisp indentation
    ;;(set-variable lisp-indent-function 'common-lisp-indent-function)
    ;;(put 'if 'common-lisp-indent-function '(2 &rest 2))
    ;;(put 'cond 'common-lisp-indent-function '(&rest (&whole 2 &rest 2)))
    ;;(put 'let  'common-lisp-indent-function '((&whole 4 &rest (&whole 2 1 2)) &body))
    ;;(put 'let* 'common-lisp-indent-function '((&whole 4 &rest (&whole 2 1 2)) &body))
    ;;(put 'defclass 'common-lisp-indent-function '(6 4 (&whole 2 &rest 1) (&whole 2 &rest 1)))
    (put 'make-instance 'common-lisp-indent-function '(4 &rest 2))

    (slime-define-key "\r" 'newline-and-indent)
    (slime-define-key [tab] 'slime-fuzzy-indent-and-complete-symbol)))

;; Do autoload when pressing C-l
(add-hook 'slime-rpl-connected-hook (lambda ()
                                      (slime-repl-eval-string "(kibo) (in-package :kibo) (values)")))

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
  (let ( (inferior-lisp-program "/bin/sh /usr/wiss/moesenle/local/sources/lenny-amd64/private/sbcl/run-sbcl.sh") )
    (slime)))

(global-set-key "\C-cl" 'acl-rpl)
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
(require 'magit)

;; Set ispell default dictionary
(ispell-change-dictionary "american")

;; ;; delete trailing whitespaces in all lines before saving
;; (add-hook 'write-file-hooks 'delete-trailing-whitespace)

;; ;; delete whitespaces and lines > 1 at end of file before saving
;; (add-hook 'write-file-hooks 'nuke-trailing-whitespace)

(whitespace-global-mode)
