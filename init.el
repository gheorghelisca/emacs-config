;-*-Mode:Emacs-Lisp-*-

;; Public emacs site
(add-to-list 'load-path "~/.emacs.d/site")
(add-to-list 'load-path "~/work/lisp/site/slime")

;; start emacs server for emacsclient
(server-start)

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

(ffap-bindings)

(setq minibuffer-max-depth nil)
(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(backup-directory-alist (quote ((".*" . "~/.emacs.d/emacs-file-backups"))))
 '(c-basic-offset (quote set-from-style))
 '(column-number-mode t)
 '(compilation-scroll-output t)
 '(compile-command "make")
 '(diary-file "~/.emacs.d/diary")
 '(ecb-tip-of-the-day nil)
 '(egg-buffer-hide-section-type-on-start (quote ((egg-status-buffer-mode . :diff) (egg-commit-buffer-mode . :diff))))
 '(gnuserv-program (concat exec-directory "/gnuserv") t)
 '(ispell-local-dictionary "american")
 '(kept-new-versions 3)
 '(kept-old-versions 3)
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
 '(version-control t)
 '(view-diary-entries-initially t)
 '(whitespace-check-leading-whitespace nil)
 '(whitespace-modes (quote (ada-mode asm-mode autoconf-mode awk-mode c-mode c++-mode cc-mode change-log-mode cperl-mode electric-nroff-mode emacs-lisp-mode f90-mode fortran-mode html-mode html3-mode java-mode jde-mode ksh-mode nil LaTeX-mode lisp-mode m4-mode makefile-mode modula-2-mode nroff-mode objc-mode pascal-mode perl-mode prolog-mode python-mode scheme-mode sgml-mode sh-mode shell-script-mode simula-mode tcl-mode tex-mode texinfo-mode vrml-mode xml-mode))))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(default ((t (:stipple nil :background "black" :foreground "#c0c0c0" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 90 :width normal :family "adobe-courier"))))
 '(font-latex-subscript-face ((t nil)))
 '(font-latex-superscript-face ((t nil)))
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
(setq-default indent-tabs-mode nil)

;; Search highlighting
;; highlight during query
(setq query-replace-highlight t)
;; highlight incremental search
(setq search-highlight t)

(require 'rect-mark)
;;(setq woman-use-own-frame nil)     ; don't create new frame for manpages
;;(setq woman-use-topic-at-point t)  ; don't prompt upon K key (manpage display)

;; C/C++ indentation config
(require 'cc-mode)
(setq c-basic-offset 4)
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

;; Some additional slime config
;; (add-to-list 'load-path (concat slime-*directory* "contrib/"))

(require 'slime)
(slime-setup '(slime-fancy slime-asdf))
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

    (define-key slime-mode-map "\r" 'newline-and-indent)
    (define-key slime-mode-map [tab] 'slime-fuzzy-indent-and-complete-symbol)))

;; use internal w3m browser (used in particular for clhs lookup)
(setq browse-url-browser-function 'w3m)
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

;; kill-ring <-> x11
(setq x-select-enable-clipboard t)
(setq interprogram-paste-function 'x-cut-buffer-or-selection-value)

