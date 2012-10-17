;-*-Mode:Emacs-Lisp-*-

;; Public emacs site
(add-to-list 'load-path "~/.emacs.d/site")
(add-to-list 'load-path "~/.emacs.d/emms/lisp")
(add-to-list 'load-path "~/.emacs.d/rosemacs")
(add-to-list 'load-path "~/.emacs.d/slime")

;; start emacs server for emacsclient
(server-start)

;;(setq frame-background-mode 'dark)

;; slime
(require 'slime)

;;(require 'post)
;; use cool ldap-search and mutt aliases and addressbook for composing mails
;;(require 'mail-addons)
;;(add-hook 'post-mode-hook (lambda ()
;;                            (interactive)
;;                            (set-buffer-file-coding-system 'raw-text)))
(require 'cmake-mode)


;; Emacs should always ask for confirmation on exit
(setq confirm-kill-emacs 'yes-or-no-p)

;; other settings
(require 'paren)
(show-paren-mode 1)
(global-set-key '[delete] 'delete-char)

;; Use regexp search per default. Swap regexp and normal search key
;; bindings.
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-r") 'isearch-backward)
(global-set-key (kbd "M-%") 'query-replace-regexp)
(global-set-key (kbd "C-M-%") 'query-replace)

;; disable iconification bindings
(global-unset-key "\C-z")
(global-unset-key "\C-x\C-z")

(setq minibuffer-max-depth nil)

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(LaTeX-verbatim-regexp "\\(verbatim\\|lstlisting\\)\\*?")
 '(TeX-PDF-mode t)
 '(TeX-view-program-selection (quote (((output-dvi style-pstricks) "dvips and gv") (output-dvi "xdvi") (output-pdf "Evince") (output-html "xdg-open"))))
 '(backup-directory-alist (quote ((".*" . "~/.emacs.d/emacs-file-backups"))))
 '(c-basic-offset (quote set-from-style))
 '(calendar-mark-diary-entries-flag t)
 '(calendar-view-diary-initially-flag t)
 '(column-number-mode t)
 '(compilation-scroll-output t)
 '(compile-command "make ")
 '(delete-old-versions t)
 '(desktop-save-mode nil)
 '(diary-file "~/.emacs.d/diary")
 '(ecb-tip-of-the-day nil)
 '(emms-cache-file "~/.emacs.d/emms-cache")
 '(emms-stream-bookmarks-file "~/.emacs.d/emms-streams")
 '(flymake-allowed-file-name-masks (quote (("\\.c\\'" flymake-simple-make-init) ("\\.cpp\\'" flymake-simple-make-init) ("\\.xml\\'" flymake-xml-init) ("\\.html?\\'" flymake-xml-init) ("\\.cs\\'" flymake-simple-make-init) ("\\.p[ml]\\'" flymake-perl-init) ("\\.php[345]?\\'" flymake-php-init) ("\\.h\\'" flymake-master-make-header-init flymake-master-cleanup) ("\\.java\\'" flymake-simple-make-java-init flymake-simple-java-cleanup) ("[0-9]+\\.tex\\'" flymake-master-tex-init flymake-master-cleanup) ("\\.tex\\'" flymake-simple-tex-init) ("\\.idl\\'" flymake-simple-make-init) ("\\.py\\'" flymake-pychecker-init))))
 '(flymake-master-file-dirs (quote ("." "./src" "./UnitTest" "./source")))
 '(gdb-many-windows t)
 '(gnuserv-program (concat exec-directory "/gnuserv") t)
 '(gud-tooltip-mode t)
 '(icomplete-prospects-height 3)
 '(ido-completion-buffer-all-completions t)
 '(ido-default-buffer-method (quote selected-window))
 '(ido-default-file-method (quote samewindow))
 '(ido-enable-dot-prefix t)
 '(ido-enable-flex-matching t)
 '(ido-enabled (quote both) t)
 '(ido-everywhere t)
 '(ido-max-window-height 5)
 '(ido-read-file-name-as-directory-commands (quote (find-dired)))
 '(ido-show-dot-for-dired t)
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
 '(mode-line-format (quote ("%e" #("-" 0 1 (help-echo "mouse-1: Select (drag to resize)
mouse-2: Make current window occupy the whole frame
mouse-3: Remove current window from display")) mode-line-mule-info mode-line-client mode-line-modified mode-line-remote mode-line-frame-identification mode-line-buffer-identification #("   " 0 3 (help-echo "mouse-1: Select (drag to resize)
mouse-2: Make current window occupy the whole frame
mouse-3: Remove current window from display")) mode-line-position (vc-mode vc-mode) #("  " 0 2 (help-echo "mouse-1: Select (drag to resize)
mouse-2: Make current window occupy the whole frame
mouse-3: Remove current window from display")) mode-line-modes (:eval (ros-current-pkg-modeline-entry)) (which-func-mode ("" which-func-format #("--" 0 2 (help-echo "mouse-1: Select (drag to resize)
mouse-2: Make current window occupy the whole frame
mouse-3: Remove current window from display")))) (global-mode-string (#("--" 0 2 (help-echo "mouse-1: Select (drag to resize)
mouse-2: Make current window occupy the whole frame
mouse-3: Remove current window from display")) global-mode-string)) #("-%-" 0 3 (help-echo "mouse-1: Select (drag to resize)
mouse-2: Make current window occupy the whole frame
mouse-3: Remove current window from display")))))
 '(next-line-add-newlines nil)
 '(paren-mode (quote paren) nil (paren))
 '(pc-select-meta-moves-sexps t)
 '(pc-select-selection-keys-only t)
 '(pc-selection-mode t nil (pc-select))
 '(post-email-address "moesenle@in.tum.de")
 '(post-mail-message "\\(mutt-[a-zA-Z0-9-.]+-[0-9]+-[0-9]+-[a-z0-9]+\\|mutt[a-zA-Z0-9._-]\\{6\\}\\)\\'")
 '(py-imenu-show-method-args-p t)
 '(python-indent 2)
 '(ros-completion-function (quote ido-completing-read))
 '(safe-local-variable-values (quote ((TeX-PDF . t) (readtable . nisp) (readtable . :nisp) (Package . NISP) (Syntax . Common-Lisp) (Package . SAX) (Encoding . utf-8) (Syntax . COMMON-LISP) (Package . CL-PPCRE) (package . rune-dom) (readtable . runes) (Syntax . ANSI-Common-Lisp) (Base . 10))))
 '(slime-ros-completion-function (quote ido-completing-read))
 '(standard-indent 2)
 '(tool-bar-mode nil)
 '(transient-mark-mode t)
 '(version-control t)
 '(view-diary-entries-initially t)
 '(w3m-session-crash-recovery nil)
 '(whitespace-check-leading-whitespace nil)
 '(whitespace-modes (quote (ada-mode asm-mode autoconf-mode awk-mode c-mode c++-mode cc-mode change-log-mode cperl-mode electric-nroff-mode emacs-lisp-mode f90-mode fortran-mode html-mode html3-mode java-mode jde-mode ksh-mode nil LaTeX-mode lisp-mode m4-mode makefile-mode modula-2-mode nroff-mode objc-mode pascal-mode perl-mode prolog-mode python-mode scheme-mode sgml-mode sh-mode shell-script-mode simula-mode tcl-mode tex-mode texinfo-mode vrml-mode xml-mode)))
 '(yas/fallback-behavior (quote call-other-command))
 '(yas/root-directory (quote ("~/.emacs.d/snippets" "/usr/share/emacs/site-lisp/yasnippet/snippets")) nil (yasnippet)))

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
                (".sbclrc" . lisp-mode)
                ("\\.launch"   . xml-mode)
                ("\\(mutt-[a-zA-Z0-9-.]+-[0-9]+-[0-9]+-[a-z0-9]+\\|mutt[a-zA-Z0-9._-]\\{6\\}\\)\\'" . post-mode)
                ("\\.launch"   . nxml-mode)
                ("manifest.xml" . nxml-mode)
                ("PKGBUILD" . sh-mode)
                ("CMakeLists\\.txt\\'" . cmake-mode)
                ("\\.cmake\\'" . cmake-mode)
                ) auto-mode-alist))

(setq default-tab-width 2)
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
(require 'google-c-style)
(add-hook 'c-mode-common-hook 'google-set-c-style)
(define-key c-mode-base-map "\C-c\C-c" 'recompile)

;; [ and ] should be handled paranthesis-like in lisp files.
(modify-syntax-entry ?\[ "(]  " lisp-mode-syntax-table)
(modify-syntax-entry ?\] ")[  " lisp-mode-syntax-table)

(slime-setup '(slime-fancy slime-asdf slime-indentation slime-ros))
(setq slime-complete-symbol-function 'slime-fuzzy-complete-symbol)

(setq slime-multiprocessing t)

;;; adjust lisp indentation
(put 'make-instance 'common-lisp-indent-function '(4 &rest 2))

(define-key slime-mode-map "\r" 'newline-and-indent)
(define-key slime-mode-map [tab] (lambda ()
                                   (interactive)
                                   (let ((yas/fallback-behavior nil))
                                     (unless (yas/expand)
                                       (slime-fuzzy-indent-and-complete-symbol)))))

(define-key slime-mode-map (kbd "M-,")
  (lambda ()
    (interactive)
    (condition-case nil
        (slime-pop-find-definition-stack)
      (error (tags-loop-continue)))))

(define-key lisp-mode-map (kbd "M-a") 
  (lambda ()
    (interactive)
    (let ((ppss (syntax-ppss)))
      (if (nth 3 ppss)
          (goto-char (1+ (nth 8 ppss)))
        (progn
          (backward-up-list 1)
          (down-list 1))))))

(define-key lisp-mode-map (kbd "M-e") 
  (lambda ()
    (interactive)
    (let ((ppss (syntax-ppss)))
      (if (nth 3 ppss)
          (progn
            (goto-char (nth 8 ppss))
            (forward-sexp 1)
            (backward-char 1))
        (progn
          (up-list 1)
          (backward-down-list 1))))))

;; use internal w3m browser (used in particular for clhs lookup)
(setq browse-url-browser-function (lambda (url &optional new-window)
                                    (when (one-window-p)
                                      (split-window))
                                    (other-window 1)
                                    (w3m url new-window nil)))

(global-set-key "\C-cl" 'slime-ros)
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
(define-key paredit-mode-map (kbd ")") 'paredit-close-round-and-newline)
(define-key paredit-mode-map (kbd "M-)") 'paredit-close-round)

;; Mouse wheel
(autoload 'mwheel-install "mwheel" "Enable mouse wheel support.") (mwheel-install)

;; Ignore .svn stuff in grep-find
(setq grep-find-command "find . -type f -not -name \"*.svn-base\" -and -not -name \"*.tmp\" -print0 | xargs -0 -e grep -i -n -s -F ")

;; Load auctex
(load "auctex")

;; M-u and M-l
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

;; Flyspell mode
(dolist (hook '(text-mode-hook))
  (add-hook hook (lambda () (flyspell-mode 1))))
(dolist (hook '(change-log-mode-hook log-edit-mode-hook))
  (add-hook hook (lambda () (flyspell-mode -1))))
(dolist (hook '(lisp-mode-hook c++-mode-hook python-mode-hook))
  (add-hook hook (lambda () (flyspell-prog-mode))))

;; Set ispell default dictionary
(ispell-change-dictionary "american")

;; winner-mode to remember window config
(require 'winner)
(winner-mode)

;; numbered windows
(require 'window-number)
(window-number-mode)

;; Load rosemacs
(require 'rosemacs)
(require 'slime-ros)
(invoke-rosemacs)
(global-set-key "\C-x\C-r" ros-keymap)
(require 'rng-loc)
(push (concat (ros-package-path "rosemacs") "/rng-schemas.xml") rng-schema-locating-files)

;; kill-ring <-> x11
(setq x-select-enable-clipboard t)
(setq interprogram-paste-function 'x-cut-buffer-or-selection-value)

(require 'org)
(setq org-ditaa-jar-path "~/.emacs.d/bin/ditaa.jar")

;; Python stuff
;; (require 'pymacs)
;; (pymacs-load "ropemacs" "rope-")

(defun flymake-pychecker-init ()
  (let* ((temp-file (flymake-init-create-temp-buffer-copy
                     'flymake-create-temp-inplace))
         (local-file (file-relative-name
                      temp-file
                      (file-name-directory buffer-file-name))))
    (list "~/.emacs.d/bin/flymake-pychecker" (list local-file))))

(require 'flymake-cursor)

(require 'yasnippet)
(yas/initialize)
(yas/load-directory "~/.emacs.d/snippets")

;;(require 'inf-haskell)

;; Configure EMMS
;;(require 'emms-setup)
;;(emms-standard)
;;(emms-default-players)
;;(emms-mode-line-disable)

;;(global-set-key (kbd "C-c e s") 'emms-start)
;;(global-set-key (kbd "C-c e P") 'emms-pause)
;;(global-set-key (kbd "C-c e n") 'emms-next)
;;(global-set-key (kbd "C-c e p") 'emms-previous)

(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 )

(put 'set-goal-column 'disabled nil)
