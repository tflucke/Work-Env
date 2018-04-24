;; Requires:
;; aspell

;; ----------- Default Variables -----------
;; Global variables
(defvar backup-directory (concat user-emacs-directory "backups"))
(unless (file-exists-p backup-directory)
  (make-directory backup-directory))

(setq-default inhibit-startup-screen t
			  column-number-mode t
			  scroll-error-top-bottom t
			  show-paren-delay 0.25
              tab-width 4
	          indent-tabs-mode nil
              x-select-enable-clipboard t
              backup-directory-alist `(("." . ,backup-directory))
              delete-old-versions t)

;; (global-linum-mode)

;; Delete selected text when typing (normal editor behavior)
(delete-selection-mode t)

(defun mapcar-dot* (f list)
  (loop for (a . b) on list
        collect (funcall f a)
        unless (listp b)
        collect (funcall f b)))

;; ------------- Keybindings -------------
(defun smart-beginning-of-line ()
  "Move point to first non-whitespace character or beginning-of-line.

Move point to the first non-whitespace character on this line.
If point was already at that position, move point to beginning of line."
  (interactive "^")
  ;(if (version< "22" emacs-version) (interactive "^") (interactive))
  (let ((oldpos (point)))
    (beginning-of-line-text); goes to first significant character
    ;(back-to-indentation); goes to first non-whitespace
    (and (= oldpos (point))
         (beginning-of-line))))

(global-set-key [home] 'smart-beginning-of-line)
(global-set-key (kbd "C-c /") 'comment-or-uncomment-region)
(global-set-key (kbd "C-c C-k") 'compile)

;; ----------- Package Managing -----------
;; The package manager
(require 'package)

;; Add package sources
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("melpa" . "https://melpa.org/packages/")
                         ("melpa-stable" . "https://stable.melpa.org/packages/"))
      package-archive-priorities '(("melpa" . 1)))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure t)

;; ---------- Use X11 clipboard -----------
(use-package xclip
  :if (executable-find "xclip")
  :config (xclip-mode 1))

;; ---------- Color Themes ----------
(use-package color-theme
  :init
  ;; TODO: Fixes error about missing directory.  Don't know why.
  (unless (file-exists-p "~/.emacs.d/elpa/color-theme-20070910.1007/themes")
    (make-directory "~/.emacs.d/elpa/color-theme-20070910.1007/themes"))
  :config
  (color-theme-initialize))

(use-package base16-theme
  :requires color-theme
  :config (load-theme 'base16-tomorrow-night t))

;; --------- Parentheses Matching ---------
(show-paren-mode)
(use-package highlight-parentheses
  :config
  (define-globalized-minor-mode global-highlight-parentheses-mode
	highlight-parentheses-mode
	(lambda ()
	  (highlight-parentheses-mode t)))
  (global-highlight-parentheses-mode t))

(use-package autopair
  :config
  (autopair-global-mode))

;; ------------ xTerm Mouse ------------
;; Disable because it became annoying, sounds cool though.
(use-package mouse
  :disabled
  :config
  (xterm-mouse-mode t))

;; ----------- Ensime -----------
;; Java/Scala featues.  Includes:
;; * Inferred types
;; * Autocomplete
;; * Syntax highlighting
;; * Jump to source/docs
;; * Refactoring
;; * Error detection
(use-package scala-mode
  :commands (scala-mode))
  ;:mode "\\.scala\\'")

(use-package company)

(when (version<= "24.4" emacs-version)
  (use-package ensime
    :requires company
    :hook (scala-mode java-mode)
    :config (setq ensime-startup-notification nil)
    :pin melpa-stable))

; --------- C Syntax checker ---------
(use-package flycheck-irony
  :hook c-mode)
;  :mode ("\\.c\\'" "\\.h\\'")

(use-package auto-complete)

(use-package auto-complete-clang-async
  :requires auto-complete
  :hook c-mode
  :mode ("\\.c\\'" "\\.h\\'"))

;; ------------ Web Mode ------------
(use-package multi-web-mode
  :init
  (setq mweb-default-major-mode 'html-mode
        mweb-tags '((php-mode "<\\?php\\|<\\? \\|<\\?=" "\\?>")
                    (js-mode  "<script[^>]*>" "</script>")
                    (css-mode "<style[^>]*>" "</style>"))
        mweb-filename-extensions '("php" "htm" "html" "ctp" "phtml" "php4" "php5"))
;  :mode (mapcar-dot* (format "\\.%s\\'" ext '("php" "htm" "html" "ctp" "phtml" "php4" "php5")))
  :config
  (multi-web-global-mode 1))

;; ------------ Git Mode ------------
(when (version<= "24.4" emacs-version)
  (use-package magit
    :bind ("C-x g" . magit-status)))

;; ----------- Rust Mode ------------
(use-package rust-mode)
;  :mode ("\\.rs\\'")
(use-package rust-playground
  :requires rust-mode)
(use-package cargo
  :after rust-mode)
(use-package flycheck-rust
  :after rust-mode)

;; ----------- i3 Support ----------
(use-package i3wm
  :disabled
  :if (equal (getenv "DESKTOP_SESSION") "i3"))

;; --------- Racket Mode ----------
(use-package racket-mode)

;; ---------- C# Mode -------------
(use-package csharp-mode
  :if (version<= "24.4" emacs-version))
;  :mode ("\\.cs\\'")

;; ------- Markdown Mode ----------
(when (version<= "24.4" emacs-version)
  (use-package markdown-mode))
;    :mode ("\\.markdown\\'" "\\.md\\'")

;; ---- StackOverflow Client ------
(use-package sx
  :disabled)

;; -------- Spellcheck ------------
(defun flyspell-detect-ispell-args (&optional run-together)
  (cond ((string-match  "aspell$" ispell-program-name)
		 (append (list "--sug-mode=ultra" "--lang=en_US") (if run-together '("--run-together" "--run-together-limit=5" "--run-together-min=2"))))
		((string-match "hunspell$" ispell-program-name)
		 "-d en_US")))

(use-package flyspell-correct-popup
  :bind ("M-s" . ispell-word)
  :hook ((text-mode . flyspell-mode)
         (prog-mode . flyspell-prog-mode)
         ((flyspell-mode flyspell-prog-mode) . flyspell-buffer))
  :config
  (cond
   ((executable-find "aspell")
	(setq ispell-program-name "aspell"))
   ((executable-find "hunspell")
	(setq ispell-program-name "hunspell")
	(setq ispell-local-dictionary "en_US")
	(setq ispell-local-dictionary-alist '(("en_US" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil ("-d" "en_US") nil utf-8))))
   (t
	(setq ispell-program-name nil)))
  (setq-default ispell-extra-args (flyspell-detect-ispell-args t))
  (setq-default flyspell-issue-message-flag nil))

;; -------- REST Client ---------
(use-package restclient
  :commands (restclient-copy-curl-command
             restclient-http-send-current
             restclient-http-send-current-raw
             restclient-http-send-current-stay-in-window
             restclient-jump-next
             restclient-jump-prev
             restclient-mark-current
             restclient-mode
             restclient-narrow-to-current
             restclient-outline-mode
             restclient-toggle-body-visibility
             restclient-toggle-body-visibility-or-indent))

;; ------- Highlight TODO -------
(use-package hl-todo
  :commands (hl-todo-mode
             hl-todo-next
             hl-todo-occur
             hl-todo-previous)
  :hook (prog-mode . hl-todo-mode))

;; ---- Printer Integration -----
(use-package printing
  :bind ("M-p" . print-buffer)
  :commands (print-buffer
             print-region
             lpr-buffer
             lpr-customize
             lpr-region)
  :config
  (pr-update-menus t))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
<<<<<<< HEAD
    (company xclip hl-todo comment-tags restclient markdown-mode autopair highlight-parentheses flyspell-correct-popup rust-mode rust-playground slime-volleyball use-package multi-web-mode magit ensime color-theme base16-theme))))
=======
    (auto-complete auto-complete-clang-async xclip hl-todo comment-tags restclient markdown-mode autopair highlight-parentheses flyspell-correct-popup rust-mode rust-playground slime-volleyball use-package multi-web-mode magit ensime color-theme base16-theme))))
>>>>>>> 7ce8d5af1ffc98a8f6ac335c67e48bbbd53e8f56
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

