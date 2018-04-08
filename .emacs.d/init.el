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
              backup-directory-alist `(("." . backup-directory))
              delete-old-versions t)

;; (global-linum-mode)

;; Delete selected text when typing (normal editor behavior)
(delete-selection-mode t)

;; ------------- Keybindings -------------
(global-set-key (kbd "C-c /") 'comment-or-uncomment-region)
(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C-c C-k") 'compile)
(global-set-key (kbd "M-p") 'print-buffer)

;; ----------- Package Managing -----------
;; The package manager
(require 'package)

;; Add package sources
(setq
 package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                    ("org" . "https://orgmode.org/elpa/")
                    ("melpa" . "https://melpa.org/packages/")
                    ("melpa-stable" . "https://stable.melpa.org/packages/"))
 package-archive-priorities '(("melpa-stable" . 1)))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure t)

;; ---------- Use X11 clipboard -----------
(use-package xclip
  :config
  (dolist (hook '(text-mode-hook prog-mode-hook)) (add-hook hook (lambda () (xclip-mode)))))

;; --------- Parentheses Matching ---------
(use-package highlight-parentheses
  :config
  (define-globalized-minor-mode global-highlight-parentheses-mode
	highlight-parentheses-mode
	(lambda ()
	  (highlight-parentheses-mode t)))
  (global-highlight-parentheses-mode t)
  )

(use-package autopair
  :config
  (autopair-global-mode))

;; ----------- Ensime -----------
;; Java/Scala featues.  Includes:
;; * Inferred types
;; * Autocomplete
;; * Syntax highlighting
;; * Jump to source/docs
;; * Refactoring
;; * Error detection
(use-package ensime
  :pin melpa-stable)

; --------- C Syntax checker ---------
(use-package flycheck-irony)

;; xTerm mouse support
;; Disable because it became annoying, sounds cool though.
;;
;;(require 'mouse)
;;(xterm-mouse-mode t)

;; ---------- Color Themes ----------
(use-package color-theme
  ;; TODO: Fixes error about missing directory.  Don't know why.
  :init
  (unless (file-exists-p "~/.emacs.d/elpa/color-theme-20070910.1007/themes")
    (make-directory "~/.emacs.d/elpa/color-theme-20070910.1007/themes"))
  :config
  (color-theme-initialize))

(use-package base16-theme
  :requires color-theme
  :config (load-theme 'base16-tomorrow-night t))

;; ------------ Web Mode ------------
(use-package multi-web-mode
  :config
  (setq mweb-default-major-mode 'html-mode)
  (setq mweb-tags 
		'((php-mode "<\\?php\\|<\\? \\|<\\?=" "\\?>")
		  (js-mode  "<script[^>]*>" "</script>")
		  (css-mode "<style[^>]*>" "</style>")))
  (setq mweb-filename-extensions '("php" "htm" "html" "ctp" "phtml" "php4" "php5"))
  (multi-web-global-mode 1))

;; ------------ Git Mode ------------
(use-package magit)

;; ----------- Rust Mode ------------
(use-package rust-mode)
(use-package rust-playground
  :requires rust-mode)
(use-package cargo
  :requires rust-mode)
(use-package flycheck-rust
  :requires rust-mode)

;; ----------- i3 Support ----------
;(if (equal (getenv "DESKTOP_SESSION") "i3")
;    (use-package i3wm))

;; --------- Racket Mode ----------
(use-package racket-mode)

;; --------- Scala Mode -----------
(use-package scala-mode)

;; ---------- C# Mode -------------
(use-package csharp-mode)

;; ------- Markdown Mode ----------
(use-package markdown-mode)

;; ---- StackOverflow Client ------
;(use-package sx)

;; -------- Spellcheck ------------
(defun flyspell-detect-ispell-args (&optional run-together)
  (cond ((string-match  "aspell$" ispell-program-name)
		 (append (list "--sug-mode=ultra" "--lang=en_US") (if run-together '("--run-together" "--run-together-limit=5" "--run-together-min=2"))))
		((string-match "hunspell$" ispell-program-name)
		 "-d en_US")))

(use-package flyspell-correct-popup
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
  (global-set-key (kbd "M-s") 'ispell-word)
  (setq-default flyspell-issue-message-flag nil)
  (dolist (hook '(text-mode-hook magit-mode-hook)) (add-hook hook (lambda () (flyspell-mode))))
  (add-hook 'prog-mode-hook (lambda () (flyspell-prog-mode))))

;; -------- REST Client ---------
(use-package restclient)

;; ------- Highlight TODO -------
(use-package hl-todo
  :config
  (add-hook 'prog-mode-hook (lambda () (hl-todo-mode))))

;; ---- Printer Integration -----
(use-package printing
  :config
  (pr-update-menus t))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (xclip hl-todo comment-tags restclient markdown-mode autopair highlight-parentheses flyspell-correct-popup rust-mode rust-playground slime-volleyball use-package multi-web-mode magit ensime color-theme base16-theme))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

