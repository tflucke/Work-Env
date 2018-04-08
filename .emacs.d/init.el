;; ----------- Default Variables -----------
;; Global variables
(setq
 inhibit-startup-screen t
 column-number-mode t
 scroll-error-top-bottom t
 show-paren-delay 0.25)

;; Tab Config
(setq tab-width 4)

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
  ;; Investiigate: Fixes error about missing directory
  :init
  (unless (file-exists-p "~/.emacs.d/elpa/color-theme-20070910.1007/themes") (make-directory "~/.emacs.d/elpa/color-theme-20070910.1007/themes"))
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

;; ---- StackOverflow Client ------
;(use-package sx)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (rust-mode rust-playground slime-volleyball use-package multi-web-mode magit ensime color-theme base16-theme))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

