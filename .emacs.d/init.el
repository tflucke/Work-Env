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
