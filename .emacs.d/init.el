;; ----------- Default Variables -----------
;; Global variables
(setq
 inhibit-startup-screen t
 column-number-mode t
 scroll-error-top-bottom t
 show-paren-delay 0.25)

;; Tab Config
(setq tab-width 2)
(setq-default indent-tabs-mode t)

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

;; ----------- Ensime -----------
;; Java/Scala featues.  Includes:
;; * Inferred types
;; * Autocomplete
;; * Syntax highlighting
;; * Jump to source/docs
;; * Refactoring
;; * Error detection
(use-package ensime
  :ensure t
  :pin melpa-stable)

;; xTerm mouse support
;; Disable because it became annoying, sounds cool though.
;;
;(require 'mouse)
;(xterm-mouse-mode t)

;; ---------- Color Themes ----------
(use-package color-theme
  :ensure t
  :config
  (color-theme-initialize)
  (use-package base16-theme
    :ensure t
    :config (load-theme 'base16-tomorrow-night t))
  )
