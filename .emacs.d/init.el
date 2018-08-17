;; Requires:
;; aspell
;; Optional:
;; clang

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
              ;x-select-enable-clipboard t
              ;interprogram-paste-function 'x-cut-buffer-or-selection-value
              backup-directory-alist `(("." . ,backup-directory))
              delete-old-versions t)

;; (global-linum-mode)

;; Delete selected text when typing (normal editor behavior)
(delete-selection-mode t)

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
(global-set-key (kbd "M-<left>") 'backward-list)
(global-set-key (kbd "M-<right>") 'forward-list)
;; TODO: See if can get working without smartparen lib.
;; Otherwise, switch to smartparen
;;(global-set-key (kbd "M-<up>") 'sp-up-sexp)
;;(global-set-key (kbd "M-<down>") 'sp-down-sexp)
(global-set-key (kbd "M-<delete>") 'kill-sexp)

;; ----------- Package Managing -----------
;; The package manager
(require 'package)

;; Add package sources
(setq package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")
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

;; ---------- Color Themes ----------
;; TODO: Fix warning underline to always be orange/yellow
(use-package color-theme
  :init
  ;; TODO: Fixes error about missing directory.  Don't know why.
  ;(unless (file-exists-p "~/.emacs.d/elpa/color-theme-20070910.1007/themes")
  ;  (make-directory "~/.emacs.d/elpa/color-theme-20070910.1007/themes"))
  :config
  (color-theme-initialize)
  (load-theme 'Thomas-Experiement t))

;; ---------- Use X11 clipboard -----------
;; TODO: Bug.  Copy to xclip doesn't work.  Paste from does.
(use-package xclip
  :if (executable-find "xclip")
  ;;:config (xclip-mode 1)
  )

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
  ;; TODO: Disabled.  Forgot why (Maybe redundant?).  Figure out later.
  :disabled
  :commands (scala-mode))

(use-package company)

(when (version<= "24.4" emacs-version)
  (use-package ensime
    :requires company
    :hook (scala-mode java-mode)
    :pin melpa-stable
    :config
    (setq ensime-startup-notification nil)
    (eval-after-load 'ensime-mode
      '(define-key ensime-mode-map (kbd "C-c i")
         (lambda () "Generate ensime.sbt file"
           (interactive)
           (write-region "ensimeScalaVersion in ThisBuild := \"2.11.8\""
                                  nil (concat (read-directory-name "SBT Root:") "ensime.sbt")))))))

;; --------- C Syntax checker ---------
;; TODO: C autocomplete.  Both with clang integration and backup naive method
;; (use-package irony
;;   ;;:hook (c-mode c++-mode objc-mode)
;;   :init
;;   (add-hook 'c++-mode-hook 'irony-mode)
;;   (add-hook 'c-mode-hook 'irony-mode)
;;   (add-hook 'objc-mode-hook 'irony-mode)
;;   :commands (irony-mode irony-version)
;;   :config
;;   (defun my-irony-mode-hook ()
;;     (define-key irony-mode-map [remap completion-at-point]
;;       'irony-completion-at-point-async)
;;     (define-key irony-mode-map [remap complete-symbol]
;;       'irony-completion-at-point-async))
;;   (add-hook 'irony-mode-hook 'my-irony-mode-hook)
;;   (add-hook 'irony-mode-hook irony-cdb-autosetup-compile-options)
;;   )

;; (use-package company
;;   :init (add-hook 'after-init-hook 'global-company-mode)
;;   :config
;;   (setq company-idle-delay              nil
;;         company-minimum-prefix-length   2
;;         company-show-numbers            t
;;         company-tooltip-limit           20
;;         company-dabbrev-downcase        nil
;;         company-backends                '((company-irony company-gtags))
;;         )
;;   :bind ("C-;" . company-complete-common)
;;   )

;; (use-package company-irony
;;   :requires company)

;; (use-package flycheck-irony
;;   :hook c-mode)
;; ;  :mode ("\\.c\\'" "\\.h\\'")

;; ------------ Web Mode ------------
;; TODO: Automatic closing tab would be nice
(defun my-sgml-insert-gt ()
  "Inserts a `>' character and calls 
`my-sgml-close-tag-if-necessary', leaving point where it is."
  (interactive)
  (insert ">")
  (save-excursion (my-sgml-close-tag-if-necessary)))

(defun my-sgml-close-tag-if-necessary ()
  "Calls sgml-close-tag if the tag immediately before point is
an opening tag that is not followed by a matching closing tag."
  (when (looking-back "<\\s-*\\([^</> \t\r\n]+\\)[^</>]*>")
    (let ((tag (match-string 1)))
      (unless (and (not (sgml-unclosed-tag-p tag))
           (looking-at (concat "\\s-*<\\s-*/\\s-*" tag "\\s-*>")))
        (sgml-close-tag)))))

(use-package multi-web-mode
  :init
  (setq mweb-default-major-mode 'html-mode
        mweb-tags '((php-mode "<\\?php\\|<\\? \\|<\\?=" "\\?>")
                    (js-mode  "<script[^>]*>" "</script>")
                    (css-mode "<style[^>]*>" "</style>"))
        mweb-filename-extensions '("php" "htm" "html" "ctp" "phtml" "php4" "php5"))
  :config
  (multi-web-global-mode 1)
  (eval-after-load "sgml-mode"
    '(define-key sgml-mode-map ">" 'my-sgml-insert-gt)))

;; ------------ Git Mode ------------
;; TODO: make resolving merge conflicts hotkeys not use "^"
(when (version<= "24.4" emacs-version)
  (use-package magit
    :bind ("C-x g" . magit-status)))

;; ----------- Rust Mode ------------
(use-package rust-mode)
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
  :disabled ;; TODO: Disabled because compiler warnings with newest version
  :if (version<= "24.4" emacs-version))

;; ------- Markdown Mode ----------
(when (version<= "24.4" emacs-version)
  (use-package markdown-mode))

;; ---- StackOverflow Client ------
(use-package sx
  ;; TODO: I don't even know if this works.  Doesn't seem well maintained
  :disabled)

;; -------- Spellcheck ------------
(defun flyspell-detect-ispell-args (&optional run-together)
  (cond ((string-match  "aspell$" ispell-program-name)
		 (append (list "--sug-mode=ultra" "--lang=en_US")
                 (if run-together '("--run-together" "--run-together-limit=5" "--run-together-min=2"))))
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
	(setq ispell-local-dictionary-alist
          '(("en_US" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil ("-d" "en_US") nil utf-8))))
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
 '(custom-safe-themes
   (quote
    ("102c7a106e03ec19e9a31562bc611632fbb4b7f1ca09aca0d2da77e57cb510b0" "eef1aa0f203162ff23ca375cae72922bddff2451d979d9370e79b4357000529d" default))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
