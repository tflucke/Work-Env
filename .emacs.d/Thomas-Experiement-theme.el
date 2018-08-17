(deftheme Thomas-Experiement
  "Created 2018-08-17.")

(custom-theme-set-faces
 'Thomas-Experiement
 '(cursor ((t (:background "red"))))
 '(fixed-pitch ((t (:family "Monospace"))))
 '(variable-pitch ((((type w32)) (:foundry "outline" :family "Arial")) (t (:family "Sans Serif"))))
 '(escape-glyph ((((background dark)) (:foreground "cyan")) (((type pc)) (:foreground "magenta")) (t (:foreground "brown"))))
 '(homoglyph ((((background dark)) (:foreground "cyan")) (((type pc)) (:foreground "magenta")) (t (:foreground "brown"))))
 '(minibuffer-prompt ((t (:foreground "blue"))))
 '(highlight ((t (:background "color-88"))))
 '(region ((t (:background "brightblue"))))
 '(shadow ((t (:foreground "brightblack"))))
 '(secondary-selection ((t (:background "brightblack"))))
 '(trailing-whitespace ((t (:foreground "yellow" :background "cyan"))))
 '(font-lock-builtin-face ((t (:foreground "cyan"))))
 '(font-lock-comment-delimiter-face ((t (:foreground "brightyellow"))))
 '(font-lock-comment-face ((t (:foreground "brightblack"))))
 '(font-lock-constant-face ((t (:foreground "color-28"))))
 '(font-lock-doc-face ((t (:foreground "brightblue"))))
 '(font-lock-function-name-face ((t (:foreground "color-112"))))
 '(font-lock-keyword-face ((t (:foreground "color-202"))))
 '(font-lock-negation-char-face ((t (:foreground "green"))))
 '(font-lock-preprocessor-face ((t (:foreground "color-163" :weight semi-bold))))
 '(font-lock-regexp-grouping-backslash ((t (:foreground "yellow"))))
 '(font-lock-regexp-grouping-construct ((t (:foreground "magenta"))))
 '(font-lock-string-face ((t (:foreground "color-184"))))
 '(font-lock-type-face ((t (:foreground "color-93"))))
 '(font-lock-variable-name-face ((t (:foreground "color-41"))))
 '(font-lock-warning-face ((t (:underline (:color "color-130" :style wave)))))
 '(button ((t (:inherit (link)))))
 '(link ((t (:underline (:color foreground-color :style line) :foreground "blue"))))
 '(link-visited ((t (:underline (:color foreground-color :style line) :foreground "magenta"))))
 '(fringe ((t (:background "brightgreen"))))
 '(header-line ((t (:foreground "magenta" :inherit (mode-line)))))
 '(tooltip ((t (:foreground "black" :background "lightyellow" :inherit (variable-pitch)))))
 '(mode-line ((t (:box nil :foreground "brightblue" :background "brightyellow"))))
 '(mode-line-buffer-id ((t (:foreground "green"))))
 '(mode-line-emphasis ((t (:slant italic :foreground "brightmagenta"))))
 '(mode-line-highlight ((t (:weight bold :box nil :foreground "magenta"))))
 '(mode-line-inactive ((t (:box nil :foreground "brightblack" :background "brightgreen"))))
 '(isearch ((t (:inverse-video t :foreground "yellow" :background "brightgreen"))))
 '(isearch-fail ((t (:inverse-video t :background "brightgreen" :inherit (font-lock-warning-face)))))
 '(lazy-highlight ((((class color) (min-colors 88) (background light)) (:background "paleturquoise")) (((class color) (min-colors 88) (background dark)) (:background "paleturquoise4")) (((class color) (min-colors 16)) (:background "turquoise3")) (((class color) (min-colors 8)) (:background "turquoise3")) (t (:underline (:color foreground-color :style line)))))
 '(match ((t (:inverse-video t :foreground "blue" :background "brightgreen"))))
 '(next-error ((t (:inherit (region)))))
 '(query-replace ((t (:inherit (isearch)))))
 '(ensime-errline-highlight ((t (:inherit nil :foreground "brightred" :underline (:color foreground-color :style wave) :slant italic :weight semi-bold))))
 '(ensime-warnline-highlight ((t (:inherit font-lock-warning-face))))
 '(scala-font-lock:var-face ((t (:inherit font-lock-variable-name-face))))
 '(show-paren-match ((t (:background "blue" :foreground "green"))))
 '(show-paren-match-expression ((t (:inherit (show-paren-match)))))
 '(show-paren-mismatch ((t (:background "red" :foreground "white" :underline (:color foreground-color :style wave)))))
 '(default ((t (:family "default" :foundry "default" :width normal :height 1 :weight normal :slant normal :underline nil :overline nil :strike-through nil :box nil :inverse-video nil :foreground "white" :background "black" :stipple nil :inherit nil))))
 '(c-annotation-face ((t (:inherit font-lock-preprocessor-face)))))

(provide-theme 'Thomas-Experiement)
