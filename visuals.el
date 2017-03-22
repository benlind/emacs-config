;;;; VISUALS ;;;;
;; This file sets up theme, colors, and other visual preferences.


;;; THEME

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")

(load-theme 'zenburn t)

;; ;; Solarized
;; (add-to-list 'custom-theme-load-path "~/.emacs.d/themes/emacs-color-theme-solarized/")
;; (load-theme 'solarized t)
;; (set-terminal-parameter nil 'background-mode 'dark) ; comment out for light solarized


;;; COLORS

;; Highlight the current line
(global-hl-line-mode 1)
(set-face-background 'hl-line "#333")
(set-face-foreground 'highlight nil)

;; Set the region highlight color
(set-face-attribute 'region nil :background "#4e4e4e")

;; Set the line number color
(set-face-foreground 'linum "#444")

;; Highlight matching pairs (parens, braces, etc.)
(show-paren-mode 1)
;; (set-face-foreground 'show-paren-match "#8da5b1")
;; (set-face-background 'show-paren-match "#203742")
;; (set-face-attribute 'show-paren-match nil :weight 'extra-bold)

;; ;; Enable lazy highlighting (maintain match highlights after exiting isearch)
;; (setq lazy-highlight-cleanup nil)
;; (setq lazy-highlight-initial-delay 0)
(set-face-attribute  'lazy-highlight nil :background "yellow")
(set-face-attribute  'lazy-highlight nil :foreground "#272727")

;; Set fill column indicator color
;; (setq fci-rule-character-color "#464646")


;;; MISC

;; On alarm (like pressing C-g), only flash top and bottom of screen
(setq visible-bell 'top-bottom)