;;;; INDENTATION ;;;;
;; This file sets up indentation preferences


;;; TAB WIDTH

;; Indent with 4 spaces
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(setq c-basic-offset 2)
(setq tab-width 2)
(setq js-indent-level 2)
(setq css-indent-offset 2)

(setq indent-line-function 'insert-tab)


;;; AUTO-INDENTATION

;; (require 'auto-indent-mode)
;; (auto-indent-global-mode)
;; (setq auto-indent-newline-function 'newline-and-indent) ;; don't indent prev line on RET
;; ;; The following variable defines what <delete> does at the beginning of a line.
;; ;; I would like it to just remove the previous newline, but currently it removes
;; ;; all newlines. I created an issue for this here:
;; ;; https://github.com/mattfidler/auto-indent-mode.el/issues/47
;; (setq auto-indent-backward-delete-char-behavior 'all)


;;; MISC

;; Automatically match indentation style in existing files
(autoload 'dtrt-indent-mode "dtrt-indent" "Adapt to foreign indentation offsets" t)
(add-hook 'c-mode-common-hook 'dtrt-indent-mode)

;; ;; Enable smart tabs
;; (autoload 'smart-tabs-mode "smart-tabs-mode"
;;   "Intelligently indent with tabs, align with spaces!")
;; (autoload 'smart-tabs-mode-enable "smart-tabs-mode")
;; (autoload 'smart-tabs-advice "smart-tabs-mode")
;; (autoload 'smart-tabs-insinuate "smart-tabs-mode")
;; (smart-tabs-insinuate 'c 'c++ 'java 'javascript 'cperl 'python
;;                       'ruby 'nxml)

;; Don't auto-indent the previous line on RET
(setq-default electric-indent-inhibit t)
