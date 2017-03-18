;;;; MODES ;;;;
;; This file sets up preferences for individual major and minor modes.


;;; YAML

(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.ya?ml\\'" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.conf\\'"  . yaml-mode))


;;; PERL

;; Highlight Perl test files
(add-to-list 'auto-mode-alist '("\\.t\\'" . perl-mode))

;; Use CPerlMode (improves indentation)
(fset 'perl-mode 'cperl-mode)
(setq cperl-indent-level 4
      cperl-close-paren-offset -4
      cperl-continued-statement-offset 4
      cperl-indent-parens-as-block t
      cperl-tab-always-indent t
      cperl-invalid-face (quote off))
(add-hook 'cperl-mode-hook   ;; prevent extra right-brace on left-brace, like: {}}
          (lambda () (local-unset-key (kbd "{"))))


;;; SMEX (M-x fuzzy completion)

(autoload 'smex "smex"
  "Smex is a M-x enhancement for Emacs, it provides a
   convenient interface to your recently and most frequently
   used commands.")


;;; AUTOFILL

;; Only wrap lines in comments
(defun comment-auto-fill ()
  (setq-local comment-auto-fill-only-comments t)
  (auto-fill-mode 1))

(add-hook 'c-mode-common-hook 'comment-auto-fill)
(add-hook 'cperl-mode-hook 'comment-auto-fill)
(add-hook 'lisp-mode-hook 'comment-auto-fill)

;; Autofill everything in text mode
(add-hook 'text-mode-hook 'turn-on-auto-fill)


;;; LINE NUMBERS

;; Show line numbers
(global-linum-mode 1)

;; Right-align line numbers and add single space padding to right
(defadvice linum-update-window (around linum-dynamic activate)
  (let* ((w (length (number-to-string
                     (count-lines (point-min) (point-max)))))
         (linum-format (concat "%" (number-to-string w) "d ")))
    ad-do-it))


;;; FILL COLUMN

;; Set fill column to 80 chars (for M-q and fci-mode)
(setq-default fill-column 80)

;; ;; Enable fill column mode
;; (require 'fill-column-indicator)
;; (add-hook 'c-mode-hook 'fci-mode) ;; turn on fci-mode for C files


;;; AUTO-COMPLETION

;; auto complete mode
(require 'auto-complete)
(global-auto-complete-mode t)
(ac-linum-workaround) ;; Stop flickering line numbers for auto-complete dropdown


;;; IDO (Interactively Do Things)

;; Use vertical auto-complete list for M-x and similar
(require 'ido-vertical-mode)
(ido-mode 1)
(ido-everywhere 1)
(ido-vertical-mode 1)
(setq ido-vertical-define-keys 'C-n-C-p-up-and-down)  ;; use C-n and C-p to cycle

;; IDO ubiquitous: use IDO everywhere
(require 'ido-ubiquitous)
(ido-ubiquitous-mode 1)


;;; AUTO-PAIRING

;; ;; Electric pairing
;; (electric-pair-mode)

;; ;; Autopair

;; ;; Auto-close braces and quotes, and auto indent on RET inside braces
;; (require 'autopair)
;; (autopair-global-mode)  ;; enable autopair in all buffers
;; (setq autopair-blink nil)

;; ;; Disable autopair in minibuffer
;; (defun my-minibuffer-setup ()
;;   (autopair-mode -1))
;; (add-hook 'minibuffer-setup-hook 'my-minibuffer-setup)
;; ;; (defvar autopair-dont-pair `(:string (?\") :comment (?{))) ; ?\" doesn't seem to translate to double quote


;;; WHICH FUNCTION

;; Show the current function name in the header line
(which-function-mode)
(setq-default header-line-format
              '((which-func-mode ("" which-func-format " "))))
(setq mode-line-misc-info
      ;; We remove Which Function Mode from the mode line, because it's mostly
      ;; invisible here anyway.
      (assq-delete-all 'which-func-mode mode-line-misc-info))
(setq which-func-unknown "")


;;; WEB MODE

;; Enable web mode in certain files
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.html\\'"      . web-mode))
(add-to-list 'auto-mode-alist '("\\.php\\'"       . web-mode))
(add-to-list 'auto-mode-alist '("\\.phtml\\'"     . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[agj]sp\\'"   . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'"   . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'"       . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'"  . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'"    . web-mode))
(add-to-list 'auto-mode-alist '("\\.tt\\'"        . web-mode))

(defun my-web-mode-hook ()
  "Hooks for Web mode."
  ;; (set-face-attribute 'web-mode-html-tag-bracket-face nil :foreground "#555")
  (setq web-mode-enable-auto-closing              t) ; this could break yanking html
  (setq web-mode-enable-auto-pairing              t)
  (setq web-mode-code-indent-offset               4)
  (setq web-mode-css-indent-offset                4)
  (setq web-mode-markup-indent-offset             4)
  (setq web-mode-enable-css-colorization          t)
  (setq web-mode-enable-current-element-highlight t)
  )
(add-hook 'web-mode-hook 'my-web-mode-hook)


;;; MOUSE SUPPORT

(unless window-system
  (require 'mouse)
  (xterm-mouse-mode t)
  (global-set-key [mouse-4] (lambda () (interactive) (scroll-down 1)))
  (global-set-key [mouse-5] (lambda () (interactive) (scroll-up 1)))
  (defun track-mouse (e))
  (setq mouse-sel-mode t)
  (setq mouse-wheel-progressive-speed nil)
  ;; Delay updates to give Emacs a chance for other changes
  (setq linum-delay t)
  ;; Scroll one line at a time:
  (setq mouse-wheel-scroll-amount '(1 ((shift) . 1) ((control) . nil))))


;;; SNIPPETS

;; (require 'yasnippet) ;; WARNING: This seems to slow down load time significantly
;; (setq yas-snippet-dirs
;;       '("~/.emacs.d/snippets"))
;; (yas-global-mode 1)


;;; MARKDOWN

(autoload 'markdown-mode "markdown-mode"
  "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))


;;; SPELL CHECKING

;; Check spelling in strings and comments, or everywhere (if in text mode)
;;
;; Note: to add words to the dictionary, execute `M-x ispell-region` and press
;; `i` when over the word you want to add. Press `x` to exit. Or use `M-x
;; ispell-word` so you don't have to select a region first.
(add-hook 'text-mode-hook 'flyspell-mode)
(add-hook 'prog-mode-hook 'flyspell-prog-mode)


;;; MISC

;; Show column number in status bar
(setq column-number-mode t)

;; Disable vc-git to improve startup time
(setq vc-handled-backends ())

;; Turn off top menu
(menu-bar-mode 0)

;; Make Delete key delete selection; also, typing over selection replaces it
(delete-selection-mode 1)

;; Multiple cursors (see keybindings.el)
(require 'multiple-cursors)
