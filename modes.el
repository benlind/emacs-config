;;;; MODES ;;;;
;; This file sets up preferences for individual major and minor modes.


;;; YAML

(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.ya?ml\\'" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.conf\\'"  . yaml-mode))

;; Use yaml-fill-paragraph instead of fill-paragraph
(add-hook 'yaml-mode-hook
          (lambda () (local-set-key (kbd "M-q") #'yaml-fill-paragraph)))


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
      cperl-invalid-face (quote off)
      cperl-merge-trailing-else nil)
(add-hook 'cperl-mode-hook   ; prevent extra right-brace on left-brace, like: {}}
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

;; Don't wrap horizontal rules (any sequence of repeating non-alphanumeric
;; characters that are on their own line). Example:
;; ----------
;; You should be able to `fill-paragraph` these two comment blocks separately
;; without having them merge.
;;
;; See http://emacs.stackexchange.com/a/31609/12866
;;
;; I adapted the answer above to allow for not wrapping over rules within
;; comments. This is kind of hacky, but it works for most situations. Also,
;; writing emacs regex in strings is ANNOYING because you have to double-escape
;; all escaped characters (http://stackoverflow.com/q/538842/1054633).
;;
;; The situation where my rule-in-comment hack doesn't work is if you are
;; wrapping comments that are placed after code, like so:
;;
;;     my $x = "hot";          # This comment, if wrapped...
;;     my $y = "diggity";      # ---
;;     my $z = "dog";          # ...will be run together with the rule.
(setq paragraph-start "^\s*[^[:alnum:]]\\{0,2\\}\s*\\([^[:alnum:]]\\)\\1+\n\\|\f\\|[   ]*$")
(setq paragraph-separate "^\s*[^[:alnum:]]\\{0,2\\}\s*\\([^[:alnum:]]\\)\\1+\n\\|[  \f]*$")


;;; LINE NUMBERS

;; Show line numbers. Note, this can make scrolling and line navigation slow,
;; even with the below linum-delay setting.
;; (global-linum-mode 1)
;; (setq linum-delay t)   ; help prevent linum from slowing things down

;; ;; Right-align line numbers and add single space padding to right
;; (defadvice linum-update-window (around linum-dynamic activate)
;;   (let* ((w (length (number-to-string
;;                      (count-lines (point-min) (point-max)))))
;;          (linum-format (concat "%" (number-to-string w) "d ")))
;;     ad-do-it))


;;; FILL COLUMN

;; Set fill column to 80 chars (for M-q and fci-mode)
(setq-default fill-column 80)

;; ;; Enable fill column mode
;; (require 'fill-column-indicator)
;; (add-hook 'c-mode-hook 'fci-mode) ; turn on fci-mode for C files


;;; AUTO-COMPLETION

;; (require 'auto-complete)
;; (global-auto-complete-mode t)
;; (ac-linum-workaround) ; stop flickering line numbers for auto-complete dropdown


;;; IDO (Interactively Do Things)

;; Use vertical auto-complete list for M-x and similar
(require 'ido-vertical-mode)
(ido-mode 1)
(ido-everywhere 1)
(ido-vertical-mode 1)
(setq ido-vertical-define-keys 'C-n-C-p-up-and-down)  ; use C-n and C-p to cycle

;; IDO ubiquitous: use IDO everywhere
;; (require 'ido-ubiquitous)
;; (ido-ubiquitous-mode 1)


;;; AUTO-PAIRING

;; Electric pairing

;; XXX: I can't figure out how to ONLY enable electir-pair-mode in specific
;; major modes.
(electric-pair-mode)

;; Don't pair single and double quotes
(setq electric-pair-inhibit-predicate
      (lambda (c)
        (if (char-equal c ?\") t (electric-pair-default-inhibit c))))

;; Don't overwrite a matching char even if electric-pair thinks adding another
;; will unbalance things. E.g., if I have {{ "|" }} where | is the cursor, and I
;; type a quote, I want it to end up as {{ """ }}}. This is to fix the problem
;; where electric-pair overwrites a matching quote even if there's whitespace
;; before it (like when you type a quote at the end of a JSON line).
(setq electric-pair-skip-self nil)

;; ;; Autopair

;; ;; Auto-close braces and quotes, and auto indent on RET inside braces
;; (require 'autopair)
;; (autopair-global-mode)  ; enable autopair in all buffers
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

;; Make PHP use single-line comments everywhere
(setq-default web-mode-comment-formats (remove '("php" . "/*") web-mode-comment-formats))
(add-to-list 'web-mode-comment-formats '("php" . "//"))

(defun my-web-mode-hook ()
  "Hooks for Web mode."
  ;; (set-face-attribute 'web-mode-html-tag-bracket-face nil :foreground "#555")
  (setq web-mode-enable-auto-closing              t) ; this could break yanking html
  (setq web-mode-enable-auto-pairing              t)
  (setq web-mode-code-indent-offset               2)
  (setq web-mode-css-indent-offset                2)
  (setq web-mode-markup-indent-offset             2)
  (setq web-mode-script-padding                   2) ; indenting inline scripts (default 1)
  (setq web-mode-style-padding                    2) ; inline styles
  (setq web-mode-block-padding                    2) ; inline server code like PHP
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

(require 'yasnippet) ; WARNING: This seems to slow down load time significantly
(setq yas-snippet-dirs
      '("~/.emacs.d/snippets"))
(yas-global-mode 1)
(setq yas/indent-line nil) ; don't auto-indent snippet code


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


;;; PROJECTILE

(projectile-mode +1)

;; Cache ALL the FILES \(°□°)/
;;
;; Running `C-u C-c p f' will invalidate the cache prior to prompting you for a
;; file to jump to.
;; (setq projectile-enable-caching t)


;;; HELM

(require 'helm)
(require 'helm-config)
(require 'helm-projectile)
(helm-projectile-on)
(require 'tramp)  ;; get rid of 'void tramp-methods' error


;;; IBUFFER

;; ibuffer makes it easy to mark a bunch of buffers (t), unmark some (u) and
;; delete the marked ones (D).

;; Ensure ibuffer opens with point at the current buffer's entry.
;; (https://stackoverflow.com/a/3419686/1054633)
(defadvice ibuffer
  (around ibuffer-point-to-most-recent) ()
  "Open ibuffer with cursor pointed to most recent buffer name."
  (let ((recent-buffer-name (buffer-name)))
    ad-do-it
    (ibuffer-jump-to-buffer recent-buffer-name)))
(ad-activate 'ibuffer)


;;; GIT GUTTER

(global-git-gutter+-mode)
;; (setq git-gutter+-window-width 2)


;;; FLYMD
;; This is for previewing markdown files with auto-reload. Run flymd-flyit in a
;; markdown file and it will open Firefox with the rendered markdown.

(defun my-flymd-browser-function (url)
  (let ((process-environment (browse-url-process-environment)))
    (apply 'start-process
           (concat "firefox " url)
           nil
           "/usr/bin/open"
           (list "-a" "firefox" url))))
(setq flymd-browser-open-function 'my-flymd-browser-function)


;;; DIRED
;; Note, to show file details in dired+, hit left parenthesis: (
;; (https://stackoverflow.com/a/22921872/1054633)

;; Tell Dired+ to reuse buffers
(diredp-toggle-find-file-reuse-dir 1)

;; Auto-revert dired buffers when files change
(add-hook 'dired-mode-hook 'auto-revert-mode)

;; Auto-guess the desired target directory when copying files. If you have two
;; dired buffers open side-by-side and copy from one, dired will autofill the
;; path of the other dired buffer.
(setq dired-dwim-target t)

;; Always delete dired directory buffers when the directory has been deleted
(define-advice dired-clean-up-after-deletion
    (:around (old-fun &rest r) kill-dired-buffer-quietly)
  (define-advice y-or-n-p (:around (old-fun prompt) just-yes)
    (if (string-prefix-p "Kill Dired buffer" prompt)
        t
      (funcall old-fun prompt)))
  (unwind-protect (apply old-fun r)
    (advice-remove 'y-or-n-p #'y-or-n-p@just-yes)))


;;; ORG MODE

;; Wrap lines on word boundaries
(add-hook 'org-mode-hook #'toggle-word-wrap)


;;; MISC

;; Show column number in status bar
(setq column-number-mode t)

;; Disable vc-git to improve startup time (unnecessary if using emacs daemon)
;; (setq vc-handled-backends ())

;; Turn off top menu
(menu-bar-mode 0)

;; Make Delete key delete selection; also, typing over selection replaces it
(delete-selection-mode 1)

;; Highlight the current line when the buffer moves
;; (beacon-mode)

;; Auto-revert unmodified files if they change on disk
(global-auto-revert-mode t)

;; Load Jenkinsfiles in Groovy mode
(add-to-list 'auto-mode-alist '("Jenkinsfile\\'" . groovy-mode))

;; Run gofmt on save. You will have to run this command before goimports works:
;;   go get golang.org/x/tools/cmd/goimports
(setq gofmt-command "goimports")
(add-hook 'before-save-hook 'gofmt-before-save)

;; Auto-focus the *Occur* buffer when it is opened. This is used for toc().
(add-hook 'occur-hook
          '(lambda ()
             (switch-to-buffer-other-window "*Occur*")))
