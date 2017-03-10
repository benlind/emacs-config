;;;;; TODO (some things I'd like to get emacs to do at some point)
;; Note: some of these are from Prelude
;; - C-c s: swap active buffers
;; - C-x \: align regexp
;; - C-c I: open init file
;; - Use a smarter auto-complete (that doesn't just make predictions based on
;;   what's in the currently opened buffers; look into Helm)
;;
;; Look at these example init files for more ideas:
;; https://github.com/bbatsov/emacs.d/blob/master/init.el


;;; PACKAGE MANAGERS
;; Load MELPA for loading packages
;; (http://ergoemacs.org/emacs/emacs_package_system.html)
(when (>= emacs-major-version 24)
  (require 'package)
  (package-initialize)
  (setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                           ("marmalade" . "https://marmalade-repo.org/packages/")
                           ("melpa" . "https://melpa.org/packages/"))))

;; Set elisp directories
(add-to-list 'load-path "~/.emacs.d/lisp/")
(add-to-list 'load-path "~/.emacs.d/lisp/multiple-cursors")

;; Load modules (these correspond to filenames in the lisp directory)
(load "web-mode")
(load "auto-indent-mode")
(load "smex")
(load "popup")              ;; for auto-complete
(load "auto-complete")
(load "ido-vertical-mode")
(load "ido-ubiquitous")
(load "yasnippet")
(load "multiple-cursors")
(load "buffer-move")
;; (load "autopair")
(load "yaml-mode")
(load "markdown-mode")
(load "groovy-mode")
(load "dtrt-indent")
;; (load "smart-tabs-mode")



;;; THEMES

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/emacs-color-theme-solarized/")
;; (load-theme 'solarized t)
;; (set-terminal-parameter nil 'background-mode 'dark) ; comment out for light solarized
(load-theme 'zenburn t)


;;;; Customize theme colors

(global-hl-line-mode 1)       ;; Turn on highlighting current line
(set-face-background 'hl-line "#272727")  ;; set line highlight color
(set-face-foreground 'highlight nil)
(set-face-attribute 'region nil :background "#4e4e4e")  ;; region highlight color
(set-face-attribute 'lazy-highlight nil :background "yellow")
(set-face-attribute 'lazy-highlight nil :foreground "#272727")
(setq lazy-highlight-initial-delay 0)

;; Show matching parens automatically
(show-paren-mode 1)
;; (set-face-foreground 'show-paren-match "#8da5b1")
;; (set-face-background 'show-paren-match "#203742")
;; (set-face-attribute 'show-paren-match nil :weight 'extra-bold)



;;; INDENTATION

;; Set up indenting to 4 spaces
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq sgml-basic-offset 4)
(setq c-basic-offset 4)
(setq tab-width 4)

(setq indent-line-function 'insert-tab)

;; (require 'auto-indent-mode)
;; (auto-indent-global-mode)
;; (setq auto-indent-newline-function 'newline-and-indent) ;; don't indent prev line on RET
;; ;; The following variable defines what <delete> does at the beginning of a line.
;; ;; I would lik  e it to just remove the previous newline, but currently it removes
;; ;; all newlines. I created an issue for this here:
;; ;; https://github.com/mattfidler/auto-indent-mode.el/issues/47
;; (setq auto-indent-backward-delete-char-behavior 'all)

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

(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.ya?ml\\'" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.conf\\'"  . yaml-mode))
;; (add-hook 'yaml-mode-hook 'my-yaml-mode-hook)
;; (defun my-yaml-mode-hook ()
;;   (auto-indent-mode 0)
;;   (define-key yaml-mode-map "\C-m" 'newline-and-indent))

;; Indent the whole buffer
(defun iwb ()
  "indent whole buffer"
  (interactive)
  (delete-trailing-whitespace)
  (indent-region (point-min) (point-max) nil)
  (untabify (point-min) (point-max)))



;;; BACKUP FILES

;; Put autosave files (ie #foo#) and backup files (ie foo~) in ~/.emacs.d/.
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-save-file-name-transforms (quote ((".*" "~/.emacs.d/autosaves/\\1" t))))
 '(backup-directory-alist (quote ((".*" . "~/.emacs.d/backups/"))))
 '(custom-safe-themes (quote ("06b2849748590f7f991bf0aaaea96611bb3a6982cad8b1e3fc707055b96d64ca" default)))
 '(text-mode-hook (quote (text-mode-hook-identify))))

(setq create-lockfiles nil)

;; Create the autosave dir if necessary, since emacs won't.
(make-directory "~/.emacs.d/autosaves/" t)
(make-directory "~/.emacs.d/backups/" t)



;;; MISC

;; Syntax highlighting for different filetypes
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

;; Only wrap lines in comments
(defun comment-auto-fill ()
  (setq-local comment-auto-fill-only-comments t)
  (auto-fill-mode 1))
(add-hook 'c-mode-common-hook 'comment-auto-fill)
(add-hook 'cperl-mode-hook 'comment-auto-fill)
(add-hook 'lisp-mode-hook 'comment-auto-fill)

;; Wrap everything in text mode
(add-hook 'text-mode-hook 'turn-on-auto-fill)

;; Set fill column to 80 chars (for M-q)
(setq-default fill-column 80)

;; Show column number
(setq column-number-mode t)

;; Line numbers
(global-linum-mode 1)
;; Right-align line numbers and add single space padding to right
(defadvice linum-update-window (around linum-dynamic activate)
  (let* ((w (length (number-to-string
                     (count-lines (point-min) (point-max)))))
         (linum-format (concat "%" (number-to-string w) "d ")))
    ad-do-it))

;; ;; 80 Character Fill Column Indicator
;; (require 'fill-column-indicator)
;; (setq fci-rule-character-color "#464646")
;; (add-hook 'c-mode-hook 'fci-mode) ;; turn on fci-mode for C files

;; Load smex (M-x fuzzy completion)
(require 'smex)
(smex-initialize) ; Can be omitted. This might cause a (minimal) delay
                  ; when Smex is auto-initialized on its first run.
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)  ;; This is your old M-x.

;; auto complete mode
(require 'auto-complete)
(global-auto-complete-mode t)
(ac-linum-workaround) ;; Stop flickering line numbers for auto-complete dropdown

;; IDO vertical mode: for M-x, use vertical auto-complete list
(require 'ido-vertical-mode)
(ido-mode 1)
(ido-everywhere 1)
(ido-vertical-mode 1)
(setq ido-vertical-define-keys 'C-n-C-p-up-and-down)  ;; use C-n and C-p to cycle
;; IDO ubiquitous: use IDO everywhere
(require 'ido-ubiquitous)
(ido-ubiquitous-mode 1)

;; On alarm (like pressing C-g), only flash top and bottom of screen
(setq visible-bell 'top-bottom)


;; Auto-paring
;; (electric-pair-mode)

;; ;; Auto-close braces and quotes, and auto indent on RET inside braces
;; (require 'autopair)
;; (autopair-global-mode)  ;; enable autopair in all buffers
;; (setq autopair-blink nil)

;; ;; Disable autopair in minibuffer
;; (defun my-minibuffer-setup ()
;;   (autopair-mode -1))
;; (add-hook 'minibuffer-setup-hook 'my-minibuffer-setup)
;; ;; (defvar autopair-dont-pair `(:string (?\") :comment (?{))) ; ?\" doesn't seem to translate to double quote


(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; Show the current function name in the header line
(which-function-mode)
(setq-default header-line-format
                            '((which-func-mode ("" which-func-format " "))))
(setq mode-line-misc-info
                  ;; We remove Which Function Mode from the mode line, because it's mostly
                  ;; invisible here anyway.
                  (assq-delete-all 'which-func-mode mode-line-misc-info))
(setq which-func-unknown "")

;; Disable vc-git to improve startup
(setq vc-handled-backends ())


;; Web mode! Do cool stuff with template files.
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.html\\'"      . web-mode))
(add-to-list 'auto-mode-alist '("\\.php\\'"      . web-mode))
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
  (setq web-mode-enable-auto-closing t) ; this could break yanking html
  (setq web-mode-enable-auto-pairing t)
  (setq web-mode-code-indent-offset   4)
  (setq web-mode-css-indent-offset    4)
  (setq web-mode-markup-indent-offset 4)
  (setq web-mode-enable-css-colorization t)
  (setq web-mode-enable-current-element-highlight t)
)
(add-hook 'web-mode-hook 'my-web-mode-hook)


(fset 'yes-or-no-p 'y-or-n-p) ;; Make all "yes or no" prompts show "y or n" instead
(menu-bar-mode 0)             ;; Turn off menu
(delete-selection-mode 1)     ;; Make Delete key delete selection; also, typing over selection replaces it
(column-number-mode 1)        ;; Show current point position in status bar
;; (add-hook 'before-save-hook 'delete-trailing-whitespace) ;; remove trailing whitespace on save

;; (setq scroll-conservatively 10000)  ;; set scrolling to always be a line at a time

;; Mouse support
(unless window-system
  (require 'mouse)
  (xterm-mouse-mode t)
  (global-set-key [mouse-4] (lambda ()
                              (interactive)
                              (scroll-down 1)))
  (global-set-key [mouse-5] (lambda ()
                              (interactive)
                              (scroll-up 1)))
  (defun track-mouse (e))
  (setq mouse-sel-mode t)
  (setq linum-delay t)  ;; delay updates to give Emacs a chance for other changes
  (setq mouse-wheel-progressive-speed nil)
  (setq mouse-wheel-scroll-amount '(1 ((shift) . 1) ((control) . nil)))  ;; scroll one line at a time
)


;; Snippets!
(require 'yasnippet) ;; WARNING: This seems to slow down load time significantly
(setq yas-snippet-dirs
      '("~/.emacs.d/snippets"))
(yas-global-mode 1)


;; Multiple cursors (see below for key bindings)
(require 'multiple-cursors)

;; When creating new files, auto-create any nonexistent directories in the path
(defadvice find-file (before make-directory-maybe (filename &optional wildcards) activate)
  "Create parent directory if not exists while visiting file."
  (unless (file-exists-p filename)
    (let ((dir (file-name-directory filename)))
      (unless (file-exists-p dir)
        (make-directory dir)))))

;; Markdown
(autoload 'markdown-mode "markdown-mode"
  "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

;; Groovy (Gradle)
(autoload 'groovy-mode "groovy-mode"
  "Major mode for editing Groovy files" t)
(add-to-list 'auto-mode-alist '("\\.gradle\\'" . groovy-mode))

;; Check spelling in strings and comments or everywhere (if in text mode)
;;
;; Note: to add words to the dictionary, execute `M-x ispell-region` and press
;; `i` when over the word you want to add. Press `x` to exit. Or use `M-x
;; ispell-word` so you don't have to select a region first.
(add-hook 'text-mode-hook 'flyspell-mode)
(add-hook 'prog-mode-hook 'flyspell-prog-mode)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                              MY KEY BINDINGS                                ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; My minor keys mode, so that  my key bindings have the highest priority
(defvar my-keys-mode-map (make-sparse-keymap)
  "Keymap while my-mode is active.")

;;;###autoload
(define-minor-mode my-keys-mode
  "A minor mode so that my key settings override annoying major modes."
  nil
  :lighter " my-keys-mode"
  my-keys-mode-map)

;; Source: http://stackoverflow.com/questions/683425/globally-override-key-binding-in-emacs
(defadvice load (after give-my-keybindings-priority)
  "Try to ensure that my keybindings always have priority."
  (if (not (eq (car (car minor-mode-map-alist)) 'my-keys-mode))
      (let ((mykeys (assq 'my-keys-mode minor-mode-map-alist)))
        (assq-delete-all 'my-keys-mode minor-mode-map-alist)
        (add-to-list 'minor-mode-map-alist mykeys))))
(ad-activate 'load)

;;;###autoload
(defun turn-on-my-keys-mode ()
  "Turns on my-keys-mode."
  (interactive)
  (my-keys-mode t))

;;;###autoload
(defun turn-off-my-keys-mode ()
  "Turns off my-keys-mode."
  (interactive)
  (my-keys-mode -1))

;;;###autoload
(define-globalized-minor-mode global-my-keys-mode my-keys-mode turn-on-my-keys-mode)

;; Turn off the minor mode in the minibuffer
(add-hook 'minibuffer-setup-hook 'turn-off-my-keys-mode)

(provide 'my-keys-mode)

(global-my-keys-mode 1)

(define-key my-keys-mode-map (kbd "C-x C-o")    'other-window)
(define-key my-keys-mode-map (kbd "C-o")        'other-window)
(define-key my-keys-mode-map (kbd "C-c o")      'my-split-window-focus)
(define-key my-keys-mode-map (kbd "C-c C-o")    'my-split-window-focus)
(define-key my-keys-mode-map (kbd "<backtab>")  (lambda () (interactive) (shift-left 4)))
(define-key my-keys-mode-map (kbd "C-c /")      'comment-region)
(define-key my-keys-mode-map (kbd "C-c C-_")    'comment-region)
(define-key my-keys-mode-map (kbd "C-c ?")      'uncomment-region)
(define-key my-keys-mode-map (kbd "C-c <down>") 'my-duplicate-current-line-or-region)
(define-key my-keys-mode-map (kbd "C-c d")      'my-duplicate-current-line-or-region)
(define-key my-keys-mode-map (kbd "C-c n")      'my-duplicate-current-line-or-region)
(define-key my-keys-mode-map (kbd "C-c C-n")    'my-duplicate-current-line-or-region)
(define-key my-keys-mode-map (kbd "C-c C-a")    'align)         ;; auto align Perl hashes and other things
(define-key my-keys-mode-map (kbd "C-c C-A")    'align-regexp)  ;; align based on entered regexp
(define-key my-keys-mode-map (kbd "<mouse-4>")  'scroll-down-line)
(define-key my-keys-mode-map (kbd "<mouse-5>")  'scroll-up-line)
(define-key my-keys-mode-map (kbd "M-n")        'scroll-up-line)
(define-key my-keys-mode-map (kbd "M-p")        'scroll-down-line)
(define-key my-keys-mode-map (kbd "<M-backspace>") 'my-backward-delete-word-no-kill)
(define-key my-keys-mode-map (kbd "<M-DEL>")    'my-backward-delete-word-no-kill)
(define-key my-keys-mode-map (kbd "M-;")        'comment-or-uncomment-region-or-line)
(define-key my-keys-mode-map (kbd "C-S-n")      'View-scroll-half-page-forward)  ;; doesn't work b/c
(define-key my-keys-mode-map (kbd "C-S-p")      'View-scroll-half-page-backward) ;; of shift key
(define-key my-keys-mode-map (kbd "C-j")        'mc/mark-next-like-this) ;; multiple cursors
(define-key my-keys-mode-map (kbd "M-j")        'mc/mark-previous-like-this)
(define-key my-keys-mode-map (kbd "C-c J")      'mc/mark-all-like-this)
(define-key my-keys-mode-map (kbd "C-c C-j")    'mc/edit-lines)
(define-key my-keys-mode-map (kbd "C-c j")      'mc/edit-lines)
(define-key my-keys-mode-map (kbd "C-c <tab>")  'my-indent-closing-hash)
(define-key my-keys-mode-map (kbd "C-c \\")     'my-indent-closing-hash)
(define-key my-keys-mode-map (kbd "C-c C-\\")   'my-indent-closing-hash)
(define-key my-keys-mode-map (kbd "<M-RET>")    'my-open-new-line-unindented)
(define-key my-keys-mode-map (kbd "C-y")        'my-yank)
(define-key my-keys-mode-map (kbd "<RET>")      'newline-dwim)

(define-key isearch-mode-map (kbd "<M-RET>")    'isearch-exit-mark-match)


(defun my-open-new-line-unindented ()
  "Moves to a new, left-indented line below the current line."
  (interactive)
  (open-line 1)
  (next-line 1)
  (move-beginning-of-line nil))

(defun my-unindent-region (N)
  (interactive "p")
  (if (use-region-p)
      (progn (indent-rigidly (region-beginning) (region-end) (* N -4))
             (setq deactivate-mark nil))
    (self-insert-command N)))

(defun my-duplicate-current-line-or-region (arg)
  "Duplicates the current line or region ARG times.

If there's no region, the current line will be duplicated. However, if
there's a region, all lines that region covers will be duplicated."
  (interactive "p")
  (let (beg end (origin (point)))
    (if (and mark-active (> (point) (mark)))
        (exchange-point-and-mark))
    (setq beg (line-beginning-position))
    (if mark-active
        (exchange-point-and-mark))
    (setq end (line-end-position))
    (let ((region (buffer-substring-no-properties beg end)))
      (dotimes (i arg)
        (goto-char end)
        (newline)
        (insert region)
        (setq end (point)))
      (goto-char (+ origin (* (length region) arg) arg)))))

(defun my-split-window-focus ()
  "Run `split-window-right` and `other-window` in sequence."
  (interactive)
  (split-window-right)
  (other-window 1)
  (ido-find-file))

(defun my-delete-word-no-kill (arg)
  "Delete characters forward until encountering the end of a word.
With argument, do this that many times.
*****This command does not push text to `kill-ring'.*****"
  (interactive "p")
  (delete-region
   (point)
   (progn
     (forward-word arg)
     (point))))

(defun my-backward-delete-word-no-kill (arg)
  "Delete characters backward until encountering the beginning of a word.
With argument, do this that many times.
*****This command does not push text to `kill-ring'.*****"
  (interactive "p")
  (my-delete-word-no-kill (- arg)))
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

(defun comment-or-uncomment-region-or-line ()
  "Comments or uncomments the region or the current line if there's no active region."
  (interactive)
  (let (beg end)
    (if (region-active-p)
        (setq beg (region-beginning) end (region-end))
      (setq beg (line-beginning-position) end (line-end-position)))
    (comment-or-uncomment-region beg end)))

(defun shift-text (distance)
  (if (use-region-p)
      (let ((mark (mark)))
        (save-excursion
          (indent-rigidly (region-beginning)
                          (region-end)
                          distance)
          (push-mark mark t t)
          (setq deactivate-mark nil)))
    (indent-rigidly (line-beginning-position)
                    (line-end-position)
                    distance)))

(defun shift-right (count)
  (interactive "p")
  (shift-text count))

(defun shift-left (count)
  (interactive "p")
  (shift-text (- count)))

(require 'buffer-move)
(defun swap-buffers ()
  "Swap windows using buffer-move.el"
  (interactive)
  (if
      (null
       (windmove-find-other-window 'right))
      (buf-move-left)
    (buf-move-right)))

(defun my-indent-closing-hash ()
  "Correctly indent the closing brace of a function call's hash"
  (interactive)
  ;; XXX: Could use (current-indentation) instead of moving mark?
  (back-to-indentation) ;; move to first non-whitespace character on line
  (kill-line 0)         ;; kill line backwards
  (indent-relative)     ;; indent to first non-whitespace char of prev line
  (shift-left 4)
  (move-end-of-line nil))

(defun my-yank ()
  "Yank, indent, and trim trailing whitespace"
  (interactive)
  (yank)
  (delete-trailing-whitespace (region-beginning) (region-end))
  (indent-region (region-beginning) (region-end)))

(defun smarter-move-beginning-of-line (arg)
  "Move point back to indentation of beginning of line.

Move point to the first non-whitespace character on this line.
If point is already there, move to the beginning of the line.
Effectively toggle between the first non-whitespace character and
the beginning of the line.

If ARG is not nil or 1, move forward ARG - 1 lines first.  If
point reaches the beginning or end of the buffer, stop there."
  (interactive "^p")
  (setq arg (or arg 1))

  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))

  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))

;; remap C-a to `smarter-move-beginning-of-line'
(global-set-key [remap move-beginning-of-line]
                'smarter-move-beginning-of-line)

(defun isearch-exit-mark-match ()
  "Exit isearch, but keep the current match selected"
  (interactive)
  (isearch-exit)
  (push-mark isearch-other-end)
  (activate-mark))


;;;;; Customize newline function
(defun newline-dwim ()
  (interactive)
  (run-hooks 'newline-hooks))

(add-hook 'newline-hooks #'basic-newline)
(add-hook 'newline-hooks #'extra-newline-inside-braces)

(defun basic-newline ()
  (newline-and-indent))

;; Auto expand when pressing enter between braces.
;;
;; Example:
;;
;; If you start with:
;;
;;     function() {|} <RET>
;;
;; You should end with:
;;
;;     function() {
;;         |
;;     }
;;
;; TODO: make it so that you can go from position 2 to 1 by pressing delete
(defun extra-newline-inside-braces ()
  (let ((break-open-pair (or (and (looking-back "{") (looking-at "}"))
                             (and (looking-back ">") (looking-at "<"))
                             (and (looking-back "(") (looking-at ")"))
                             (and (looking-back "\\[") (looking-at "\\]")))))
    (when break-open-pair
      ;; (save-excursion (basic-newline))
      (indent-for-tab-command))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                          IDEAS FOR NEW FUNCTIONS                           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Make a function that selects (and copies?) the current word. For example,
;; this could be bound to C->. When that's pressed without an active selection,
;; the word under the point is selected. If C-> is pressed again, the selection
;; is EXPANDED to the right by one word. If C-< is pressed, the same things
;; happen but this time expansion happens to the left.
