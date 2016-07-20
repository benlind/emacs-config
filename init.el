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
(load "autopair")
(load "auto-indent-mode")
(load "smex")
(load "popup")              ;; for auto-complete
(load "auto-complete")
(load "ido-vertical-mode")
(load "ido-ubiquitous")
(load "yasnippet")
(load "multiple-cursors")
(load "buffer-move")



;;; THEMES

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/emacs-color-theme-solarized")
(load-theme 'solarized t)
(set-terminal-parameter nil 'background-mode 'dark) ; comment out for light



;;; INDENTATION

;; Set up indenting to 4 spaces
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq sgml-basic-offset 4)

(setq indent-line-function 'insert-tab)
(define-key global-map (kbd "RET") 'newline-and-indent)

(require 'auto-indent-mode)
(auto-indent-global-mode)
(setq auto-indent-newline-function 'newline-and-indent) ;; don't indent prev line on RET
;; The following variable defines what <delete> does at the beginning of a line.
;; I would like it to just remove the previous newline, but currently it removes
;; all newlines. I created an issue for this here:
;; https://github.com/mattfidler/auto-indent-mode.el/issues/47
(setq auto-indent-backward-delete-char-behavior 'all)

;; Disable auto-indent for certain modes (could cause emacs to freeze)
(add-hook 'yaml-mode-hook 'my-yaml-mode-hook)
(defun my-yaml-mode-hook ()
  (auto-indent-mode 0)
  (define-key yaml-mode-map "\C-m" 'newline-and-indent))

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
 '(text-mode-hook (quote (text-mode-hook-identify))))

;; Create the autosave dir if necessary, since emacs won't.
(make-directory "~/.emacs.d/autosaves/" t)
(make-directory "~/.emacs.d/backups/" t)



;;; MISC

;; Syntax highlighting for different filetypes
(add-to-list 'auto-mode-alist '("\\.t\\'" . perl-mode))

;; Don't auto-wrap long lines
(auto-fill-mode -1)
(remove-hook 'text-mode-hook #'turn-on-auto-fill)
(remove-hook 'html-mode-hook #'turn-on-auto-fill)
(setq auto-fill-mode -1)
(auto-fill-mode -1)

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

;; ;; auto wrap paragraphs to fill column width in text mode
;; (add-hook 'text-mode-hook 'turn-on-auto-fill)

;; IDO vertical mode: for M-x, use vertical auto-complete list
(require 'ido-vertical-mode)
(ido-mode 1)
(ido-everywhere 1)
(ido-vertical-mode 1)
(setq ido-vertical-define-keys 'C-n-C-p-up-and-down)  ;; use C-n and C-p to cycle
;; IDO ubiquitous: use IDO everywhere
(require 'ido-ubiquitous)
(ido-ubiquitous-mode 1)

;; Show matching parens automatically
(show-paren-mode 1)
(set-face-foreground 'show-paren-match "#8da5b1")
(set-face-background 'show-paren-match "#203742")
;; (set-face-attribute 'show-paren-match nil :weight 'extra-bold)

;; ;; Auto-closing of parens and quotes
;; (electric-pair-mode 1)

;; On alarm (like pressing C-g), only flash top and bottom of screen
(setq visible-bell 'top-bottom)

;; Auto-close braces and quotes, and auto indent on RET inside braces
(require 'autopair)
(autopair-global-mode)  ;; enable autopair in all buffers
(setq autopair-blink nil)

;; Disable autopair in minibuffer
(defun my-minibuffer-setup ()
  (autopair-mode -1))
(add-hook 'minibuffer-setup-hook 'my-minibuffer-setup)
;; (defvar autopair-dont-pair `(:string (?\") :comment (?{))) ; ?\" doesn't seem to translate to double quote


;; Web mode! Do cool stuff with template files.
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.html\\'"      . web-mode))
(add-to-list 'auto-mode-alist '("\\.js\\'"        . web-mode))
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



(fset 'yes-or-no-p 'y-or-n-p) ;; Make all "yes or no" prompts show "y or n" instead
(menu-bar-mode 0)             ;; Turn off menu
(delete-selection-mode 1)     ;; Make Delete key delete selection; also, typing over selection replaces it
(global-hl-line-mode 1)       ;; Turn on highlighting current line
(column-number-mode 1)        ;; Show current point position in status bar

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
(define-key my-keys-mode-map (kbd "C-c C-a")    'align) ;; auto align Perl hashes and other things
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                          IDEAS FOR NEW FUNCTIONS                           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Make a function that selects (and copies?) the current word. For example,
;; this could be bound to C->. When that's pressed without an active selection,
;; the word under the point is selected. If C-> is pressed again, the selection
;; is EXPANDED to the right by one word. If C-< is pressed, the same things
;; happen but this time expansion happens to the left.

