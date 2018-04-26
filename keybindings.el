;;;; KEYBINDINGS ;;;;
;; This file defines my keybindings through a custom minor mode. The minor mode
;; ensures that the keybindings will ALWAYS take effect.


;;; SET UP MY CUSTOM KEYS MODE

;; Source: http://stackoverflow.com/a/683575/1054633

(defvar my-keys-minor-mode-map
  (let ((map (make-sparse-keymap)))
    ;; M-x replacement (to use the non-smex version, just use M-x):
    (define-key map (kbd "C-c C-m")       'smex)
    (define-key map (kbd "C-c m")         'smex)
    (define-key map (kbd "C-x C-o")       'other-window)
    (define-key map (kbd "C-o")           'other-window)
    (define-key map (kbd "C-c o")         'my-split-window-focus)
    (define-key map (kbd "C-c C-o")       'my-split-window-focus)
    (define-key map (kbd "<backtab>")     'my-unindent)
    (define-key map (kbd "C-c ;")         'comment-or-uncomment-region-or-line)
    (define-key map (kbd "C-c <down>")    'my-duplicate-current-line-or-region)
    (define-key map (kbd "C-c d")         'my-duplicate-current-line-or-region)
    (define-key map (kbd "C-c n")         'my-duplicate-current-line-or-region)
    (define-key map (kbd "C-c C-n")       'my-duplicate-current-line-or-region)
    (define-key map (kbd "C-c C-a")       'align)  ; auto align Perl hashes and other things
    (define-key map (kbd "C-c C-A")       'align-regexp)  ; align based on entered regexp
    (define-key map (kbd "C-v")           'golden-ratio-scroll-screen-up)  ; scroll by half windows
    (define-key map (kbd "M-v")           'golden-ratio-scroll-screen-down)
    (define-key map (kbd "<mouse-4>")     'my-scroll-up-line)
    (define-key map (kbd "<mouse-5>")     'my-scroll-down-line)
    (define-key map (kbd "M-n")           'my-scroll-down-line)
    (define-key map (kbd "M-p")           'my-scroll-up-line)
    (define-key map (kbd "<M-backspace>") 'my-backward-delete-word-no-kill)
    (define-key map (kbd "<M-DEL>")       'my-backward-delete-word-no-kill)
    (define-key map (kbd "M-;")           'comment-or-uncomment-region-or-line)
    (define-key map (kbd "C-j")           'mc/mark-next-like-this) ; multiple cursors
    (define-key map (kbd "M-j")           'mc/mark-previous-like-this)
    (define-key map (kbd "C-c J")         'mc/mark-all-like-this)
    (define-key map (kbd "C-c C-j")       'mc/edit-lines)
    (define-key map (kbd "C-c j")         'mc/edit-lines)
    (define-key map (kbd "C-c <tab>")     'my-indent-closing-hash)
    (define-key map (kbd "C-c \\")        'my-indent-closing-hash)
    (define-key map (kbd "C-c C-\\")      'my-indent-closing-hash)
    (define-key map (kbd "C-c |")         'my-indent-function-parameters)
    (define-key map (kbd "C-c C-|")       'my-indent-function-parameters)
    (define-key map (kbd "<M-RET>")       'my-open-new-line-unindented)
    ;; (define-key map (kbd "C-y")           'my-yank)
    (define-key map (kbd "<RET>")         'newline-dwim)
    (define-key map (kbd "C-c C-l")       'goto-line)
    (define-key map (kbd "C-c l")         'goto-line)
    (define-key map (kbd "C-x C-l")       'linum-mode)  ; toggle line numbers
    (define-key map (kbd "C-x l")         'linum-mode)
    (define-key map (kbd "C-x t")         'toc)
    (define-key map (kbd "C-x C-t")       'toc)
    (define-key map (kbd "C-c C-f")       'helm-projectile-find-file)
    (define-key map (kbd "C-c C-s")       'helm-projectile-ack)
    (define-key map (kbd "C-x g")         'magit-status)
    (define-key map (kbd "C-x M-g")       'magit-dispatch-popup)
    (define-key map (kbd "C-c C-p")       nil)  ; otherwise flyspell starts checking everything
    ;; Press M-= once to activate and then = to expand, - to contract, 0 to quit:
    (define-key map (kbd "M-=")           'er/expand-region)
    (define-key map (kbd "C-x C-b")       'ibuffer)
    (define-key map (kbd "C-c <left>")    'windmove-left)
    (define-key map (kbd "C-c [")         'windmove-left)
    (define-key map (kbd "C-c <right>")   'windmove-right)
    (define-key map (kbd "C-c ]")         'windmove-right)
    (define-key map (kbd "C-c <up>")      'windmove-up)
    (define-key map (kbd "C-c k")         'windmove-up)
    (define-key map (kbd "C-c <down>")    'windmove-down)
    (define-key map (kbd "C-c j")         'windmove-down)
    map)
  "my-keys-minor-mode keymap")

(define-minor-mode my-keys-minor-mode
  "A minor mode so that my key settings override annoying major modes."
  :init-value t
  :lighter " my-keys")

(my-keys-minor-mode 1)

(defun my-minibuffer-setup-hook ()
  (my-keys-minor-mode 0))

(add-hook 'minibuffer-setup-hook 'my-minibuffer-setup-hook)


;;; MISC KEYBINDINGS

(define-key isearch-mode-map (kbd "<M-RET>") 'isearch-exit-mark-match)

;; remap C-a to `smarter-move-beginning-of-line'
(global-set-key [remap move-beginning-of-line]
                'smarter-move-beginning-of-line)
