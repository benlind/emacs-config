;;;; KEYBINDINGS ;;;;
;; This file defines my keybindings through a custom minor mode. The minor mode
;; ensures that the keybindings will ALWAYS take effect.


;;; SET UP MY CUSTOM KEYS MODE

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

;; Turn on my keys mode
(provide 'my-keys-mode)
(global-my-keys-mode 1)


;;; DEFINE MY CUSTOM KEYBINDINGS

;; M-x replacement (to use the non-smex version, just use M-x):
(define-key my-keys-mode-map (kbd "C-c C-m")       'smex)

(define-key my-keys-mode-map (kbd "C-x C-o")       'other-window)
(define-key my-keys-mode-map (kbd "C-o")           'other-window)
(define-key my-keys-mode-map (kbd "C-c o")         'my-split-window-focus)
(define-key my-keys-mode-map (kbd "C-c C-o")       'my-split-window-focus)
(define-key my-keys-mode-map (kbd "<backtab>")     (lambda () (interactive) (shift-left 4)))
(define-key my-keys-mode-map (kbd "C-c /")         'comment-region)
(define-key my-keys-mode-map (kbd "C-c C-_")       'comment-region)
(define-key my-keys-mode-map (kbd "C-c ?")         'uncomment-region)
(define-key my-keys-mode-map (kbd "C-c <down>")    'my-duplicate-current-line-or-region)
(define-key my-keys-mode-map (kbd "C-c d")         'my-duplicate-current-line-or-region)
(define-key my-keys-mode-map (kbd "C-c n")         'my-duplicate-current-line-or-region)
(define-key my-keys-mode-map (kbd "C-c C-n")       'my-duplicate-current-line-or-region)
(define-key my-keys-mode-map (kbd "C-c C-a")       'align)  ; auto align Perl hashes and other things
(define-key my-keys-mode-map (kbd "C-c C-A")       'align-regexp)  ; align based on entered regexp
(define-key my-keys-mode-map (kbd "<mouse-4>")     'scroll-down-line)
(define-key my-keys-mode-map (kbd "<mouse-5>")     'scroll-up-line)
(define-key my-keys-mode-map (kbd "M-n")           'scroll-up-line)
(define-key my-keys-mode-map (kbd "M-p")           'scroll-down-line)
(define-key my-keys-mode-map (kbd "<M-backspace>") 'my-backward-delete-word-no-kill)
(define-key my-keys-mode-map (kbd "<M-DEL>")       'my-backward-delete-word-no-kill)
(define-key my-keys-mode-map (kbd "M-;")           'comment-or-uncomment-region-or-line)
(define-key my-keys-mode-map (kbd "C-j")           'mc/mark-next-like-this) ; multiple cursors
(define-key my-keys-mode-map (kbd "M-j")           'mc/mark-previous-like-this)
(define-key my-keys-mode-map (kbd "C-c J")         'mc/mark-all-like-this)
(define-key my-keys-mode-map (kbd "C-c C-j")       'mc/edit-lines)
(define-key my-keys-mode-map (kbd "C-c j")         'mc/edit-lines)
(define-key my-keys-mode-map (kbd "C-c <tab>")     'my-indent-closing-hash)
(define-key my-keys-mode-map (kbd "C-c \\")        'my-indent-closing-hash)
(define-key my-keys-mode-map (kbd "C-c C-\\")      'my-indent-closing-hash)
(define-key my-keys-mode-map (kbd "C-c |")         'my-indent-function-parameters)
(define-key my-keys-mode-map (kbd "C-c C-|")       'my-indent-function-parameters)
(define-key my-keys-mode-map (kbd "<M-RET>")       'my-open-new-line-unindented)
(define-key my-keys-mode-map (kbd "C-y")           'my-yank)
(define-key my-keys-mode-map (kbd "<RET>")         'newline-dwim)
(define-key my-keys-mode-map (kbd "C-c C-l")       'goto-line)
(define-key my-keys-mode-map (kbd "C-c l")         'goto-line)
(define-key my-keys-mode-map (kbd "C-x C-l")       'linum-mode)  ; toggle line numbers
(define-key my-keys-mode-map (kbd "C-x l")         'linum-mode)

(define-key isearch-mode-map (kbd "<M-RET>")       'isearch-exit-mark-match)

;; remap C-a to `smarter-move-beginning-of-line'
(global-set-key [remap move-beginning-of-line]
                'smarter-move-beginning-of-line)
