;;;; FUNCTIONS ;;;;
;; This file defines various custom functions that are typically invoked via
;; custom keybindings (see keybindings.el).

(defun my-open-new-line-unindented ()
  "Inserts a newline with no indentation."
  (interactive)
  (if (current-mode-one-of 'org-mode)
      (org-insert-heading)  ; do the Right Thing for org mode
    ;; Otherwise insert a newline with no indentation
    (open-line 1)
    (next-line 1)
    (move-beginning-of-line nil)))

(defun my-unindent-region (N)
  (interactive "p")
  (if (use-region-p)
      (progn (indent-rigidly (region-beginning) (region-end) (* N -4))
             (setq deactivate-mark nil))
    (self-insert-command N)))

(defun my-duplicate-current-line-or-region (arg)
  "Duplicates the current line or region ARG times.

   If there's no region, the current line will be
   duplicated. However, if there's a region, all lines that
   region covers will be duplicated."
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

(defun my-find-file ()
  (interactive)
  (projectile-find-file))

(defun my-split-window-focus ()
  "Run `split-window-right` and `other-window` in sequence."
  (interactive)
  (split-window-right)
  (other-window 1)
  (projectile-find-file))

(defun my-delete-word-no-kill (arg)
  "Delete characters forward until encountering the end of a word.
   With argument, do this that many times.
   ***This command does not push text to `kill-ring'.***"
  (interactive "p")
  (delete-region
   (point)
   (progn
     (forward-word arg)
     (point))))

(defun my-backward-delete-word-no-kill (arg)
  "Delete characters backward until encountering the beginning of a word.
   With argument, do this that many times.
   ***This command does not push text to `kill-ring'.***"
  (interactive "p")
  (my-delete-word-no-kill (- arg)))
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)


(defun comment-or-uncomment-region-or-line()
  "Comments or uncomments the region or the current line if
   there's no active region."
  (interactive)
  (let (beg end)
    (if (region-active-p)
        (progn
          (setq beg (region-beginning) end (region-end))
          (save-excursion
            (setq beg (progn (goto-char beg) (line-beginning-position))
                  end (progn (goto-char end) (line-end-position)))))
      (setq beg (line-beginning-position)
            end (line-end-position)))
    (comment-or-uncomment-region beg end)))

;; Source: https://stackoverflow.com/a/35183657/1054633
(defun indent-region-custom (numSpaces)
  "Shifts the current line or region by the passed number of
   spaces. The distance can be negative. Restores the region
   afterwards so you can hit shift the same region multiple times
   without re-selection."
  (interactive)
  (progn
    ;; default to start and end of current line
    (setq regionStart (line-beginning-position))
    (setq regionEnd (line-end-position))

    ;; if there's a selection, use that instead of the current line
    (when (use-region-p)
      (setq regionStart (region-beginning))
      (setq regionEnd (region-end)))

    (save-excursion                          ; restore the position afterwards
      (goto-char regionStart)                ; go to the start of region
      (setq start (line-beginning-position)) ; save the start of the line
      (goto-char regionEnd)                  ; go to the end of region
      (setq end (line-end-position))         ; save the end of the line

      (indent-rigidly start end numSpaces)   ; indent between start and end
      (setq deactivate-mark nil)             ; restore the selected region
      )))

;; TODO: Don't use a hard-coded tab width. Instead check the tab width for the
;; current mode.
(defun untab-region (N)
    (interactive "p")
    (indent-region-custom -2))

(defun tab-region (N)
  (interactive "p")
  (if (active-minibuffer-window)
      (minibuffer-complete)        ; tab is pressed in minibuffer window, so do completion
    (if (string= (buffer-name) "*shell*")
        (comint-dynamic-complete)  ; in a shell, use tab completion
      (if (use-region-p)           ; tab is pressed is any other buffer, so
                                   ; execute with space insertion
          (indent-region-custom 2) ; region was selected, call indent-region
        (insert "    ")            ; else insert four spaces as expected
        ))))

(defun my-unindent ()
  (interactive)
  (if (current-mode-one-of 'Custom-mode)
      ;; When using Customize, focus the previous field
      (widget-backward)
    (untab-region 2)))

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
  "Correctly indent the closing brace of a function call's
   hash (Perl)"
  (interactive)
  ;; XXX: Could use (current-indentation) instead of moving mark?
  (back-to-indentation) ; move to first non-whitespace character on line
  (kill-line 0)         ; kill line backwards
  (indent-relative)     ; indent to first non-whitespace char of prev line
  (shift-left 4)
  (move-end-of-line nil))

(defun my-indent-function-parameters ()
  "Correctly indent function parameters on new lines"
  (interactive)
  (back-to-indentation) ; move to first non-whitespace character on line
  (kill-line 0)         ; kill line backwards
  (indent-relative)     ; indent to first non-whitespace char of prev line
  (shift-right 4)
  (move-end-of-line nil))

(defun my-yank ()
  "Yank, indent, and trim trailing whitespace"
  (interactive)
  (yank)

  (if (current-mode-one-of 'web-mode) (web-mode))  ; re-enable web-mode

  (delete-trailing-whitespace (region-beginning) (region-end))

  ;; Auto-indent the yanked code
  (unless (current-mode-one-of  ; do not auto-indent for these modes
           'yaml-mode
           'fundamental-mode
           'sql-mode
           'makefile-gmake-mode
           )
    (indent-region (region-beginning) (region-end))))

(defun smarter-move-beginning-of-line (arg)
  "Move point back to indentation of beginning of line.

   Move point to the first non-whitespace character on this line.
   If point is already there, move to the beginning of the line.
   Effectively toggle between the first non-whitespace character
   and the beginning of the line.

   If ARG is not nil or 1, move forward ARG - 1 lines first. If
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

(defun isearch-exit-mark-match ()
  "Exit isearch, but keep the current match selected"
  (interactive)
  (isearch-exit)
  (push-mark isearch-other-end)
  (activate-mark))

(defun goto-paragraph (arg)
  "Go to paragrah ARG."
  (interactive "nGoto paragraph: ")
  (goto-char (point-min))
  (forward-paragraph arg))

(defun replace-smart-quotes (beg end)
  "Replace 'smart quotes' in buffer or region with ascii quotes."
  (interactive "r")
  (format-replace-strings '(("\x201C" . "\"")
                            ("\x201D" . "\"")
                            ("\x2018" . "'")
                            ("\x2019" . "'"))
                          nil beg end))

(defun toc ()
  "Show a 'Table of Contents' for the current file using occur"
  (interactive)
  (let (regexp
        (case-fold-search nil))  ; make regexp case-sensitive
    (cond ((current-mode-one-of 'cperl-mode) ; regexp for perl
           (setq regexp "^\\(sub\\|has\\|=head1\\|requires\\|around\\) "))
          ((current-mode-one-of 'sh-mode)    ; regexp for shell scripts
           (setq regexp "^[A-za-z0-9_]+()"))
          ((current-mode-one-of 'go-mode)    ; regexp for go
           (setq regexp "^func "))
      (setq regexp "^function "))            ; regexp for everything else
    (occur regexp))) ; the buffer will be auto-focused (see modes.el)

(defun revert-all-file-buffers ()
  "Refresh all open file buffers without confirmation.
Buffers in modified (not yet saved) state in emacs will not be reverted. They
will be reverted though if they were modified outside emacs.
Buffers visiting files which do not exist any more or are no longer readable
will be killed."
  (interactive)
  (dolist (buf (buffer-list))
    (let ((filename (buffer-file-name buf)))
      ;; Revert only buffers containing files, which are not modified;
      ;; do not try to revert non-file buffers like *Messages*.
      (when (and filename
                 (not (buffer-modified-p buf)))
        (if (file-readable-p filename)
            ;; If the file exists and is readable, revert the buffer.
            (with-current-buffer buf
              (revert-buffer :ignore-auto :noconfirm :preserve-modes))
          ;; Otherwise, kill the buffer.
          (let (kill-buffer-query-functions) ; No query done when killing buffer
            (kill-buffer buf)
            (message "Killed non-existing/unreadable file buffer: %s" filename))))))
  (message "Finished reverting buffers containing unmodified files."))


;;; CUSTOMIZE NEWLINE FUNCTION

;; To figure out what a keybinding SHOULD be in a mode, comment out the <RET>
;; line in keybindings.el, start a new emacs session, enter the desired mode,
;; and hit C-h k RET. There might be a better way via describe-mode
;; (https://stackoverflow.com/a/13980476/1054633).
(defun newline-dwim ()
  (interactive)
  (if (current-mode-one-of 'dired-mode)
    ;; Find file if in dired mode
    (dired-find-file)
  (if (current-mode-one-of 'Custom-mode)
    ;; Activate button if in Customize mode
    (Custom-newline (point))
  (if (current-mode-one-of 'magit-status-mode)
    (call-interactively #'magit-diff-visit-file-other-window)
  (if (current-mode-one-of 'magit-mode 'magit-log-mode)
    (magit-visit-thing)
  (if (current-mode-one-of 'magit-revision-mode)
    (call-interactively #'magit-diff-visit-file-other-window)
  (if (current-mode-one-of 'ibuffer-mode)
    (call-interactively #'ibuffer-visit-buffer)
  (if (current-mode-one-of 'helm-ag-mode)
    (call-interactively #'helm-ag-mode-jump-other-window)
  ;; ...otherwise run my hooks
  (run-hooks 'newline-hooks)))))))))

(add-hook 'newline-hooks #'extra-newline-inside-braces)
(add-hook 'newline-hooks #'newline-maybe-indent)

(defun newline-maybe-indent ()
  "Add a newline and auto-indent it if we're in certain modes."
  (if (current-mode-one-of
       ;; List of modes to NOT auto-indent in:
       'fundamental-mode
       'text-mode
       'sql-mode
       'conf-colon-mode
       )

      ;; If we're in one of the above modes, DO NOT auto-indent
      (newline)

    ;; Otherwise (for all other modes), DO auto-indent
    (newline-and-indent)))

(defun current-mode-one-of (&rest modes)
  "Returns true if the current major-mode is one of the passed
modes. This is different from derived-mode-p, which evaluates the
current mode AND all of its parent modes."
  (let ((cur-mode major-mode))
     (memq cur-mode modes)))

(defun describe-current-mode ()
  "Displays the current major mode in the minibuffer"
  (interactive)
  (message "Current mode: '%s'" major-mode))

;; Auto expand when pressing enter between braces.
;;
;; If you start with:
;;     function() {|} <RET>
;;
;; You should end with:
;;     function() {
;;         |
;;     }
;;
;; TODO: make it so that you can go from position 2 to 1 by pressing delete
(defun extra-newline-inside-braces ()
  (when (or (and (looking-back "{") (looking-at "}"))
            (and (looking-back ">") (looking-at "<"))
            (and (looking-back "(") (looking-at ")"))
            (and (looking-back "\\[") (looking-at "\\]")))
    ;; NOTE: newline-maybe-indent will be run after this
    (newline-and-indent)
    (previous-line 1)
    (move-end-of-line nil)))

(defun my-scroll-down-line ()
  (interactive)
  (if (current-mode-one-of 'git-rebase-mode)
      (git-rebase-move-line-down 1)
  ;; To select next siblings in magit status:
  ;; (if (current-mode-one-of 'magit-status-mode)
  ;;     (magit-section-forward-sibling)
    (scroll-up-line)))

(defun my-scroll-up-line ()
  (interactive)
  (if (current-mode-one-of 'git-rebase-mode)
      (git-rebase-move-line-up 1)
  ;; (if (current-mode-one-of 'magit-status-mode)
  ;;     (magit-section-backward-sibling)
    (scroll-down-line)))

(defun move-line-up ()
  "Move the current line up and indent"
  (interactive)
  (transpose-lines 1)
  (forward-line -2)
  (indent-according-to-mode))

(defun move-line-down ()
  "Move the current line down and indent"
  (interactive)
  (forward-line 1)
  (transpose-lines 1)
  (forward-line -1)
  (indent-according-to-mode))


;;; IDEAS FOR NEW FUNCTIONS

;; Make a function that selects (and copies?) the current word. For example,
;; this could be bound to C->. When that's pressed without an active selection,
;; the word under the point is selected. If C-> is pressed again, the selection
;; is EXPANDED to the right by one word. If C-< is pressed, the same things
;; happen but this time expansion happens to the left.
