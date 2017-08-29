;;;; FUNCTIONS ;;;;
;; This file defines various custom functions that are typically invoked via
;; custom keybindings (see keybindings.el).

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

(defun my-split-window-focus ()
  "Run `split-window-right` and `other-window` in sequence."
  (interactive)
  (split-window-right)
  (other-window 1)
  (ido-find-file))

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

(defun shift-text(distance)
  "Shifts the current line or region by the passed distance. The
   distance can be negative."
  ; TODO: restore mark after shift (https://superuser.com/a/455404/536749)
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
    (indent-rigidly beg end distance)))

(defun shift-right (distance)
  "Shifts the current line or region right by the passed distance."
  (interactive "p")
  (shift-text distance))

(defun shift-left (distance)
  "Shifts the current line or region left by the passed distance."
  (interactive "p")
  (shift-text (- distance)))

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
  (back-to-indentation) ;; move to first non-whitespace character on line
  (kill-line 0)         ;; kill line backwards
  (indent-relative)     ;; indent to first non-whitespace char of prev line
  (shift-left 4)
  (move-end-of-line nil))

(defun my-indent-function-parameters ()
  "Correctly indent function parameters on new lines"
  (interactive)
  (back-to-indentation) ;; move to first non-whitespace character on line
  (kill-line 0)         ;; kill line backwards
  (indent-relative)     ;; indent to first non-whitespace char of prev line
  (shift-right 4)
  (move-end-of-line nil))

(defun my-yank ()
  "Yank, indent, and trim trailing whitespace"
  (interactive)
  (yank)

  (if (derived-mode-p 'web-mode) (web-mode))  ;; re-enable web-mode

  (delete-trailing-whitespace (region-beginning) (region-end))

  ;; Auto-indent the yanked code
  (unless (derived-mode-p  ;; do not auto-indent for these modes
           'yaml-mode
           'fundamental-mode
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
        (case-fold-search nil))      ;; make regexp case-sensitive
    (if (derived-mode-p 'cperl-mode) ;; regexp for perl
        (setq regexp "^\\(sub\\|has\\|=head1\\|requires\\) ")
      (setq regexp "^function "))    ;; regexp for everything else
    (occur regexp)))


;;; CUSTOMIZE NEWLINE FUNCTION

(defun newline-dwim ()
  (interactive)
  (if (derived-mode-p 'dired-mode) (dired-find-file)  ;; find file if in dired mode
    (run-hooks 'newline-hooks)))                      ;; ...otherwise run my hooks

(add-hook 'newline-hooks #'newline-maybe-indent)
(add-hook 'newline-hooks #'extra-newline-inside-braces)

(defun newline-maybe-indent ()
  "Add a newline and auto-indent it if we're in certain modes."
  (if (derived-mode-p
       ;; List of modes to NOT auto-indent in:
       'fundamental-mode
       'text-mode
       'sql-mode
       )

      ;; If we're in one of the above modes, DO NOT auto-indent
      (newline)

    ;; Otherwise (for all other modes), DO auto-indent
    (newline-and-indent)))

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


;;; IDEAS FOR NEW FUNCTIONS

;; Make a function that selects (and copies?) the current word. For example,
;; this could be bound to C->. When that's pressed without an active selection,
;; the word under the point is selected. If C-> is pressed again, the selection
;; is EXPANDED to the right by one word. If C-< is pressed, the same things
;; happen but this time expansion happens to the left.
