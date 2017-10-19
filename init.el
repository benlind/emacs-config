;;;; INIT ;;;;
;; This file sets up my emacs configuration by including third-party and
;; personal modules. It also has small miscellaneous configurations.


;;; TODO
;;
;; These are some things I'd like emacs to be able to do. Note: some of these
;; are from Prelude, so look there for guidance.
;;
;; - C-c s: swap active buffers
;; - C-c I: open init file
;; - Use a smarter auto-complete (that doesn't just make predictions based on
;;   what's in the currently opened buffers; look into Helm)
;; - "Paste mode": add a keybinding that disables auto-indentation and anything
;;   else that might slow down pasting (yasnippet?) so that you can quickly
;;   paste. The same keybinding should re-enable it afterwards. Maybe C-c v?
;; - "Copy mode": add a keybinding that turns off line numbers (linum-mode) and
;;   closes all buffers besides the current one.
;; - expand-region: https://github.com/magnars/expand-region.el
;; - smartparens: https://github.com/Fuco1/smartparens
;; - undo-tree: https://www.emacswiki.org/emacs/UndoTree
;;
;; Look at these example init files for more ideas:
;; https://github.com/bbatsov/emacs.d/blob/master/init.el


;;; LOAD THIRD-PARTY MODULES

;; Load Melpa

(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (url (concat (if no-ssl "http" "https") "://melpa.org/packages/")))
  (add-to-list 'package-archives (cons "melpa" url) t))
(when (< emacs-major-version 24)
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize)

;; NOTE: I used to load packages manually because (I thought) my server's
;; firewall didn't let me connect to Melpa or Elpa. However, I now know I can
;; run `http_proxy= https_proxy= emacs` and connect to the repos. So...I will
;; now install my packages via Melpa (M-x list-packages). This is great because
;; I can easily update packages.
;;
;; HOWEVER, I will still maintain a commented list below of the packages I have
;; installed so that I can go back to manually loading them in the future if
;; necessary.

;; Set elisp directories

(add-to-list 'load-path "~/.emacs.d/lisp/")
;; (add-to-list 'load-path "~/.emacs.d/lisp/multiple-cursors")

;; These "load" params correspond to file names in ~/.emacs.d/lisp:
(load "web-mode")
(load "auto-indent-mode")
(load "smex")
(load "popup")              ;; for auto-complete
(load "auto-complete")
(load "ido-vertical-mode")
(load "yasnippet")
;; (load "multiple-cursors")
(load "buffer-move")
(load "yaml-mode")
(load "markdown-mode")
(load "dtrt-indent")
(load "org-present")
(load "color-theme-sanityinc-tomorrow")

;; Disabled modules:
;; (load "autopair")
;; (load "smart-tabs-mode")
;; (load "ido-ubiquitous")    ;; slows down emacs and is not very useful

;; (load "dash")
;; (load "s")
;; (load "powerline")
;; (load "powerline-separators")
;; (load "powerline-themes")
;; (load "spaceline")
;; (load "spaceline-segments")
;; (load "spaceline-config")


;;; LOAD PERSONAL LISP FILES

;; Set up functions for loading my lisp files
(defconst user-init-dir
  ;; Define ~/.emacs.d/ as the user init directory, to make loading user files
  ;; (via `load-user-file`) easier.
  (cond ((boundp 'user-emacs-directory)
         user-emacs-directory)
        ((boundp 'user-init-directory)
         user-init-directory)
        (t "~/.emacs.d/")))

(defun load-user-file (file)
  (interactive "f")
  "Load a file in current user's configuration directory"
  (load-file (expand-file-name file user-init-dir)))

;; Load my list files
(load-user-file "modes.el")
(load-user-file "functions.el")
(load-user-file "visuals.el")
(load-user-file "indentation.el")
(load-user-file "keybindings.el")


;;; BACKUP, AUTOSAVE, and LOCK FILES

;; Put autosave files (#foo#) and backup files (foo~) in ~/.emacs.d/.
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-save-file-name-transforms (quote ((".*" "~/.emacs.d/autosaves/\\1" t))))
 '(backup-directory-alist (quote ((".*" . "~/.emacs.d/backups/"))))
 '(custom-safe-themes (quote ("06b2849748590f7f991bf0aaaea96611bb3a6982cad8b1e3fc707055b96d64ca" default)))
 '(fill-column 80)
 '(safe-local-variable-values (quote ((require-final-newline))))
 '(text-mode-hook (quote (text-mode-hook-identify))))

;; Create the autosave and backups dirs if necessary, since emacs won't.
(make-directory "~/.emacs.d/autosaves/" t)
(make-directory "~/.emacs.d/backups/" t)

;; Do not create lock files (files that begin with `.#`) because they make
;; `git status` annoying when emacs has a file open.
(setq create-lockfiles nil)


;;; MISC

(fset 'yes-or-no-p 'y-or-n-p) ;; Make all "yes or no" prompts show "y or n" instead

;; (add-hook 'before-save-hook 'delete-trailing-whitespace) ;; remove trailing whitespace on save

;; When creating new files, auto-create any nonexistent directories in the path
(defadvice find-file (before make-directory-maybe (filename &optional wildcards) activate)
  "Create parent directory if not exists while visiting file."
  (unless (file-exists-p filename)
    (let ((dir (file-name-directory filename)))
      (unless (file-exists-p dir)
        (make-directory dir)))))

;; Always redraw the screen on every scroll step (smoother scrolling)
(setq redisplay-dont-pause t)

;; Open the *scratch* buffer in fundamental mode with no message on startup
(setq initial-major-mode 'fundamental-mode)
(setq initial-scratch-message nil)

;; The following line will make emacs always open the *scratch* buffer on start,
;; but that means when you edit a commit message it opens *scratch* instead of
;; COMMIT_EDITMSG.
;; (setq initial-buffer-choice t)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
