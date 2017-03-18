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
;;
;; Look at these example init files for more ideas:
;; https://github.com/bbatsov/emacs.d/blob/master/init.el


;;; LOAD THIRD-PARTY MODULES

;; Set elisp directories
(add-to-list 'load-path "~/.emacs.d/lisp/")
(add-to-list 'load-path "~/.emacs.d/lisp/multiple-cursors")

;; These "load" params correspond to file names in ~/.emacs.d/lisp:
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
(load "yaml-mode")
(load "markdown-mode")
(load "dtrt-indent")

;; Disabled modules:
;; (load "autopair")
;; (load "smart-tabs-mode")


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
 '(auto-save-file-name-transforms (quote ((".*" "~/.emacs.d/autosaves/\\1" t))))
 '(backup-directory-alist (quote ((".*" . "~/.emacs.d/backups/"))))
 '(custom-safe-themes (quote ("06b2849748590f7f991bf0aaaea96611bb3a6982cad8b1e3fc707055b96d64ca" default)))
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

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
