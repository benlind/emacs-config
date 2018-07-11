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
;; - smartparens: https://github.com/Fuco1/smartparens
;; - undo-tree: https://www.emacswiki.org/emacs/UndoTree
;; - editorconfig-emacs: https://github.com/editorconfig/editorconfig-emacs
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

;; I used to load packages manually because (I thought) my server's firewall
;; didn't let me connect to Melpa or Elpa. However, I now know I can run
;; `http_proxy= https_proxy= emacs` and connect to the repos. So...I will now
;; install my packages via Melpa (M-x list-packages). This is great because I
;; can easily update packages.
;;
;; I no longer manually maintain a list of installed packages.
;; package-selected-packages below keeps track of my installed packages
;; automatically. To install them all on a new system, I just have to run
;; package-install-selected-packages after evaluating that custom-set-variables
;; expression.


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


;;; GENERAL CUSTOMIZATIONS

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-save-file-name-transforms (quote ((".*" "~/.emacs.d/autosaves/\\1" t))))
 '(backup-directory-alist (quote ((".*" . "~/.emacs.d/backups/"))))
 '(compilation-message-face (quote default))
 '(custom-safe-themes
   (quote
    ("4e63466756c7dbd78b49ce86f5f0954b92bf70b30c01c494b37c586639fa3f6f" "7ef2884658a1fed818a11854c232511fa25721c60083a2695e6ea34ce14777ee" "8db4b03b9ae654d4a57804286eb3e332725c84d7cdab38463cb6b97d5762ad26" "d8f76414f8f2dcb045a37eb155bfaa2e1d17b6573ed43fb1d18b936febc7bbc2" "8288b9b453cdd2398339a9fd0cec94105bc5ca79b86695bd7bf0381b1fbe8147" "d494af9adbd2c04bec4b5c414983fefe665cd5dadc5e5c79fd658a17165e435a" "cd0d4fdf764f757fd659ee2697239a62f38d15203000ced1ad8e43c978942c68" "3b0a350918ee819dca209cec62d867678d7dac74f6195f5e3799aa206358a983" "1012cf33e0152751078e9529a915da52ec742dabf22143530e86451ae8378c1a" "4980e5ddaae985e4bae004280bd343721271ebb28f22b3e3b2427443e748cd3f" "71b9b4c5d2a5126586d204e20c3fb4797f70d3d057a0c8b03bac2c51893007a2" "7d3ee5cee22625af0a2acd2349242f5c1951f481d0f32c43afab45dd0c92477a" "3448e3f5d01b39ce75962328a5310438e4a19e76e4b691c21c8e04ca318a5f62" "3a5f04a517096b08b08ef39db6d12bd55c04ed3d43b344cf8bd855bde6d3a1ae" "b65a3bb7dd1c43bf2e301143969a456a5cc380627076196f5529ce8fbf9fb8ac" "1ce99f3eacdcb4d53e26274cb491f5ef4b9c623ebd7e5b2b380a91cf9c1429e9" "a24c5b3c12d147da6cef80938dca1223b7c7f70f2f382b26308eba014dc4833a" "bc4b650c41b16b98166b35da94b366c6a9e1e7883bbf4937c897fb7bd05aa619" "8e4efc4bed89c4e67167fdabff77102abeb0b1c203953de1e6ab4d2e3a02939a" "8ed752276957903a270c797c4ab52931199806ccd9f0c3bb77f6f4b9e71b9272" "938d8c186c4cb9ec4a8d8bc159285e0d0f07bad46edf20aa469a89d0d2a586ea" "8bb8a5b27776c39b3c7bf9da1e711ac794e4dc9d43e32a075d8aa72d6b5b3f59" "ff7625ad8aa2615eae96d6b4469fcc7d3d20b2e1ebc63b761a349bebbb9d23cb" "06b2849748590f7f991bf0aaaea96611bb3a6982cad8b1e3fc707055b96d64ca" default)))
 '(fill-column 80)
 '(highlight-changes-colors (quote ("#FD5FF0" "#AE81FF")))
 '(highlight-tail-colors
   (quote
    (("#3C3D37" . 0)
     ("#679A01" . 20)
     ("#4BBEAE" . 30)
     ("#1DB4D0" . 50)
     ("#9A8F21" . 60)
     ("#A75B00" . 70)
     ("#F309DF" . 85)
     ("#3C3D37" . 100))))
 '(magit-diff-use-overlays nil)
 '(package-selected-packages
   (quote
    (go-mode flymd git-gutter+ vue-html-mode vue-mode expand-region golden-ratio-scroll-screen helm-projectile helm zone-rainbow zone-sl beacon smex projectile tango-plus-theme tangotango-theme tango-2-theme color-theme-solarized spacegray-theme smyx-theme seoul256-theme railscasts-reloaded-theme railscasts-theme pastelmac-theme noctilux-theme obsidian-theme mbo70s-theme liso-theme majapahit-theme lavender-theme mellow-theme material-theme color-theme-sanityinc-tomorrow dtrt-indent markdown-mode buffer-move yaml-mode ido-vertical-mode yasnippet auto-indent-mode web-mode multiple-cursors magit)))
 '(pos-tip-foreground-color "#272822")
 '(safe-local-variable-values (quote ((require-final-newline))))
 '(text-mode-hook (quote (text-mode-hook-identify)))
 '(weechat-color-list
   (unspecified "#272822" "#3C3D37" "#F70057" "#F92672" "#86C30D" "#A6E22E" "#BEB244" "#E6DB74" "#40CAE4" "#66D9EF" "#FB35EA" "#FD5FF0" "#74DBCD" "#A1EFE4" "#F8F8F2" "#F8F8F0")))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(cperl-array-face ((t (:foreground "color-208"))))
 '(cperl-hash-face ((t (:foreground "color-208"))))
 '(ediff-current-diff-C ((t (:background "color-17"))))
 '(git-gutter+-added ((t (:inherit default :background "color-234" :foreground "green"))))
 '(git-gutter+-deleted ((t (:inherit bold :background "color-234" :foreground "#FF1493"))))
 '(git-gutter+-modified ((t (:inherit bold :background "color-234" :foreground "#5FD7FF"))))
 '(golden-ratio-scroll-highlight-line-face ((t (:background "color-236"))))
 '(hl-line ((t (:background "color-235"))))
 '(smerge-refined-added ((t (:inherit smerge-refined-change :background "color-22")))))

;; Create the autosave and backups dirs if necessary, since emacs won't.
(make-directory "~/.emacs.d/autosaves/" t)
(make-directory "~/.emacs.d/backups/" t)

;; Do not create lock files (files that begin with `.#`) because they make
;; `git status` annoying when emacs has a file open.
(setq create-lockfiles nil)


;;; MISC

(fset 'yes-or-no-p 'y-or-n-p) ;; Make all "yes or no" prompts show "y or n" instead

(add-hook 'before-save-hook 'delete-trailing-whitespace) ;; remove trailing whitespace on save

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

;; Set # as the default comment syntax (mainly for *scratch*)
(setq-default comment-start "# ")

;; The following line will make emacs always open the *scratch* buffer on start,
;; but that means when you edit a commit message it opens *scratch* instead of
;; COMMIT_EDITMSG.
;; (setq initial-buffer-choice t)
