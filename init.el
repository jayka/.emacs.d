;;; Commentary : init file for emacs
;;; Code: code

;;(load "~/.emacs.d/prelude/init.el")
;;(require 'package)
;;(add-to-list 'package-archives
;;             '("elpy" . "http://jorgenschaefer.github.io/packages/"))

(require 'package)
(setq package-initialize-at-startup nil)

(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
			 ;;("melpa-stable" . "http://melpa.milkbox.net/packages/")
			 ("melpa" . "http://melpa.org/packages/")))

(package-initialize)

;; Setup packages
(setq my-packages '(elpy
		    popup-switcher
		    redo+
		    web-mode
		    ensime
		    haskell-mode
		    key-chord
		    dirtree)) 

;; install packages

(dolist (package my-packages)
  (unless (package-installed-p package)
    (package-refresh-contents)
    (message "Installing Package %s" package)
    (package-install package)
    (message ".done")))

    
;; ======== UI SETUP ============

;; the toolbar is just a waste of valuable screen estate
;; in a tty tool-bar-mode does not properly auto-load, and is
;; already disabled anyway
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))

(menu-bar-mode -1)

;; set a normal readable font size
(set-face-attribute 'default nil :height 100)

;; the blinking cursor is nothing, but an annoyance
(blink-cursor-mode -1)

;; disable startup screen
(setq inhibit-startup-screen t)

;; nice scrolling
(setq scroll-margin 0
      scroll-conservatively 100000
      scroll-preserve-screen-position 1)

;; mode line settings
(line-number-mode t)
(column-number-mode t)
(size-indication-mode t)

;; enable y/n answers
(fset 'yes-or-no-p 'y-or-n-p)

;; more useful frame title, that show either a file or a
;; buffer name (if the buffer isn't visiting a file)
(setq frame-title-format
      '("" invocation-name (:eval (if (buffer-file-name)
                                            (abbreviate-file-name (buffer-file-name))
                                          "%b"))))

;; Kill buffer when pressing 'q' instead of just burying it
(defadvice quit-window (before quit-window-always-kill)
  "When running `quit-window', always kill the buffer."
  (ad-set-arg 0 t))
(ad-activate 'quit-window)

;; load theme
(load-theme 'tango-dark)		    


;; ============ MODE SETUP ===========
(require 'redo+)
(elpy-enable)
(ido-mode)
(show-paren-mode 1)
(setq show-paren-delay 0)
(add-hook 'after-init-hook 'global-company-mode)

;; for html editing
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(defun my-web-mode-hook ()
  "Hooks for Web mode."
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
)
(add-hook 'web-mode-hook  'my-web-mode-hook)

;; Save History across sessions
(savehist-mode 1)
(setq savehist-file "~/.emacs.d/tmp/savehist")

;; enable dirtree
(require 'dirtree)

;; enable recentfiles list mode
(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-items 100)
;;(global-set-key "C-x C-r" 'recentf-open-files)

;; ======== KEY BINDINGS =========
(global-set-key [f2] 'psw-switch-buffer)

(global-set-key (kbd "C-<prior>") (lambda() (interactive)(other-window -1)))

(global-set-key (kbd "C-<next>") (lambda() (interactive)(other-window 1)))

;; scroll without moving point
(global-set-key (kbd "M-n") (lambda() (interactive)(scroll-up 1)))
(global-set-key (kbd "M-p") (lambda() (interactive)(scroll-up -1)))

;; undo/redo+
(global-set-key (kbd "C-z") 'undo)
(global-set-key (kbd "C-S-z") 'redo)

;; Comment/Uncomment
(defun comment-source ()
  (interactive)
;;  (comment-region)
    ;;(if mark-active
;;	(comment-region)
  (beginning-of-line)
  (push-mark nil)
  (set-mark-command nil)
  (end-of-line)
  (comment-region)
  (pop-mark)
)


(global-set-key (kbd "C-/") 'comment-source);;comment-region)
(global-set-key (kbd "C-?") 'uncomment-region)

;; key binding for find-function-at-point
(global-set-key (kbd "C-h C-f") 'find-function-at-point)

;; ERLANG EDTS setup
;;(add-to-list 'load-path "~/.emacs.d/edts/")
;;(require 'edts-start)


;; scala mode setup
(require 'ensime)
(add-hook 'scala-mode-hook 'ensime-scala-mode-hook)
(add-hook 'scala-mode-hook 'linum-mode)

;;(setq ensime-auto-generate-config t)

;; emacs debug enable
;;(setq debug-on-error t)

;; store all backup and autosave files in the tmp dir
(setq backup-directory-alist '(("" . "~/.emacs.d/emacs-backup")))


;; ========= SHELL SETUP ==============

;; Sets your shell to use cygwin's bash, if Emacs finds it's running
;; under Windows and c:\cygwin exists. Assumes that C:\cygwin\bin is
;; not already in your Windows Path (it generally should not be).
;;

;;(let*
;;    ((cygwin-root "c:/cygwin")
;;     (cygwin-bin
;;      (concat cygwin-root "/bin")))
;;  (when
;;      (and
;;       (eq 'windows-nt system-type)
;;       (file-readable-p cygwin-root))
;;    (setq exec-path
;;	  (cons cygwin-bin exec-path))
;;    (setenv "PATH"
;;	    (concat cygwin-bin ";"	;
;;		    (getenv "PATH")))
;;    ;; By default use the Windows HOME.
;;    ;; Otherwise, uncomment below to set a HOME
;;    ;;      (setenv "HOME" (concat cygwin-root "/home/eric"))
;;    
;;    ;; NT-emacs assumes a Windows shell. Change to bash.
;;    (setq shell-file-name "bash")
;;    (setenv "SHELL" shell-file-name)
;;    (setq explicit-shell-file-name shell-file-name)
;;    ;; This removes unsightly ^M characters that would otherwise
;;    ;; appear in the output of java applications.
;;    (add-hook 'comint-output-filter-functions 'comint-strip-ctrl-m)))

;;(let*
;;    ((msys-root "C:/Softwares/MinGW/msys/1.0")
;;     (msys-bin
;;      (concat msys-root "/bin")))
;;  (when
;;      (and
;;       (eq 'windows-nt system-type)
;;       (file-readable-p msys-root))
;;    (setq exec-path
;;	  (cons msys-bin exec-path))
;;    (setenv "PATH"
;;	    (concat msys-bin ";"	;
;;		    (getenv "PATH")))
;;    ;; By default use the Windows HOME.
;;    ;; Otherwise, uncomment below to set a HOME
;;    ;;      (setenv "HOME" (concat cygwin-root "/home/eric"))
;;    
;;    ;; NT-emacs assumes a Windows shell. Change to bash.
;;    (setq shell-file-name "bash")
;;    (setenv "SHELL" shell-file-name)
;;    (setq explicit-shell-file-name shell-file-name)
;;    ;; This removes unsightly ^M characters that would otherwise
;;    ;; appear in the output of java applications.
;;   (add-hook 'comint-output-filter-functions 'comint-strip-ctrl-m)))


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(edts-inhibit-package-check t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


