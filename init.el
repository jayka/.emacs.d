;;; Commentary : init file for emacs
;;; Code: code

;;;(load "~/.emacs.d/prelude/init.el")
;;(require 'package)
;;(add-to-list 'package-archives
;;             '("elpy" . "http://jorgenschaefer.github.io/packages/"))

(require 'package)
(setq package-initialize-at-startup nil)
(package-initialize)

(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
			  ("melpa-stable" . "http://melpa.milkbox.net/packages/")))

;; Setup packages
(setq my-packages '(elpy
		    popup)) 

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
;; load theme
(load-theme 'tango-dark)		    

;;(defun company-my-backend (command &optional arg &rest ignored)
;;  (pcase command
;;    (`prefix (when (looking-back "foo\\>")
;;              (match-string 0)))
;;    (`candidates (list "foobar" "foobaz" "foobarbaz"))
;;    (`meta (format "This value is named %s" arg))))


;; ========= SHELL SETUP ==============

;; Sets your shell to use cygwin's bash, if Emacs finds it's running
;; under Windows and c:\cygwin exists. Assumes that C:\cygwin\bin is
;; not already in your Windows Path (it generally should not be).
;;

(let*
    ((cygwin-root "c:/cygwin")
     (cygwin-bin
      (concat cygwin-root "/bin")))
  (when
      (and
       (eq 'windows-nt system-type)
       (file-readable-p cygwin-root))
    (setq exec-path
	  (cons cygwin-bin exec-path))
    (setenv "PATH"
	    (concat cygwin-bin ";"	;
		    (getenv "PATH")))
    ;; By default use the Windows HOME.
    ;; Otherwise, uncomment below to set a HOME
    ;;      (setenv "HOME" (concat cygwin-root "/home/eric"))
    
    ;; NT-emacs assumes a Windows shell. Change to bash.
    (setq shell-file-name "bash")
    (setenv "SHELL" shell-file-name)
    (setq explicit-shell-file-name shell-file-name)
    ;; This removes unsightly ^M characters that would otherwise
    ;; appear in the output of java applications.
    (add-hook 'comint-output-filter-functions 'comint-strip-ctrl-m)))


(elpy-enable)

(ido-mode)

(global-set-key (kbd "C-<tab>") 'ido-switch-buffer)
;; (global-linum-mode t)
