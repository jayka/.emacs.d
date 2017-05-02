;;; Commentary : init file for emacs
;;; Code: code


(require 'package)
(setq package-initialize-at-startup nil)
;; add /usr/local/bin to path
(if (eq system-type 'darwin)
    (setq exec-path (append '("/usr/local/bin") exec-path)))

;;(setq temporary-file-directory "~/.emacs.d/emacs-backup")

(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
			 ;;("melpa-stable" . "http://melpa-stable.milkbox.net/packages/")
			 ("melpa" . "http://melpa.org/packages/")
			 ))

(package-initialize)

;; Setup packages
(setq my-packages '(elpy
		    redo+
		    haskell-mode
		    key-chord
		    ivy
		    ivy-hydra
		    counsel
		    magit
		    js2-mode
		    ;;powerline
		    cider
		    hc-zenburn-theme
		    monokai-theme
    		    )) 


;; install packages

(dolist (package my-packages)
  (unless (package-installed-p package)
    (package-refresh-contents)
    (message "Installing Package %s" package)
    (package-install package)
    (message ".done")))

;; tern mode for javascript and nodejs
;; tern mode configs are in mode-setup section below
(add-to-list 'load-path "~/.emacs.d/tern/emacs/")
    
;; ======== UI SETUP ============

;; set a normal readable font size
(set-face-attribute 'default nil :font "Source Code Pro for Powerline" :weight 'light :height 110)

;;; I prefer cmd key for meta
(setq mac-option-key-is-meta nil
      mac-command-key-is-meta t
      mac-command-modifier 'meta
      mac-option-modifier 'super)

;; the toolbar is just a waste of valuable screen estate
;; in a tty tool-bar-mode does not properly auto-load, and is
;; already disabled anyway
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))

(setq initial-frame-alist '( (left . 0) (top . 0) (height . 60) (width . 100) ))

(menu-bar-mode -1)

;; the blinking cursor is nothing, but an annoyance
(blink-cursor-mode -1)

;; disable startup screen
(setq inhibit-startup-screen t)

;; nice scrolling
(setq scroll-margin 0
      scroll-conservatively 100000
      scroll-preserve-screen-position 1
      scroll-bar-width 5)

(scroll-bar-mode -1)

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
(load-theme 'hc-zenburn t)		    

;; powerline
;;(setq powerline-arrow-shape 'curve)
;;(setq powerline-default-separator-dir '(right . left))
;; 
;;(setq sml/theme 'powerline)
;;(sml/setup)

;; ============ MODE SETUP ===========
(require 'cider)

(require 'redo+)
(elpy-enable)
(cua-mode 1)
(show-paren-mode 1)
(setq show-paren-delay 0)

(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

(add-hook 'after-init-hook 'global-company-mode)

;; tern-mode
;;(autoload 'tern-mode "tern.el" nil t)
;;(add-hook 'js2-mode-hook (lambda () (tern-mode t)))
;;(eval-after-load 'tern
;;   '(progn
;;      (require 'tern-auto-complete)
;;      (tern-ac-setup)))


;; ivy mode
(ivy-mode 1)
(setq ivy-use-virtual-buffers t)
(setq ivy-count-format "(%d/%d) ")
(global-set-key (kbd "C-s") 'swiper)
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "<f1> f") 'counsel-describe-function)
(global-set-key (kbd "<f1> v") 'counsel-describe-variable)
(global-set-key (kbd "<f1> l") 'counsel-find-library)
(global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
(global-set-key (kbd "<f2> u") 'counsel-unicode-char)
(global-set-key (kbd "C-c g") 'counsel-git)
(global-set-key (kbd "C-c j") 'counsel-git-grep)
(global-set-key (kbd "C-c k") 'counsel-ag)
(global-set-key (kbd "C-x l") 'counsel-locate)


;; Save History across sessions
(savehist-mode 1)
(if (not (file-exists-p "~/.emacs.d/tmp")) (mkdir "~/.emacs.d/tmp"))
(if (not (file-exists-p "~/.emacs.d/tmp/savehist")) (write-file "~/.emacs.d/tmp/savehist"))
(setq savehist-file "~/.emacs.d/tmp/savehist")


;; enable recentfiles list mode
(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-items 100)
;;(global-set-key "C-x C-r" 'recentf-open-files)

;; ======== KEY BINDINGS =========
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

;; emacs debug enable
;;(setq debug-on-error t)

;; store all backup and autosave files in the tmp dir
(setq backup-directory-alist '((".*" . "~/.emacs.d/emacs-backup")))
;; (setq auto-save-file-name-transforms '((".*" , "~/.emacs.d/emacs-backup" t)))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (monokai-theme hc-zenburn-theme cider redo+ magit key-chord js2-mode ivy-hydra haskell-mode elpy counsel))))
