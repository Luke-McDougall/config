(setq gc-cons-threshold most-positive-fixnum)

(require 'package)
(setq package-archives '(("ELPA"  . "http://tromey.com/elpa/")
                         ("gnu"   . "http://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")
                         ("org"   . "https://orgmode.org/elpa/")))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

(use-package cus-edit
  :config
  (setq custom-file "~/.emacs.d/custom.el")

  (unless (file-exists-p custom-file)
    (write-region "" nil custom-file))

  (load custom-file))

(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))

;; Disabling some default emacs keys to stop them messing with the ones below
(global-unset-key (kbd "C-w"))

(defun c-mode-buffer-bindings ()
  (define-key c-mode-base-map (kbd "C-d") nil)
  )
(add-hook 'c-mode-common-hook 'c-mode-buffer-bindings)

(use-package emacs
  :bind (("M-<right>" . forward-word)               ;; Jumps to the first character after the current/next word
         ("M-<left>"  . backward-word)              ;; Jumps to the first character of the current/previous word
         ("M-<down>"  . forward-paragraph)          ;; Jumps to the next empty line
         ("M-<up>"    . backward-paragraph)         ;; Jumps to the previous empty line
         ("C-<right>" . move-end-of-line)           ;; Jumps to end of current line
         ("C-<left>"  . move-beginning-of-line)     ;; Jumps to beginning of current line
         ("C-s"       . save-buffer)                ;; Saves current buffer to file if it's been modified
         ("C-d"       . kill-region)                ;; Cut hilighted text (this would be C-x but Emacs won't let me)
         ("C-k"       . kill-whole-line)            ;; Delete current line. Can be pasted with C-v
         ("C-y"       . kill-ring-save)             ;; Copy hilighted text. (I have failed, I can't figure out how to make C-c work everywhere)
         ("C-v"       . yank)                       ;; Paste previously cut/copied text
         ("C-z"       . undo-tree-undo)             ;; Undo latest change
         ("C-r"       . undo-tree-redo)             ;; Redo latest undo
         ("C-t"       . treemacs)                   ;; Launch file explorer
         ("C-w w"     . delete-other-windows)       ;; Close all windows except the focused one
         ("C-w q"     . delete-window)              ;; Close the focused window
         ("C-w v"     . split-window-right)         ;; Vertically split window
         ("<f5>"      . dot-bat-compile)            ;; Run a build.bat in the same directory as the current file
         ("<f8>"      . window-toggle-side-windows) ;; Toggle display of treemacs
         )
  )

;; This is the custom compile function called by pressing F5
(defun dot-bat-compile ()
  (interactive)
  (compile "build.bat"))

(use-package which-key
  :ensure t
  :init
  (which-key-mode))

(use-package ivy
  :ensure t
  :init
  (setq ivy-use-virtual-buffers nil)
  (setq enable-recursive-minibuffers t)
  :bind (("C-b" . ivy-switch-buffer) ;; Interactively switch to an open buffer
         :map ivy-minibuffer-map
         ("\r"  . ivy-alt-done)      ;; When completing a file name either opens the file or starts a new search in the selected directory 
         )
  :config
  (ivy-mode 1))

(use-package counsel
  :ensure t
  :init
  (global-unset-key (kbd "M-x"))
  :bind (("C-o" . counsel-find-file) ;; Interactively open a file
         ("M-x" . counsel-M-x)       ;; Exception to the don't rebind M-x rule because counsel-M-x is a replacement
         )
  )     

(use-package swiper
  :ensure t
  :bind (("C-f" . swiper-isearch) ;; Search text of current buffer and jump to match
         )
  )

(use-package undo-tree
  :ensure t
  :config
  (global-undo-tree-mode))

(use-package org
  :init
  (defun export-init-file-from-org ()
    (interactive)
    (org-babel-tangle nil "~/.emacs.d/init.el" nil))

  (defun org-buffer-hook ()
    (auto-fill-mode 1))
  :hook ((org-mode . org-buffer-hook)))

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1))

(use-package dashboard
  :ensure t
  :config
  (setq dashboard-banner-logo-title "Emacs (The c stands for Cum)")
  (setq dashboard-set-footer nil)
  (setq dashboard-items '((recents . 5)))
  (dashboard-setup-startup-hook))

(use-package doom-themes
  :ensure t
  :config
  (setq doom-themes-enable-bold t
        doom-themes-enable-itlic t)
  (load-theme 'doom-one t)
  (setq doom-themes-treemacs-theme "doom-colors");; Theme for treemacs
  (doom-themes-treemacs-config)
  (doom-themes-org-config))

(tool-bar-mode -1)
(toggle-scroll-bar -1)
;; (menu-bar-mode -1)

(use-package treemacs
  :ensure t
  :defer t
  )

(use-package company
  :ensure t
  :config
  (add-hook 'after-init-hook 'global-company-mode))

(electric-pair-mode 1)

(setq-default show-paren-delay 0)
(show-paren-mode 1)

(setq-default indent-tabs-mode nil)
(setq-default c-basic-offset 4)
(setq-default c-default-style "k&r")

(add-hook 'after-init-hook 'global-hl-line-mode)

(setq scroll-conservatively 100)

(use-package recentf
  :init
  (setq recentf-max-menu-items 10)
  (setq recentf-max-saved-items 25)
  (setq recentf-exclude '(".+autoloads\.el"
                          "ido\.last"
                          "\.cache/.+"))
  :config
  (recentf-mode 1)
  )

(use-package ido
  :init
  (setq ido-enable-flex-matching t)
  ;; Stop ido from doing bad things
  (setq ido-auto-merge-work-directories-length -1)
  (setq ido-create-new-buffer 'always)
  (setq ido-use-virtual-buffers nil)
  (setq ido-ignore-buffers '("\*.+\*"))
  (setq ido-ignore-extensions t)
  (setq ido-everywhere t)
  :config
  (ido-mode 1)
)

(add-to-list 'default-frame-alist '(fullscreen . maximized))

(add-to-list 'load-path "~/.emacs.d/gcmh")
(require 'gcmh)
(gcmh-mode 1)
