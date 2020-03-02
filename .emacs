(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")
(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  (when no-ssl (warn "\
Your version of Emacs does not support SSL connections,
which is unsafe because it allows man-in-the-middle attacks.
There are two things you can do about this warning:
1. Install an Emacs version that does support SSL and be safe.
2. Remove this warning from your init file so you won't see it again."))
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  ;; Comment/uncomment this line to enable MELPA Stable if desired.  See `package-archive-priorities`
  ;; and `package-pinned-packages`. Most users will not need or want to do this.
  (add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  )
(package-initialize)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("575d772a465e51f9ba7dd9c6213275c7aa3dc68ede1692dcd1521e5d70a7f58d" "d574db69fcc4cc241cb4a059711791fd537a959d8b75f038913639e8e006ca48" default)))
 '(initial-frame-alist (quote ((fullscreen . maximized))))
 '(package-selected-packages
   (quote
    (modus-operandi modus-vivendi smex flycheck use-package modus-operandi-theme modus-vivendi-theme undo-tree evil))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(eval-when-compile
  (require 'use-package))

(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

(defun jump-to-closing-paren ()
  "Pretty self explanatory dude."
  (interactive)
  (while (not (or (eq ?\) (char-after))
		  (eq ?\] (char-after))
		  (eq ?}  (char-after))
		  (eq ?>  (char-after))
		  (eq ?\" (char-after))))
    (forward-char 1)
  )
  (forward-char 1)
)

(defun save-and-kill-focused-buffer ()
  "Pretty self explanatory dude."
  (interactive)
  (save-buffer)
  (kill-buffer)
)

(defun dired-buffer-map ()
  "Setup bindings for dired buffer."
  (interactive)
  (define-key evil-normal-state-local-map "l" 'dired-find-file)
  (define-key evil-normal-state-local-map "h" 'dired-up-directory)
  (define-key evil-normal-state-local-map "q" 'kill-this-buffer)
)

(defun ido-my-keys ()
  "Add my key bindings for Ido."
  (define-key ido-completion-map (kbd "TAB") 'ido-next-match)
)

(defun switch-theme-dark ()
  "Pretty self explanatory dude."
  (interactive)
  (load-theme 'modus-vivendi)
)

(defun switch-theme-light ()
  "Pretty self explanatory dude."
  (interactive)
  (load-theme 'modus-operandi)
)

;; Evil
(use-package evil
  :ensure t
  :init (setq evil-vsplit-window-right t)
  :config (evil-ex-define-cmd "light" 'switch-theme-light)
	  (evil-ex-define-cmd "dark" 'switch-theme-dark)
	  (evil-ex-define-cmd "config" '(lambda ()
	  				(interactive)
	  				(evil-edit "~/.emacs")))
	  (evil-mode 1)
  :bind (:map evil-normal-state-map
	      ("H"       . 'evil-first-non-blank-of-visual-line)
	      ("L"       . 'evil-end-of-visual-line)
	      ("SPC h"   . 'evil-window-left)
	      ("SPC l"   . 'evil-window-right)
	      ("SPC k"   . 'evil-window-up)
	      ("SPC j"   . 'evil-window-down)
	      ("SPC v"   . 'evil-window-vsplit)
	      ("SPC b"   . 'switch-to-buffer)
	      ("SPC q"   . 'save-and-kill-focused-buffer)
	      ("SPC SPC" . 'ido-find-file)
	      ("C-j"     . 'evil-forward-paragraph)
	      ("C-k"     . 'evil-backward-paragraph)
	      ([left]    . 'evil-prev-buffer)
	      ([right]   . 'evil-next-buffer)
	      (";"       . 'evil-ex)
	      :map evil-insert-state-map
	      ("C-j"     . 'jump-to-closing-paren)
          )
)

(use-package dired
  :config
  (setq dired-recursive-copies 'always)
  (setq dired-recursive-deletes 'always)
  (setq dired-listing-switches "-AFlv --group-directories-first")
  :hook ((dired-mode . dired-hide-details-mode)
	 (dired-mode . hl-line-mode)
	 (dired-mode . dired-buffer-map))
)
;; C mode
(setq c-basic-offset 4
      c-default-style "k&r")

;; Ido
(use-package ido
  :init
  (setq ido-enable-flex-matching t)
  (setq ido-create-new-buffer 'always)
  (setq ido-everywhere t)
  :config
  (add-hook 'ido-setup-hook 'ido-my-keys)
  (ido-mode 1)
)

(use-package smex
  :ensure t
  :init
  (global-set-key (kbd "M-x") 'smex)
)

;; Paren zone
(electric-pair-mode 1)
(setq show-paren-delay 0)
(show-paren-mode 1)

;; Clean Screen
(setq inhibit-startup-screen t)
(setq scroll-conservatively 100)
(menu-bar-mode -1)
(tool-bar-mode -1)
(toggle-scroll-bar -1)


;;(global-display-line-numbers-mode t)
;;(setq display-line-numbers-type 'relative)
(use-package modus-vivendi-theme
  :ensure t)

(use-package modus-operandi-theme
  :ensure t
  :config
  (load-theme 'modus-operandi))
