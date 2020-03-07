;; Apparently the garbage collector makes start up slow in Emacs
;; this will temporarily disable it to make start up faster `gcmh-mode'
;; is used to reset the garbage collector at the end of this file.
(setq gc-cons-threshold most-positive-fixnum)
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
    ("3d4cf45ee28dc5595d8f0a37fc0da519365fd88a2bb98f5c272a50aba86d319b" "0e435534351b0cb0ffa265d4cfea16b4b8fe972f41ec6c51423cdf653720b165" default)))
 '(initial-frame-alist (quote ((fullscreen . maximized))))
 '(package-selected-packages
   (quote
    (async which-key all-the-icons modus-operandi modus-vivendi smex use-package modus-operandi-theme modus-vivendi-theme undo-tree evil))))

;; Why is this empty?
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; Make sure use package is installed
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

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

(defun kill-all-buffers ()
  "It kills all the buffers."
  (interactive)
  (mapc 'kill-buffer (buffer-list)))

(defun open-terminal-in-default-directory ()
  "Opens a terminal (alacritty) in the default-directory of the current buffer."
  (interactive)
  (setq command (concat "alacritty --working-directory " default-directory))
  (shell-command command)
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
	      ("H"         . 'evil-first-non-blank-of-visual-line)
	      ("L"         . 'evil-end-of-visual-line)
	      ("SPC h"     . 'evil-window-left)
	      ("SPC l"     . 'evil-window-right)
	      ("SPC k"     . 'evil-window-up)
	      ("SPC j"     . 'evil-window-down)
	      ("SPC w v"   . 'evil-window-vsplit)
	      ("SPC w h"   . 'evil-window-split)
	      ("SPC w q"   . 'delete-window)
	      ("SPC w w"   . 'delete-other-windows)
	      ("SPC b s"   . 'switch-to-buffer)
	      ("SPC b e"   . 'eval-buffer)
	      ("SPC b q"   . 'kill-this-buffer)
	      ("SPC b k a" . 'kill-all-buffers)
	      ("SPC b x"   . 'save-and-kill-focused-buffer)
	      ("SPC f r"   . 'ido-find-recent-file)
	      ("SPC SPC"   . 'ido-find-file)
	      ("SPC \r"    . 'open-terminal-in-default-directory)
	      ("SPC e n"   . 'next-error)
	      ("SPC e p"   . 'previous-error)
	      ("SPC d"     . 'open-dired-in-side-window)
	      ("C-j"       . 'evil-forward-paragraph)
	      ("C-k"       . 'evil-backward-paragraph)
              ("<f5>"      . 'compile)
	      ([left]      . 'evil-prev-buffer)
	      ([right]     . 'evil-next-buffer)
	      (";"         . 'evil-ex)
	      :map evil-insert-state-map
	      ("C-j"       . 'jump-to-closing-paren)
          )
)

(use-package which-key
  :ensure t
  :init (which-key-mode))

(use-package dired-subtree
  :ensure t
  :init (setq dired-subtree-line-prefix "--"))

(use-package dired
  :init
  (defun dired-buffer-map ()
    "Setup bindings for dired buffer."
    (interactive)
    (local-unset-key (kbd "SPC"))
    (define-key evil-normal-state-local-map "l" 'dired-subtree-insert)
    (define-key evil-normal-state-local-map "h" 'dired-subtree-remove)
    (define-key evil-normal-state-local-map "q" 'kill-this-buffer)
    (define-key evil-normal-state-local-map (kbd "TAB") 'dired-subtree-cycle)
    (define-key evil-normal-state-local-map (kbd "C-j") 'dired-subtree-down)
    (define-key evil-normal-state-local-map (kbd "C-k") 'dired-subtree-up))

  (defun open-dired-in-side-window ()
    (interactive)
    (setq dir (if (eq (vc-root-dir) nil) (dired-noselect default-directory) (dired-noselect (vc-root-dir))))
    (display-buffer-in-side-window dir
                                   `((side . left)
                                     (slot . -1)
                                     (window-width . 0.16)))
    (with-current-buffer dir
      (rename-buffer "*Dired-Side*"))
    (other-window 1)
    )

  :config
  (setq dired-recursive-copies 'always)
  (setq dired-recursive-deletes 'always)
  (setq dired-delete-by-moving-to-trash t)
  (setq dired-listing-switches "-AFlv --group-directories-first")
  :hook ((dired-mode . dired-hide-details-mode)
	 (dired-mode . hl-line-mode)
	 (dired-mode . dired-buffer-map))
)

(use-package async
  :ensure t)

(use-package dired-async
  :after (dired async)
  :hook (dired-mode . dired-async-mode))

;; C mode
(setq-default indent-tabs-mode nil)
(setq-default c-basic-offset 4)
(setq-default c-default-style "k&r")

(defun java-custom-indent-settings ()
  "My preferred settings for indentation of java code."
  (c-set-offset 'substatement-open 0)
  (c-set-offset 'case-label '+)
  (c-set-offset 'inline-open 0)
  (c-set-offset 'statement-case-intro 0)
)

(add-hook 'java-mode-hook 'java-custom-indent-settings)

(use-package rust-mode
  :ensure t)

;; Recentf
(use-package recentf
  :init
  (setq recentf-max-menu-items 10)
  (setq recentf-max-saved-items 25)
  :config
  (recentf-mode 1)
)

;; Ido
(use-package ido
  :init
  (setq ido-enable-flex-matching t)
  (setq ido-auto-merge-work-directories-length -1)
  (setq ido-create-new-buffer 'always)
  (setq ido-use-virtual-buffers 'auto)
  (setq ido-everywhere t)

  (defun ido-my-keys ()
  "Add my key bindings for Ido."
  (define-key ido-completion-map (kbd "TAB") 'ido-next-match))

  (defun ido-find-recent-file ()
    "Interactively open a recent file."
    (interactive)
    (let ((file (ido-completing-read "Choose recent file: " recentf-list nil t)))
      (when file
	(find-file file))))

  :config
  (add-hook 'ido-setup-hook 'ido-my-keys)
  (ido-mode 1)
)

(use-package smex
  :ensure t
  :init
  (global-set-key (kbd "M-x") 'smex)
)

(use-package all-the-icons
  :ensure t)

;; Paren zone
(electric-pair-mode 1)
(setq-default show-paren-delay 0)
(show-paren-mode 1)

(global-display-line-numbers-mode t)
(setq-default display-line-numbers-type 'relative)

(use-package modus-vivendi-theme
  :ensure t
  :config
  (defun switch-theme-dark ()
  "Pretty self explanatory dude."
    (interactive)
    (load-theme 'modus-vivendi))
)

(use-package modus-operandi-theme
  :ensure t
  :config
  (defun switch-theme-light ()
    "Pretty self explanatory dude."
    (interactive)
    (load-theme 'modus-operandi))
)

;; Clean Screen
(setq inhibit-startup-screen t)
(setq scroll-conservatively 100)
(menu-bar-mode -1)
(tool-bar-mode -1)
(toggle-scroll-bar -1)
(display-time-mode 1)
(load-theme 'modus-vivendi)

;; Turn the garbage collector back on
(add-to-list 'load-path "~/.emacs.d/gcmh")
(require 'gcmh)
(gcmh-mode 1)
