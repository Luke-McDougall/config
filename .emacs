;; Apparently the garbage collector makes start up slow in Emacs
;; this will temporarily disable it to make start up faster `gcmh-mode'
;; is used to reset the garbage collector at the end of this file.
(setq gc-cons-threshold most-positive-fixnum)
(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")

(require 'package)
(setq package-archives '(("ELPA"  . "http://tromey.com/elpa/")
			 ("gnu"   . "http://elpa.gnu.org/packages/")
			 ("melpa" . "https://melpa.org/packages/")
			 ("org"   . "https://orgmode.org/elpa/")))
(package-initialize)

;; Make sure use package is installed
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

(defun save-and-kill-buffer ()
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
  (start-process "*terminal*" nil "alacritty" "--working-directory" default-directory)
)

(defun rg-find-file ()
  (interactive)
  (setq dir (if (eq (vc-root-dir) nil) default-directory (vc-root-dir)))
  (setq file
        (ido-completing-read "Open: "
                             (mapcar 'abbreviate-file-name
                                     (split-string (shell-command-to-string (concat "rg --files " dir)) "\n"))))
  (find-file file)
)

(defun luke/open-project ()
  (interactive)
  (mapcar 'find-file
          (split-string (shell-command-to-string "filename_searcher ~/OOSE/Worksheet_3/AddressBookApp \"^.+\\.java$\"") "\n"))
)

(defun revert-buffer-no-confirm ()
  (interactive)
  (revert-buffer nil t nil))

;; Evil-mode
(use-package evil
  :ensure t
  :init (setq evil-vsplit-window-right t
              evil-split-window-below t
              evil-emacs-state-modes nil)

  ;; Center point after any jumps
  (defun my-center-line (&rest _)
    (evil-scroll-line-to-center nil))

  (advice-add 'evil-search-next :after #'my-center-line)
  (advice-add 'evil-jump-forward :after #'my-center-line)
  (advice-add 'evil-jump-backward :after #'my-center-line)


  (defun switch-to-window-occur (&rest _)
    "It annoys me that I have to switch to the occur buffer manually"
    (select-window (get-buffer-window "*Occur*"))
  )
  (advice-add 'occur :after #'switch-to-window-occur)

  :config (evil-ex-define-cmd "light" 'switch-theme-light)
	  (evil-ex-define-cmd "dark" 'switch-theme-dark)
	  (evil-ex-define-cmd "config" '(lambda ()
	  				(interactive)
	  				(evil-edit "~/.emacs")))
	  (evil-mode 1)
  :bind (:map evil-normal-state-map
              ;; Movement commands
	      ("H"         . 'evil-first-non-blank-of-visual-line)
	      ("L"         . 'evil-end-of-visual-line)
	      ("C-j"       . 'evil-forward-paragraph)
	      ("C-k"       . 'evil-backward-paragraph)

              ;; prefix-w for 'window' commands
	      ("SPC w h"   . 'evil-window-left)
	      ("SPC w l"   . 'evil-window-right)
	      ("SPC w k"   . 'evil-window-up)
	      ("SPC w j"   . 'evil-window-down)
	      ("SPC w v"   . 'evil-window-vsplit)
	      ("SPC w s"   . 'evil-window-split)
	      ("SPC w q"   . 'delete-window)
	      ("SPC w w"   . 'delete-other-windows)

              ;; prefix-b for 'buffer' commands
	      ("SPC b s"   . 'switch-to-buffer)
	      ("SPC b o"   . 'switch-to-buffer-other-window)
	      ("SPC b i"   . 'ibuffer-other-window)
	      ("SPC b e"   . 'eval-buffer)
	      ("SPC b q"   . 'kill-this-buffer)
	      ("SPC b k a" . 'kill-all-buffers)
	      ("SPC b x"   . 'save-and-kill-buffer)
	      ("SPC b r"   . 'revert-buffer-no-confirm)

              ;; Prefix-f for 'find' commands
	      ("SPC f r"   . 'ido-find-recent-file)
	      ("SPC f o"   . 'find-file-other-window)
	      ("SPC f f"   . 'ido-find-file)
	      ("SPC f l"   . 'find-library)

              ;; prefix-e for 'error' commands
              ("SPC e n"   . 'next-error)
	      ("SPC e p"   . 'previous-error)

              ;; prefix-j for 'jump' commands
              ("SPC j n"   . 'evil-jump-forward)
              ("SPC j p"   . 'evil-jump-backward)

              ;; prefix-d for 'dired' commands
              ("SPC d s"   . 'open-dired-in-side-window)
              ("SPC d d"   . 'dired)

              ;; Miscellaneous
	      ("SPC SPC"   . 'ido-find-file)
	      ("SPC \r"    . 'open-terminal-in-default-directory)
	      ("SPC o"     . 'occur)
              ("<f5>"      . 'compile)
	      ([left]      . 'evil-prev-buffer)
	      ([right]     . 'evil-next-buffer)
	      (";"         . 'evil-ex)
	      :map evil-insert-state-map
	      ("C-j"       . 'jump-to-closing-paren)
	      ("C-k"       . 'evil-normal-state)
          )
)

(use-package evil-snipe
  :ensure t
  :config
  (evil-snipe-mode +1))

(use-package window
  :init
  (setq display-buffer-alist
        '(("\\*Ibuffer*"
           (display-buffer-in-side-window)
           (window-height . 0.2)
           (side . bottom)
           (slot . 0))))
  :bind (("<f8>" . window-toggle-side-windows))
)

(use-package emacs
  :config
  (setq mode-line-percent-position nil)
  ;;(defvar modified-buffer-format (propertize "%b" 'face 'mode-line-emphasis))
  (setq-default mode-line-format
                '("%e"
                  mode-line-front-space
                  (:eval
                   (eyebrowse-mode-line-indicator))
                  " ["
                  (:eval
                   (cond
                    ((eq evil-state 'normal) "NORMAL")
                    ((eq evil-state 'visual) "VISUAL")
                    ((eq evil-state 'insert) "INSERT")))
                   "] ["
                   (:eval
                    (if (buffer-modified-p) "%b | +" "%b"))
                   "] "
                   buffer-file-truename
                   "  "
                   ;;mode-name
                   (:eval
                    (all-the-icons-icon-for-buffer))
                   "    "
                   "%I "
                   mode-line-end-spaces))
)

(use-package org
  :init
  (defun org-buffer-map ()
    (define-key evil-insert-state-local-map (kbd "M-h") 'org-do-promote)
    (define-key evil-insert-state-local-map (kbd "M-l") 'org-do-demote)
    (define-key evil-normal-state-local-map (kbd "C-u") 'outline-up-heading)
    (define-key evil-normal-state-local-map (kbd "C-j") 'org-next-visible-heading)
    (define-key evil-normal-state-local-map (kbd "C-k") 'org-previous-visible-heading)
    (define-key evil-normal-state-local-map (kbd "SPC s w") 'flyspell-correct-word-before-point)
    (define-key xah-math-input-keymap (kbd "S-SPC") nil)
    (define-key xah-math-input-keymap (kbd "<f1>") 'xah-math-input-change-to-symbol)
    (xah-math-input-mode 1)
    (auto-fill-mode 1)
    (flyspell-mode 1)
  )
  :hook ((org-mode . org-buffer-map))
)

(use-package which-key
  :ensure t
  :init (which-key-mode))

(use-package ibuffer
  :init (setq ibuffer-expert t)
  (defun ibuffer-buffer-map ()
    (local-unset-key (kbd "SPC"))
    (define-key evil-normal-state-local-map (kbd "J") 'ibuffer-jump-to-buffer)
    (define-key evil-normal-state-local-map (kbd "j") 'ibuffer-forward-line)
    (define-key evil-normal-state-local-map (kbd "k") 'ibuffer-backward-line)
    (define-key evil-normal-state-local-map (kbd "q") 'kill-this-buffer)
  )
  :hook ((ibuffer . ibuffer-buffer-map))
)

(use-package replace
  :init
  (defun occur-buffer-map ()
    "Keybindings for occur buffer"
    (define-key evil-normal-state-local-map "e" 'occur-edit-mode)
    (define-key evil-normal-state-local-map [mouse-2] 'occur-mode-mouse-goto)
    (define-key evil-normal-state-local-map "\r" 'occur-mode-goto-occurrence)
    (define-key evil-normal-state-local-map "\C-o" 'occur-mode-display-occurrence)
    (define-key evil-normal-state-local-map "r" 'occur-rename-buffer)
    (define-key evil-normal-state-local-map  "c" 'clone-buffer)
    (define-key evil-normal-state-local-map (kbd "SPC m e") 'next-error-follow-minor-mode)
  )
  :hook ((occur-mode . occur-buffer-map))
)

(use-package dired-subtree
  :ensure t
  :init (setq dired-subtree-line-prefix "--"))

(use-package dired
  :init
  (defun dired-buffer-map ()
    "Setup bindings for dired buffer."
    (interactive)
    (local-unset-key (kbd "SPC"))
    (local-unset-key (kbd "\r"))
    (define-key evil-normal-state-local-map "l" 'dired-subtree-insert)
    (define-key evil-normal-state-local-map "h" 'dired-subtree-remove)
    (define-key evil-normal-state-local-map "q" 'kill-this-buffer)
    (define-key evil-normal-state-local-map "c" 'dired-do-copy)
    (define-key evil-normal-state-local-map "C" 'dired-do-compress-to)
    (define-key evil-normal-state-local-map (kbd "\r") 'dired-find-file)
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
    (select-window (get-buffer-window "*Dired-Side*"))
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

(use-package fish-mode
  :ensure t)

;; Recentf
(use-package recentf
  :init
  (setq recentf-max-menu-items 10)
  (setq recentf-max-saved-items 25)
  :config
  (recentf-mode 1)
)

(use-package eyebrowse
  :ensure t
  :config (eyebrowse-setup-opinionated-keys)
  (eyebrowse-mode t)
  )

;; Ido
(use-package ido
  :init
  (setq ido-enable-flex-matching t)
  ;; Stop ido from doing bad things
  (setq ido-auto-merge-work-directories-length -1)
  (setq ido-create-new-buffer 'always)
  (setq ido-use-virtual-buffers 'auto)
  (setq ido-ignore-extensions t)
  (setq ido-everywhere t)

  (defun ido-my-keys ()
    "Add my key bindings for Ido."
    (define-key ido-completion-map (kbd "TAB") 'ido-next-match))

  (defun ido-find-recent-file ()
    "Interactively open a recent file."
    (interactive)
    (let ((file
           (ido-completing-read "Choose recent file: "
                                (mapcar 'abbreviate-file-name recentf-list) nil t)))
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
  :ensure t
  :config
  ;; Very minor modification of the function in `all-the-icons.el' to make it use ido.
  (defun ido-all-the-icons-insert (&optional arg family)
    "Interactive icon insertion function.
  When Prefix ARG is non-nil, insert the propertized icon.
  When FAMILY is non-nil, limit the candidates to the icon set matching it."
    (interactive "P")
    (let* ((standard-output (current-buffer))
           (candidates (if family
                           (all-the-icons--read-candidates-for-family family)
                         (all-the-icons--read-candidates)))
           (prompt     (if family
                           (format "%s Icon: " (funcall (all-the-icons--family-name family)))
                         "Icon : "))
  
           (selection (ido-completing-read prompt candidates nil t)) ;;This is the only change
           (result    (cdr (assoc selection candidates))))
  
      (if arg (prin1 result) (insert result))))
  )

;; Paren zone
(electric-pair-mode 1)
(setq-default show-paren-delay 0)
(show-paren-mode 1)

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

(use-package xah-math-input
  :ensure t)

;; Clean Screen
;;(add-to-list 'default-frame-alist '(font . "DejaVu Sans Mono-11"))
(setq inhibit-startup-screen t)
(setq scroll-conservatively 100)
(menu-bar-mode -1)
(tool-bar-mode -1)
(toggle-scroll-bar -1)
(global-display-line-numbers-mode t)
(setq-default display-line-numbers-type 'relative)
(load-theme 'modus-operandi)

;; Turn the garbage collector back on
(add-to-list 'load-path "~/.emacs.d/gcmh")
(require 'gcmh)
(gcmh-mode 1)
