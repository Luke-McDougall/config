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
  ;;(add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  )

(package-initialize)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(initial-frame-alist (quote ((fullscreen . maximized))))
 '(package-selected-packages (quote (counsel racer rust-mode modus-vivendi-theme evil))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; Ivy
(ivy-mode 1)
(setq ivy-use-virtual-buffers t)
(setq enable-recursive-minibuffers t)
(setq ivy-wrap t)

(define-key ivy-minibuffer-map (kbd "TAB") 'ivy-next-line)
(define-key ivy-minibuffer-map (kbd "RET") 'ivy-alt-done)

;; Evil mode
(setq evil-want-integration t)
(setq evil-want-keybinding nil)
(require 'evil)
(when (require 'evil-collection nil t)
  (evil-collection-init))
(evil-mode 1)
(setq evil-vsplit-window-right t
	  evil-complete-all-buffers nil)

(defun jump_to_closing_paren ()
  (interactive)
  (while (not (or (eq ?}  (char-after))
		  (eq ?\) (char-after))
		  (eq ?\" (char-after))
		  (eq ?>  (char-after))
		  (eq ?\] (char-after))))
	(forward-char 1)
  )
  (forward-char 1)
)

(defun save-and-kill-focused-buffer ()
  (interactive)
  (save-buffer)
  (kill-buffer nil)
 )

(defun open_config ()
  (interactive)
  (evil-edit "~/.emacs")
)

(defun switch-theme-dark ()
  (interactive)
  (load-theme 'modus-vivendi t)
)

(defun switch-theme-light ()
  (interactive)
  (load-theme 'modus-operandi t)
)

;; Evil key bindings
(define-key evil-normal-state-map "  " 'counsel-find-file)
(define-key evil-normal-state-map " b" 'counsel-switch-buffer)

(define-key evil-normal-state-map " l" 'evil-window-right)
(define-key evil-normal-state-map " h" 'evil-window-left)
(define-key evil-normal-state-map " j" 'evil-window-down)
(define-key evil-normal-state-map " k" 'evil-window-up)
(define-key evil-normal-state-map " q" 'save-and-kill-focused-buffer)
(define-key evil-normal-state-map " v" 'evil-window-vsplit)
(define-key evil-normal-state-map "H" 'evil-first-non-blank-of-visual-line)
(define-key evil-normal-state-map "L" 'evil-end-of-visual-line)
(define-key evil-normal-state-map "\C-k" 'evil-backward-paragraph)
(define-key evil-normal-state-map "\C-j" 'evil-forward-paragraph)
(define-key evil-normal-state-map [left] 'evil-prev-buffer)
(define-key evil-normal-state-map [right] 'evil-next-buffer)
(define-key evil-insert-state-map "\C-j" 'jump_to_closing_paren)

(define-key evil-normal-state-map ";" 'evil-ex)
(evil-ex-define-cmd "config" 'open_config)
(evil-ex-define-cmd "dark" 'switch-theme-dark)
(evil-ex-define-cmd "light" 'switch-theme-light)

;; C mode
(setq c-basic-offset 4)
(setq c-default-style "k&r")
(defun c-hook()
    (c-toggle-electric-state t)
    )
(add-hook 'c-mode-common-hook 'c-hook)

;; Rust mode
(require 'rust-mode)
(add-hook 'rust-mode-hook #'racer-mode)
(add-hook 'rust-mode-hook (lambda ()
			    (setq indent-tabs-mode nil)))
(add-hook 'racer-mode-hook #'eldoc-mode)
(setq racer-rust-src-path "/home/luke/.rustup/toolchains/stable-x86_64-unknown-linux-gnu/lib/rustlib/src/rust/src")
(define-key rust-mode-map "gd" 'racer-find-definition)

(setq show-paren-delay 0)
(show-paren-mode 1)
(setq scroll-step 1
      scroll-conservatively 10000) ;; Keyboard scrolls one line at a time
(global-display-line-numbers-mode 1)
(setq display-line-numbers-type `relative)
(electric-pair-mode t)
(menu-bar-mode -1)
(toggle-scroll-bar -1)
(tool-bar-mode -1)
(load-theme 'modus-operandi t)
