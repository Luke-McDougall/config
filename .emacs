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
;; Comment/uncomment this line to enable MELPA Stable if desired. See `package-archive-priorities`
;; and `package-pinned-packages`. Most users will not need or want to do this.
;; (add-to-list 'package-archives (cons "melpa-stable" (concat proto "://melpa.org/packages/")) t)
)
;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(require 'neotree)
(global-set-key [f8] 'neotree-toggle)


;; Ivy
(ivy-mode 1)
(setq ivy-use-virtual-buffers t)
(setq enable-recursive-minibuffers t)
(setq ivy-wrap t)

(define-key ivy-minibuffer-map (kbd "TAB") 'ivy-next-line)
(define-key ivy-minibuffer-map (kbd "RET") 'ivy-alt-done)

;; CC Mode
(setq c-basic-offset 4)
(setq c-default-style "k&r")

;;(setq-default display-line-numbers 'visual
;;              display-line-numbers-widen t
;;              ;; this is the default
;;              display-line-numbers-current-absolute t)
;;
;;(defun noct:relative ()
;;  (setq-local display-line-numbers 'visual))
;;
;;(defun noct:absolute ()
;;  (setq-local display-line-numbers t))
;;
;;(add-hook 'evil-insert-state-entry-hook #'noct:absolute)
;;(add-hook 'evil-insert-state-exit-hook #'noct:relative)

;; Evil
(setq-default indent-tabs-mode nil
	      tab-width 4)
(setq evil-vsplit-window-right t
	  evil-complete-all-buffers nil)

(require 'evil)
(evil-mode 1)

(defun insert_paren ()
  (interactive)
  (insert "()")
  (backward-char 1)
 )

(defun insert_bracket ()
  (interactive)
  (insert "[]")
  (backward-char 1)
 )

(defun insert_quote ()
  (interactive)
  (insert "\"\"")
  (backward-char 1)
 )

(defun insert_brace_block ()
  (interactive)
  (insert "{}")
  (backward-char 1)
  (evil-ret 1)
  (evil-open-above 1)
)

(defun jump_to_closing_paren ()
  (interactive)
  (while (not (or (eq ?} (char-after))
				  (eq ?\) (char-after))
				  (eq ?\" (char-after))
				  (eq ?\] (char-after))))
	(forward-char 1)
	)
  (forward-char 1)
)

(defun open_config ()
  (interactive)
  (evil-edit "C:\\Users\\Luke\\AppData\\Roaming\\.emacs")
)
(add-hook 'c-mode-common-hook #'aggressive-indent-mode)
;; easy window navigation
(define-key evil-normal-state-map " l" 'evil-window-right)
(define-key evil-normal-state-map " h" 'evil-window-left)
(define-key evil-normal-state-map " j" 'evil-window-down)
(define-key evil-normal-state-map " k" 'evil-window-up)

;; Non fucked bindings for certain things
(define-key evil-normal-state-map " w" 'evil-write)
(define-key evil-normal-state-map " s" 'swiper)
(define-key evil-normal-state-map " q" 'evil-save-and-close)
(define-key evil-normal-state-map " v" 'evil-window-vsplit)
(define-key evil-normal-state-map " o" 'counsel-find-file)
(define-key evil-normal-state-map "H" 'evil-first-non-blank-of-visual-line)
(define-key evil-normal-state-map "L" 'evil-end-of-visual-line)
(define-key evil-normal-state-map "\C-k" 'evil-backward-paragraph)
(define-key evil-normal-state-map "\C-j" 'evil-forward-paragraph)
(define-key evil-normal-state-map [left] 'evil-prev-buffer)
(define-key evil-normal-state-map [right] 'evil-next-buffer)

(define-key evil-normal-state-map ";" 'evil-ex)
(evil-ex-define-cmd "config" 'open_config)

;; Insert mode code completion
(define-key evil-insert-state-map "(" 'insert_paren)
(define-key evil-insert-state-map "[" 'insert_bracket)
(define-key evil-insert-state-map "\"" 'insert_quote)
(define-key evil-insert-state-map (kbd "{ RET") 'insert_brace_block)
(define-key evil-insert-state-map "\C-j" 'jump_to_closing_paren)


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(custom-enabled-themes (quote (misterioso)))
 '(indent-tabs-mode t)
 '(package-selected-packages
   (quote
	(solarized-theme swiper-helm counsel ivy aggressive-indent neotree evil))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; Startup stuff
(setq frame-resize-pixelwise t)
(menu-bar-mode -1)
(toggle-scroll-bar -1)
(tool-bar-mode -1)
(load-theme 'solarized-dark t)
