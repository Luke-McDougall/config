;; Apparently the garbage collector makes start up slow in Emacs
;; this will temporarily disable it to make start up faster `gcmh-mode'
;; is used to reset the garbage collector at the end of this file.
(setq gc-cons-threshold most-positive-fixnum)

;; These are the repositories Emacs will search for available packages
;; to download
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

;; Emacs will automatically generate some configuration text
;; this will make Emacs save it in it's own file so that this
;; file doesn't get too messy.
(use-package cus-edit
  :config
  (setq custom-file "~/.emacs.d/custom.el")

  (unless (file-exists-p custom-file)
    (write-region "" nil custom-file))

  (load custom-file))

;; By default Emacs makes backup files named <filename>~
;; having these files clutter your directories is very
;; annoying but sometimes these backups can save your life.
;; This makes Emacs save all the backups into a backups directory
;; so that you get the benifit of the backups without having to
;; see all the disgusting files everywhere
(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))

;; Keybindings
;; This section is setting up some basic key bindings.
;; The string on the left represents the keyboard input
;; and the text right after the full stop is the function that
;; will be called when you input the corresponding keys.
;;
;; WARNING!!! If you decide to set up any of your own keybindings
;; do not rebind M-x (Meta x or alt x) or C-g (control g).
;;
;; M-x is the key that lets you run functions in emacs. It's
;; useful for functions you call infrequently enough that it
;; isn't worth setting up a keybinding for them.
;; 
;; C-g will cancel any in-progress command, it's
;; very easy to get lost in Emacs by accidentally
;; pressing the wrong key and ending up somewhere
;; you didn't expect. C-g lets you escape these
;; spooky situations.
;;
;; Legend:
;; M = Meta, emacs calls the alt key the meta key. So M-<right>
;;     means Meta-right which means alt right. Any time you see M
;;     or meta it means alt
;; C = Control, emacs didn't make up it's own name for this key
;;     it's the control key. C-<right> means control right
;; \r = Return or Enter

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
         ("<f5>"      . dot-bat-compile)            ;; Run a build.bat in the same directory as the current file
         ("<f8>"      . window-toggle-side-windows) ;; Toggle display of treemacs
         )
  )

;; This is the custom compile function called by pressing F5
(defun dot-bat-compile ()
  (interactive)
  (compile "build.bat"))

;; `which-key' is a cool package that will display a list of key bindings and
;; the functions they will call whenever you input an incomplete key binding.
(use-package which-key
  :ensure t
  :init
  (which-key-mode))

;; Ivy counsel and swiper are all related packages that make finding
;; files, searching for text, and many other things much easier. I've
;; set up the basics but it can do a lot more. See `http://oremacs.com/swiper'
;; and `https://github.com/abo-abo/swiper'
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

;; The default undo system in emacs is weird. It's supposedly more
;; powerful than other systems but I never need to do anything super
;; fancy with undo so I don't care. This is a normal undo system for
;; normal brained people. There are more commands than I have bound
;; but I've never felt the need to use them. Look here if your'e curious
;; `https://elpa.gnu.org/packages/undo-tree.html'
(use-package undo-tree
  :ensure t
  :config
  (global-undo-tree-mode))

;; Aesthetics
;; A cool looking mode-line. The mode-line is the coloured strip along the
;; bottom of the screen. It dislays information about the current buffer
;; from left to right you have.
;; 1. A symbol representing the buffers mode.
;; 2. The buffers name (red if the buffer has unsaved changes, white otherwise)
;; 3. The line number where the cursor is
;; 4. How far into the file you are represented as a percentage
;; 5. Text format of the current buffer
;; 6. The major mode of the current buffer. This is the same mode that is represented by the symbol in 1
(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1))

;; Startup screen that lets you easily access recently opened files
;; if you want to customize it look here `https://github.com/emacs-dashboard/emacs-dashboard'
(use-package dashboard
  :ensure t
  :config
  (setq dashboard-banner-logo-title "Emacs (The c stands for Cum)")
  (setq dashboard-set-footer nil)
  (setq dashboard-items '((recents . 5)))
  (dashboard-setup-startup-hook))

;; A collection of themes to use. See `https://github.com/hlissner/emacs-doom-themes'
;; you can find screenshots of all the available themes on the github page. To use one
;; change the load-theme statement to contain the name of the theme you want e.g.
;; (load-theme 'doom-one t) -> (load-theme 'cool-new-theme t)
(use-package doom-themes
  :ensure t
  :config
  (setq doom-themes-enable-bold t
        doom-themes-enable-itlic t)
  (load-theme 'doom-one t)
  (setq doom-themes-treemacs-theme "doom-colors");; Theme for treemacs
  (doom-themes-treemacs-config))

;; Interface
;; Treemacs is a file explorer/project manager. When you first start up emacs
;; C-t will launch emacs and then F8 is used to toggle visibilty of the treemacs
;; window. The two important concepts you need to understand with treemacs are
;; projects and workspaces. Projects are essentially a view into a specific
;; directory and it's subdirectories, it allows you to easily focus on only
;; the files you care about for whatever you're working on at the moment. A
;; workspace is a collection of projects. I don't lnow how useful workspaces
;; are. I suppose you might have like a programming workspace and a documentation
;; worksapce.
;;
;; REMEMBER, you can't have a project inside another project. Basically the root directory
;; of a project says that any sub directory of the root can't be the root of another project.
;;
;; See  `https://github.com/Alexander-Miller/treemacs'
(use-package treemacs
  :ensure t
  :defer t
  )

;; Company provides autocompletion.
(use-package company
  :ensure t
  :config
  (add-hook 'after-init-hook 'global-company-mode))

;; Programming settings
;; Auto insert matching characters e.g. " ( [ {
(electric-pair-mode 1)

;; Highlight matching parentheses
(setq-default show-paren-delay 0)
(show-paren-mode 1)

;; auto indent settings for c
(setq-default indent-tabs-mode nil)
(setq-default c-basic-offset 4)
(setq-default c-default-style "k&r")

;; Highlight current line, the usefulness of this is dubious
;; but in theory it will help you find where you are in a file
;; more quickly if you were looking at something else like documentation
;; or whatever. You can always turn it off though
(add-hook 'after-init-hook 'global-hl-line-mode)

;; Miscellaneous settings
;; Emacs does this weird jumping thing instead of just scrolling like
;; a normal program would do. This fixes that
(setq scroll-conservatively 100)

;; Recentf is a feature that keeps track of a list of recently visited files
;; it is very useful and `dashboard' uses it
(use-package recentf
  :init
  (setq recentf-max-menu-items 10)
  (setq recentf-max-saved-items 25)
  (setq recentf-exclude '(".+autoloads\.el"
                          "ido\.last"))
  :config
  (recentf-mode 1)
  )

;; Currently the functionality of ido is being implemented by ivy/counsel/swiper
;; ido can do almost all of the same things but it is less visual and requires
;; more setup. If you are a minimalist freak like me you might want to use ido
;; instead but if not just ignore this section.
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


;; Disable tool bar and scroll bar, you can enable them
;; if you want them by commenting out these lines. I left
;; the menu bar on because you're probably used to programs
;; having something like it but you can turn it off too by
;; uncommenting the third line.
(tool-bar-mode -1)
(toggle-scroll-bar -1)
;; (menu-bar-mode -1)

;; Startup

;; Start Emacs fullscreen
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Turn the garbage collector back on
(add-to-list 'load-path "~/.emacs.d/gcmh")
(require 'gcmh)
(gcmh-mode 1)
