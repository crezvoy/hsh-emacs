;; adjust GC for speedier startup
(setq gc-cons-threshold most-positive-fixnum)
(add-hook 'after-init-hook
	  (lambda () (setq gc-cons-threshold 100000)))
(setq max-specpdl-size 13000)

(require 'package)


;;;;;;;;;;;;;;;
;;; backups ;;;
;;;;;;;;;;;;;;;
(setq backup-directory-alist '(("." . "~/.emacs.d/saves"))
      backup-by-copying t
      delete-old-versions -1
      version-control t
      vc-make-backup-file t
      vc-follow-symlinks t
      create-lockfiles nil)
;;; history
(require 'savehist)
(savehist-mode 1)
(auto-save-visited-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;
;;; UI Simplification ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;
(set-keyboard-coding-system 'utf-8)
(setq inhibit-startup-screen t
      ring-bell-function 'ignore
      coding-system-for-read 'utf-8
      coding-system-for-write 'utf-8
      default-fill-column 120
      custom-file (concat user-emacs-directory "custom.el")
      inhibit-splash-screen t
      indicate-buffer-boundaries nil
      indicate-empty-lines nil
      use-dialog-box nil)
(defalias 'yes-or-no-p 'y-or-n-p)
(menu-bar-mode -1)
(when window-system
  (tool-bar-mode -1)
  (scrollbar-mode -1))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; package management ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq package-archives
      '(("org" . "https://orgmode.org/elpa/")
	("melpa-stable" . "https://stable.melpa.org/packages/")
	("melpa" . "https://melpa.org/packages/")
	("gnu" . "https://elpa.gnu.org/packages/")))
(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; setup environement variables ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package exec-path-from-shell
  :ensure t
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

;;;;;;;;;;;;
;;; Evil ;;;
;;;;;;;;;;;;
(use-package evil
  :ensure t
  :init 
  (setq evil-want-keybinding nil)
  (setq evil-search-module 'evil-search)
  (setq evil-ex-complete-emacs-commands nil)
  (setq evil-vsplit-window-right t)
  (setq evil-split-window-below t)
  (setq evil-shift-round nil)
  (setq evil-want-C-u-scroll t)
  :config
  (evil-mode))

(use-package evil-collection
  :ensure t
  :init
  (evil-collection-init))

;;;;;;;;;;;;;;;;;;;;;;;
;;; Menu & commands ;;;
;;;;;;;;;;;;;;;;;;;;;;;
(use-package general
  :ensure t
  :config
  (general-evil-setup)
  (general-imap "j" (general-key-dispatch 'self-insert-command
		      :timeout 0.25
		      "k" 'evil-normal-state))
  (general-define-key
   :states '(normal visual insert emacs)
   :prefix "SPC"
   :non-normal-prefix "C-SPC"
   "a" '(:ignore t :which-key "Apps")
   "b" '(:ignore t :which-key "Buffers")
   "c" '(:ignore t :which-key "Configuration")
   "ce" '((lambda ()
	    (interactive)
	    (split-window-sensibly)
	    (find-file "~/emacs.d/init.el" ))
	  :which-key "Edit Configuration")
   "cl" '((lambda ()
	    (interactive)
	    (split-window-sensibly)
	    (list-packages)
	    :which-key "List packages"))
   "cr" '((lambda ()
	    (interactive)
	    (load-file "~/.emacs.d/init.el"))
	  :which-key "Reload configuration")
   "f" '(:ignore t :which-key "Files")
   "h" '(:ignore t :which-key "Help")
   "l" '(:ignore t :which-key "Language")
   "m" '(:ignore t :which-key "Major mode")
   "p" '(:ignore t :which-key "Projects")
   "t" '(:ignore t :which-key "Text")
   "w" '(:ignore t :which-key "Windows")
   "wo" '(delete-other-windows :which-key "single window")
   "wn" '(other-window :which-key "next window")
   "ws" '(split-window-horizontally :which-key "split horizontally")
   "wv" '(split-window-vertically :which-key "split vertically")
   "wS" '(split-window-sensibly :which-key "split sensibly")))

(use-package which-key
  :ensure t
  :init (which-key-mode)
  :config
  (which-key-setup-side-window-right-bottom)
  (setq which-key-sort-order 'which-key-key-order-alpha
	which-key-side-window-max-width 0.33 which-key-idle-delay 0.5))

;; setup for speedier ivy buffers
(defun my/minibuffer-setup-hook ()
  (setq gc-cons-threshold most-positive-fixnum))

(defun my/minibuffer-exit-hook ()
  (setq gc-cons-threshold 800000))
(use-package ivy
  :ensure t
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
  :hook
  ((minibuffer-setup . my/minibuffer-setup-hook)
   (minibuffer-exit . my/minibuffer-exit-hook)))

(use-package counsel
  :ensure t
  :config
  (general-define-key
   :states '(normal visual insert emacs)
   :prefix "SPC"
   :non-normal-prefix "C-SPC"
   "fo" '(counsel-find-file :which-key "Open file")
   "fg" '(counsel-git :which-key "Open git file")))

;;;;;;;;;;;;;;;;;;;;;;;;
;;; UI Customisation ;;;
;;;;;;;;;;;;;;;;;;;;;;;;
(use-package brutalist-theme
  :ensure t
  :defer nil
  :config
  (load-theme 'brutalist-dark t nil))

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1))

(use-package diminish
  :ensure t)

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Project management ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package projectile
  :ensure t
  :after ivy
  :config
  (general-define-key
   :states '(normal visual insert emacs)
   :prefix "SPC"
   :non-normal-prefix "C-SPC"
   "ps" '(projectile-switch-project :which-key "Switch project")
   "ph" '(projectile-find-related-file :which-key "Find related file")
   "pf" '(projectile-find-file :which-key "Find file")
   "p/" '(projectile-grep :which-key "Grep in project")
   "pg" '(projectile-find-tag :which-key "Find tag")
   "pc" '(projectile-compile-project :which-key "Compile project")
   "pe" '(projectile-edit-dir-locals :which-key "Edit project configuration"))
  (projectile-mode +1)
  (setq projectile-enable-caching t)
  (setq projectile-file-exists-remote-cache-expire (* 10 60))
  (setq projectile-completion-system 'ivy))

;;;;;;;;;;;;;;;;;;;
;;; Filebrowser ;;;
;;;;;;;;;;;;;;;;;;;
(use-package neotree
  :ensure t
  :config 
  (evil-collection-neotree-setup)
  (general-define-key
   :states '(normal visual insert emacs)
   :prefix "SPC"
   :non-normal-prefix "C-SPC"
   "pt" '(my/neotree-project-dir-toggle :which-key "Show project tree"))
  (setq neo-window-width 40)  
  (setq neo-theme (if (display-graphic-p) 'icons 'arrow)))

(defun my/neotree-project-dir-toggle ()
  "Open NeoTree using the project root, using find-file-in-project,
  or the current buffer directory."
  (interactive)
  (let ((project-dir
	 (ignore-errors
	   (projectile-project-root)))
	(file-name (buffer-file-name))
	(neo-smart-open t))
    (if (and (fboundp 'neo-global--window-exists-p)
	     (neo-global--window-exists-p))
	(neotree-hide)
      (progn
	(neotree-show)
	(if project-dir
	    (neotree-dir project-dir))
	(if file-name
	    (neotree-find file-name))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Modular configuration ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun load-directory (dir)
  (let ((load-it (lambda (f)
		   (load-file (concat (file-name-as-directory dir) f)))))
    (mapc load-it (directory-files dir nil "\\.el$"))))

(when (file-directory-p "~/.emacs.d/parts/")
  (load-directory "~/.emacs.d/parts/"))


