(setq gc-cons-threshold most-positive-fixnum)
(add-hook 'after-init-hook
	  (lambda () (setq gc-cons-threshold 100000)))
(setq max-specpdl-size 13000)
(setq read-process-output-max (* 1024 1024))

(require 'package)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Global helper functions ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun my/next-in-list (e l)
  (let* ((cur (cl-position e l))
	 (len (length l))
	 (next (% (+ cur 1) len)))
    (nth next l)))

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
  (setq evil-overriding-maps nil
	evil-intercept-maps nil
	evil-pending-intercept-maps nil
	evil-pending-overriding-maps nil)
  ;; subvert evil-operation.el overrides (dired, ibuffer etc.)
  (advice-add 'evil-make-overriding-map :override #'ignore)
  (advice-add 'evil-make-intercept-map  :override #'ignore)
  (advice-add 'evil-add-hjkl-bindings   :override #'ignore)
  (evil-mode))

(use-package evil-collection
  :ensure t
  :init
  (evil-collection-init))

;;;;;;;;;;;;;;;;;;;;;;;;;
;;; UI Simplification ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;
(set-keyboard-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(set-charset-priority 'unicode)
(setq coding-system-for-read 'utf-8
      coding-system-for-write 'utf-8
      custom-file (concat user-emacs-directory "parts/custom.el")
      default-fill-column 120
      default-process-coding-system '(utf-8-unix . utf-8-unix)
      enable-local-variables :all
      indicate-buffer-boundaries nil
      indicate-empty-lines nil
      inhibit-splash-screen t
      inhibit-startup-screen t
      locale-coding-system 'utf-8
      ring-bell-function 'ignore
      use-dialog-box nil)
(defalias 'yes-or-no-p 'y-or-n-p)
(menu-bar-mode -1)


(setq shackle-rules '())

(use-package shackle
  :ensure t
  :config
  (setq shackle-rules '())
  (shackle-mode 1))
;; (use-package popwin
;;   :ensure t
;;   :config
;;   (popwin-mode 1))

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
  (general-def :states '(normal motion visual insert emacs) "SPC" nil)

  (general-create-definer my/leader
    :prefix "SPC"
    :non-normal-prefix "C-SPC"
   :states '(motion normal visual insert emacs)
   :keymaps 'override)	

  (general-create-definer my/local-leader
    :prefix "SPC m"
    :non-normal-prefix "C-SPC m"
    :states '(motion normal visual insert emacs))

  (my/leader
   "a" '(:ignore t :which-key "Apps")
   "at" '((lambda () (interactive) (term "/bin/bash")) :which-key "Terminal")
   "b" '(:ignore t :which-key "Buffers")
   "bp" '(previous-buffer :which-key "Previous buffer")
   "bn" '(next-buffer :which-key "Previous buffer")
   "bd" '(kill-buffer :which-key "Previous buffer")
   "c" '(:ignore t :which-key "Configuration")
   "ce" '((lambda ()
	    (interactive)
	    (find-file "~/.emacs.d/init.el" ))
	  :which-key "Edit Configuration")
   "cl" '((lambda ()
	    (interactive)
	    (split-window-sensibly)
	    (list-packages))
	  :which-key "List packages")
   "cr" '((lambda ()
	    (interactive)
	    (load-file "~/.emacs.d/init.el"))
	  :which-key "Reload configuration")
   "cR" '((lambda ()
	    (interactive)
	    (package-refresh-content)))
   "cU" '((lambda ()
	    (interactive)
	    (package-menu-mark-upgrades)
	    (package-menu-execute))
	  :which-key "Upgrade packages")
   "e" '(:ignore t :which-key "Edit")
   "f" '(:ignore t :which-key "Files")
   "fN" '((lambda ()
	    "Copy the current buffer file name to the clipboard."
	    (interactive)
	    (let ((filename (if (equal major-mode 'dired-mode) default-directory (buffer-file-name))))
	      (when filename
		(kill-new filename)
		(message "Copied buffer file name '%s' to the clipboard." filename))))
	  :which-key "Copy filename")
   "fD" '((lambda () (interactive) (dired "~/Downloads/"))
	  :which-key "Go to Download directory")
   "h" '(:ignore t :which-key "Help")
   "hm" '(describe-mode :which-key "Describe mode")
   "l" '(:ignore t :which-key "Language")
   "lc" '(comment-dwim :which-key "Comment code")
   "m" '(:ignore t :which-key "Major mode")
   "p" '(:ignore t :which-key "Projects")
   "w" '(:ignore t :which-key "Windows")
   "wo" '(delete-other-windows :which-key "single window")
   "wn" '(other-window :which-key "next window")
   "ws" '(split-window-horizontally :which-key "split horizontally")
   "wv" '(split-window-vertically :which-key "split vertically")
   "wS" '(split-window-sensibly :which-key "split sensibly")))

(use-package which-key
  :ensure t
  :init (which-key-mode)
  :diminish (which-key-mode . "")
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
  :init (ivy-mode)
  :diminish
  :config
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
  :ghook
  ('minibuffer-setup-hook #'my/minibuffer-setup-hook)
  ('minibuffer-exit-hook #'my/minibuffer-exit-hook))

(use-package counsel
  :ensure t
  :config
  (my/leader
   "bs" '(counsel-switch-buffer :which-key "Switch buffer")
   "fo" '(counsel-find-file :which-key "Open file")
   "fg" '(counsel-git :which-key "Open git file")
   "hv" '(counsel-describe-variable :which-key "Describe variable")
   "hf" '(counsel-describe-function :which-key "Describe function")
   "ha" '(counsel-apropos :which-key "Apropos")
   ))

;;;;;;;;;;;;;;;;;;;;;;;;
;;; UI Customisation ;;;
;;;;;;;;;;;;;;;;;;;;;;;;
(use-package doom-themes
  :ensure t
  :defer nil
  :config
  (setq my/themes '(doom-tomorrow-night
		    doom-tomorrow-day
		    doom-solarized-dark
		    doom-solarized-light))
  (setq my/current-theme (car my/themes))
  (defun my/cycle-theme ()
    (interactive)
    (setq my/current-theme (my/next-in-list my/current-theme my/themes))
    (load-theme my/current-theme t nil)
    (solaire-mode-swap-bg))

  (my/leader
   "ct" '(my/cycle-theme :which-key "Cycle theme"))

  (doom-themes-visual-bell-config)
  (doom-themes-org-config)
  (doom-themes-neotree-config)
  (setq doom-theme-enable-bold 1
	doom-theme-enable-italic 1
	doom-solarized-dark-brighter-comments t
	doom-solarized-dark-brighter-modeline t
	doom-solarized-light-brighter-comments t
	doom-solarized-light-brighter-modeline t
	;; doom-tomorrow-night-brighter-comments t
	doom-tomorrow-night-brighter-modeline t
	doom-tomorrow-day-brighter-comments t
	doom-tomorrow-day-brighter-modeline t)

  (set-face-attribute 'lazy-highlight nil :background "HotPink")
  (set-face-attribute 'evil-ex-lazy-highlight nil :background "DeepPink"))
(add-hook 'after-make-frame-functions
	    '(lambda (frame)
	       (with-selected-frame frame
		 (if (display-graphic-p frame)
		     (progn
		       (tool-bar-mode -1)
		       (scroll-bar-mode -1)
		       (set-default-font "Monospace 10")
		       (load-theme my/current-theme t nil)
		       (solaire-mode-swap-bg))
		    (progn
		      (load-theme 'brutalist-dark t nil))))))

(use-package solaire-mode
  :ensure t
  :hook
  ((change-major-mode after-revert ediff-prepare-buffer) . turn-on-solaire-mode)
  (minibuffer-setup . solaire-mode-in-minibuffer)
  :config
  (solaire-global-mode +1))

(use-package dimmer
  :ensure t
  :custom (dimmer-fraction 0.1)
  :config (dimmer-mode))

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1))

(use-package diminish
  :ensure t)

(use-package undo-tree
  :diminish
  :config
  (my/leader
   "eU" '(undo-tree-visualize :which-key "show undo-tree")
   )
  (global-undo-tree-mode +1))

(use-package all-the-icons
  :ensure t
  :config
  (when (not (file-exists-p "~/.local/share/fonts/all-the-icons.ttf"))
    (all-the-icons-install-fonts t)))

(use-package emojify
  :ensure t
  :config
  (global-emojify-mode 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Project management ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package projectile
  :ensure t
  :after ivy
  :diminish 
  :config
  (setq projectile-enable-caching t
	projectile-file-exists-remote-cache-expire (* 10 60)
	projectile-completion-system 'ivy)
  (projectile-mode +1)
  (my/leader
   "ps" '(projectile-switch-project :which-key "Switch project")
   "ph" '(projectile-find-related-file :which-key "Find related file")
   "pf" '(projectile-find-file :which-key "Find file")
   "p/" '(projectile-grep :which-key "Grep in project")
   "pg" '(projectile-find-tag :which-key "Find tag")
   "pc" '(projectile-compile-project :which-key "Compile project")
   "pe" '(projectile-edit-dir-locals :which-key "Edit project configuration")))

;;;;;;;;;;;;;;;;;;;
;;; Filebrowser ;;;
;;;;;;;;;;;;;;;;;;;
(use-package neotree
  :ensure t
  :config 
  (evil-collection-neotree-setup)
  (my/leader
   "pt" '(my/neotree-project-dir-toggle :which-key "Show project tree"))
  (setq neo-window-width 40
	neo-window-fixed-size nil)
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

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Checkers & linters ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

(show-paren-mode)
(electric-pair-mode)

(use-package guess-language
  :ensure t
  :config
  (setq guess-language-languages '(en fr)
	guess-language-min-paragraph-length 35))

(use-package flyspell
  :config

  (let ((langs '("american" "francais" )))
    (setq my/lang-ring (make-ring (length langs)))
    (dolist (elem langs) (ring-insert my/lang-ring elem)))


  (defun my/cycle-ispell-languages ()
    (interactive)
    (let ((lang (ring-ref my/lang-ring -1)))
      (ring-insert my/lang-ring lang)
      (ispell-change-dictionary lang)))

  (my/leader
   "ls" '(flyspell-mode :which-key "Toggle spellcheck")
   "lS" '(my/cycle-ispell-languages :which-key "Cycle spellcheck language")))


(use-package flycheck
  :ensure t
  :config
  (my/leader
   "ll" '(flycheck-mode :which-key "Toggle linter")))

(use-package company
  :ensure t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Modular configuration ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun load-directory (dir)
  (let ((load-it (lambda (f)
		   (load-file (concat (file-name-as-directory dir) f)))))
    (mapc load-it (directory-files dir nil "\\.el$"))))

(when (file-directory-p "~/.emacs.d/parts/")
  (load-directory "~/.emacs.d/parts/"))


