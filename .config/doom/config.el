;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Clément Rezvoy"
      user-mail-address "clement.rezvoy@gmail.com"
      custom-file (concat "~/.config/doom/parts/custom.el"))


(setq delete-by-moving-to-trash t                      ; Delete files to trash
      uniquify-buffer-name-style 'forward              ; Uniquify buffer names
      window-combination-resize t)                      ; take new window space from all

(display-time-mode 1)
(unless (equal "Battery status not available"
               (battery))
  (display-battery-mode 1))                       ; On laptops it's nice to know how much power you have

(setq eshell-aliases-file "~/.config/doom/eshell_aliases")
(defun dired-other-window ()
  (interactive)
  (other-window)
  (dired (default-directory)))

(use-package! zoom
  :hook (doom-first-input . zoom-mode)
  :config
  (setq zoom-size '(0.90 . 0.80)
        ;; zoom-ignored-major-modes '(dired-mode vterm-mode help-mode helpful-mode rxt-help-mode help-mode-menu org-mode)
        zoom-ignored-buffer-names '("*doom:scratch*" "*info*" "*helpful variable: argv*")
        zoom-ignored-buffer-name-regexps '("^\\*calc" "\\*helpful variable: .*\\*")
        zoom-ignore-predicates (list (lambda () (< (count-lines (point-min) (point-max)) 15)))))

;
; (defvar fancy-splash-image-template)
;   (expand-file-name "misc/splash-images/blackhole-lines-template.svg" doom-private-dir)

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
(setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
      doom-variable-pitch-font (font-spec :family "sans" :size 13))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:

(defun my/next-in-list (e l)
  (let* ((cur (cl-position e l))
         (len (length l))
         (next (% (+ cur 1) len)))
    (nth next l)))

(setq my/themes '(doom-solarized-light
                  doom-solarized-dark))

(setq my/current-theme (car my/themes))

(defun my/cycle-themes ()
  (interactive)
  (setq my/current-theme (my/next-in-list my/current-theme my/themes))
  (load-theme my/current-theme t nil)
  (solaire-mode-swap-bg))

(setq doom-theme my/current-theme)

(map!
 :leader
 :desc "Cycle themes"
 "h*" #'my/cycle-themes)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type nil)


;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

(defun load-directory (dir)
  (let ((load-it (lambda (f)
                   (load-file (concat (file-name-as-directory dir) f)))))
    (mapc load-it (directory-files dir nil "\\.el$"))))

(when (file-directory-p "~/.config/doom/parts/")
  (load-directory "~/.config/doom/parts/"))
