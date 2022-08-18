;; -----------------------------------
;;   PACKAGE INITIALIZATION
;; -----------------------------------
;; load emacs 24's package system. Add MELPA repository

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(when (>= emacs-major-version 24)
  (require 'package)
  (add-to-list
   'package-archives
   ;; '("melpa" . "http://stable.melpa.org/packages/") ; many packages won't show if using stable
   ;; '("melpa" . "http://melpa.milkbox.net/packages/")
   '("melpa" . "http://melpa.org/packages/")
   t))

(add-to-list 'default-frame-alist '(font . "Hack-10.5"))

;; Setup use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package)
  (require 'bind-key))

(require 'use-package-ensure)
(setq use-package-always-ensure t)


;; -----------------------------------
;;   CONSTANTS
;; -----------------------------------

;; Constants for dracula color theme
(defconst dracula-color-background "#282a36")
(defconst dracula-color-current-line "#44475a")
(defconst dracula-color-selection "#44475a")
(defconst dracula-color-foreground "#f8f8f2")
(defconst dracula-color-comment "#6272a4")
(defconst dracula-color-cyan "#8be9fd")
(defconst dracula-color-green "#50fa7b")
(defconst dracula-color-orange "#ffb86c")
(defconst dracula-color-pink "#ff79c6")
(defconst dracula-color-purple "#bd93f9")
(defconst dracula-color-red "#ff5555")
(defconst dracula-color-yellow "#f1fa8c")


;; -----------------------------------
;;   COLOR/THEME SETTINGS
;; -----------------------------------

;; Load dracula theme
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(load-theme 'dracula t)

;; Add highlighting to comment keywords in code
(defun my/add-watchwords ()
  "Highlight NOTE, FIXME, and TODO in code"
  (font-lock-add-keywords
   nil
   '(("\\<\\(FIXME\\)\\>"
      1 '((:foreground "#ffe1c2") (:weight bold)) t)
     ("\\<\\(TODO\\)\\>"
      1 '((:foreground "#ffc2c2") (:weight bold)) t)
     ("\\<\\(NOTE\\)\\>"
      1 '((:foreground "#d7befa") (:weight bold)) t))))

(add-hook 'prog-mode-hook #'my/add-watchwords)

;; Use dracula's purple color for the cursor
(apply 'set-face-attribute 'cursor nil `(:background ,dracula-color-purple))

;; Use a lighter color for line highlighting
(set-face-attribute 'hl-line nil :extend t :background "#373949")

;; -----------------------------------
;;   GENERAL SETTINGS
;; -----------------------------------

;; Set font
(add-to-list 'default-frame-alist '(font . "Hack-10.5"))

;; Disable/Group auto generated files
(make-directory "~/.emacs.d/autosaves/" t)
(setq auto-save-file-name-transforms '(("." "~/.emacs.d/autosaves/\\1" t))) ;; These are the tilde~ files
(setq backup-directory-alist '((".*" . "~/.emacs.d/backups/")))
(setq create-lockfiles nil)

;; Setup cursor blinking (or lack thereof)
(setq blink-cursor-interval 0.5)
(setq blink-cursor-mode nil)

;; Set yes or no to be y or n, and enable it for killing emacs
(defalias 'yes-or-no-p 'y-or-n-p)
(setq confirm-kill-emacs 'y-or-n-p)

;; Disable the fucking bell
(setq ring-bell-function 'ignore)

;; When saving a file add a newline and remove trailing whitepace
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(setq require-final-newline t)



;; -----------------------------------
;;   GLOBAL MODES
;; -----------------------------------

;; Enable display-batter-mode if the battery is found
(require 'battery)
(when (and battery-status-function
           (not (string-match-p "N/A"
                                (battery-format "%B"
                                                (funcall battery-status-function)))))
  (display-battery-mode 1))

;; Modes to ensure disabled
(display-time-mode 0)
(setq indent-tabs-mode nil)
(menu-bar-mode 0)
(set-scroll-bar-mode nil)

;; Modes to ensure enabled
(global-hl-line-mode 1)
(electric-pair-mode 1)
(global-undo-tree-mode 1)
(global-display-line-numbers-mode 1)
(setq line-number-mode t)
(setq column-number-mode t)


;; -----------------------------------
;;   ORG MODE
;; -----------------------------------

(use-package org
  :custom-face
  `(org-level-1 ((t (:inherit bold :foreground ,dracula-color-pink :height 1.1))))
  `(org-level-3 ((t (:foreground ,dracula-color-green :weight normal :height 1.1))))
  `(org-level-4 ((t (:foreground ,dracula-color-yellow :weight normal :height 1.1))))
  `(org-level-5 ((t (:foreground ,dracula-color-cyan :weight normal :height 1.1))))
  `(org-level-6 ((t (:foreground ,dracula-color-orange :weight normal :height 1.1))))
  `(org-level-7 ((t (:foreground ,dracula-color-red :weight normal :height 1.1))))
  `(org-level-8 ((t (:foreground ,dracula-color-fg :weight normal :height 1.1)))))


(set-face-attribute 'hl-line nil :extend t :background "#373949")

;; Use visual line mode
(add-hook 'org-mode-hook 'visual-line-mode)

;; Use org-bullets for nicer bullet headings
(use-package org-bullets
  :hook (org-mode . (lambda () (org-bullets-mode 1))))
