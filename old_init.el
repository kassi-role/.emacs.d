(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-idle-delay 0.1 nil nil "Customized with use-package company")
 '(company-show-numbers t nil nil "Customized with use-package company")
 '(css-indent-offset 2)
 '(ivy-count-format "[%d/%d] " nil nil "Customized with use-package ivy")
 '(ivy-height 10 nil nil "Customized with use-package ivy")
 '(ivy-mode t)
 '(js-indent-level 2)

 '(package-selected-packages
   '(scss-mode mmm-mode css-in-js poly-markdown company-c-headers rtags levenshtein cmake-ide company-irony irony irony-mode eglot xref gnu-elpa-keyring-update lsp-ivy lsp-ui csharp-mode lsp-mode perspective request org-roam doom-modeline chess clojure-mode cider elixir-mode typescript-mode elm-mode use-package avy ripgrep yasnippet cargo rust-mode exec-path-from-shell counsel-spotify js-comint gotham-theme nord-theme 2048-game which-key amx counsel swiper diminish spacemacs-theme yaml-mode js2-mode rjsx-mode projectile org-bullets highlight-indent-guides company undo-tree enh-ruby-mode diff-hl rainbow-delimiters ewal-spacemacs-themes ivy magit helm dracula-theme ztree))

 '(pixel-scroll-mode t)
 '(ring-bell-function 'ignore)
 '(sentence-end-double-space nil)
 '(split-height-threshold nil)
 '(split-width-threshold 0)
 '(tool-bar-mode nil)
 '(visible-bell t)
 '(which-key-mode t)
 '(window-divider-mode nil)
 '(yas-global-mode t))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(doom-modeline-bar ((t (:inherit highlight :background "#bd93f9"))))
 '(doom-modeline-bar-inactive ((t (:background "#6272a4"))))
 '(line-number-current-line ((t (:inherit default))))
 '(mode-line-inactive ((t (:background "#373844" :foreground "#6272a4" :box (:line-width 1 :color "#44475a")))))
 '(persp-selected-face ((t (:inherit font-lock-type-face :weight bold)))))


(use-package diminish)

(use-package cargo)
(use-package diff-hl)
(use-package elm-mode)
(use-package flycheck)
;; (use-package highlight-indent-guides
;;   :custom
;;   (highlight-indent-guides-method 'character)
;;   (highlight-indent-guides-responsive 'top)
;;   :hook (prog-mode . highlight-indent-guides-mode))

(use-package rjsx-mode)
(use-package rust-mode)
(use-package typescript-mode)
(use-package ztree)

(use-package avy
  :bind
  ("C-;" . avy-goto-char-timer)
  ("C-'" . avy-goto-char-2)
  ("M-g f" . avy-goto-line)
  ("M-g w" . avy-goto-word-1))

(use-package company
  :diminish
  :init (global-company-mode 1)
  :custom
  (company-idle-delay 0.1)
  (company-show-numbers t))

(use-package ivy
  :diminish
  :init
  (use-package amx :defer t)
  (use-package counsel :diminish :config (counsel-mode 1))
  (use-package swiper :defer t)
  (ivy-mode 1)
  :bind
  ("C-s" . swiper-isearch)
  ;; ("C-x 4 b" . ivy-switch-buffer-other-window)
  :custom
  (ivy-count-format "[%d/%d] ")
  (ivy-height 10))

(use-package dracula-theme
  :init
  (load-theme 'dracula t))

(use-package enh-ruby-mode
  :mode "\\(?:\\.rb\\|ru\\|rake\\|thor\\|jbuilder\\|gemspec\\|podspec\\|/\\(?:Gem\\|Rake\\|Cap\\|Thor\\|Vagrant\\|Guard\\|Pod\\)file\\)\\'")

(use-package helm
  :bind ("M-i" . helm-imenu))

(use-package js-comint
  :init (add-to-list 'exec-path "/usr/local/bin/"))

(use-package js2-mode
  :mode "\\.js\\'")

(use-package magit
  :bind
  ("C-x g" . magit-status)
  :custom
  (magit-repository-directories '(("~/projects" . 1) ("~/.emacs.d" . 0))))

(use-package projectile
  :init
  :bind
  ("C-c p" . projectile-command-map)
  :custom
  (projectile-completion-system 'ivy)
  (projectile-project-search-path '(("~/projects" . 1)))
  :config
  (projectile-mode))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package yaml-mode
  :mode "\\.yml\\'")

(use-package undo-tree
  :defer t
  :diminish
  :init (global-undo-tree-mode))

(use-package which-key
  :init (which-key-mode 1))

(use-package doom-modeline
  :custom
  (doom-modeline-bar-width 5)
  (doom-modeline-buffer-file-name-style (quote buffer-name) "Show only the file name in the buffer")
  (doom-modeline-icon nil "Don't use icons")
  (doom-modeline-mode t)
  :custom-face
  (doom-modeline-bar ((t (:inherit highlight :background "#bd93f9"))))
  (doom-modeline-bar-inactive ((t (:background "#6272a4")))))

(use-package perspective
  :bind
  (("C-x C-b" . persp-list-buffers)
  ("C-x k" . persp-kill-buffer*)
  ("C-x b" . persp-counsel-switch-buffer))
  :config
  (persp-mode))

(use-package clojure-mode)
(use-package cider)

;; (use-package lsp-mode
;;   :init
;;   ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
;;   (setq lsp-keymap-prefix "C-c l")
;;   :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
;;          ;; (XXX-mode . lsp)
;;          ;; if you want which-key integration
;;          (lsp-mode . lsp-enable-which-key-integration))
;;   :commands lsp)
;; (use-package lsp-ivy :commands lsp-ivy-workspace-symbol)

;; ------------------------------------------------------------
;; T I D E  Mode setup
;; ------------------------------------------------------------

;; (defun setup-tide-mode ()
;;   (interactive)
;;   (tide-setup)
;;   (flycheck-mode +1)
;;   (setq flycheck-check-syntax-automatically '(save mode-enabled))
;;   (eldoc-mode +1)
;;   (tide-hl-identifier-mode +1))

;; (setq company-tooltip-align-annotations t)

;; (use-package tide
;;   :ensure t
;;   :hook ((rjsx-mode . setup-tide-mode)
;;          (js2-mode . setup-tide-mode)))


(use-package org-roam
  :ensure t
  :init
  (setq org-roam-v2-ack t)
  :custom
  (org-roam-directory "~/RoamNotes")
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n i" . org-roam-node-insert))
  :config
  (org-roam-setup))

(use-package all-the-icons)

;; (use-package lsp-mode
;;   :init
;;   ;; set prefix for lsp-command-map (few alternatives - "C-l" "C-c l")
;;   (setq lsp-keymap-prefix "C-c l")
;;   :hook ((csharp-mode . lsp)
;;          ;; (c++-mode . lsp)
;;          (lsp-mode . lsp-enable-which-key-integration))
;;   :commands lsp)
;; (setq lsp-clangd-binary-path "c:/Program Files/LLVM/bin/clangd.exe")
;; (setq company-clang-executable "c:/Program Files/LLVM/bin/clang++.exe")

;; (use-package lsp-ui :commands lsp-ui-mode)

;; (use-package lsp-ivy :commands lsp-ivy-workspace-symbol)

(use-package csharp-mode
  :ensure t)

(use-package rtags)
;; (use-package cmake-ide)
;; (cmake-ide-setup)

(use-package irony
  :hook ('c++-mode))


;; create the autosave dir if necessary, since emacs won't.
(make-directory "~/.emacs.d/autosaves/" t)


;; ------------------------------------------------------------
;; key overrides
;; ------------------------------------------------------------
(global-set-key (kbd "M-o") 'other-window)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-z") 'ignore)

;; undo emacs-mac default to swap command/option
(setq mac-option-modifier 'meta)
(setq mac-command-modifier 'hyper)

;; ------------------------------------------------------------
;; smooth scrolling
;; ------------------------------------------------------------
;; Vertical Scroll
(setq scroll-step 1)
(setq scroll-margin 1)
(setq scroll-conservatively 101)
(setq scroll-up-aggressively 0.01)
(setq scroll-down-aggressively 0.01)
(setq auto-window-vscroll nil)
(setq fast-but-imprecise-scrolling nil)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))
(setq mouse-wheel-progressive-speed nil)
;; Horizontal Scroll
(setq hscroll-step 1)
(setq hscroll-margin 1)

;; ------------------------------------------------------------
;; Transparency
;; ------------------------------------------------------------
;; (set-frame-parameter (selected-frame) 'alpha '(90 . 50))
;; (add-to-list 'default-frame-alist '(alpha . (90 . 50)))



;; ------------------------------------------------------------
;; Custom Functions
;; ------------------------------------------------------------

;; Set opacity for current frame
(defun set-frame-opacity (value)
  "Sets the Opacity of the frame window. 0 = transparent/100 = opaque"
  (interactive "nOpacity Value 0 - 100 opaque: ")
  (set-frame-parameter (selected-frame) 'alpha value))

;; go to end of line and create new line
(defun newline-without-break-of-line ()
  (interactive)
  (let ((oldpos (point)))
    (end-of-line)
    (newline-and-indent)))
(global-set-key (kbd "<C-return>") 'newline-without-break-of-line)

;; insert semicolon at the end of the line and optionally start a newline
;; (defun semicolon-and-end(&optional arg)
;;   (interactive "P")
;;   (let ((oldpos (point)))
;;     (end-of-line)
;;     (insert ";")
;;     (if arg
;;         (newline-and-indent))))
;; (global-set-key (kbd "C-;") 'semicolon-and-end)


(defun open-test-file()
  (interactive)
  (let* ((path-to-file (buffer-file-name))
         (file-extension (car (last (split-string path-to-file "\\."))))
         (file-path-no-extension (car (butlast (split-string path-to-file "\\."))))
         (use-existing-window (equal 2 (length (window-list))))
         (open-file-function
          (if use-existing-window
              (lambda (f) (other-window 1) (find-file f))
            (lambda (f) (find-file-other-window f)))))
    (cond
     ((or (equal file-extension "js") (equal file-extension "jsx"))
      (funcall open-file-function (string-join (list file-path-no-extension "test" file-extension) ".")))
     ((equal file-extension "rb")
      (let ((new-file-path (replace-regexp-in-string "/app/" "/spec/" file-path-no-extension))
            (new-file-suffix (string-join (list "spec" file-extension) ".")))
        (funcall open-file-function (string-join (list new-file-path new-file-suffix) "_"))))
     (t (message "Unknown Extension")))))

(defun sort-newline-args ()
  (interactive)
  (let ((oldpos (point)))
    (backward-up-list)
    (next-line)
    (beginning-of-line)
    (set-mark (point))
    (backward-up-list)
    (forward-sexp)
    (previous-line)
    (end-of-line)
    (sort-lines nil (mark) (point))))

(defun kassi/better-eval-print (&optional eval-last-sexp-arg-internal)
  "Does what eval-print-last-sexp but prefixes with a \"=>\".
Also pads following lines to be at the same indentation level"
  (interactive "P")
  (let (start end)
    (save-excursion
      (eval-print-last-sexp)
      (previous-line)
      (setq end (point)) (message "end: %d" end))
    (next-line)
    (move-beginning-of-line nil)
    (insert "=> ")
    (move-beginning-of-line nil)
    (next-line)
    (setq start (point))
    (setq end (+ 3 end)) ; Stored value for 'end' gets a little fucked up when inserting "=> "
    (when (<= start end)
        (string-rectangle start end "   ")
        (next-line))))

(add-hook 'lisp-interaction-mode-hook
          (lambda () (local-set-key (kbd "C-j") 'kassi/better-eval-print)))

(defvar kassi/config-persp-name "config"
  "The perspective name to use for config persp. Defaulted to \"config\"")

(defun kassi/open-config (&optional arg)
  "Opens init.el and magit status in a new perspective window.

If the optional ARG is passed then it does not open magit status.

If the perspective already exists, then this only opens it as it was previously"
  (interactive "P")
  (let* ((persp-already-exists (gethash kassi/config-persp-name (perspectives-hash)))
         (init-persp (not persp-already-exists))
         (open-magit (not (consp arg))))
    (persp-switch kassi/config-persp-name)
    (if init-persp
        (progn
          (find-file "~/.emacs.d/init.el")
          (if open-magit
              (magit-status "~/.emacs.d/"))))))

(defun kassi/is-config-open ()
  "Returns whether or not config perspective is currently open"
  (string-equal (persp-current-name) kassi/config-persp-name))

(defun kassi/exit-config ()
  "Returns to previous perspective if current perspective is kassi/config-persp-name"
  (interactive)
  (if (kassi/is-config-open)
      (persp-switch-last)))

(defun kassi/config-dwim (&optional arg)
  "If in config then backs out to the previous perspective. If not in config then opens config, passing interactive argument"
  (interactive "P")
  (if (kassi/is-config-open)
      (kassi/exit-config)
    (kassi/open-config arg)))

(global-set-key (kbd "C-c C-SPC") 'kassi/config-dwim)
