;; The default is 800 kilobytes.  Measured in bytes.
(setq gc-cons-threshold (* 50 1000 1000))

;; ensure stuff like eglot is updated
(setopt package-install-upgrade-built-in t)

(setq straight-use-package-by-default 1)
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(defun efs/display-startup-time ()
  (message "Emacs loaded in %s with %d garbage collections."
           (format "%.2f seconds"
                   (float-time
                     (time-subtract after-init-time before-init-time)))
           gcs-done))

(add-hook 'emacs-startup-hook #'efs/display-startup-time)

(straight-use-package 'use-package)

;; allow "y" or "n" instead of "yes" or "no"
(setopt use-short-answers t)

(use-package no-littering
  :straight t)

(use-package wakatime-mode
  :straight t)

;; no-littering doesn't set this by default so we must place
;; auto save files in the same path as it uses for sessions
(setq auto-save-file-name-transforms
      `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))

(setq inhibit-startup-message t)

(scroll-bar-mode -1)        ; Disable visible scrollbar
(tool-bar-mode -1)          ; Disable the toolbar
(tooltip-mode -1)           ; Disable tooltips
(set-fringe-mode 10)        ; Give some breathing room

;; remove visible bell entirely
(setq visible-bell       nil
      ring-bell-function #'ignore)

(menu-bar-mode -1)            ; Disable the menu bar

(column-number-mode)
(global-display-line-numbers-mode t)

;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
                term-mode-hook
                shell-mode-hook
                treemacs-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; stop dired buffers from accumulating
(setf dired-kill-when-opening-new-dired-buffer t)

(defun isearch-forward-backward ()
  "Start an incremental search, alternating direction with each press of C-s.
Return to original position if the search is canceled with ESC."
  (interactive)
  (let ((start-point (point)))  ; Save the starting position
    (condition-case nil
        (progn
          (if (eq last-command 'isearch-forward-backward)
              (isearch-repeat (if isearch-forward 'backward 'forward))
            (isearch-forward)))
      ((quit error)  ; Catch both quit and error signals
       (goto-char start-point)))))

;; Bind ESC in isearch-mode-map to cancel the search
(define-key isearch-mode-map (kbd "<escape>") 'isearch-abort)

(global-set-key (kbd "C-s") 'isearch-forward-backward)

(when (executable-find "xclip")
  ;; Set up clipboard copy
  (defun copy-to-xclip (text &optional push)
    (with-temp-buffer
      (insert text)
      (call-process-region (point-min) (point-max) "xclip" nil 0 nil "-selection" "clipboard")))

  ;; Set up clipboard paste
 (defun paste-from-xclip ()
    (let ((xclip-output (shell-command-to-string "xclip -o -selection clipboard")))
      (unless (string= (car kill-ring) xclip-output)
        xclip-output)))

  (setq interprogram-cut-function 'copy-to-xclip)
  (setq interprogram-paste-function 'paste-from-xclip))

;; avy-goto-char-2 similar to leap.nvim
(unless (package-installed-p 'undo-fu)
  (package-install 'undo-fu))
(use-package avy
  :straight t
  :init
  (avy-setup-default)
  :config
  (global-set-key (kbd "C-'") 'avy-goto-char-2))

;; Download Evil
(unless (package-installed-p 'evil)
  (package-install 'evil))

(use-package evil
  :straight t
  :init
  (setq evil-want-integration t) ;; This is optional since it's already set to t by default.
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump nil)
  :config
  (evil-mode 1)
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)
  ;; Use visual line motions even outside of visual-line-mode buffers
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal))

(use-package evil-collection
  :after evil
  :straight t
  :config
  (evil-collection-init))

;; ZQ to quit
(with-eval-after-load 'evil
  (define-key evil-normal-state-map (kbd "Z Q") 'kill-this-buffer))

;; Download undo-fu
(unless (package-installed-p 'undo-fu)
  (package-install 'undo-fu))

(use-package undo-fu)
;; larger undo history
(setq undo-limit 67108864) ; 64mb.
(setq undo-strong-limit 100663296) ; 96mb.
(setq undo-outer-limit 1006632960) ; 960mb.

;; tighter line spacing
(setq-default line-spacing 0)

;; treat _ as part of the word
(modify-syntax-entry ?_ "w")

;; No ugly button for checkboxes
(setq widget-image-enable nil)

;; Line cursor and no blink
(set-default 'cursor-type  '(bar . 3))
(blink-cursor-mode 0)

;; Paren mode is part of the theme
(show-paren-mode t)

;; simplified mode line
(defun mode-line-render (left right)
  (let* ((available-width (- (window-width) (length left) )))
    (format (format "%%s %%%ds" available-width) left right)))
(setq-default mode-line-format
              '((:eval
                 (mode-line-render
                  (format-mode-line (list
                                     (propertize "☰" 'face `(:inherit mode-line-buffer-id)
                                                 'help-echo "Mode(s) menu"
                                                 'mouse-face 'mode-line-highlight
                                                 'local-map   mode-line-major-mode-keymap)
                                     " %b "))
                  (format-mode-line "%4l:%2c  ")))))

;; Vertical window divider
(setq window-divider-default-right-width 1)
(setq window-divider-default-places 'right-only)
(window-divider-mode)

(unless (package-installed-p 'ace-window)
  (package-refresh-contents)
  (package-install 'ace-window))

(use-package ace-window
  :straight t
  :bind (("M-o" . ace-window))  ;; Bind M-o to ace-window
  :config
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))  ;; Customize keys for window selection
  (setq aw-background nil))  ;; Optional: make selected window more visible



;; Download mini-frame
(unless (package-installed-p 'mini-frame)
  (package-install 'mini-frame))

(use-package mini-frame
  :config
  (setq x-gtk-resize-child-frames 'resize-mode) ;; fix for gtk
  (mini-frame-mode))


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("712dda0818312c175a60d94ba676b404fc815f8c7e6c080c9b4061596c60a1db" "d41229b2ff1e9929d0ea3b4fde9ed4c1e0775993df9d998a3cdf37f2358d386b" "fbf73690320aa26f8daffdd1210ef234ed1b0c59f3d001f342b9c0bbf49f531c" default))
 '(mini-frame-show-parameters '((top . 0) (width . 0.7) (left . 0.5) (height . 15)))
 '(package-selected-packages
   '(posframe projectile mini-frame wakatime-mode evil-collection yasnippet evil no-littering auto-package-update))
 '(wakatime-cli-path "/usr/local/bin/wakatime-cli"))

(global-wakatime-mode)

;; fzf alternative
(use-package affe
  :config
  ;; Manual preview key for `affe-grep'
  (consult-customize affe-grep :preview-key "M-."))

(use-package posframe
  :straight t
  :config
  ;; Optional customization for debugging purposes
  (setq posframe-mouse-banish t))    ;; Move cursor away from popups

(use-package yasnippet)
(yas-global-mode 1)

(use-package eldoc
    :init
    (global-eldoc-mode))

(use-package rust-mode
  :init
  (setq rust-mode-treesitter-derive t))


(add-hook 'rust-mode-hook
          (lambda () (setq indent-tabs-mode nil)))

(use-package eglot
  :hook ((rust-mode nix-mode) . eglot-ensure)
  :config

  (fset #'jsonrpc--log-event #'ignore)
  (eglot-events-buffer-size 0)
  (eglot-booster-mode))

(add-hook 'rust-mode-hook 'eglot-ensure)

(setq treesit-language-source-alist
   '((bash "https://github.com/tree-sitter/tree-sitter-bash")
     (cmake "https://github.com/uyha/tree-sitter-cmake")
     (css "https://github.com/tree-sitter/tree-sitter-css")
     (elisp "https://github.com/Wilfred/tree-sitter-elisp")
     (go "https://github.com/tree-sitter/tree-sitter-go")
     (html "https://github.com/tree-sitter/tree-sitter-html")
     (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
     (json "https://github.com/tree-sitter/tree-sitter-json")
     (make "https://github.com/alemuller/tree-sitter-make")
     (markdown "https://github.com/ikatyang/tree-sitter-markdown")
     (python "https://github.com/tree-sitter/tree-sitter-python")
     (rust "https://github.com/tree-sitter/tree-sitter-rust")
     (toml "https://github.com/tree-sitter/tree-sitter-toml")
     (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
     (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
     (yaml "https://github.com/ikatyang/tree-sitter-yaml")))

;;(mapc #'treesit-install-language-grammar (mapcar #'car treesit-language-source-alist))

(use-package company
    :commands (global-company-mode)
    :init
    (global-company-mode)
    :custom
    (company-tooltip-align-annotations 't)
    (company-minimum-prefix-length 1)
    (company-idle-delay 0.1))

(use-package company-posframe
  :init
  (company-posframe-mode 1))


;; A few more useful configurations...
(use-package emacs
  :custom
  ;; TAB cycle if there are only few candidates
  ;; (completion-cycle-threshold 3)

  ;; Enable indentation+completion using the TAB key.
  ;; `completion-at-point' is often bound to M-TAB.
  (tab-always-indent 'complete)

  ;; Emacs 30 and newer: Disable Ispell completion function. As an alternative,
  ;; try `cape-dict'.
  (text-mode-ispell-word-completion nil)

  ;; Hide commands in M-x which do not apply to the current mode.  Corfu
  ;; commands are hidden, since they are not used via M-x. This setting is
  ;; useful beyond Corfu.
  (read-extended-command-predicate #'command-completion-default-include-p))

;; i don't know keybinds
(use-package which-key
 :config (which-key-mode))

;; no trailing whitespace
(use-package ws-butler
  :hook ((text-mode . ws-butler-mode)
         (prog-mode . ws-butler-mode)))

;; tabs for makefiles
(add-hook 'makefile-mode-hook
  '(lambda()
     (setq indent-tabs-mode t)
   )
)

(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :custom ((projectile-completion-system 'ivy))
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  ;; NOTE: Set this to the folder where you keep your Git repos!
  (when (file-directory-p "~/Documents")
    (setq projectile-project-search-path '("~/Documents")))
  (setq projectile-switch-project-action #'projectile-dired))

(use-package magit
  :commands magit-status
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

;; Download Modus theme
(unless (package-installed-p 'modus-themes)
  (package-install 'modus-themes))

(use-package modus-themes
  :load-path "themes")

;; All customizations here
(setq modus-themes-bold-constructs t
      modus-themes-italic-constructs t)

;; Maybe define some palette overrides, such as by using our presets
(setq modus-themes-common-palette-overrides
      modus-themes-preset-overrides-intense)

;; Load the theme of choice (built-in themes are always "safe" so they
;; do not need the `no-require' argument of `load-theme').
(load-theme 'modus-operandi-tinted)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(use-package nano-modeline
    :init
    (nano-modeline-prog-mode t)
    :custom
    (nano-modeline-position 'nano-modeline-footer)
    :hook
    (prog-mode           . nano-modeline-prog-mode)
    (text-mode           . nano-modeline-text-mode)
    (org-mode            . nano-modeline-org-mode)
    (pdf-view-mode       . nano-modeline-pdf-mode)
    (mu4e-headers-mode   . nano-modeline-mu4e-headers-mode)
    (mu4e-view-mode      . nano-modeline-mu4e-message-mode)
    (elfeed-show-mode    . nano-modeline-elfeed-entry-mode)
    (elfeed-search-mode  . nano-modeline-elfeed-search-mode)
    (term-mode           . nano-modeline-term-mode)
    (xwidget-webkit-mode . nano-modeline-xwidget-mode)
    (messages-buffer-mode . nano-modeline-message-mode)
    (org-capture-mode    . nano-modeline-org-capture-mode)
    (org-agenda-mode     . nano-modeline-org-agenda-mode))

(use-package company
    :commands (global-company-mode)
    :init
    (global-company-mode)
    :custom
    (company-tooltip-align-annotations 't)
    (company-minimum-prefix-length 1)
    (company-idle-delay 0.1))

(use-package markdown-mode
    :magic "\\.md\\'")

;; Make gc pauses faster by decreasing the threshold.
(setq gc-cons-threshold (* 2 1000 1000))
