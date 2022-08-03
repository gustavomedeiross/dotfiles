;; TODO: add >> or << support in visual mode
;; TODO: remove mouse hover on suggestions lsp and/or company mode
;; TODO: remove checkers below modeline on mouse hover (lsp and/or flycheck)
;; TODO: hide special buffers in buffer-list
;; TODO: improve pop-up experience (vterm, vertico, which-key conflicts, etc.)
;; TODO: simplify modeline (remove perspectives & POPUP)
;; TODO: make C-c and C-r "instantaneous" in vterm & eshell
;; TODO: add projectile.el + persp-mode integration (already in "workspaces.el")
;; TODO: organize vterm.el and workspaces.el modules better
;; TODO: add org-mode


(setq inhibit-startup-message t) ; Remove welcome screen
(setq make-backup-files nil) ; Disable backup~ files
(setq create-lockfiles nil) ; Disable .#lock files
(setq ring-bell-function 'ignore) ; Disable beeps
(scroll-bar-mode -1) ; Disable visible scrollbar
(tool-bar-mode -1) ; Disable the toolbar
(tooltip-mode -1) ; Disable tooltips
(set-fringe-mode 10) ; Give some breathing room
(menu-bar-mode -1) ; Disable the menu bar
(global-eldoc-mode -1) ; Disable eldoc

;; Tabs
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq indent-line-function 'insert-tab)

;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(column-number-mode)
(global-display-line-numbers-mode t)
(setq display-line-numbers-type 'relative)


;; Make background transparent
(set-frame-parameter (selected-frame) 'alpha '(90 . 90))
(add-to-list 'default-frame-alist '(alpha . (90 . 90)))

;; Move autosave and backup files to ~/.auto-saves
(defvar user-temporary-file-directory
  (concat temporary-file-directory user-login-name "/"))
(make-directory user-temporary-file-directory t)
(setq backup-by-copying t)
(setq backup-directory-alist
      `(("." . ,user-temporary-file-directory)
        (,tramp-file-name-regexp nil)))
(setq auto-save-list-file-prefix
      (concat user-temporary-file-directory ".auto-saves-"))
(setq auto-save-file-name-transforms
      `((".*" ,user-temporary-file-directory t)))

;; Makes emacs frame maximized by default (useful for floating window managers systems)
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; MacOS
(setq mac-option-key-is-meta nil
      mac-command-key-is-meta t
      mac-command-modifier 'meta
      mac-option-modifier 'none)

;; Initialize package sources
(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

  ;; Initialize use-package on non-Linux platforms
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

(setq custom-file (concat user-emacs-directory "/custom.el"))

;; Package specifics

(use-package doom-themes
  :config
  (load-theme 'doom-gruvbox t))

(winner-mode 1)

(use-package command-log-mode)

(use-package doom-themes)

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 1))

;; Enable vertico
(use-package vertico
  :init
  (vertico-mode))

(use-package consult
  :hook (completion-list-mode . consult-preview-at-point-mode))

(defun split-window-vertically-with-focus ()
  (interactive)
  (evil-window-vsplit)
  (other-window 1))

(defun split-window-horizontally-with-focus ()
  (interactive)
  (evil-window-split)
  (other-window 1))

(use-package general
  :config
  (general-create-definer leader-keys
    :states 'normal
    :keymaps '(normal visual emacs override)
    :prefix "SPC")
  (leader-keys
    "," '(switch-to-buffer :which-key "Switch to buffer")
    "<" '(persp-switch-to-buffer :which-key "Switch to buffer (But to another perspective if necessary)")
    "." '(find-file :which-key "Find file")

    ;; Window
    "w s" '(evil-window-split :which-key "Horizontal split")
    "w S" '(split-window-horizontally-with-focus :which-key "Horizontal split with focus")
    "w v" '(evil-window-vsplit :which-key "Vertical split")
    "w V" '(split-window-vertically-with-focus :which-key "Vertical split with focus")
    "w c" '(evil-window-delete :which-key "Close window")
    "w q" '(evil-quit :which-key "Quit window")
    "w z" '(zoom-window-zoom :which-key "Toggle window zoom")
    "w h" '(windmove-left :which-key "Move to left window")
    "w j" '(windmove-down :which-key "Move to lower window")
    "w k" '(windmove-up :which-key "Move to upper window")
    "w l" '(windmove-right :which-key "Move to right window")

    ;; Buffers
    "b k" '(kill-buffer :which-key "Kill buffer")
    "b r" '(rename-buffer :which-key "Rename buffer")

    ;; Magit
    "g s" '(magit-status :which-key "Magit status")
    "g g" '(magit-status :which-key "Magit status")

    ;; Terminal
    "o t" '(multi-vterm :which-key "Open new vterm window")

    ;; Project
    "p f" '(consult-find :which-key "Run a fuzzy find against project files")
    "p s" '(consult-ripgrep :which-key "Run ripgrep against project files")

    ;; Workspaces
    "TAB ," '(persp-switch :which-key "Switch to a workspace")
    "TAB d" '(persp-kill :which-key "Delete workspace")
    "TAB k" '(persp-kill :which-key "Delete workspace")
    "TAB q" '(persp-kill :which-key "Delete workspace"))

  (general-create-definer tmux-keys
    :keymaps '(normal visual emacs)
    :prefix "C-a")
  (tmux-keys
    "%" '(split-window-vertically-with-focus :which-key "Horizontal split")
    "\"" '(split-window-horizontally-with-focus :which-key "Vertical split")
    "x y" '(delete-window :which-key "Delete window")
    "z" '(zoom-window-zoom :which-key "Toggle window zoom"))

  (general-define-key
    :states 'normal
    :keymaps '(normal override)
    "K" '(lsp-ui-doc-glance :which-key "Show module docs")
    "g r" '(lsp-find-references :which-key "Find usages of code"))

  (general-define-key
    :keymaps '(normal visual)
    "C-/" '(comment-or-uncomment-region :which-key "Comment or uncomment region"))

  ; Vi keybindings to navigate between splits
  (general-define-key
    :keymaps '(normal emacs)
    "C-h" '(windmove-left :which-key "Move to left window")
    "C-j" '(windmove-down :which-key "Move to lower window")
    "C-k" '(windmove-up :which-key "Move to upper window")
    "C-l" '(windmove-right :which-key "Move to right window")))

;; Add vim keybindings to vertico
;; TODO: put this inside "use-package" somehow
(eval-after-load 'vertico
  '(general-define-key :keymaps '(vertico-map)
    "C-J"      #'vertico-next-group
    "C-K"      #'vertico-previous-group
    "C-j"      #'vertico-next
    "C-k"      #'vertico-previous))

(use-package perspective
  :init (persp-mode)
  :custom
  (persp-suppress-no-prefix-key-warning t))

;; UI

(use-package all-the-icons
  :if (display-graphic-p))

(use-package doom-modeline
 :init (doom-modeline-mode 1))

(use-package emacs
  :custom
  (display-buffer-alist
   ;; '((inferior-haskell-mode
   '(("\\*haskell\\*"
      (display-buffer-in-side-window)
      (window-height . 0.25)
      (side . bottom)
      (slot . -1)))))

;; (use-package popper
;;   :bind (("C-`"   . popper-toggle-latest)
;;          ("M-`"   . popper-cycle)
;;          ("C-M-`" . popper-toggle-type))
;;   :init
;;   (setq popper-reference-buffers
;; 	'("\\*Messages\\*"
;; 	"Output\\*$"
;; 	"\\*Async Shell Command\\*"
;; 	help-mode
;; 	compilation-mode
;; 	inferior-haskell-mode
;; 	vterm-mode))
;;   (popper-mode +1)
;;   (popper-echo-mode +1))

(use-package zoom-window
  :custom (zoom-window-mode-line-color nil))

;; Evil Mode
(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-undo-system 'undo-fu)
  :config
  (evil-mode 1)
  (setq evil-insert-state-cursor 'box))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(use-package evil-escape
  :init
  (setq-default evil-escape-key-sequence "jk")
  :config
  (evil-escape-mode)
  (setq evil-escape-excluded-major-modes '(vterm-mode)))

;; Undo
(use-package undo-fu
  :ensure t
  :commands (undo-fu-only-undo)
  :defer nil)
(use-package undo-fu-session
  :ensure t
  :init (global-undo-fu-session-mode))

;; Shell
(use-package eshell
  :hook
  (eshell-mode . (lambda () (display-line-numbers-mode 0)))
  ;; Truncate buffer for performance
  (eshell-output-filter-functions . eshell-truncate-buffer)
  ;; Save command history when commands are entered
  (eshell-pre-command-hook . eshell-save-some-history)
  :config
  (setq eshell-history-size 10000
	eshell-buffer-maximum-lines 10000
	eshell-hist-ignoredups t
	eshell-scroll-to-bottom-on-input t)
  (evil-normalize-keymaps))

(use-package multi-vterm
  :commands vterm
  :hook
  (vterm-mode . (lambda ()
		  (display-line-numbers-mode 0)
		  (evil-insert-state)))
  :config
  (setq vterm-shell "zsh")
  (setq vterm-max-scrollback 10000))

(use-package exec-path-from-shell
  :init
  (exec-path-from-shell-initialize)
  :config
  (exec-path-from-shell-copy-envs '("PATH")))

;; Magit
(use-package magit
  :commands (magit-status magit-get-current-branch)
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)
  :config
  (general-define-key
   :keymaps 'transient-base-map
   "<escape>" 'transient-quit-one))

;; Projectile
(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :init
  (setq projectile-switch-project-action #'projectile-dired))

;; LSP & Auto-completion

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :config
  (lsp-enable-which-key-integration t))

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :custom
  (lsp-ui-doc-position 'at-point)
  (lsp-ui-doc-enable nil)
  (lsp-headerline-breadcrumb-enable nil)
  (lsp-ui-sideline-enable nil)
  (lsp-modeline-code-actions-enable nil)
  (lsp-eldoc-enable-hover nil)
  (lsp-modeline-diagnostics-enable nil)
  (lsp-signature-auto-activate nil)
  (lsp-signature-render-documentation nil))

(use-package company
  :after lsp-mode
  :hook (prog-mode . company-mode)
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.0))

(use-package company-box
  :hook (company-mode . company-box-mode))

(use-package flycheck
  :config
  (global-flycheck-mode))

(use-package flycheck-posframe
  :after flycheck
  :config
  (add-hook 'flycheck-mode-hook #'flycheck-posframe-mode))

(use-package json-mode)

(use-package yaml-mode)

;; Haskell
(use-package lsp-haskell)
(use-package haskell-mode
  :init
  (add-hook 'haskell-mode-hook #'lsp)
  (add-hook 'haskell-literate-mode-hook #'lsp))

;; OCaml
(use-package tuareg
  :hook (tuareg-mode . lsp-deferred)
  :config
  (lsp-register-client
    (make-lsp-client
     :new-connection (lsp-stdio-connection '("opam" "exec" "--" "ocamllsp"))
     :major-modes '(tuareg-mode)
     :server-id 'ocamlmerlin-lsp)))

;; TypeScript
(use-package typescript-mode
  :mode "\\.ts\\'"
  :hook (typescript-mode . lsp-deferred)
  :config
  (setq typescript-indent-level 2))

;; Kotlin
(use-package kotlin-mode
  :mode "\\.kt\\'"
  :hook (kotlin-mode . lsp-deferred))

;; PHP
(use-package php-mode
  :mode "\\.php\\'"
  :hook (php-mode . lsp-deferred))
