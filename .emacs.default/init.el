(setq inhibit-startup-message t)

(setq ring-bell-function 'ignore) ; Disable beeps
(scroll-bar-mode -1) ; Disable visible scrollbar
(tool-bar-mode -1) ; Disable the toolbar
(tooltip-mode -1) ; Disable tooltips
(set-fringe-mode 10) ; Give some breathing room
(menu-bar-mode -1) ; Disable the menu bar

(load-theme 'doom-gruvbox t)

;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(column-number-mode)
(global-display-line-numbers-mode t)
(setq display-line-numbers-type 'relative)


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

(winner-mode 1)
(defvar window-zoomed? nil)
(defun toggle-window-zoom ()
  "Toggles the window zoom on the current selected window"
  (interactive)
  (if window-zoomed?
      (progn
        (setq window-zoomed? nil)
        (winner-undo))
    (progn
      (setq window-zoomed? t)
      (delete-other-windows))))


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

;; Add vim keybindings to vertico
;; TODO: put this inside "use-package" somehow
(eval-after-load 'vertico
  '(general-define-key :keymaps '(vertico-map)
    "C-J"      #'vertico-next-group
    "C-K"      #'vertico-previous-group
    "C-j"      #'vertico-next
    "C-k"      #'vertico-previous))

(defun split-window-vertically-with-focus ()
  (interactive)
  (split-window-vertically)
  (other-window 1))

(defun split-window-horizontally-with-focus ()
  (interactive)
  (split-window-horizontally)
  (other-window 1))

(use-package general
  :config
  (general-create-definer leader-keys
    :states 'normal
    :keymaps '(normal visual emacs override)
    :prefix "SPC")
  (leader-keys
    "," '(switch-to-buffer :which-key "Switch to buffer")
    "." '(find-file :which-key "Find file")
    ;; Window
    "w s" '(split-window-horizontally :which-key "Horizontal split")
    "w v" '(split-window-vertically :which-key "Vertical split")
    "w c" '(delete-window :which-key "Close window")
    "w z" '(toggle-window-zoom :which-key "Toggle window zoom")
    "w h" '(windmove-left :which-key "Move to left window")
    "w j" '(windmove-down :which-key "Move to lower window")
    "w k" '(windmove-up :which-key "Move to upper window")
    "w l" '(windmove-right :which-key "Move to right window"))

  (general-create-definer tmux-keys
    :keymaps '(normal visual emacs)
    :prefix "C-a")
  (tmux-keys
    "%" '(split-window-horizontally-with-focus :which-key "Horizontal split")
    "\"" '(split-window-vertically-with-focus :which-key "Vertical split")
    ;; TODO: create with empty buffer
    "c" '(eyebrowse-create-window-config :which-key "Create window config")
    "x y" '(delete-window :which-key "Delete window")
    "z" '(toggle-window-zoom :which-key "Zoom window in"))

  ;; TODO: I don't really know which key to use
  (general-define-key
    :keymaps '(normal insert visual emacs)
    "M-," '(eyebrowse-switch-to-window-config :which-key "Switch to window config"))

  (general-define-key
    :states 'normal
    :keymaps '(normal override)
    "K" '(lsp-ui-doc-glance :which-key "Show module docs"))

  ; Vi keybindings to navigate between splits
  (general-define-key
    :keymaps '(normal emacs)
    "C-h" '(windmove-left :which-key "Move to left window")
    "C-j" '(windmove-down :which-key "Move to lower window")
    "C-k" '(windmove-up :which-key "Move to upper window")
    "C-l" '(windmove-right :which-key "Move to right window"))
  )

(use-package eyebrowse
  :config
  (eyebrowse-mode t)
  (eyebrowse-setup-opinionated-keys))

;; UI improvements
(use-package all-the-icons
  :if (display-graphic-p))

(use-package doom-modeline
 :init (doom-modeline-mode 1))

;; Evil Mode
(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  :config
  (evil-mode 1))

(use-package evil-collection
  :after evil
  :ensure t
  :config
  (evil-collection-init))

(use-package evil-escape
  :init
  (setq-default evil-escape-key-sequence "jk")
  :config
  (evil-escape-mode))

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :config
  (lsp-enable-which-key-integration t))

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :custom
  (lsp-ui-doc-position 'at-point)
  (lsp-ui-doc-enable nil))

(use-package company
  :after lsp-mode
  :hook (prog-mode . company-mode)
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.0))

(use-package company-box
  :hook (company-mode . company-box-mode))

(use-package lsp-haskell)

(use-package haskell-mode
  :init
  (add-hook 'haskell-mode-hook #'lsp)
  (add-hook 'haskell-literate-mode-hook #'lsp))
