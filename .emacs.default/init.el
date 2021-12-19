(setq inhibit-startup-message t)

(setq ring-bell-function 'ignore) ; Disable beeps
(scroll-bar-mode -1) ; Disable visible scrollbar
(tool-bar-mode -1) ; Disable the toolbar
(tooltip-mode -1) ; Disable tooltips
(set-fringe-mode 10) ; Give some breathing room
(menu-bar-mode -1) ; Disable the menu bar

(load-theme 'wombat)

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

;; Package specifics

(use-package command-log-mode)

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


(use-package general
  :config
  (general-create-definer leader-keys
    :keymaps '(normal visual emacs)
    :prefix "SPC")
  (leader-keys
    "," '(switch-to-buffer :which-key "Switch to buffer")
    "." '(find-file :which-key "Find file"))

  (general-create-definer tmux-keys
    :keymaps '(normal visual emacs)
    :prefix "C-a")
  (tmux-keys
    "%" '(split-window-horizontally :which-key "Horizontal split")
    "\"" '(split-window-vertically :which-key "Vertical split")
    "x y" '(delete-window :which-key "Delete window"))

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

(setq custom-file (concat user-emacs-directory "/custom.el"))
