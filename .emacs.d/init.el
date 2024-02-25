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

(set-frame-font "Fira Code 12" nil t)

;; Makes emacs frame maximized by default (useful for floating window managers systems)
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Auto-refresh dired
(add-hook 'dired-mode-hook 'auto-revert-mode)

;; Easily copy-paste files with split windows
(setq dired-dwim-target t)

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

;; Copy current file path and line number to clipboard
(defun copy-current-line-position-to-clipboard ()
    "Copy current line in file to clipboard as '</path/to/file>:<line-number>'."
    (interactive)
    (let* ((project-path (projectile-project-root))
          (path-with-line-number
           (concat (dired-replace-in-string project-path "" (buffer-file-name)) ":" (number-to-string (line-number-at-pos)))))
      (kill-new path-with-line-number)
      (message (concat path-with-line-number " copied to clipboard"))))

;; MacOS
(setq mac-option-key-is-meta nil
      mac-command-key-is-meta t
      mac-command-modifier 'meta
      mac-option-modifier 'none)

(setq custom-file (concat user-emacs-directory "/custom.el"))

;; straight.el

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Replace use-package with straight-use-package
(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

;; Package specifics

(use-package string-inflection)

(use-package multiple-cursors)

(use-package git-link)

(defun gm/org-font-setup ()
  ;; Replace list hyphen with dot
  (font-lock-add-keywords 'org-mode
                          '(("^ *\\([-]\\) "
                             (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

  ;; Ensure that anything that should be fixed-pitch in Org files appears that way
  (set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-code nil   :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-table nil   :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch))

(use-package org
  :hook (org-mode . (lambda ()
                      (org-indent-mode)
                      (visual-line-mode 1)))
  :config
  (setq org-ellipsis " ▾")
  (gm/org-font-setup)

  ;; TODO keywords.
  (setq org-todo-keywords
        '((sequence "TODO(t)" "NEXT(n)" "PROG(p)" "INTR(i)" "DONE(d)")))
  (setq org-capture-templates
        '(("t" "Todo" entry (file+headline "~/org/tasks.org" "Tasks")
           "* TODO %?\n  %i\n  %a")
          ("j" "Journal" entry (file+datetree "~/org/journal.org")
           "* %?\nEntered on %U\n  %i\n  %a"))))

(use-package org-agenda
  :ensure nil
  :straight nil
  :bind
  (:map org-agenda-mode-map
        ("j" . evil-next-line)
        ("k" . evil-previous-line))
  :config
  (setq org-agenda-files '("~/org/tasks.org"))
  ;; Show the daily agenda by default.
  (setq org-agenda-span 'day)
  ;; Hide tasks that are scheduled in the future.
  (setq org-agenda-todo-ignore-scheduled 'future)
  ;; Use "second" instead of "day" for time comparison.
  ;; It hides tasks with a scheduled time like "<2020-11-15 Sun 11:30>"
  (setq org-agenda-todo-ignore-time-comparison-use-seconds t)
  ;; Hide the deadline prewarning prior to scheduled date.
  (setq org-agenda-skip-deadline-prewarning-if-scheduled 'pre-scheduled)
  ;; Customized view for the daily workflow. (Command: "C-c a n")
  (setq org-agenda-custom-commands
        '(("n" "Agenda / INTR / PROG / NEXT"
           ((agenda "" nil)
            (todo "INTR" nil)
            (todo "PROG" nil)
            (todo "NEXT" nil))
           nil))))

(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

(use-package tex-site
  :straight (auctex :host github
                    :repo "emacsmirror/auctex"
                    :files (:defaults (:exclude "*.el.in")))
  :defer t
  :config
  (setq TeX-auto-save nil))

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 1))

(use-package vertico
  :init
  (vertico-mode))

(use-package consult
  :hook (completion-list-mode . consult-preview-at-point-mode))

(defun gm/split-window-vertically-with-focus ()
  (interactive)
  (evil-window-vsplit)
  (other-window 1))

(defun gm/split-window-horizontally-with-focus ()
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
    "w S" '(gm/split-window-horizontally-with-focus :which-key "Horizontal split with focus")
    "w v" '(evil-window-vsplit :which-key "Vertical split")
    "w V" '(gm/split-window-vertically-with-focus :which-key "Vertical split with focus")
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

    ;; Forge
    "f o p" '(forge-browse-pullreqs :which-key "Forge open pull requests")

    ;; Org Mode
    "o t" '(org-todo :which-key "Org TODO")
    "o a" '(org-agenda :which-key "Org agenda")
    "o c" '(org-capture :which-key "Org capture")
    "o q" '(org-set-tags-command :which-key "Org set tags")
    "o s" '(org-schedule :which-key "Org schedule")

    ;; Project
    "p f" '(consult-find :which-key "Run a fuzzy find against project files")
    "p s" '(consult-ripgrep :which-key "Run ripgrep against project files")
    "p p" '(projectile-persp-switch-project :which-key "Switch to project in a new perspective")
    "p e" '(project-eshell :which-key "Open a new eshell instance in the project directory")

    ;; Workspaces
    "TAB ," '(persp-switch :which-key "Switch to a workspace")
    "TAB r" '(persp-rename :which-key "Rename workspace")
    "TAB d" '(persp-kill :which-key "Delete workspace")
    "TAB k" '(persp-kill :which-key "Delete workspace")
    "TAB q" '(persp-kill :which-key "Delete workspace"))

  (general-define-key
    :states 'normal
    :keymaps '(normal override)
    "K" '(lsp-ui-doc-glance :which-key "Show module docs")
    "g r" '(lsp-find-references :which-key "Find usages of code"))

  (general-define-key
    :keymaps '(insert normal)
    "C-SPC" '(company-complete :which-key "Trigger completion at point"))

  (general-define-key
    :keymaps '(normal visual)
    "C-/" '(comment-or-uncomment-region :which-key "Comment or uncomment region")))

;; Add vim keybindings to vertico
;; TODO: put this inside "use-package" somehow
(eval-after-load 'vertico
  '(general-define-key :keymaps '(vertico-map)
    "C-J"      #'vertico-next-group
    "C-K"      #'vertico-previous-group
    "C-j"      #'vertico-next
    "C-k"      #'vertico-previous))

;; UI

(use-package dashboard
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-center-content t)
  (setq dashboard-icon-type 'all-the-icons)
  (setq dashboard-display-icons-p t)
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  (setq dashboard-page-separator "\n\n")
  (setq dashboard-items '((recents  . 5)
                          (bookmarks . 5)
                          (projects . 5)
                          (agenda . 5)
                          (registers . 5)))
  (setq dashboard-heading-icons '((recents   . "history")
                                    (bookmarks . "bookmark")
                                    (agenda    . "calendar")
                                    (projects  . "rocket")
                                    (registers . "database")))
  (setq dashboard-projects-switch-function #'projectile-persp-switch-project))

(use-package all-the-icons
  :if (display-graphic-p))

(use-package all-the-icons-dired
  :init
  (add-hook 'dired-mode-hook 'all-the-icons-dired-mode)
  :custom
  (all-the-icons-dired-monochrome nil))

(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :config
  (setq doom-modeline-modal nil)
  (setq doom-modeline-battery t)
  (setq doom-modeline-buffer-encoding nil)
  (setq doom-modeline-lsp nil)
  (setq doom-modeline-checker-simple-format t))

(use-package doom-themes
  :config
  (load-theme 'doom-gruvbox t))

(use-package emacs
  :custom
  (display-buffer-alist
   '(("\\*haskell\\*"
      (display-buffer-in-side-window)
      (window-height . 0.25)
      (side . bottom)
      (slot . -1)))))

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
  (evil-escape-mode))

;; Undo

(use-package undo-fu
  :commands (undo-fu-only-undo)
  :defer nil)

(use-package undo-fu-session
  :init (undo-fu-session-global-mode))

;; Shell

(use-package eshell
  :hook
  (eshell-mode . (lambda ()
                   (display-line-numbers-mode 0)
                   (setenv "TERM" "xterm-256color")))
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

(defun spawn-eshell (name)
  "Create a new named eshell buffer"
  (interactive "MName: ")
  (setq name (concat "$" name))
  (eshell)
  (rename-buffer name))

(use-package exec-path-from-shell
  :init
  (exec-path-from-shell-initialize)
  :config
  (exec-path-from-shell-copy-envs '("PATH")))

(use-package eat
  :straight
  (:type git
         :host codeberg
         :repo "akib/emacs-eat"
         :files ("*.el" ("term" "term/*.el") "*.texi"
                 "*.ti" ("terminfo/e" "terminfo/e/*")
                 ("terminfo/65" "terminfo/65/*")
                 ("integration" "integration/*")
                 (:exclude ".dir-locals.el" "*-tests.el"))))

(use-package vterm
  :config
  (setq vterm-max-scrollback 10000))

;; Magit

(use-package magit
  :commands (magit-status magit-get-current-branch)
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)
  :config
  (general-define-key
   :keymaps 'transient-base-map
   "<escape>" 'transient-quit-one))

(use-package forge
  :after magit
  :config
  (setq auth-sources '("~/.authinfo")))

;; Perspectives + Projectile

(use-package perspective
  :init (persp-mode)
  :custom
  (persp-suppress-no-prefix-key-warning t))

(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :init
  ;; When switching to a new project, magit-status if the project is a git repo, otherwise dired
  (setq projectile-switch-project-action #'magit-status))

(use-package persp-projectile
    :straight (persp-projectile
               :host github
               :repo "bbatsov/persp-projectile")
    :commands (projectile-persp-switch-project))

;; LSP & Auto-completion

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :config
  (lsp-enable-which-key-integration t)
  (setq lsp-enable-file-watchers t)
  (setq lsp-file-watch-threshold nil))

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

(use-package direnv
 :config
 (direnv-mode))

(use-package json-mode)

(use-package yaml-mode)

;; Nix

(use-package nix-mode
  :mode "\\.nix\\'")

;; OCaml
(use-package tuareg
  :hook (tuareg-mode . lsp-deferred)
  :config
  (lsp-register-client
    (make-lsp-client
     :new-connection (lsp-stdio-connection '("opam" "exec" "--" "ocamllsp"))
     :major-modes '(tuareg-mode)
     :server-id 'ocamlmerlin-lsp)))

;; Haskell
(use-package lsp-haskell)
(use-package haskell-mode
  :init
  (add-hook 'haskell-mode-hook #'lsp)
  (add-hook 'haskell-literate-mode-hook #'lsp))

;; Elixir
(use-package elixir-mode
  :hook (elixir-mode . lsp-deferred))

;; Rust
(use-package rust-mode
  :hook (rust-mode . lsp-deferred))

;; TypeScript
(use-package typescript-mode
  :mode "\\.ts\\'"
  :hook (typescript-mode . lsp-deferred)
  :config
  (setq typescript-indent-level 2))

;; Lilypond
(use-package lilypond-mode
  ;; :straight (:host github :repo "lilypond/lilypond" :files ("elisp" "*.el"))
  :straight nil
  :load-path "~/.emacs.d/site-lisp"
  :mode ("\\.ly\\'" . LilyPond-mode))

;; Copilot
(use-package copilot
  :straight (:host github :repo "zerolfx/copilot.el" :files ("dist" "*.el"))
  :init
  (add-hook 'prog-mode-hook 'copilot-mode)
  :config
  (define-key copilot-completion-map (kbd "C-<return>") 'copilot-accept-completion)
  (define-key copilot-completion-map (kbd "C-RET") 'copilot-accept-completion))
