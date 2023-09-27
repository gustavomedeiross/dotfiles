;;; early-init.el --- Emacs early init -*- lexical-binding: t; -*-

;;; Commentary:

;; Early init file for Emacs 27 and above.

;;; Code:

(defconst emacs-start-time (current-time))

;; Prevent package.el loading packages prior to their init-file loading
(setq package-enable-at-startup nil)

(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

;; Disable white flash when Emacs starts up
(set-face-attribute 'default nil :background "#282828" :foreground "#D1C6A5")

;; Makes emacs frame maximized by default (useful for floating window managers systems)
(push '(fullscreen . maximized) initial-frame-alist)
(push '(ns-transparent-titlebar . t) default-frame-alist)

(setq frame-inhibit-implied-resize t
      frame-resize-pixelwise t)

(provide 'early-init)

;;; early-init.el ends here
