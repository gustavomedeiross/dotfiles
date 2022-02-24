;;;###autoload
(defun +vterm/here (arg)
  "Open a terminal buffer in the current window at project root.
If prefix ARG is non-nil, cd into `default-directory' instead of project root.
Returns the vterm buffer."
  (interactive "P")
  (+vterm--configure-project-root-and-display
   arg
   (lambda()
     (require 'vterm)
     ;; HACK forces vterm to redraw, fixing strange artefacting in the tty.
     (save-window-excursion
       (pop-to-buffer "*scratch*"))
     (let (display-buffer-alist)
       (vterm vterm-buffer-name)))))

(defun +vterm--configure-project-root-and-display (arg display-fn)
  "Sets the environment variable PROOT and displays a terminal using `display-fn`.
If prefix ARG is non-nil, cd into `default-directory' instead of project root.
Returns the vterm buffer."
  ;; TODO: fix this when I have projectile.el
  ;; (let* ((project-root (or (doom-project-root) default-directory))
  ;;        (default-directory
  ;;          (if arg
  ;;              default-directory
  ;;            project-root)))
  ;;   (setenv "PROOT" project-root)
    (funcall display-fn))
