(defun +workspace-message (message &optional type)
  "Show an 'elegant' message in the echo area next to a listing of workspaces."
  (message "%s" (+workspace--message-body message type)))

(defun +workspace-error (message &optional noerror)
  "Show an 'elegant' error in the echo area next to a listing of workspaces."
  (funcall (if noerror #'message #'error)
           "%s" (+workspace--message-body message 'error)))

(defun +workspace--message-body (message &optional type)
  (concat (+workspace--tabline)
          (propertize " | " 'face 'font-lock-comment-face)
          (propertize (format "%s" message)
                      'face (pcase type
                              ('error 'error)
                              ('warn 'warning)
                              ('success 'success)
                              ('info 'font-lock-comment-face)))))

(defun +workspace-list-names ()
  "Return the list of names of open workspaces."
  (reverse (persp-names)))

(defun +workspace-current-name ()
  "Get the name of the current workspace."
  (persp-current-name))

(defun +workspace-exists (name)
  "Return t if NAME is the name of an existing workspace."
  (member name (+workspace-list-names)))

(defun +workspace-switch (name)
  "Switch to another workspace named NAME (a string).  If the workspace does not exists, throws an error."
  (if (+workspace-exists name)
      (persp-switch name)
    (error "%s is not an available workspace" name)))

(defun +workspace/display ()
  "Display a list of workspaces (like tabs) in the echo area."
  (interactive)
  (let (message-log-max)
    (message "%s" (+workspace--tabline))))

(defun +workspace--tabline (&optional names)
  (let ((names (or names (+workspace-list-names)))
        (current-name (+workspace-current-name)))
    (mapconcat
     #'identity
     (cl-loop for name in names
              for i to (length names)
              collect
              (propertize (format " [%d] %s " (1+ i) name)
                          'face (if (equal current-name name)
                                    '+workspace-tab-selected-face
                                  '+workspace-tab-face)))
     " ")))

(defface +workspace-tab-selected-face '((t (:inherit highlight)))
  "The face for selected tabs displayed by `+workspace/display'"
  :group 'persp-mode)

(defface +workspace-tab-face '((t (:inherit default)))
  "The face for selected tabs displayed by `+workspace/display'"
  :group 'persp-mode)


(defun +workspace/switch-to (index)
  "Switch to a workspace at a given INDEX.
A negative number will start from the end of the workspace list."
  (interactive
   (list (or current-prefix-arg
	     (completing-read "Switch to workspace: " (+workspace-list-names)))))
  (when (and (stringp index)
             (string-match-p "^[0-9]+$" index))
    (setq index (string-to-number index)))
  (condition-case-unless-debug ex
      (let ((names (+workspace-list-names))
            (old-name (+workspace-current-name)))
        (cond ((numberp index)
               (let ((dest (nth index names)))
                 (unless dest
                   (error "No workspace at #%s" (1+ index)))
                 (+workspace-switch dest)))
              ((stringp index) ;; is this desirable?
               (+workspace-switch index))
              (t
               (error "Not a valid index: %s" index)))
        (unless (called-interactively-p 'interactive)
          (if (equal (+workspace-current-name) old-name)
              (+workspace-message (format "Already in %s" old-name) 'warn)
	    (+workspace/display))))))


(defmacro +workspace/make-switch-to-nth (nth)
  (list 'defun (intern (format "+workspace/switch-to-%d" nth)) ()
	(list 'interactive)
	(list '+workspace/switch-to nth)))

(+workspace/make-switch-to-nth 0)
(+workspace/make-switch-to-nth 1)
(+workspace/make-switch-to-nth 2)
(+workspace/make-switch-to-nth 3)
(+workspace/make-switch-to-nth 4)
(+workspace/make-switch-to-nth 5)
(+workspace/make-switch-to-nth 6)
(+workspace/make-switch-to-nth 7)
(+workspace/make-switch-to-nth 8)
(+workspace/make-switch-to-nth 9)
