
(defcustom slime-c-p-c-unambiguous-prefix-p t
  "If true, set point after the unambigous prefix.
If false, move point to the end of the inserted text."
  :type 'boolean
  :group 'slime-ui)

(defcustom slime-complete-symbol*-fancy nil
  "Use information from argument lists for DWIM'ish symbol completion."
  :group 'slime-mode
  :type 'boolean)


;; FIXME: this is the old code to display completions.  Remove it once
;; `slime-complete-symbol*' and `slime-fuzzy-complete-symbol' can be
;; used together with `completion-at-point'.

(defvar slime-completions-buffer-name "*Completions*")

;; FIXME: can probably use quit-window instead
(make-variable-buffer-local
 (defvar slime-complete-saved-window-configuration nil
   "Window configuration before we show the *Completions* buffer.
This is buffer local in the buffer where the completion is
performed."))

(make-variable-buffer-local
 (defvar slime-completions-window nil
   "The window displaying *Completions* after saving window configuration.
If this window is no longer active or displaying the completions
buffer then we can ignore `slime-complete-saved-window-configuration'."))

(defun slime-complete-maybe-save-window-configuration ()
  "Maybe save the current window configuration.
Return true if the configuration was saved."
  (unless (or slime-complete-saved-window-configuration
              (get-buffer-window slime-completions-buffer-name))
    (setq slime-complete-saved-window-configuration
          (current-window-configuration))
    t))

(defun slime-complete-delay-restoration ()
  (add-hook 'pre-command-hook
            'slime-complete-maybe-restore-window-configuration
            'append
            'local))

(defun slime-complete-forget-window-configuration ()
  (setq slime-complete-saved-window-configuration nil)
  (setq slime-completions-window nil))

(defun slime-complete-restore-window-configuration ()
  "Restore the window config if available."
  (remove-hook 'pre-command-hook
               'slime-complete-maybe-restore-window-configuration)
  (when (and slime-complete-saved-window-configuration
             (slime-completion-window-active-p))
    (save-excursion (set-window-configuration
                     slime-complete-saved-window-configuration))
    (setq slime-complete-saved-window-configuration nil)
    (when (buffer-live-p slime-completions-buffer-name)
      (kill-buffer slime-completions-buffer-name))))

(defun slime-complete-maybe-restore-window-configuration ()
  "Restore the window configuration, if the following command
terminates a current completion."
  (remove-hook 'pre-command-hook
               'slime-complete-maybe-restore-window-configuration)
  (condition-case err
      (cond ((cl-find last-command-event "()\"'`,# \r\n:")
             (slime-complete-restore-window-configuration))
            ((not (slime-completion-window-active-p))
             (slime-complete-forget-window-configuration))
            (t
             (slime-complete-delay-restoration)))
    (error
     ;; Because this is called on the pre-command-hook, we mustn't let
     ;; errors propagate.
     (message "Error in slime-complete-restore-window-configuration: %S"
              err))))

(defun slime-completion-window-active-p ()
  "Is the completion window currently active?"
  (and (window-live-p slime-completions-window)
       (equal (buffer-name (window-buffer slime-completions-window))
              slime-completions-buffer-name)))

(defun slime-display-completion-list (completions start end)
  (let ((savedp (slime-complete-maybe-save-window-configuration)))
    (with-output-to-temp-buffer slime-completions-buffer-name
      (display-completion-list completions)
      (with-current-buffer standard-output
        (setq completion-base-position (list start end))
        (set-syntax-table lisp-mode-syntax-table)))
    (when savedp
      (setq slime-completions-window
            (get-buffer-window slime-completions-buffer-name)))))

(defun slime-display-or-scroll-completions (completions start end)
  (cond ((and (eq last-command this-command)
              (slime-completion-window-active-p))
         (slime-scroll-completions))
        (t
         (slime-display-completion-list completions start end)))
  (slime-complete-delay-restoration))

(defun slime-scroll-completions ()
  (let ((window slime-completions-window))
    (with-current-buffer (window-buffer window)
      (if (pos-visible-in-window-p (point-max) window)
          (set-window-start window (point-min))
        (save-selected-window
          (select-window window)
          (scroll-up))))))

(defun slime-minibuffer-respecting-message (format &rest format-args)
  "Display TEXT as a message, without hiding any minibuffer contents."
  (let ((text (format " [%s]" (apply #'format format format-args))))
    (if (minibuffer-window-active-p (minibuffer-window))
        (minibuffer-message text)
      (message "%s" text))))
