
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


;;(defun slime-c-p-c-completion-at-point ()
;;  #'slime-complete-symbol*)


(defvar orig-completion-styles)

(defun slime-c-p-c-setup-completion-styles ()
  (interactive)
  (assert (not (boundp 'orig-completion-styles)))
  (setq-local orig-completion-styles completion-styles)
  (setq completion-styles (delete 'partial-completion completion-styles))
  (setq completion-styles (cons  'partial-completion completion-styles)))


(defun slime-c-p-c-restore-completion-styles ()
  (interactive)
  (assert (boundp 'orig-completion-styles))
  (kill-local-variable 'completion-styles)
  (setq completion-styles orig-completion-styles)
  (kill-local-variable 'orig-completion-styles)
  (makunbound 'orig-completion-styles))


(cl-defun slime-c-p-c-completion-at-point ()
  "Complete the symbol at point.
slime-expand-abbreviations-and-complete
Perform completion similar to `elisp-completion-at-point'."
  (let* ((end (move-marker (make-marker) (slime-symbol-end-pos)))
         (beg (move-marker (make-marker) (slime-symbol-start-pos)))
         (prefix (buffer-substring-no-properties beg end))
	 (completion-result (slime-contextual-completions beg end))
         (completion-set (cl-first completion-result))
         (completed-prefix (cl-second completion-result)))
    (if (null completion-set)
	(progn
	   (slime-minibuffer-respecting-message
                "Can't find completion for \"%s\"" prefix)
	   (cl-return-from slime-c-p-c-completion-at-point
	     (list beg end nil))))
    ;; some XEmacs issue makes this distinction necessary
    (when t
    (cond ((> (length completed-prefix) (- end beg))
	   (goto-char end)
	   (insert-and-inherit completed-prefix)
	   (delete-region beg end)
	   (goto-char (+ beg (length completed-prefix))))
	  (t nil)))
    (cond ((and (member completed-prefix completion-set)
                (slime-length= completion-set 1))
	   (slime-minibuffer-respecting-message "Sole completion")
           (when slime-complete-symbol*-fancy
	     ;;Insert a space or close-paren based on arglist information.
	     (let ((arglist (slime-retrieve-arglist (slime-symbol-at-point))))
	       (unless (eq arglist :not-available)
		 (let ((args
			;; Don't intern these symbols
			(let ((obarray (make-vector 10 0)))
			  (cdr (read arglist))))
		       (function-call-position-p
			(save-excursion
			  (backward-sexp)
			  (equal (char-before) ?\())))
		   (when function-call-position-p
		     (setf (car completion-set)
			   (concat completed-prefix
				   (if (null args)
				       ")"
				     " "))))))))
	   (when (and (slime-background-activities-enabled-p)
                         (not (minibuffer-window-active-p (minibuffer-window))))
                (slime-echo-arglist))
	   (list beg (+ beg (length completed-prefix))
		 completion-set))
          ;; Incomplete
          (t
           (when (member completed-prefix completion-set)
             (slime-minibuffer-respecting-message "Complete but not unique")
	     )
	   (when slime-c-p-c-unambiguous-prefix-p
	     (let ((unambiguous-completion-length
		    (cl-loop for c in completion-set
			     minimizing (or (cl-mismatch completed-prefix c)
                                            (length completed-prefix)))))
	       (goto-char (+ beg unambiguous-completion-length))))
	   (let ((end (max (point) end)))
	     (list beg end completion-set))))))

(when nil
(cl-defun slime-c-p-c-completion-at-point ()
  "Complete the symbol at point.
sly-expand-abbreviations-and-complete
Perform completion similar to `elisp-completion-at-point'."
  (lexical-let (beg end)
    (list (setq beg (slime-symbol-start-pos))
	  (setq end (slime-symbol-end-pos))
	  (completion-table-dynamic
	   (lambda (_)
	     (first (slime-contextual-completions beg end)))
	   t)))))
