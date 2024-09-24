;; -*- lexical-binding: t; -*-

;; madhu 240924 adapted from https://github.com/enometh/sly
;; contrib/sly-mk-defsystem.el


(require 'slime)
(require 'cl-lib)
(require 'grep)

(define-slime-contrib slime-mk-defsystem
  "MK-DEFSYSTEM support."
  (:authors "Daniel Barlow       <dan@telent.net>"
            "Marco Baringer      <mb@bese.it>"
            "Edi Weitz           <edi@agharta.de>"
            "Stas Boukarev       <stassats@gmail.com>"
            "Tobias C Rittweiler <tcr@freebits.de>"
	    "Madhu               <enometh@meer.net>")
  (:license "GPL")
  (:slime-dependencies slime-repl)
  (:swank-dependencies swank-mk-defsystem)
  (:on-load
   (add-to-list 'slime-edit-uses-xrefs :depends-on t)
   (define-key slime-who-map [?d] 'slime-who-depends-on)))

;;; NOTE: `system-name' is a predefined variable in Emacs.  Try to
;;; avoid it as local variable name.

;;; Utilities

(defgroup slime-mk-defsystem nil
  "MK-DEFSYSTEM support for Slime."
  :prefix "slime-mk-defsystem-"
  :group 'slime)

(defvar slime-system-history nil
  "History list for MK-DEFSYSTEM system names.")

(defun slime-read-system-name (&optional prompt 
                                         default-value
                                         determine-default-accurately)
  "Read a system name from the minibuffer, prompting with PROMPT.
If no `default-value' is given, one is tried to be determined: if
`determine-default-accurately' is true, by an RPC request which
grovels through all defined systems; if it's not true, by looking
in the directory of the current buffer."
  (let* ((completion-ignore-case nil)
         (prompt (or prompt "System"))
         (system-names (slime-eval `(swank:list-mk-defsystem-systems)))
         (default-value
           (or default-value 
               (if determine-default-accurately
                   (slime-determine-mk-defsystem-system (buffer-file-name)
                                                (slime-current-package))
                   (slime-find-mk-defsystem-file (or default-directory
                                            (buffer-file-name))
                                        system-names))))
         (prompt (concat prompt (if default-value
                                    (format " (default `%s'): " default-value)
                                    ": "))))
    (completing-read prompt (slime-bogus-completion-alist system-names)
                     nil nil nil
                     'slime-system-history default-value)))



(defun slime-find-mk-defsystem-file (directory system-names)
  "Tries to find an MK-DEFSYSTEM system definition file in the
`directory' and returns it if it's in `system-names'."
  (let ((asd-files
         (directory-files (file-name-directory directory) nil "\.system$")))
    (cl-loop for system in asd-files
             for candidate = (file-name-sans-extension system)
             when (cl-find candidate system-names :test #'string-equal)
             do (cl-return candidate))))

(defun slime-determine-mk-defsystem-system (filename buffer-package)
  "Try to determine the mk-defsystem system that `filename' belongs to."
  (slime-eval
   `(swank:mk-defsystem-determine-system ,(and filename
                                       (slime-to-lisp-filename filename))
                                 ,buffer-package)))

(defun slime-who-depends-on-rpc (system)
  (slime-eval `(swank:who-depends-on ,system)))

(defcustom slime-mk-defsystem-collect-notes t
  "Collect and display notes produced by the compiler.

See also `slime-highlight-compiler-notes' and
`slime-compilation-finished-hook'."
  :group 'slime-mk-defsystem)

(defun slime-mk-defsystem-operation-finished-function (system)
  (if slime-mk-defsystem-collect-notes
      #'slime-compilation-finished
      (slime-curry (lambda (system result)
                     (let (slime-highlight-compiler-notes
                           slime-compilation-finished-hook)
                       (slime-compilation-finished result)))
                   system)))

(defun slime-oos (system operation &rest keyword-args)
  "Operate On System."
  (slime-save-some-lisp-buffers)
  (slime-display-output-buffer)
  (message "Performing MK-DEFSYSTEM %S%s on system %S"
           operation (if keyword-args (format " %S" keyword-args) "")
           system)
  (slime-repl-shortcut-eval-async
   `(swank:operate-on-system-for-emacs ,system ',operation ,@keyword-args)
   (slime-mk-defsystem-operation-finished-function system)))


;;; Interactive functions

(defun slime-load-system (&optional system)
  "Compile and load an MK-DEFSYSTEM system.  

Default system name is taken from first file matching *.asd in current
buffer's working directory"
  (interactive (list (slime-read-system-name)))
  (slime-oos system :load))

(defun slime-open-system (name &optional load interactive)
  "Open all files in an MK-DEFSYSTEM system."
  (interactive (list (slime-read-system-name) nil t))
  (when (or load
            (and interactive
                 (not (slime-eval `(swank:mk-defsystem-system-loaded-p ,name)))
                 (y-or-n-p "Load it? ")))
    (slime-load-system name))
  ;; ;madhu 241011 - this throws an error because of pathname syntax, but we really don't want to open all files in the system
  (slime-eval-async
   `(swank:mk-defsystem-system-files ,name)
   (lambda (files)
     (when files
       (let ((files (mapcar 'slime-from-lisp-filename
                            (nreverse files))))
         (find-file-other-window (car files))
         (mapc 'find-file (cdr files)))))))

(defun slime-browse-system (name)
  "Browse files in an MK-DEFSYSTEM system using Dired."
  (interactive (list (slime-read-system-name)))
  (slime-eval-async `(swank:mk-defsystem-system-directory ,name)
   (lambda (directory)
     (when directory
       (dired (slime-from-lisp-filename directory))))))

(if (fboundp 'rgrep)
    (defun slime-rgrep-system (sys-name regexp)
      "Run `rgrep' on the base directory of an MK-DEFSYSTEM system."
      (interactive (progn (grep-compute-defaults)
                          (list (slime-read-system-name nil nil t)
                                (grep-read-regexp))))
      (rgrep regexp "*.lisp"
             (slime-from-lisp-filename
              (slime-eval `(swank:mk-defsystem-system-directory ,sys-name)))))
    (defun slime-rgrep-system ()
      (interactive)
      (error "This command is only supported on GNU Emacs >21.x.")))

(if (boundp 'multi-isearch-next-buffer-function)
    (defun slime-isearch-system (sys-name)
      "Run `isearch-forward' on the files of an MK-DEFSYSTEM system."
      (interactive (list (slime-read-system-name nil nil t)))
      (let* ((files (mapcar 'slime-from-lisp-filename
                            (slime-eval `(swank:mk-defsystem-system-files ,sys-name))))
             (multi-isearch-next-buffer-function
              (let* 
                  ((buffers-forward  (mapcar #'find-file-noselect files))
                   (buffers-backward (reverse buffers-forward)))
                #'(lambda (current-buffer wrap)
                    ;; Contrarily to the docstring of
                    ;; `multi-isearch-next-buffer-function', the first
                    ;; arg is not necessarily a buffer. Report sent
                    ;; upstream. (2009-11-17)
                    (setq current-buffer (or current-buffer (current-buffer)))
                    (let* ((buffers (if isearch-forward
                                        buffers-forward
                                        buffers-backward)))
                      (if wrap
                          (car buffers)
                          (second (memq current-buffer buffers))))))))
        (isearch-forward)))
    (defun slime-isearch-system ()
      (interactive)
      (error "This command is only supported on GNU Emacs >23.1.x.")))

(defun slime-read-query-replace-args (format-string &rest format-args)
  (let* ((minibuffer-setup-hook (slime-minibuffer-setup-hook))
         (minibuffer-local-map slime-minibuffer-map)
         (common (query-replace-read-args (apply #'format format-string
                                                 format-args)
                                          t t)))
    (list (nth 0 common) (nth 1 common) (nth 2 common))))

(defun slime-query-replace-system (name from to &optional delimited)
  "Run `query-replace' on an MK-DEFSYSTEM system."
  (interactive (let ((system (slime-read-system-name nil nil t)))
                 (cons system (slime-read-query-replace-args
                               "Query replace throughout `%s'" system))))
  (condition-case c
      ;; `tags-query-replace' actually uses `query-replace-regexp'
      ;; internally.
      (tags-query-replace (regexp-quote from) to delimited
                          '(mapcar 'slime-from-lisp-filename
                            (slime-eval `(swank:mk-defsystem-system-files ,name))))
    (error
     ;; Kludge: `tags-query-replace' does not actually return but
     ;; signals an unnamed error with the below error
     ;; message. (<=23.1.2, at least.)
     (unless (string-equal (error-message-string c) "All files processed")
       (signal (car c) (cdr c)))        ; resignal
     t)))

(defun slime-query-replace-system-and-dependents
    (name from to &optional delimited)
  "Run `query-replace' on an MK-DEFSYSTEM system and all the systems
depending on it."
  (interactive (let ((system (slime-read-system-name nil nil t)))
                 (cons system (slime-read-query-replace-args
                               "Query replace throughout `%s'+dependencies"
                               system))))
  (slime-query-replace-system name from to delimited)
  (dolist (dep (slime-who-depends-on-rpc name))
    (when (y-or-n-p (format "Descend into system `%s'? " dep))
      (slime-query-replace-system dep from to delimited))))

(defun slime-delete-system-fasls (name)
  "Delete FASLs produced by compiling a system."
  (interactive (list (slime-read-system-name)))
  (slime-repl-shortcut-eval-async
   `(swank:delete-system-fasls ,name)
   'message))

(defun slime-reload-system (system)
  "Reload an MK-DEFSYSTEM system without reloading its dependencies."
  (interactive (list (slime-read-system-name)))
  (slime-save-some-lisp-buffers)
  (slime-display-output-buffer)
  (message "Performing MK-DEFSYSTEM LOAD-OP on system %S" system)
  (slime-repl-shortcut-eval-async
   `(swank:reload-system ,system)
   (slime-mk-defsystem-operation-finished-function system)))

(defun slime-who-depends-on (system-name)
  (interactive (list (slime-read-system-name)))
  (slime-xref :depends-on system-name))

(defun slime-save-system (system)
  "Save files belonging to an MK-DEFSYSTEM system."
  (interactive (list (slime-read-system-name)))
  (slime-eval-async
      `(swank:mk-defsystem-system-files ,system)
    (lambda (files)
      (dolist (file files)
        (let ((buffer (get-file-buffer (slime-from-lisp-filename file))))
          (when buffer
            (with-current-buffer buffer
              (save-buffer buffer)))))
      (message "Done."))))


;;; REPL shortcuts


(defslime-repl-shortcut slime-repl-load-system ("load-system")
  (:handler (lambda ()
              (interactive)
              (slime-oos (slime-read-system-name) :load)))
  (:one-liner "Compile (as needed) and load an MK-DEFSYSTEM system."))

(defslime-repl-shortcut slime-repl-compile-system ("compile-system")
  (:handler (lambda ()
              (interactive)
              (slime-oos (slime-read-system-name) :compile)))
  (:one-liner "Compile (but not load) an MK-DEFSYSTEM system."))


(defslime-repl-shortcut slime-repl-open-system ("open-system")
  (:handler 'slime-open-system)
  (:one-liner "Open all files in an MK-DEFSYSTEM system."))

(defslime-repl-shortcut slime-repl-browse-system ("browse-system")
  (:handler 'slime-browse-system)
  (:one-liner "Browse files in an MK-DEFSYSTEM system using Dired."))

(defslime-repl-shortcut slime-repl-delete-system-fasls ("delete-system-fasls")
  (:handler 'slime-delete-system-fasls)
  (:one-liner "Delete FASLs of an MK-DEFSYSTEM system."))

(defslime-repl-shortcut slime-repl-reload-system ("reload-system")
  (:handler 'slime-reload-system)
  (:one-liner "Recompile and load an MK-DEFSYSTEM system."))

;;;
;;;
;;;
(cl-defun asd-hack-prompt-system-name (&optional source-dir)
  (let* ((source-dir (or source-dir default-directory))
         (asd-files (directory-files source-dir nil "\\.asd$"))
         (names (mapcar 'file-name-sans-extension asd-files))
         (fallback (file-name-nondirectory
                    (directory-file-name source-dir))))
    (completing-read "system name: "
                     (cl-adjoin fallback  names :test #'equal)
                     nil
                     nil
                     (or (car names) fallback))))


(cl-defun asd-hack-dump-defsystem-file (dest-location name source-dir)
  (interactive
   (let (guessed-name guessed-dir guessed-dest)
     (list (progn
             (setq guessed-dir
                   (read-directory-name "Source dir: "
                                        default-directory
                                        default-directory
                                        t
                                        ))
             (setq guessed-name
                   (asd-hack-prompt-system-name guessed-dir))
             (setq guessed-dest
                   (file-name-concat (if (not (consp current-prefix-arg))
                                         guessed-dir
                                       "/dev/shm/")
                                     (file-name-with-extension guessed-name "sy\
stem")))
             (read-file-name "Dest location: "
                             (file-name-directory guessed-dest)
                             guessed-dest
                             nil
                             (file-name-nondirectory guessed-dest)))
           guessed-name
           guessed-dir)))
  (let ((form `(make::asd-hack-dump-defsystem-file
                ,dest-location ,name
                (cl-user::sanitize-tilde-in-pathname
                 (cl::namestring ,source-dir))
                :root-dir-form
                (cl::format nil "~S" ,source-dir))
              ))
    (message "%S" form)
    (when (or (not (file-exists-p dest-location))
              (y-or-n-p "overwrite? "))
      (slime-eval form "MAKE"))))

(unless (fboundp 'find-system)
(defun find-system (str)
  (interactive "sSystem name: ")
  (let ((evaler (cond ((and (boundp 'sly-current-thread)
			    sly-current-thread)
		       'sly-eval)
		      ((and (boundp 'slime-current-thread)
			    slime-current-thread)
		       'slime-eval)
		      ((error "no slime or sly connection"))))
	(pkg str))
    (if (symbolp pkg) (setq pkg (symbol-name pkg)))
    (setq pkg (downcase pkg))
    (let ((path (funcall evaler `(cl:namestring (mk::find-system-definition-pathname ,pkg)))))
      (when (file-exists-p path)
	(find-file path))))))

(provide 'slime-mk-defsystem)
