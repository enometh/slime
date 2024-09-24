;;; swank-mk-defsystem.lisp -- MK-DEFSYSTEM support

;; madhu 240924 - adapted from github.com/enometh/sly
;; contrib/slynk-mk-defsystem.lisp

;;
;; Authors: Daniel Barlow <dan@telent.net>
;;          Marco Baringer <mb@bese.it>
;;          Edi Weitz <edi@agharta.de>
;;          Francois-Rene Rideau <tunes@google.com>
;;          and others
;; License: Public Domain
;;

(in-package :swank)
#+sbcl(declaim (sb-ext:muffle-conditions style-warning))

;;; Now for SLIME-specific stuff

(defun map-system-components (fn system)
  (map-component-subcomponents fn (mk::ensure-system system)))

(defun map-component-subcomponents (fn component)
  (when component
    (funcall fn component)
    (when (member (mk::component-type component) '(:module :defsystem))
      (dolist (c (mk::component-components component))
        (map-component-subcomponents fn c)))))

;;; Maintaining a pathname to component table
;;; Maintaining a pathname to system table

(defvar *pathname-component* (make-hash-table :test 'equal))
(defvar *pathname-system* (make-hash-table :test #'equal))

(defvar *current-source-file* nil)

(defun clear-pathname-component-table ()
  (clrhash *pathname-component*))

(defun clear-pathname-system-table ()
  (clrhash *pathname-system*))

(defvar *current-system* nil)
(defun register-system-pathnames (system)
  (let ((*current-system* system))
    (map-system-components 'register-component-pathname system)))

(defun recompute-pathname-component-table ()
  (clear-pathname-component-table)
  (map nil 'register-system-pathnames (mk:defined-systems)))

(defun pathname-component (x)
  (or
   (let ((truename (probe-file x)))
     (or (when truename
	   (gethash (pathname x) *pathname-component*))
	(gethash x *pathname-component*)))))

(defun pathname-system (pathname)
  (or
   (let ((truename (probe-file pathname)))
     (when truename
       (gethash (namestring truename) *pathname-system*)))
   (gethash (namestring pathname) *pathname-system*)))

(defun register-component-pathname (component)
  (when (member (mk::component-type component) '(:file))
    (let ((p (mk::component-full-pathname component :source)))
      (when p
	(let ((truename (probe-file p)))
	  (when truename
	    (let ((path (namestring truename)))
	      (assert (mk::component-p *current-system*))
	      (setf (gethash path *pathname-system*) *current-system*)
	      (setf (gethash path *pathname-component*) component))))))))

(recompute-pathname-component-table)

;;; This is a crude hack, see MK-DEFSYSTEM's LP #481187.

(defslimefun who-depends-on (system)
  (let* ((system (mk::ensure-system system))
	 (name (mk::canonicalize-system-name (mk::component-name system)))
	 ret)
    (maphash (lambda (k v)
	       (dolist (dep (mk::component-depends-on v))
		 (when (equal name
			      (mk::canonicalize-system-name dep))
		   (pushnew k ret :test #'equal))))
	     mk::*defined-systems*)
    ret))

(defmethod xref-doit ((type (eql :depends-on)) thing)
  (when (typep thing '(or string symbol))
    (loop for dependency in (who-depends-on thing)
          for asd-file = (mk::system-definition-pathname dependency)
          when asd-file
          collect (list dependency
                        (swank/backend:make-location
                         `(:file ,(namestring asd-file))
                         `(:position 1)
                         `(:snippet ,(format nil "(defsystem :~A" dependency)
                           :align t))))))

(defslimefun operate-on-system-for-emacs (system-name operation &rest keywords)
  "Compile and load SYSTEM using MK-DEFSYSTEM.
Record compiler notes signalled as `compiler-condition's."
  (collect-notes
   (lambda ()
     (apply #'operate-on-system system-name operation keywords))))

(defun operate-on-system (system-name operation-name &rest keyword-args)
  "Perform OPERATION-NAME on SYSTEM-NAME using MK-DEFSYSTEM.
The KEYWORD-ARGS are passed on to the operation.
Example:
\(operate-on-system \"cl-ppcre\" 'compile-op :force t)"
;;  (handler-case
      (with-compilation-hooks ()
        (apply #'mk:oos system-name operation-name keyword-args)
        t))
;;    ((or mk:compile-error #+mk-defsystem3 mk-defsystem/lisp-build:compile-file-error)
;;      () nil)))

(defun unique-string-list (&rest lists)
  (sort (delete-duplicates (apply #'append lists) :test #'string=) #'string<))

(defslimefun list-all-systems-in-central-registry ()
  "Returns a list of all systems in MK-DEFSYSTEM's central registry
AND in its source-registry. (legacy name)"
  (unique-string-list
   (mapcar #'pathname-name
	   (loop for dir in mk:*central-registry*
		 for defaults = (eval dir)
		 when defaults
		 append (directory (merge-pathnames "*.system" defaults)
				   #+clozure :follow-links
				   #+clozure nil)))))

(defslimefun list-all-systems-known-to-mk-defsystem ()
  "Returns a list of all systems MK-DEFSYSTEM knows already."
  (let (ret)
    (maphash (lambda (k v)
	       (declare (ignore k))
	       (push (mk::component-name v) ret))
	     mk::*defined-systems*)
    ret))

(defslimefun list-mk-defsystem-systems ()
  "Returns the systems in MK-DEFSYSTEM's central registry and those which MK-DEFSYSTEM
already knows."
  (unique-string-list
   (list-all-systems-known-to-mk-defsystem)
   (list-all-systems-in-central-registry)))



(defslimefun mk-defsystem-system-loaded-p (name)
  (find name MK::*MODULES* :test #'equalp))

(defslimefun mk-defsystem-system-directory (name)
  (namestring (translate-logical-pathname (mk::system-source-directory name))))

(defslimefun mk-defsystem-determine-system (file buffer-package-name)
  (or
   (and file
        (pathname-system file))
   (and file
        (progn
          ;; If not found, let's rebuild the table first
          (recompute-pathname-component-table)
          (pathname-system file)))
   ;; If we couldn't find an already defined system,
   ;; try finding a system that's named like BUFFER-PACKAGE-NAME.
   (loop with package = (guess-buffer-package buffer-package-name)
      for name in (swank::package-names package)
      for system = (mk:find-system name :load-or-nil)
      when (and system
                (or (not file)
                    (pathname-system file)))
      return (mk::component-name system))))

(defslimefun delete-system-fasls (name)
  (mk:clean-system name :propagate nil))

(defvar *recompile-system* nil)

(defslimefun reload-system (name)
  (let ((*recompile-system* (mk:find-system name)))
    (operate-on-system-for-emacs name ':load)))


(defslimefun mk-defsystem-system-files (name)
  (let (ret)
    (mk::system-map-files (mk:find-system name)
			  (lambda (x) (push x ret))
			  :recursively-handle-deps :module
			  :type :source)
    ret))

(provide :swank-mk-defsystem)
