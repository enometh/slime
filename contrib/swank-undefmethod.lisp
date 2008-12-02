;;; -*- Mode: LISP; Package: :cl-user; BASE: 10; Syntax: ANSI-Common-Lisp; -*-
;;;
;;; swank-undefmethod.lisp - Portable undefmethod
;;;
;;; Touched: Sat Apr 29 10:50:38 2006 +0530
;;; Bugs-To: Madhu <enometh@net.meer>
;;; License: public Domain.
;;;
(in-package :swank)

(defun parse-undefmethod-args (args)
  "Return values METHOD-QUALIFIERS and METHOD-SPECIALIZERS from
  parsing ARGS of the form (DEFMETHOD NAME &REST ARGS)."
  (let (p q method-qualifiers specializers)
    (loop (cond ((and (atom (setq p (car args))) p)
		 (push p method-qualifiers))
		(t (return)))		; now P is the specialized-lambda-list
       (setq args (cdr args)))
    (loop (when (null p) (return))
       (cond ((symbolp (setq q (car p)))
	      (case q
		((&aux &key &optional &rest &allow-other-keys) (return))
		(t			; handle eql specializers:
		 (push (find-class T) specializers))))
	     ((consp (cadr q)) (push (cadr q) specializers))
	     (t (push (find-class (cadr q)) specializers)))
       (setq p (cdr p)))
    (values (nreverse method-qualifiers) (nreverse specializers))))

(defmacro undefmethod (function-name &rest args)
  `(let ((fdefn (fdefinition ',function-name)))
     (multiple-value-bind (qualifiers specializers)
	 (parse-undefmethod-args ',args)
       (let ((meth (find-method fdefn qualifiers specializers)))
	 (when meth (remove-method fdefn meth))))))

(provide :swank-undefmethod)
