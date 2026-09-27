;;; -*- Mode: LISP; Package: :cl-user; BASE: 10; Syntax: ANSI-Common-Lisp; -*-
;;;
;;;   Time-stamp: <>
;;;   Touched: Mon Sep 14 06:41:48 2026 +0530 <enometh@net.meer>
;;;   Bugs-To: enometh@net.meer
;;;   Status: Experimental.  Do not redistribute
;;;   Copyright (C) 2026 Madhu.  All Rights Reserved.
;;;
(defpackage "SWANK/GENERA"
  (:use "CL" "SWANK/BACKEND"))
(in-package "SWANK/GENERA")

(defimplementation getpid ()
  0) ; TODO: implement

(defimplementation gray-package-name ()
  "GRAY-STREAMS")

;;; Compilation (stubs)

(defimplementation call-with-compilation-hooks (function)
  (funcall function))

(defimplementation swank-compile-string
    (string &key buffer position filename line column policy)
  (declare (ignore line column policy))
  (with-input-from-string (stream string)
    (compiler:compile-from-stream strem nil #'compiler::compiler-to-core nil)))

(defimplementation command-line-args ()
  nil)

(defimplementation swank-compile-file (input-file output-file load-p
                                             external-format
                                             &key policy)
  (declare (ignore policy))
  (compile-file input-file :output-file output-file :external-format
		external-format (or external-format :default)))


(defimplementation lisp-implementation-program ()
  "genera")

;;;; Debugging (stubs)

(defimplementation call-with-debugging-environment (debugger-loop-fn)
  (funcall debugger-loop-fn))

(defimplementation call-with-debugger-hook (hook fun)
  (let ((*debugger-hook* hook))
    (funcall fun)))

(defimplementation install-debugger-globally (function)
  (setf *debugger-hook* function))

(defimplementation compute-backtrace (start end)
  (declare (ignore start end))
  nil)

(defimplementation print-frame (frame stream)
  (format stream "~A" frame))

(defimplementation frame-source-location (frame-number)
  (declare (ignore frame-number))
  nil)

(defimplementation frame-catch-tags (frame-number)
  (declare (ignore frame-number))
  nil)

(defimplementation frame-locals (frame-number)
  (declare (ignore frame-number))
  nil)

(defimplementation frame-var-value (frame-number var-id)
  (declare (ignore frame-number var-id))
  nil)

(defimplementation eval-in-frame (form frame-number)
  (declare (ignore frame-number))
  (eval form))

(defimplementation frame-call (frame-number)
  (declare (ignore frame-number))
  nil)

(defimplementation print-condition (condition stream)
  (format stream "~A" condition))

(defimplementation condition-extras (condition)
  (declare (ignore condition))
  nil)

(defimplementation call-with-syntax-hooks (fn)
  (funcall fn))

(defimplementation package-local-nicknames (package)
  (declare (ignore package))
  nil)

(defimplementation set-stream-timeout (stream timeout)
  (declare (ignore stream timeout))
  nil)

(defimplementation default-directory ()
  (namestring *default-pathname-defaults*))

(defimplementation set-default-directory (directory)
  (setf *default-pathname-defaults* (pathname directory))
  (default-directory))

(defimplementation macroexpand-all (form &optional env)
  (declare (ignore env))
  (macroexpand form))
