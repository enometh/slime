;;; -*- Mode: LISP; Package: :cl-user; BASE: 10; Syntax: ANSI-Common-Lisp; -*-
;;;
;;;   Time-stamp: <>
;;;   Touched: Mon Sep 14 06:41:48 2026 +0530 <enometh@net.meer>
;;;   Bugs-To: enometh@net.meer
;;;   Status: Experimental.  Do not redistribute
;;;   Copyright (C) 2026 Madhu.  All Rights Reserved.
;;;
(defpackage swank/genera
  (:use cl swank/backend))
(in-package swank/genera)


#+genera
(defimplementation gray-package-name ()
  "GRAY-STREAMS")
