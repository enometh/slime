;;; slime-undefmethod.el -*- Emacs-Lisp -*-
;;;
;;; Touched: Mon Dec 01 20:01:31 2008 +0530
;;; Bugs-To: Madhu <enometh@net.meer>
;;; License: GNU GPL (same license as Emacs)
;;;

(defun slime-undefmethod-init ()
  (slime-require :swank-undefmethod)
  (define-key slime-mode-map "\C-c\M-u" 'slime-undefmethod))

(defun slime-undefmethod ()
  "Remove the method which is defined at point."
  (interactive)
  (let ((form (slime-defun-at-point)))
    (when (string-match "^(defmethod " form)
      (slime-interactive-eval
       (replace-regexp-in-string "^(defmethod "
				 "(swank::undefmethod " form)))))

(provide 'slime-undefmethod)
