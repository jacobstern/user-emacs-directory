;;; sj-quickfix.el --- Vim quickfix emulation. -*- lexical-binding: t -*-


;; Package-Requires: ((dash "2.14.1") (dash-functional "1.2.0") (emacs "24"))

;;; Commentary:

;;; Code:
(require 'dash)

(defun sj-quickfix--setup ()
  "Initialize `sj-quickfix-mode'.")

(defun sj-quickfix--bottom-right-window-cmp (it other)
  "Comparison predicate for finding a window at the bottom right.
Return t if IT is closer to the bottom right corner of the frame than
OTHER."
  (-let (((_ _ it-right it-bottom) (window-edges it))
	 ((_ _ other-right other-bottom) (window-edges other)))
    (or (> it-right other-right) (> it-bottom other-bottom))))

(defun sj-quickfix--bottom-right-window ()
  "Return the window at the bottom right corner of the frame."
  (-max-by #'sj-quickfix--bottom-right-window-cmp (window-list)))

(defun sj-quickfix--get-or-create-quickfix-window ()
  "Find an existing quickfix window or create a new one."
  (-if-let ((existing-window
	     (--first (window-parameter it 'quickfix) (window-list))))
      existing-window
    (let ((new-window
	   (split-window (sj-quickfix--bottom-right-window) -11)))
      (set-window-parameter new-window 'quickfix t)
      new-window)))

(defun sj-quickfix--teardown ()
  "Clean up on disabling `sj-quickfix-mode'.")

(defun sj-quickfix--display-buffer (buffer &optional alist)
  "Display BUFFER in the quickfix window with the param ALIST."
  (window--display-buffer
   buffer
   (sj-quickfix--get-or-create-quickfix-window) 'window alist))

(defun sj-quickfix-flycheck-list-errors ()
  "Display flycheck errors in the quickfix window."
  (interactive)
  (when (fboundp 'flycheck-list-errors)
    (let ((display-buffer-alist
	   (cons `(,(rx bos "*Flycheck errors*" eos)
		   (sj-quickfix--display-buffer display-buffer-no-window)
		   (allow-no-window . t))
		 display-buffer-alist)))
      (flycheck-list-errors))))

(define-minor-mode sj-quickfix-mode
  "Toggle `sj-quickfix-mode'."
  :global t
  (sj-quickfix--setup)
  (sj-quickfix--teardown))

(provide 'sj-quickfix)
;;; sj-quickfix.el ends here
