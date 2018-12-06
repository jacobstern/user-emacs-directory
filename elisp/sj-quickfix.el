;;; sj-quickfix.el --- Vim quickfix emulation. -*- lexical-binding: t -*-


;; Package-Requires: ((dash "2.14.1") (dash-functional "1.2.0") (emacs "24"))

;;; Commentary:

;;; Code:
(require 'dash)

(require 'cl-lib)
(require 'tabulated-list)

(cl-defstruct sj-quickfix-item
  (buffer nil :read-only t)
  (line nil :read-only t)
  (column nil :read-only t)
  (message nil :read-only t)
  (error-level nil :read-only t))

(cl-defstruct (sj-quickfix-item-active (:include sj-quickfix-item))
  (marker nil :read-only t))

(defun sj-quickfix--item-buffer-position (item)
  "Calculate the position within a live buffer for ITEM, or nil if the buffer is not live."
  (let ((buffer (sj-quickfix-item-buffer item)))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
	(save-restriction
	  (save-excursion
	    (let ((line (sj-quickfix-item-line item))
		  (column (sj-quickfix-item-column item)))
	      (widen)
	      (goto-char (point-min))
	      (forward-line (- line 1))
	      (when column
		(move-to-column column))
	      (point))))))))

(defun sj-quickfix--item-make-active (item)
  "Initialize a quickfix list item ITEM with a marker and other data structures for use in the quickfix buffer."
  (let ((buffer (sj-quickfix-item-buffer item)))
    (make-sj-quickfix-item-active
     :buffer buffer
     :line (sj-quickfix-item-line item)
     :column (sj-quickfix-item-column item)
     :message (sj-quickfix-item-message item)
     :error-level (sj-quickfix-item-error-level item)
     :marker (let* ((position (sj-quickfix--item-buffer-position item))
		    (marker (make-marker)))
	       (set-marker marker position buffer)))))

(defun sj-quickfix--item-from-flycheck-error (error)
  "Create a quickfix item from a Flycheck error ERROR."
  (when (and (fboundp 'flycheck-error-buffer))))

(defun sj-quickfix--item-active-dispose (item)
  "Clears out the marker of ITEM to avoid leaking markers."
  (set-marker (sj-quickfix-item-active-marker item) nil))

(defvar sj-quickfix--current-list nil
  "Current list of items in the quickfix list.")

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

(defun sj-quickfix--get-quickfix-window ()
  "Find an existing quickfix window in the current frame."
  (--first (window-parameter it 'quickfix) (window-list)))

(defun sj-quickfix--get-or-create-quickfix-window ()
  "Find an existing quickfix window or create a new one."
  (-if-let ((existing-window (sj-quickfix--get-quickfix-window)))
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

(defconst sj-quickfix-list-buffer "*Quickfix*"
  "The name of the buffer to show the quickfix list.")

(defconst sj-quickfix-list-format
  `[("Buffer" 15)
    ("Line" 5 :right-align t)
    ("Col" 5 :right-align t)
    ("Level" 8)
    ("Message" 0 t)])

(defconst sj-quickfix-list-padding 0
  "Padding used in the quickfix list.")

(defun sj-quickfix-list-open ()
  "Open the quickfix list, or select the window if is already open."
  (interactive)
  (unless (get-buffer sj-quickfix-list-buffer)
    (with-current-buffer (get-buffer-create sj-quickfix-list-buffer)
      (sj-quickfix-list-mode)))
  (-if-let (quickfix-window (sj-quickfix--get-quickfix-window))
      (select-window quickfix-window)
    (display-buffer
     (get-buffer sj-quickfix-list-buffer)
     '((sj-quickfix--display-buffer display-buffer-no-window)
       (allow-no-window . t)))))

(defun sj-quickfix-list-debug-kill-buffer-and-window ()
  (interactive)
  (-if-let (quickfix-window (sj-quickfix--get-quickfix-window))
      (delete-window quickfix-window))
  (-if-let (quickfix-buffer (get-buffer sj-quickfix-list-buffer))
      (kill-buffer quickfix-buffer)))

(define-derived-mode sj-quickfix-list-mode tabulated-list-mode
  "Quickfix"
  "Major mode for the quickfix list."
  (setq tabulated-list-format sj-quickfix-list-format
	tabulated-list-padding sj-quickfix-list-padding
	tabulated-list-entries nil
	window-size-fixed t)
  (tabulated-list-init-header))

(define-minor-mode sj-quickfix-mode
  "Toggle `sj-quickfix-mode'."
  :global t
  (sj-quickfix--setup)
  (sj-quickfix--teardown))

(provide 'sj-quickfix)
;;; sj-quickfix.el ends here
