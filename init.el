(require 'package)

(package-initialize)

(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
        ("melpa-stable" . "https://stable.melpa.org/packages/")
        ("melpa" . "http://melpa.org/packages/")))

(setq x-underline-at-descent-line t)
(setq ns-use-srgb-colorspace nil)

(setq indent-tabs-mode nil)
(setq inhibit-splash-screen t)
(setq ring-bell-function 'ignore)
(setq enable-recursive-minibuffers t)
(setq column-number-mode t)

(setq confirm-kill-processes nil)
(setq confirm-kill-emacs #'y-or-n-p)

(setq enable-local-variables :safe)

(add-to-list 'default-frame-alist '(fullscreen . maximized))

(menu-bar-mode -1)
(toggle-scroll-bar -1)
(tool-bar-mode -1)
(global-auto-revert-mode +1)

(defun sj-inhibit-electric-pair-mode (_char)
  (minibufferp))

(setq electric-pair-inhibit-predicate #'sj-inhibit-electric-pair-mode)

(show-paren-mode +1)
(electric-pair-mode +1)

(delete-selection-mode +1)

(set-face-attribute 'default nil :font "Source Code Pro-13")

(add-hook 'prog-mode-hook #'display-line-numbers-mode)
(add-hook 'text-mode-hook #'display-line-numbers-mode)

(define-key global-map (kbd "C-x ;") #'ignore)
(define-key global-map (kbd "C-c o") #'mode-line-other-buffer)
(define-key global-map (kbd "M-z") #'zap-up-to-char)

(defun sj-do-grep-ag (arg)
  "Run helm-grep-ag in the current directory or project root"
  (interactive "P")
  (helm-grep-ag (or (projectile-project-root) (expand-file-name default-directory))
		arg))

(defun sj-do-grep-ag-file-type ()
  "Run helm-grep-ag and choose a file type"
  (interactive)
  (sj-do-grep-ag 4))

;; Automatically wrap i-search when reaching the end of the file
;; Source: https://stackoverflow.com/questions/285660/automatically-wrapping-i-search
(define-key isearch-mode-map [remap isearch-delete-char] 'isearch-del-char)

(defadvice isearch-search (after isearch-no-fail activate)
  (unless isearch-success
    (ad-disable-advice #'isearch-search 'after 'isearch-no-fail)
    (ad-activate #'isearch-search)
    (isearch-repeat (when isearch-forward 'forward))
    (ad-enable-advice #'isearch-search 'after 'isearch-no-fail)
    (ad-activate #'isearch-search)))

(defun query-replace-entire-buffer (wrapped &rest args)
  "Query replace the whole buffer or region."
  (unless (nth 3 args)
    (setf (nth 3 args)
          (if (use-region-p)
              (region-beginning)
            (point-min))))
  (unless (nth 4 args)
    (setf (nth 4 args)
          (if (use-region-p)
              (region-end)
            (point-max))))
  (apply wrapped args))

(advice-add 'query-replace :around #'query-replace-entire-buffer)
(advice-add 'query-replace-regexp :around #'query-replace-entire-buffer)

(eval-when-compile
  (require 'use-package))

(require 'delight)

(require 'general)

(general-unbind :states 'motion ",")

(general-create-definer sj-leader-def
  :prefix ","
  :states 'motion)

(sj-leader-def ":" #'eval-expression)

(use-package restart-emacs
  :ensure t)

(use-package comint
  :demand t
  :init
  (setq comint-prompt-read-only t)
  (setq comint-scroll-show-maximum-output nil))

(defun sj-helm-display (buffer &optional _resume)
  (interactive)
  (display-buffer-in-side-window (get-buffer buffer)
                                 '((window-height . 24) (side . bottom) (slot . 0))))

(use-package helm
  :ensure t
  :delight
  :general
  (sj-leader-def "," #'helm-M-x)
  (sj-leader-def "b" #'helm-mini)
  (general-nmap "s-g" #'sj-do-grep-ag)
  (general-nmap "s-G" #'sj-do-grep-ag-file-type)
  :bind (("M-x" . #'helm-M-x)
         ("C-x C-f" . #'helm-find-files)
         ("C-x b" . #'helm-mini)
         ("C-x C-b" . #'helm-buffers-list)
         ("C-x y" . #'helm-show-kill-ring)
         ("M-i" . #'helm-imenu)
         ("C-h SPC" . #'helm-all-mark-rings)
	 ("C-h a" . #'helm-apropos)
         :map helm-map
         ("<escape>" . #'helm-keyboard-quit))
  :init
  (setq helm-M-x-fuzzy-match t)
  (setq helm-buffers-fuzzy-matching t)
  (setq helm-recentf-fuzzy-match t)
  (setq helm-completion-in-region-fuzzy-match t)
  (setq helm-buffer-max-length 50)
  (setq helm-ff-file-name-history-use-recentf t)
  (setq helm-display-function #'sj-helm-display)
  (setq helm-grep-ag-command "rg --color=always --smart-case --no-heading --line-number %s %s %s")
  :config
  (require 'helm-config)
  (helm-mode 1)
  (add-to-list 'helm-boring-buffer-regexp-list "^:") ; Hide dired-sidebar buffer
  )

(use-package projectile
  :ensure t
  :delight '(:eval (concat "Proj[" (projectile-project-name) "]"))
  :init
  (setq projectile-enable-caching t)
  :config
  (projectile-mode 1)
  (setq projectile-switch-project-action #'projectile-vc))

(defun sj-refresh-projectile-list ()
  (interactive)
  (projectile-invalidate-cache nil)
  (helm-refresh))

(use-package helm-projectile
  :ensure t
  :bind (("s-p" . #'helm-projectile-find-file)
         ("s-s" . #'helm-projectile-switch-project)
         :map helm-projectile-find-file-map
         ("C-r" . #'sj-refresh-projectile-list))
  :general
  (general-nmap "C-p" #'helm-projectile)
  (general-nmap "C-n" #'ignore)
  :init
  (setq projectile-enable-caching t)
  (setq helm-projectile-truncate-lines t)
  (setq helm-projectile-sources-list
        '(helm-source-projectile-recentf-list helm-source-projectile-files-list))
  :config
  (helm-projectile-on))

(use-package wgrep-helm
  :ensure t
  :after helm)

(use-package whole-line-or-region
  :ensure t
  :delight whole-line-or-region-local-mode
  :config
  (whole-line-or-region-global-mode t))

(use-package multiple-cursors
  :ensure t
  :init
  (setq mc/always-run-for-all t))

;; (use-package winum
;;   :ensure t
;;   :init
;;   (setq winum-auto-setup-mode-line nil)
;;   :config
;;   (winum-mode))

(use-package powerline
  :ensure t)

(use-package spaceline
  :ensure t
  :after (helm evil)
  :init
  (setq spaceline-highlight-face-func #'spaceline-highlight-face-evil-state)
  :config
  (spaceline-spacemacs-theme)
  (spaceline-helm-mode))

(use-package avy
  :ensure t
  :bind (("C-'" . #'avy-goto-char-2)
         ("M-g w" . #'avy-goto-word-1)
         ("M-g f" . #'avy-goto-line))
  :general
  (sj-leader-def "s" #'avy-goto-char-2 :keymaps 'override)
  )

(use-package helm-swoop
  :ensure t
  :general
  (general-nmap "s-o" #'helm-swoop-without-pre-input)
  :config
  (setq helm-swoop-split-window-function #'sj-helm-display)
  )

(use-package undo-tree
  :ensure t
  :delight
  :config
  (global-undo-tree-mode 1)
  )

(use-package solarized-theme
  :ensure t
  :init
  (setq solarized-high-contrast-mode-line t))

(use-package company
  :ensure t
  :defer nil
  :delight
  :general
  (general-imap "C-/" #'company-complete-common)
  :init
  (setq company-tooltip-align-annotations t)
  :config
  (global-company-mode 1))

(use-package flycheck
  :ensure t
  :config
  (add-hook 'after-init-hook #'global-flycheck-mode))

(defun sj-colorize-compilation-buffer ()
  (ansi-color-apply-on-region compilation-filter-start (point-max)))

(defun sj-colorize-buffer (proc &rest args)
  (interactive)
  (with-current-buffer (process-buffer proc)
    (read-only-mode -1)
    (ansi-color-apply-on-region (point-min) (point-max))
    (read-only-mode 1)))

(use-package ansi-color
  :ensure t
  :config
  (add-hook 'compilation-filter-hook #'sj-colorize-compilation-buffer)
  ;; https://github.com/magit/magit/issues/1878
  (advice-add 'magit-process-filter :after #'sj-colorize-buffer))

(use-package js
  :demand t
  :config
  (setq js-indent-level 2))

(use-package css-mode
  :demand t
  :config
  (setq css-indent-offset 2))

(use-package typescript-mode
  :ensure t
  :after add-node-modules-path
  :init
  (setq typescript-indent-level 2))

(use-package prettier-js
  :ensure t
  :config
  (add-hook 'typescript-mode-hook #'prettier-js-mode)
  (add-hook 'js-jsx-mode-hook #'prettier-js-mode)
  (add-hook 'js-mode-hook #'prettier-js-mode)
  (add-hook 'web-mode-hook #'prettier-js-mode))

(defvar tsx-extra-pairs '((?' . ?')))

(use-package web-mode
  :ensure t
  :after (add-node-modules-path)
  :init
  (setq web-mode-enable-auto-quoting nil)
  (setq web-mode-enable-auto-indentation t)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-attr-indent-offset 2)
  (setq web-mode-attr-value-indent-offset 2)
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-jsx-depth-faces nil)   ; Disable JSX highlighting
  :config
;  (sp-local-pair 'web-mode "'" "'")
  (add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))
  (add-hook 'web-mode-hook
	    #'(lambda ()
                (when (string-equal "tsx" (file-name-extension buffer-file-name))
                  (add-node-modules-path)
		  (setq-local electric-pair-pairs (append electric-pair-pairs tsx-extra-pairs))
		  (setq-local electric-pair-text-pairs electric-pair-pairs)))))

(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  (company-mode +1))

(defun setup-tide-mode-for-js ()
  (interactive)
  (tide-setup)
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  (company-mode +1))

(use-package tide
  :ensure t
  :after typescript-mode
  :init
  (setq tide-tsserver-executable "/usr/local/bin/tsserver")
  :config
  (add-hook 'typescript-mode-hook #'setup-tide-mode)
  (add-hook 'web-mode-hook
            (lambda ()
              (when (string-equal "tsx" (file-name-extension buffer-file-name))
                (setup-tide-mode))))
  (add-hook 'js-mode-hook #'setup-tide-mode-for-js)
  (add-hook 'js-jsx-mode-hook #'setup-tide-mode-for-js))

(use-package vscode-icon
  :ensure t
  :commands (vscode-icon-for-file)
  :init
  (setq dired-sidebar-theme 'vscode))

(use-package dired-sidebar
  :ensure t
  :after evil
  :bind
  (("C-c b" . #'dired-sidebar-toggle-sidebar)
   :map dired-sidebar-mode-map
   ([remap keyboard-quit] . #'dired-sidebar-hide-sidebar))
  :config
  (advice-add #'dired-sidebar-find-file :after #'dired-sidebar-hide-sidebar)
  :init
  (setq dired-sidebar-icon-scale 0.9))

(use-package add-node-modules-path
  :ensure t
  :config
  (add-hook 'typescript-mode-hook #'add-node-modules-path)
  (add-hook 'js-jsx-mode-hook #'add-node-modules-path)
  (add-hook 'js-mode-hook #'add-node-modules-path))

(when (memq window-system '(mac ns x))
  (use-package exec-path-from-shell
    :ensure t
    :pin melpa
    :config
    (exec-path-from-shell-initialize)))

(use-package feature-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\.feature$" . feature-mode))
  (add-hook 'feature-mode-hook #'display-line-numbers-mode))

(use-package powershell
  :ensure t
  :commands (powershell))

(defun setup-psc-ide-mode()
  (interactive)
  (psc-ide-mode)
  (company-mode)
  (flycheck-mode)
  (turn-on-purescript-indentation))

(use-package purescript-mode
  :ensure t)

(use-package psc-ide
  :ensure t
  :config
  (add-hook 'purescript-mode-hook #'setup-psc-ide-mode)
  (add-hook 'purescript-mode-hook #'add-node-modules-path))

(use-package aggressive-indent
  :ensure t
  :config
  (add-hook 'emacs-lisp-mode-hook #'aggressive-indent-mode))

(use-package magit
  :ensure t
  :bind
  (("C-x g" . #'magit-status)
   ("C-x M-g" . #'magit-dispatch-popup))
  :general
  (sj-leader-def "g g" #'magit-status)
  (sj-leader-def "g f" #'magit-file-popup)
  (sj-leader-def "g x" #'magit-dispatch-popup)
  :init
  (setq magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(use-package gitignore-mode
  :ensure t
  :config
  (add-hook #'gitignore-mode-hook #'display-line-numbers-mode))

;; (use-package lsp-mode
;;   :ensure t)

;; (use-package company-lsp
;;   :after lsp-mode
;;   :ensure t
;;   :config
;;   (push 'company-lsp company-backends))

;; (use-package lsp-javascript-typescript
;;   :after lsp-mode
;;   :ensure t
;;   :config
;;   (add-hook 'ts-mode-hook #'lsp-javascript-typescript-enable))

;; (use-package smartparens
;;   :ensure t)

;; (use-package smartparens-config
;;   :ensure smartparens
;;   :config
;;   (smartparens-global-mode)
;;   (sp-use-smartparens-bindings))

(defun sj-fix-underscore-word-syntax ()
  (interactive)
  (modify-syntax-entry ?_ "w"))

(defun sj-fix-elisp-word-syntax ()
  (interactive)
  (modify-syntax-entry ?- "w")
  (modify-syntax-entry ?/ "w")
  (modify-syntax-entry ?* "w")
  )

(use-package evil
  :ensure t
  :init
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-cross-lines nil)
  (setq evil-search-module 'evil-search)
  (setq evil-ex-search-case 'sensitive)
  (setq evil-ex-search-vim-style-regexp t)
  (setq evil-normal-state-tag "Nrm")
  (setq evil-insert-state-tag "Ins")
  (setq evil-motion-state-tag "Mot")
  (setq evil-operator-state-tag "Pnd")
  (setq evil-visual-state-tag "Vis")
  (setq evil-emacs-state-tag "Emc")
  :config
  (evil-mode 1)
  (add-hook 'prog-mode-hook #'sj-fix-underscore-word-syntax)
  (add-hook 'emacs-lisp-mode-hook #'sj-fix-elisp-word-syntax)

  (general-evil-setup)
  (general-define-key :keymaps 'minibuffer-local-map "<escape>" 'minibuffer-keyboard-quit)

  (evil-define-text-object evil-defun (count &optional beg end type)
    "Select around defun."
    (save-excursion
      (mark-defun)
      (evil-range (region-beginning) (region-end) type :expanded t)
      ))
  (define-key evil-outer-text-objects-map "f" 'evil-defun)
  (define-key evil-inner-text-objects-map "f" 'evil-defun)
  )

  (use-package evil-collection
    :ensure t
    :after evil
    :init
    (setq evil-collection-term-sync-state-and-mode-p nil)
    (setq evil-collection-company-use-tng nil)
    :config
    (evil-collection-init))

(use-package evil-magit
  :ensure t)

(use-package evil-snipe
  :ensure t
  :after evil
  :delight evil-snipe-local-mode
  :init
  (setq evil-snipe-scope 'buffer)
  (setq evil-snipe-spillover-scope 'buffer)
  (setq evil-snipe-repeat-scope 'buffer)
  (setq evil-snipe-use-vim-sneak-bindings t)
  :config
  (evil-snipe-mode +1)
  (add-hook 'magit-mode-hook #'turn-off-evil-snipe-override-mode))

(use-package evil-commentary
  :ensure t
  :delight
  :config
  (evil-commentary-mode +1))

(use-package evil-surround
  :ensure t
  :config
  (global-evil-surround-mode 1))

(use-package org
  :demand t
  :general
  (sj-leader-def "l" #'org-store-link))

(use-package evil-org
  :ensure t
  :after org
  :config
  (add-hook 'org-mode-hook #'evil-org-mode)
  (evil-org-set-key-theme '(return))
  :general
  (general-nmap :keymaps 'evil-org-mode-map "gx" #'org-open-at-point))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(custom-enabled-themes (quote (solarized-light)))
 '(custom-safe-themes
   (quote
    ("8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" default)))
 '(package-selected-packages
   (quote
    (evil-org helm-swoop lsp-javascript-typescript company-lsp lsp-mode evil-surround smartparens evil-commentary wgrep-helm winum whole-line-or-region web-mode vscode-icon use-package tide spaceline solarized-theme restart-emacs purescript-mode psc-ide prettier-js powershell multiple-cursors helm-projectile gitignore-mode general feature-mode exec-path-from-shell evil-snipe evil-magit evil-collection dired-sidebar delight avy aggressive-indent add-node-modules-path)))
 '(selected-packages
   (quote
    (general evil-snipe evil-magit evil-collection evil web-mode winum whole-line-or-region vscode-icon use-package undo-tree tide spaceline solarized-theme restart-emacs purescript-mode psc-ide prettier-js powershell multiple-cursors magit helm-swoop helm-projectile feature-mode exec-path-from-shell dired-sidebar delight avy aggressive-indent add-node-modules-path))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; Local Variables:
;; eval: (flycheck-mode -1)
;; End:
