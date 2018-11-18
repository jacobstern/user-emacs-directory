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

;; Never need to fully type out "yes" or "no"
(defalias #'yes-or-no-p #'y-or-n-p)

(setq enable-local-variables :safe)

(add-to-list 'default-frame-alist '(fullscreen . maximized))

(menu-bar-mode -1)
(toggle-scroll-bar -1)
(tool-bar-mode -1)
(global-auto-revert-mode +1)

(show-paren-mode +1)
(electric-pair-mode +1)

(delete-selection-mode +1)

(set-face-attribute 'default nil :font "Source Code Pro-13")

(add-hook 'prog-mode-hook #'display-line-numbers-mode)
(add-hook 'text-mode-hook #'display-line-numbers-mode)

(define-key global-map (kbd "C-;") #'comment-line)
(define-key global-map (kbd "C-x ;") #'ignore)
(define-key global-map (kbd "M-o") #'mode-line-other-buffer)
(define-key global-map (kbd "C-M-S-g") #'goto-line)

(defun js-do-grep-ag (arg)
  "Run helm-grep-ag in the current directory or project root"
  (interactive "P")
  (helm-grep-ag (or (projectile-project-root) (expand-file-name default-directory))
		arg))

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

(use-package restart-emacs
  :ensure t)

(use-package comint
  :demand t
  :init
  (setq comint-prompt-read-only t)
  (setq comint-scroll-show-maximum-output nil))

;; (use-package evil
;;   :ensure t
;;   :init
;;   (setq evil-want-keybinding nil)
;;   (setq evil-want-C-u-scroll t)
;;   (setq evil-want-Y-yank-to-eol t)
;;   (setq evil-cross-lines nil)
;;   (setq evil-search-module 'evil-search)
;;   (setq evil-ex-search-case 'sensitive)
;;   (setq evil-ex-search-vim-style-regexp t)
;;   :config
;;   (evil-mode 1)
;;   (general-def 'motion
;;     ";" 'evil-ex
;;     ":" 'evil-repeat-find-char))


;; (use-package evil-collection
;;   :ensure t
;;   :after evil
;;   :init
;;   (setq evil-collection-company-use-tng nil)
;;   :config
;;   (evil-collection-init))

(use-package helm
  :ensure t
  :delight
  :bind (("M-x" . #'helm-M-x)
         ("C-x C-f" . #'helm-find-files)
         ("C-x b" . #'helm-mini)
         ("C-M-S-y" . #'helm-show-kill-ring)
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
  :config
  (require 'helm-config)
  (helm-mode 1)
  (add-to-list 'helm-boring-buffer-regexp-list "^:") ; Hide dired-sidebar buffer
  ;; (setq ;; helm-display-function 'helm-display-buffer-in-own-frame
  ;;  helm-display-function 'my-helm-display-child-frame
  ;;  helm-display-buffer-reuse-frame t
  ;;  helm-display-buffer-width 80)  
  )

(use-package projectile
  :ensure t
  :delight '(:eval (concat "Proj[" (projectile-project-name) "]"))
  :init
  (setq projectile-enable-caching t)
  :config
  (projectile-mode 1)
  (define-key global-map (kbd "C-M-S-a") #'js-do-grep-ag))

(use-package helm-projectile
  :ensure t
  :bind-keymap ("C-c p" . projectile-command-map)
  :bind ("C-M-S-p" . #'helm-projectile)
  :init
  (autoload 'projectile "helm-projectile-switch-project" t)
  (setq projectile-enable-caching t)
  (setq helm-projectile-truncate-lines t)
  (setq projectile-switch-project-action #'helm-projectile)
  :config
  (helm-projectile-on))

(use-package whole-line-or-region
  :ensure t
  :delight whole-line-or-region-local-mode
  :config
  (whole-line-or-region-global-mode t))

(use-package multiple-cursors
  :ensure t
  :init
  (setq mc/always-run-for-all t))

(use-package winum
  :ensure t
  :init
  (setq winum-auto-setup-mode-line nil)
  :config
  (winum-mode))

(use-package powerline
  :ensure t)

(use-package spaceline
  :ensure t
  :after (helm)
  :config
  (spaceline-spacemacs-theme)
  (spaceline-helm-mode))

(general-create-definer sj-leader-def
  :prefix ","
  :states 'motion)

(general-unbind 'operator ",")

(use-package avy
  :ensure t
  :bind (("C-'" . #'avy-goto-char-2)
         ("M-g w" . #'avy-goto-word-1)
         ("M-g f" . #'avy-goto-line))
  ;; :general
  ;; (sj-leader-def "s" #'avy-goto-char-2 :keymaps 'override)
  )

(use-package helm-swoop
  :ensure t
  :bind ("C-M-S-s" . #'helm-swoop)
  :init
  (setq helm-swoop-pre-input-function
	(lambda () "")))

(use-package undo-tree
  :ensure t
  :delight
  :config
  (global-undo-tree-mode 1)
  :bind (("C-M-S-r" . #'undo-tree-redo)))

(use-package solarized-theme
  :ensure t
  :init
  (setq solarized-high-contrast-mode-line t))

(use-package company
  :ensure t
  :defer nil
  :delight
  :bind (("M-/" . #'company-complete))
  :init
  (setq company-minimum-prefix-length 1)
  (setq company-tooltip-align-annotations t)
  (setq company-idle-delay 0.2)
  :config
  (global-company-mode 1))

(use-package flycheck
  :ensure t
  :config
  (add-hook 'after-init-hook #'global-flycheck-mode))

(defun js-colorize-compilation-buffer ()
  (ansi-color-apply-on-region compilation-filter-start (point-max)))

(defun js-colorize-buffer (proc &rest args)
  (interactive)
  (with-current-buffer (process-buffer proc)
    (read-only-mode -1)
    (ansi-color-apply-on-region (point-min) (point-max))
    (read-only-mode 1)))

(use-package ansi-color
  :ensure t
  :config
  (add-hook 'compilation-filter-hook #'js-colorize-compilation-buffer)
  ;; https://github.com/magit/magit/issues/1878
  (advice-add 'magit-process-filter :after #'js-colorize-buffer))

(use-package js
  :demand t
  :config
  (setq js-indent-level 2))

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

(use-package web-mode
  :ensure t
  :init
  (setq web-mode-enable-auto-quoting nil)
  (setq web-mode-enable-auto-indentation t)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-attr-indent-offset 2)
  (setq web-mode-attr-value-indent-offset 2)
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-jsx-depth-faces nil)   ; Disable JSX highlighting
  :config
  (add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode)))

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
  :bind
  (("C-M-S-b" . #'dired-sidebar-toggle-sidebar)
   :map dired-sidebar-mode-map
   ([remap keyboard-quit] . #'dired-sidebar-hide-sidebar))
  :config
  (advice-add 'dired-sidebar-find-file :after #'dired-sidebar-hide-sidebar))

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
  :pin melpa
  :config
  (add-to-list 'auto-mode-alist '("\.feature$" . feature-mode)))

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
   ("C-x M-g" . #'magit-dispatch-popup)))

(use-package gitignore-mode
  :ensure t
  :config
  (add-hook #'gitignore-mode-hook #'display-line-numbers-mode))

;; (use-package evil-magit
;;   :ensure t)

;; (use-package evil-snipe
;;   :ensure t
;;   :after evil
;;   :delight (evil-snipe-local)
;;   :init
;;   (setq evil-snipe-scope 'line)
;;   (setq evil-snipe-spillover-scope 'line)
;;   (setq evil-snipe-repeat-scope 'line)
;;   (setq evil-snipe-use-vim-sneak-bindings t)
;;   (setq evil-snipe-override-evil-repeat-keys nil)
;;   :config
;;   (evil-snipe-mode +1)
;;   (evil-snipe-override-mode +1)
;;   (add-hook 'magit-mode-hook #'turn-off-evil-snipe-override-mode)
;;   (general-def 'motion ":" #'evil-snipe-repeat))

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
