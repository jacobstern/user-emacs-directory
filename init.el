(require 'package)

(package-initialize)

(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
        ("melpa-stable" . "https://stable.melpa.org/packages/")
        ("melpa" . "http://melpa.org/packages/")))

(setq x-underline-at-descent-line t)

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

(let ((default-directory "~/.emacs.d/elisp/"))
  (normal-top-level-add-subdirs-to-load-path))

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

(defun sj-projectile-grep-ag (arg)
  "Run helm-grep-ag in the project root"
  (interactive "P")
  (helm-grep-ag (projectile-project-root) arg))

(defun sj-projectile-grep-ag-file-type ()
  "Run helm-grep-ag and choose a file type"
  (interactive)
  (sj-projectile-grep-ag t))

(defun sj-dired-default-directory ()
  "Open dired, bypassing choice of directory"
  (interactive)
  (dired default-directory))

(eval-when-compile
  (require 'use-package))

(require 'delight)

(require 'general)

(general-unbind :states 'motion ",")

(general-create-definer sj-leader-def
  :prefix ","
  :prefix-command 'sj-leader-prefix
  :non-normal-prefix "C-,"
  :states '(normal insert)
  )

(sj-leader-def "e e" #'eval-expression)
(sj-leader-def "e d" #'eval-defun)
(sj-leader-def "e b" #'eval-buffer)
(sj-leader-def "x s" #'save-buffer)
(sj-leader-def "x S" #'save-some-buffers)
(sj-leader-def "x d" #'sj-dired-default-directory)

(use-package restart-emacs
  :ensure t
  )

(use-package comint
  :init
  (setq comint-prompt-read-only t)
  (setq comint-scroll-show-maximum-output nil)
  )

(defun sj-helm-display (buffer &optional _resume)
  (interactive)
  (display-buffer-in-side-window (get-buffer buffer)
                                 '((window-height . 24) (side . bottom) (slot . 0))))

(use-package helm
  :ensure t
  :delight
  :general
  (sj-leader-def "x x" #'helm-M-x)
  (sj-leader-def "x b" #'helm-buffers-list)
  (sj-leader-def "x f" #'helm-find-files)
  (sj-leader-def "x r" #'helm-recentf)
  (sj-leader-def "x i" #'helm-imenu)
  (sj-leader-def "x z" #'helm-resume)
  (sj-leader-def "x o" #'helm-occur)
  (helm-map "TAB" #'helm-execute-persistent-action)
  (helm-map "C-/" #'helm-select-action)
  (helm-map "<escape>" #'helm-keyboard-quit)
  (helm-map "C-r" #'helm-refresh)
  :init
  (setq helm-M-x-fuzzy-match t)
  (setq helm-buffers-fuzzy-matching t)
  (setq helm-recentf-fuzzy-match t)
  (setq helm-completion-in-region-fuzzy-match t)
  (setq helm-buffer-max-length 50)
  (setq helm-ff-file-name-history-use-recentf t)
  (setq helm-display-function #'sj-helm-display)
  (setq helm-grep-ag-command "rg --color=always --no-heading --line-number %s %s %s")
  :custom
  (helm-mode-handle-completion-in-region nil)
  :config
  (require 'helm-config)
  (helm-mode 1)
  (add-to-list 'helm-boring-buffer-regexp-list "^:") ; Hide dired-sidebar buffer
  )

(use-package projectile
  :ensure t
  :delight '(:eval (concat "Proj[" (projectile-project-name) "]"))
  (setq projectile-enable-caching t)
  :general
  (sj-leader-def "j d" #'projectile-dired)
  :config
  (projectile-mode 1)
  (setq projectile-switch-project-action #'projectile-vc)
  )

(defun sj-refresh-projectile-list ()
  (interactive)
  (projectile-invalidate-cache nil)
  (helm-refresh))

(use-package helm-projectile
  :ensure t
  :general
  (sj-leader-def "j g" #'sj-projectile-grep-ag)
  (sj-leader-def "j G" #'sj-projectile-grep-ag-file-type)
  (sj-leader-def "j j" #'helm-projectile)
  (sj-leader-def "j f" #'helm-projectile-find-file)
  (sj-leader-def "j b" #'helm-projectile-switch-to-buffer)
  (sj-leader-def "j s" #'helm-projectile-switch-project)
  (helm-projectile-find-file-map "C-r" #'sj-refresh-projectile-list)
  :custom
  (projectile-enable-caching t)
  (helm-projectile-truncate-lines t)
  (helm-projectile-sources-list
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

(use-package powerline
  :ensure t
  :init
  (setq powerline-image-apple-rgb t))

(use-package spaceline
  :ensure t
  :after (helm evil)
  :init
  (setq spaceline-highlight-face-func #'spaceline-highlight-face-evil-state)
  :config
  (spaceline-spacemacs-theme)
  (spaceline-helm-mode)
  (spaceline-toggle-workspace-number-off))

(use-package avy
  :ensure t
  :general
  (sj-leader-def "s" #'avy-goto-char-2 :keymaps 'override))

(use-package helm-swoop
  :ensure t
  :bind
  ("s-o" . #'helm-swoop-without-pre-input)
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
  (general-imap "C-/" #'company-complete-common :keymaps company-mode-map)
  (company-active-map "C-c" #'company-abort)
  :init
  (setq company-tooltip-align-annotations t)
  :config
  (global-company-mode 1))

(use-package flycheck
  :ensure t
  :general
  (sj-leader-def "!" flycheck-command-map)
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
  :config
  (setq js-indent-level 2))

(use-package css-mode
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
  :config
  (advice-add #'dired-sidebar-find-file :after #'dired-sidebar-hide-sidebar)
  :general
  (general-nmap :keymaps 'dired-sidebar-mode-map "<escape>" #'dired-sidebar-toggle-sidebar)
  (general-nmap "s-b" #'dired-sidebar-toggle-sidebar)
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

(defun sj-fix-underscore-word-syntax ()
  (interactive)
  (modify-syntax-entry ?_ "w"))

(defun sj-fix-elisp-word-syntax ()
  (interactive)
  (modify-syntax-entry ?- "w")
  (modify-syntax-entry ?/ "w")
  (modify-syntax-entry ?* "w")
  )

(defun js-insert-tab ()
  "Inserts a number of spaces equal to evil-shift-width."
  (interactive)
  (insert-char 32 evil-shift-width))

(use-package evil
  :ensure t
  :demand t
  :init
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-cross-lines nil)
  (setq evil-search-module 'evil-search)
  (setq evil-ex-search-case 'sensitive)
  (setq evil-ex-search-vim-style-regexp t)
  (setq evil-echo-state nil)
  (setq evil-normal-state-tag "Nrm")
  (setq evil-insert-state-tag "Ins")
  (setq evil-motion-state-tag "Mot")
  (setq evil-operator-state-tag "Pnd")
  (setq evil-visual-state-tag "Vis")
  (setq evil-emacs-state-tag "Emc")
  (setq evil-replace-state-tag "Rep")
  (setq evil-insert-state-cursor 'hbar)
  :general 
  (general-nmap "C-;" #'evil-paste-pop)
  (general-nmap "C-n" #'ignore)
  (general-nmap "C-p" #'ignore)
  (general-imap :keymaps 'override "TAB" #'js-insert-tab)
  (sj-leader-def "w" 'evil-window-map)
  ('minibuffer-local-map "<escape>" 'minibuffer-keyboard-quit)
  :config
  (evil-mode 1)
  (add-hook 'prog-mode-hook #'sj-fix-underscore-word-syntax)
  (add-hook 'emacs-lisp-mode-hook #'sj-fix-elisp-word-syntax)

  (general-evil-setup)

  (evil-define-text-object evil-defun (count &optional beg end type)
    "Select around defun."
    (save-excursion
      (mark-defun)
      (evil-range (region-beginning) (region-end) type :expanded t)
      ))
  (define-key evil-outer-text-objects-map "d" #'evil-defun)

  (evil-define-text-object evil-inner-defun (count &optional beg end type)
    "Select inside defun."
    (save-excursion
      (beginning-of-defun)
      (set-mark (point))
      (end-of-defun)
      (evil-range (region-beginning) (region-end) type :expanded t)
      ))
  (define-key evil-inner-text-objects-map "d" #'evil-inner-defun)
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
  :ensure t
  )

(use-package evil-snipe
  :ensure t
  :demand t
  :after (evil)
  :delight evil-snipe-local-mode
  :general
  (evil-snipe-parent-transient-map "," #'sj-leader-prefix)
  :config
  (evil-snipe-mode +1)
  (evil-snipe-override-mode +1)
  (add-hook 'magit-mode-hook #'turn-off-evil-snipe-override-mode)
  )

(use-package evil-commentary
  :ensure t
  :delight
  :config
  (evil-commentary-mode +1)
  )

(use-package evil-surround
  :ensure t
  :config
  (global-evil-surround-mode 1)
  )

(use-package org
  :general
  (sj-leader-def "l" #'org-store-link)
  )

(use-package evil-org
  :ensure t
  :after org
  :config
  (add-hook 'org-mode-hook #'evil-org-mode)
  (evil-org-set-key-theme '(return))
  :general
  (general-nmap :keymaps 'evil-org-mode-map "gx" #'org-open-at-point)
  )

(use-package diff-hl
  :ensure t
  :config
  (global-diff-hl-mode)
  (add-hook 'magit-post-refresh-hook #'diff-hl-magit-post-refresh)
  (add-hook 'dired-mode-hook #'diff-hl-dired-mode-unless-remote)
  :general
  (sj-leader-def "v" diff-hl-command-map)
  )

(use-package json-mode
  :ensure t
  )

(use-package evil-indent-plus
  :ensure t
  :config
  (evil-indent-plus-default-bindings)
  )

(use-package evil-matchit
  :ensure t
  :config
  (global-evil-matchit-mode +1)
  )

(use-package smerge-mode
  :general
  (sj-leader-def "^ RET" #'smerge-keep-current :keymaps 'smerge-mode-map)
  (sj-leader-def "^ C" #'smerge-combine-with-next :keymaps 'smerge-mode-map)
  (sj-leader-def "^ E" #'smerge-ediff :keymaps 'smerge-mode-map)
  (sj-leader-def "^ R" #'smerge-refine :keymaps 'smerge-mode-map)
  (sj-leader-def "^ a" #'smerge-keep-all :keymaps 'smerge-mode-map)
  (sj-leader-def "^ b" #'smerge-keep-base :keymaps 'smerge-mode-map)
  (sj-leader-def "^ l" #'smerge-keep-lower :keymaps 'smerge-mode-map)
  (sj-leader-def "^ c" #'smerge-keep-upper :keymaps 'smerge-mode-map)
  (sj-leader-def "^ n" #'smerge-next :keymaps 'smerge-mode-map)
  (sj-leader-def "^ p" #'smerge-prev :keymaps 'smerge-mode-map)
  (sj-leader-def "^ f" #'smerge-resolve :keymaps 'smerge-mode-map))

(use-package dockerfile-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode)))

(use-package conf-mode
  :config
  ; For .circleci/config.yml
  (add-hook 'conf-colon-mode-hook #'display-line-numbers-mode)
  (add-hook 'conf-colon-mode-hook #'sj-fix-underscore-word-syntax))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(compilation-message-face (quote default))
 '(cua-global-mark-cursor-color "#2aa198")
 '(cua-normal-cursor-color "#657b83")
 '(cua-overwrite-cursor-color "#b58900")
 '(cua-read-only-cursor-color "#859900")
 '(custom-enabled-themes (quote (spacemacs-light)))
 '(custom-safe-themes
   (quote
    ("fa2b58bb98b62c3b8cf3b6f02f058ef7827a8e497125de0254f56e373abee088" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" default)))
 '(eyebrowse-new-workspace t)
 '(fci-rule-color "#eee8d5")
 '(helm-mode-handle-completion-in-region nil)
 '(helm-projectile-sources-list
   (quote
    (helm-source-projectile-recentf-list helm-source-projectile-files-list)) t)
 '(helm-projectile-truncate-lines t t)
 '(highlight-changes-colors (quote ("#d33682" "#6c71c4")))
 '(highlight-symbol-colors
   (--map
    (solarized-color-blend it "#fdf6e3" 0.25)
    (quote
     ("#b58900" "#2aa198" "#dc322f" "#6c71c4" "#859900" "#cb4b16" "#268bd2"))))
 '(highlight-symbol-foreground-color "#586e75")
 '(highlight-tail-colors
   (quote
    (("#eee8d5" . 0)
     ("#B4C342" . 20)
     ("#69CABF" . 30)
     ("#69B7F0" . 50)
     ("#DEB542" . 60)
     ("#F2804F" . 70)
     ("#F771AC" . 85)
     ("#eee8d5" . 100))))
 '(hl-bg-colors
   (quote
    ("#DEB542" "#F2804F" "#FF6E64" "#F771AC" "#9EA0E5" "#69B7F0" "#69CABF" "#B4C342")))
 '(hl-fg-colors
   (quote
    ("#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3")))
 '(hl-paren-colors (quote ("#2aa198" "#b58900" "#268bd2" "#6c71c4" "#859900")))
 '(magit-diff-use-overlays nil)
 '(nrepl-message-colors
   (quote
    ("#dc322f" "#cb4b16" "#b58900" "#546E00" "#B4C342" "#00629D" "#2aa198" "#d33682" "#6c71c4")))
 '(package-selected-packages
   (quote
    (dockerfile-mode helm-ag rainbow-delimiters spacemacs-theme helpful evil-matchit evil-indent-plus json-mode diff-hl evil-org helm-swoop lsp-javascript-typescript company-lsp lsp-mode evil-surround smartparens evil-commentary wgrep-helm winum whole-line-or-region web-mode vscode-icon use-package tide spaceline solarized-theme restart-emacs purescript-mode psc-ide prettier-js powershell multiple-cursors helm-projectile gitignore-mode general feature-mode exec-path-from-shell evil-snipe evil-magit evil-collection dired-sidebar delight avy aggressive-indent add-node-modules-path)))
 '(pos-tip-background-color "#eee8d5")
 '(pos-tip-foreground-color "#586e75")
 '(powerline-image-apple-rgb t)
 '(projectile-enable-caching t t)
 '(selected-packages
   (quote
    (general evil-snipe evil-magit evil-collection evil web-mode winum whole-line-or-region vscode-icon use-package undo-tree tide spaceline solarized-theme restart-emacs purescript-mode psc-ide prettier-js powershell multiple-cursors magit helm-swoop helm-projectile feature-mode exec-path-from-shell dired-sidebar delight avy aggressive-indent add-node-modules-path)))
 '(smartrep-mode-line-active-bg (solarized-color-blend "#859900" "#657b83" 0.2))
 '(term-default-bg-color "#fdf6e3")
 '(term-default-fg-color "#657b83")
 '(vc-annotate-background nil)
 '(vc-annotate-background-mode nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#dc322f")
     (40 . "#c9485ddd1797")
     (60 . "#bf7e73b30bcb")
     (80 . "#b58900")
     (100 . "#a5a58ee30000")
     (120 . "#9d9d91910000")
     (140 . "#9595943e0000")
     (160 . "#8d8d96eb0000")
     (180 . "#859900")
     (200 . "#67119c4632dd")
     (220 . "#57d79d9d4c4c")
     (240 . "#489d9ef365ba")
     (260 . "#3963a04a7f29")
     (280 . "#2aa198")
     (300 . "#288e98cbafe2")
     (320 . "#27c19460bb87")
     (340 . "#26f38ff5c72c")
     (360 . "#268bd2"))))
 '(vc-annotate-very-old-color nil)
 '(weechat-color-list
   (quote
    (unspecified "#fdf6e3" "#eee8d5" "#990A1B" "#dc322f" "#546E00" "#859900" "#7B6000" "#b58900" "#00629D" "#268bd2" "#93115C" "#d33682" "#00736F" "#2aa198" "#657b83" "#839496")))
 '(xterm-color-names
   ["#eee8d5" "#dc322f" "#859900" "#b58900" "#268bd2" "#d33682" "#2aa198" "#073642"])
 '(xterm-color-names-bright
   ["#fdf6e3" "#cb4b16" "#93a1a1" "#839496" "#657b83" "#6c71c4" "#586e75" "#002b36"]))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(powerline-reset)

;; Local Variables:
;; eval: (flycheck-mode -1)
;; End:
