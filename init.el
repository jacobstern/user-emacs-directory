(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))

(package-initialize)

;; Bootstrap `use-package' and friends
(unless  (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package)
  )

(eval-when-compile
  (require 'use-package))

(setq x-underline-at-descent-line t)

(setq indent-tabs-mode nil)
(setq inhibit-splash-screen t)
(setq ring-bell-function 'ignore)
(setq enable-recursive-minibuffers t)
(setq column-number-mode t)

(setq confirm-kill-processes nil)
(setq confirm-kill-emacs #'y-or-n-p)

(setq enable-local-variables :safe)

(setq electric-pair-inhibit-predicate #'sj-inhibit-electric-pair-mode)

(set-face-attribute 'default nil :height 130)

(autoload 'zap-up-to-char "misc"
  "Kill up to, but not including ARGth occurrence of CHAR.
  
  \(fn arg char)"
  'interactive)

(add-to-list 'default-frame-alist '(fullscreen . maximized))

(global-auto-revert-mode 1)
(show-paren-mode 1)
(electric-pair-mode 1)
(electric-indent-mode 1)
(delete-selection-mode 1)
(winner-mode 1)
(global-hl-line-mode 1)
(tool-bar-mode -1)
(toggle-scroll-bar -1)
(menu-bar-mode -1)

(set-face-attribute 'default nil :height 130)

(add-hook 'prog-mode-hook #'display-line-numbers-mode)
(add-hook 'text-mode-hook #'display-line-numbers-mode)

(define-key global-map (kbd "C-x ;") #'ignore) ; Useless command and easy to type by accident
(define-key global-map (kbd "M-z") #'zap-up-to-char)
(define-key global-map (kbd "C-x C-b") #'ibuffer)

(define-key global-map (kbd "C-c o") #'mode-line-other-buffer)
(define-key global-map (kbd "C-c u") #'browse-url-at-point)

(define-key global-map (kbd "M-<up>") #'windmove-up)
(define-key global-map (kbd "M-<down>") #'windmove-down)
(define-key global-map (kbd "M-<right>") #'windmove-right)
(define-key global-map (kbd "M-<left>") #'windmove-left)

(define-key global-map (kbd "C-s") #'isearch-forward-regexp)
(define-key global-map (kbd "C-r") #'isearch-backward-regexp)

(defun sj-inhibit-electric-pair-mode (_char)
  (minibufferp))

(setq sj-shell-clear-regex "clear")

(defun sj-shell-clear-next-output (output)
  "Clear the next output from ComInt and remove this hook."
  (remove-hook 'comint-preoutput-filter-functions #'sj-shell-clear-next-output)
  (recenter-top-bottom 0) output)

(defun sj-shell-clear-listener (input)
  (when (string-match-p sj-shell-clear-regex (string-trim input))
    (add-hook 'comint-preoutput-filter-functions #'sj-shell-clear-next-output)))

(defun sj-projectile-grep-ag (arg)
  "Run helm-grep-ag in the project root"
  (interactive "P")
  (helm-grep-ag (projectile-project-root) arg))

(defun sj-colorize-compilation-buffer ()
  (ansi-color-apply-on-region compilation-filter-start (point-max)))

(defun sj-colorize-buffer (proc &rest args)
  (interactive)
  (with-current-buffer (process-buffer proc)
    (read-only-mode -1)
    (ansi-color-apply-on-region (point-min) (point-max))
    (read-only-mode 1)))

(defun sj-fix-underscore-word-syntax ()
  (interactive)
  (modify-syntax-entry ?_ "w")
  )

(defun sj-fix-elisp-word-syntax ()
  (interactive)
  (modify-syntax-entry ?- "w")
  (modify-syntax-entry ?/ "w")
  (modify-syntax-entry ?* "w")
  )

(defun sj-projectile-grep-ag (arg)
  "Run helm-grep-ag in the project root"
  (interactive "P")
  (helm-grep-ag (projectile-project-root) arg))

(defun sj-projectile-grep-ag-file-type ()
  "Run helm-grep-ag and choose a file type"
  (interactive)
  (sj-projectile-grep-ag t))

(defun sj-shackle-split (buffer _alist _plist)
  (get-buffer-window (evil-split-buffer buffer))
  )

(defun sj-shackle-find-or-split (buffer alist plist)
  (or (get-buffer-window buffer)
      (sj-shackle-split buffer alist plist))
  )

(use-package delight
  :ensure t)

(use-package general
  :ensure t
  :config
  (general-create-definer sj-leader-def
    :prefix "SPC"
    :keymaps 'override
    :prefix-command 'sj-leader-prefix
    :states '(normal visual))
  (sj-leader-def "z" #'shell)
  )

(use-package org)

(use-package uniquify
  :init
  (setq uniquify-buffer-name-style 'forward))

(use-package comint
  :init
  (setq comint-prompt-read-only t)
  (setq comint-scroll-show-maximum-output nil))

(use-package shell
  :after comint
  :config
  (add-hook 'shell-mode-hook
            #'(lambda ()
                (add-hook 'comint-input-filter-functions
                          #'sj-shell-clear-listener nil t))))

(use-package whole-line-or-region
  :ensure t
  :after delight
  :delight whole-line-or-region-local-mode
  :config
  (whole-line-or-region-global-mode t))

(use-package doom-themes
  :ensure t
  :init
  (setq doom-themes-enable-italic t)
  (setq doom-themes-enable-bold t)
  :config
  (add-hook 'after-init-hook (apply-partially 'load-theme 'doom-one))
  )

(use-package helm
  :ensure t
  :delight
  :after (general delight)
  :general
  (sj-leader-def "x x" #'helm-M-x)
  (sj-leader-def "x b" #'helm-buffers-list)
  (sj-leader-def "x f" #'helm-find-files)
  (sj-leader-def "x r" #'helm-recentf)
  (sj-leader-def "x i" #'helm-imenu)
  (sj-leader-def "x TAB" #'helm-resume)
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
  (setq helm-grep-ag-command "rg --color=always --smart-case --no-heading --line-number %s %s %s")
  (setq helm-mode-handle-completion-in-region nil)
  (setq helm-display-function #'pop-to-buffer) ; For Shackle compatibility
  )

(use-package helm-config
  :ensure helm
  :config
  (helm-mode 1)
  )

(use-package smex
  :ensure t
  :config
  (smex-initialize))

;; (use-package hydra
;;   :ensure t)

;; (use-package ivy-hydra
;;   :ensure t
;;   :after (ivy hydra))

;; (use-package ivy
;;   :ensure t
;;   :after delight
;;   :delight
;;   :demand t
;;   :after (hydra smex)
;;   :bind (("C-c s" . #'swiper))
;;   :init
;;   (setq ivy-use-virtual-buffers nil)
;;   :config
;;   (ivy-mode 1))

;; (use-package counsel
;;   :ensure t
;;   :after (ivy delight)
;;   :delight
;;   :demand t
;;   :bind (("C-c t" . #'counsel-recentf)
;;          ("C-c i" . #'counsel-imenu))
;;   :config
;;   (counsel-mode 1))

(use-package restart-emacs
  :ensure t
  :commands (restart-emacs))

(use-package exec-path-from-shell
  :ensure t
  :if (memq window-system '(mac ns x))
  :after (nix)
  :config
  (exec-path-from-shell-initialize))

(use-package powerline
  :ensure t
  :init
  (setq powerline-height 24)
  (setq powerline-default-separator 'slant))

(use-package spaceline
  :ensure t
  :after (powerline helm evil)
  :init
  (setq spaceline-highlight-face-func #'spaceline-highlight-face-evil-state)
  :config
  (spaceline-spacemacs-theme)
  (spaceline-helm-mode))

(use-package projectile
  :ensure t
  :init
  (setq projectile-enable-caching t)
  :config
  (projectile-mode 1))

(use-package helm-projectile
  :ensure t
  :pin melpa
  :after (helm projectile general)
  :general
  (sj-leader-def "SPC" #'helm-projectile-find-file)
  (sj-leader-def "p g" #'sj-projectile-grep-ag)
  (sj-leader-def "p G" #'sj-projectile-grep-ag-file-type)
  (sj-leader-def "p p" #'helm-projectile-switch-project)
  (sj-leader-def "p f" #'helm-projectile-find-file)
  (sj-leader-def "p b" #'helm-projectile-switch-to-buffer)
  (sj-leader-def "p d" #'helm-projectile-find-dir)
  (helm-projectile-find-file-map "C-r" #'sj-refresh-projectile-list)
  :init
  (setq projectile-enable-caching t)
  (setq helm-projectile-truncate-lines t)
  (setq projectile-switch-project-action #'projectile-dired)
  )

(use-package aggressive-indent
  :ensure t
  :config
  (add-hook 'emacs-lisp-mode-hook #'aggressive-indent-mode)
  )

(use-package company
  :ensure t
  :after (general)
  :init
  (setq company-selection-wrap-around t)
  (setq company-tooltip-align-annotations t)
  (setq company-dabbrev-downcase nil)
  (setq company-require-match nil)
  :general
  (general-imap "C-SPC" #'company-complete-common)
  (company-active-map "C-c" #'company-abort)
  :config
  (global-company-mode 1)
  )

(use-package undo-tree
  :ensure t
  :after delight
  :delight
  :config
  (global-undo-tree-mode 1))

(use-package flycheck
  :ensure t
  :config
  (add-hook 'after-init-hook #'global-flycheck-mode))

(use-package avy
  :ensure t
  :after general
  :init
  (setq avy-all-windows nil)
  :general
  (sj-leader-def "s" #'avy-goto-char-2)
  (sj-leader-def "j" #'avy-goto-line)
  )

(use-package nix-mode
  :ensure t
  :after (company))

(use-package magit
  :ensure t
  :bind (("C-x g" . #'magit-status)
         ("C-x M-g" . #'magit-dispatch-popup)
         ("C-c M-g" . #'magit-file-popup)
	 ("C-c b" . #'magit-blame)))

(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode)))

(use-package yaml-mode
  :ensure t
  :commands (yaml-mode)
  :mode "\\.ya?ml\\'")

(use-package dhall-mode
  :ensure t
  :mode "\\.dhall\\'"
  :init
  (setq dhall-format-at-save nil)
  (setq dhall-repl-executable "dhall repl")
  (setq dhall-use-header-line nil)
  :config
  (add-hook 'dhall-mode-hook #'(lambda () (setq-local indent-tabs-mode nil))))

(use-package which-key
  :ensure t
  :after delight
  :delight
  :config
  (which-key-mode))

(use-package goto-last-change
  :ensure t
  :bind (("C-c j" . #'goto-last-change)))

(use-package anzu
  :ensure t
  :demand t
  :after delight
  :delight
  :bind (("C-c r" . #'anzu-query-replace-at-cursor)
	 ("M-%" . #'anzu-query-replace-regexp)
	 ("M-s %" . #'anzu-isearch-query-replace-regexp))
  :init
  (setq anzu-cons-mode-line-p nil)
  :config
  (global-anzu-mode))

(use-package ansi-color
  :ensure t
  :config
  (add-hook 'compilation-filter-hook #'sj-colorize-compilation-buffer)
  ;; https://github.com/magit/magit/issues/1878
  (advice-add 'magit-process-filter :after #'sj-colorize-buffer))

(use-package haskell-mode
  :ensure t
  :config
  (add-hook 'haskell-mode-hook #'subword-mode))

(use-package intero
  :ensure t
  :after (general shackle)
  :general
  (sj-leader-def :keymaps 'intero-mode-map "m l" #'intero-repl-load)
  (sj-leader-def :keymaps 'intero-mode-map "m z" #'intero-repl)
  (sj-leader-def :keymaps 'intero-mode-map "m t" #'intero-type-at)
  :config
  (add-hook 'haskell-mode-hook #'intero-mode)
  )

(use-package shackle
  :ensure t
  :pin melpa
  :init
  (setq swiper-helm-display-function #'pop-to-buffer)
  (setq shackle-rules '(("*Help*" :custom sj-shackle-find-or-split)
                        ("\\`\\*helm.*?\\*\\'" :regexp t :align t :size 28)
                        ("*swiper*" :regexp t :align t :size 28)
                        ("*evil-registers*" :regexp t :align t :size 28)
                        ("*shell*" :regexp t :custom sj-shackle-find-or-split)
                        ("*Warnings*" :regexp t :custom sj-shackle-find-or-split)))
  ;; Why doesn't this work in the intero def?
  (add-to-list 'shackle-rules '("\\*intero.*?repl\\*" :regexp t :custom sj-shackle-find-or-split))
  :config
  (shackle-mode 1))

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
  (setq evil-normal-state-tag "NRM")
  (setq evil-insert-state-tag "INS")
  (setq evil-motion-state-tag "MOT")
  (setq evil-operator-state-tag "PND")
  (setq evil-visual-state-tag "VIS")
  (setq evil-emacs-state-tag "EMC")
  (setq evil-replace-state-tag "REP")
  (setq evil-insert-state-modes nil) ; Shell etc.
  :general 
  (minibuffer-local-map "<escape>" #'minibuffer-keyboard-quit)
  :config
  (evil-mode 1)
  (add-hook 'prog-mode-hook #'sj-fix-underscore-word-syntax)
  (add-hook 'emacs-lisp-mode-hook #'sj-fix-elisp-word-syntax)

  (general-evil-setup)
  (general-nmap "C-n" #'ignore)
  (general-nmap "C-p" #'ignore)

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

(defun sj-evil-prefix-translations (_mode mode-keymaps &rest _rest)
  (evil-collection-translate-key 'normal mode-keymaps
    "q" nil
    ))

(use-package evil-collection
  :ensure t
  :after evil
  :init
  (setq evil-collection-term-sync-state-and-mode-p nil)
  (setq evil-collection-company-use-tng nil)
  (add-hook 'evil-collection-setup-hook #'sj-evil-prefix-translations)
  :config
  (evil-collection-init))

(use-package evil-magit
  :ensure t
  :after (magit)
  )

(use-package evil-snipe
  :ensure t
  :demand t
  :after (evil)
  :delight evil-snipe-local-mode
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


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("6b2636879127bf6124ce541b1b2824800afc49c6ccd65439d6eb987dbf200c36" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "fa2b58bb98b62c3b8cf3b6f02f058ef7827a8e497125de0254f56e373abee088" default)))
 '(package-selected-packages
   (quote
    (evil-matchit evil-indent-plus evil-surround evil-commentary evil-snipe evil-magit general evil-collection helm-projectile shackle doom-themes git-gutter intero haskell-mode direnv dhall-mode yaml-mode ivy-hydra hydra smex counsel-projectile counsel ivy anzu goto-last-change which-key markdown-mode exec-path-from-shell magit nix-mode solarized-theme aggressive-indent projectile delight restart-emacs winum avy undo-tree flycheck company spaceline powerline whole-line-or-region use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; Local Variables:
;; eval: (flycheck-mode -1)
;; End:
