(require 'package)
  (package-initialize)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(unless (package-installed-p 'use-package)
  (package-install 'use-package))
  (require 'use-package)

  (setq custom-file (concat user-emacs-directory "custom.el"))
  (load custom-file t)

  (setq auto-save-file-name-transforms
      `(("." ,user-emacs-directory t))
      backup-directory-alist
      `(("." . ,(concat user-emacs-directory "backups"))))

(defun efs/display-startup-time ()
  (message
   "Emacs loaded in %s with %d garbage collections."
   (format
    "%.2f seconds"
    (float-time
     (time-subtract after-init-time before-init-time)))
   gcs-done))

(add-hook 'emacs-startup-hook #'efs/display-startup-time)

(set-face-attribute 'default nil
		    :font "Jetbrains Mono"
		    :weight 'light
		    :height 120)

(use-package doom-themes
  :ensure t
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
	doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-horizon t)
  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Enable custom neotree theme (all-the-icons must be installed!)
  (doom-themes-neotree-config)
  ;; or for treemacs users
  (setq doom-themes-treemacs-theme "doom-colors") ; use "doom-colors" for less minimal icon theme
  (doom-themes-treemacs-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))


(defun disable-all-themes ()
  "disable all active themes."
  (dolist (i custom-enabled-themes)
    (disable-theme i)))

(defadvice load-theme (before disable-themes-first activate)
  (disable-all-themes))

;; scroll one line at a time (less "jumpy" than defaults)

(use-package gcmh
  :ensure t
  :demand
  :hook
  (focus-out-hook . gcmh-idle-garbage-collect)

  :custom
  (gcmh-idle-delay 10)
  (gcmh-high-cons-threshold 104857600)

  :config
  (gcmh-mode +1))
(pixel-scroll-precision-mode)

;(setq pixel-scroll-precision-interpolate-mice 0)
;(setq pixel-scroll-precision-interpolation-factor 1.7)
;(setq pixel-scroll-precision-initial-velocity-factor 0)
;(setq pixel-scroll-precision-momentum-min-velocity 0)

;(setq pixel-scroll-precision-large-scroll-height 40.0)
;(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq scroll-step           1
	scroll-conservatively 10000)

(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)
(global-display-line-numbers-mode)
(setq display-line-numbers-type 'relative)

(use-package all-the-icons
  :ensure t
  :if (display-graphic-p))

;;(use-package centaur-tabs
;;  :ensure t
;;  :demand
;;  :config
;;  (centaur-tabs-mode t)
;;  :bind
;;  ("C-<prior>" . centaur-tabs-backward)
;;  ("C-<next>" . centaur-tabs-forward))

(use-package doom-modeline
  :ensure t
  :init
  (doom-modeline-mode 1)
  :config
  (set-face-attribute 'tab-bar-tab nil
		    :inherit 'doom-modeline-panel
		    :foreground nil
		    :background nil))

(setq evil-want-keybinding nil)
  (use-package evil-collection
    :ensure t)

  (evil-collection-init)

    (use-package evil :ensure t :config (evil-mode)
    (evil-set-undo-system 'undo-redo))

    (use-package evil-org
	:ensure t
  :hook org-mode)


    (use-package treemacs-evil :ensure t)

(with-eval-after-load 'evil
(with-eval-after-load 'company
    (define-key evil-insert-state-map (kbd "C-j") nil)
    (define-key evil-insert-state-map (kbd "C-k") nil)
    (evil-define-key nil company-active-map (kbd "C-j") #'company-select-next)
    (evil-define-key nil company-active-map (kbd "C-k") #'company-select-previous)))

(with-eval-after-load 'evil
(with-eval-after-load 'vertico
    (define-key evil-insert-state-map (kbd "C-j") nil)
    (define-key evil-insert-state-map (kbd "C-k") nil)
    (evil-define-key nil vertico-map (kbd "C-j") #'vertico-next)
    (evil-define-key nil vertico-map (kbd "C-k") #'vertico-previous)))

(use-package vterm
  :ensure t
  :config
  (add-hook 'vterm-mode-hook (lambda () (
					message "Hallo ?"
					)))
  (add-hook 'vterm-mode-hook (lambda () (
					evil-local-mode -1
					)))
  (add-hook 'vterm-mode-hook (lambda () (
					setq cursor-type 'bar
					)))
  )

(use-package which-key
:ensure t
:config
(which-key-mode))

(require 'org-tempo)

(use-package magit :ensure t)

(use-package phi-autopair
:ensure t
:config
(phi-autopair-global-mode)
)

(use-package rainbow-mode
:ensure t)

;;; Vertico
(use-package vertico
:ensure t
:config
(vertico-mode))

(use-package orderless
:ensure t
:config
(setq completion-styles '(orderless)))

(use-package marginalia
:ensure t
:config
(marginalia-mode))
(use-package consult
:ensure t)

(use-package perspective
:ensure t)


(use-package winum
:ensure t
:config
(setq winum-keymap
	(let ((map (make-sparse-keymap)))
	(global-set-key (kbd "M-0") 'treemacs-select-window)
	(global-set-key (kbd "M-1") 'winum-select-window-1)
	(global-set-key (kbd "M-2") 'winum-select-window-2)
	(global-set-key (kbd "M-3") 'winum-select-window-3)
	(global-set-key (kbd "M-4") 'winum-select-window-4)
	(global-set-key (kbd "M-5") 'winum-select-window-5)
	(global-set-key (kbd "M-6") 'winum-select-window-6)
	(global-set-key (kbd "M-7") 'winum-select-window-7)
	(global-set-key (kbd "M-8") 'winum-select-window-8)
	map))
(winum-mode)
)

(use-package projectile
    :ensure t
    :bind-keymap
    ("C-c p" . projectile-command-map)
    :config
    (setq projectile-project-search-path '(("~/programming/" . 2) ("~/gitpacks" . 1) ("~/design_patterns_rust/" . 2)))
    ;(define-key projectile-mode-map (kbd "SPC p") 'projectile-command-map)
    (projectile-mode)
)

;(define-key evil-normal-state-map " " nil)
(define-key evil-motion-state-map " " nil)

    (use-package treemacs-projectile
    :ensure t)

    (use-package treemacs
    :ensure t)

(font-lock-add-keywords 'rustic-mode
		   '(("\\<\\([a-zA-Z_]*\\) *("  1 font-lock-function-name-face)))

(use-package tree-sitter
  :ensure t)

(use-package tree-sitter-langs
  :ensure t
  :config
    (set-face-attribute 'tree-sitter-hl-face:function.call nil
			:inherit 'font-lock-function-name-face
			:foreground nil
			:background nil)
    (set-face-attribute 'tree-sitter-hl-face:property nil
			:inherit nil
			:foreground nil
			:background nil)
  )

(use-package lsp-mode
:ensure t
:bind (:map lsp-mode-map
	    ("C-c d" . lsp-describe-thing-at-point)
	    ("C-c a" . lsp-execute-code-action))
:bind-keymap ("C-c l" . lsp-command-map)
:config
(lsp-enable-which-key-integration t))

(use-package lsp-ui
     :ensure t)

(use-package company
:ensure t
:hook ((emacs-lisp-mode . (lambda ()
			    (setq-local company-backends '(company-elisp))))
	(emacs-lisp-mode . company-mode))
:bind(
	(:map company-active-map ("<tab>" . company-complete-selection)
	("C-j" . company-select-next-or-abort)
	("C-k" . company-select-previous-or-abort)))
:config
;(company-keymap--unbind-quick-access company-active-map)
;(company-tng-configure-default)
(setq company-idle-delay 0.1
	company-minimum-prefix-length 1))


(use-package flycheck
:ensure t)

(use-package rustic
:ensure t
:bind(
(:map rust-mode-map
	("<f6>" . rustic-format-buffer)
	("<f5>" . my-cargo-run)
)
	)
:config
(require 'lsp-rust)
(setq lsp-rust-analyzer-completion-add-call-parenthesis nil)
)

(defun my-cargo-run ()
"Build and run Rust code."
    (interactive)
    (no-confirm #'rustic-cargo-run-rerun)
    (let (
	(orig-win (selected-window))
	(run-win (display-buffer (get-buffer "*cargo-run*") nil 'visible))
	)
	(select-window run-win)
	(comint-mode)
	(read-only-mode 0)
	(select-window orig-win)
    )
)

(defun no-confirm (fun &rest args)
    "Apply FUN to ARGS, skipping user confirmations."
    (cl-flet ((always-yes (&rest _) t))
    (cl-letf (((symbol-function 'y-or-n-p) #'always-yes)
	    ((symbol-function 'yes-or-no-p) #'always-yes))
	(apply fun args))))

(defun org-babel-tangle-config()
  (when (string-equal (buffer-file-name)
		      (expand-file-name(concat user-emacs-directory "config.org")))
    ;; Dynamic scoping to the rescue
    (let ((org-confirm-babel-evaluate nil))
      (org-babel-tangle))))

(add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'org-babel-tangle-config)))

(setq org-confirm-babel-evaluate nil)
(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (python . t)))

(define-key org-mode-map (kbd "C-c C-i") 'org-edit-src-code)

(add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
(add-to-list 'org-structure-template-alist '("py" . "src python"))
(add-to-list 'org-structure-template-alist '("sh" . "src sh"))
(add-to-list 'org-structure-template-alist '("rs" . "src rust"))

(add-hook 'projectile-after-switch-project-hook 'treemacs-add-and-display-current-project-exclusively)
