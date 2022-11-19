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

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
	      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
     (url-retrieve-synchronously
	"https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
	'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

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
        ;:weight 'light
        :height 120)

(use-package doom-themes
 	:ensure t
 	:config
  	;; Global settings (defaults)
  	(setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
  	      doom-themes-enable-italic t) ; if nil, italics is universally disabled
  	(load-theme 'doom-snazzy t)
  	;; Enable flashing mode-line on errors
  	(doom-themes-visual-bell-config)
  	;; Enable custom neotree theme (all-the-icons must be installed!)
  	(doom-themes-neotree-config)
  	;; or for treemacs users
  	;(setq doom-themes-treemacs-theme "Idea") ; use "doom-colors" for less minimal icon theme
  	(doom-themes-treemacs-config)
  	;; Corrects (and improves) org-mode's native fontification.
  	(doom-themes-org-config))


(use-package badwolf-theme
  :ensure t)

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

(use-package evil
  :ensure t
  :config
  (evil-mode)
  (evil-set-undo-system 'undo-redo)
  (define-key evil-normal-state-map (kbd "j") 'evil-next-visual-line)
  (define-key evil-normal-state-map (kbd "k") 'evil-previous-visual-line))
    

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
                                         evil-local-mode -1)))
          					  
  (add-hook 'vterm-mode-hook (lambda () (
                                         setq cursor-type 'bar))))
          					  
    

(use-package which-key
 :ensure t
 :config
 (which-key-mode))

(require 'org-tempo)

(use-package magit :ensure t)

(use-package phi-autopair
 :ensure t
 :config
 (phi-autopair-global-mode))
  

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
 (winum-mode))

(use-package projectile
  	:ensure t
  	:bind-keymap
  	("C-c p" . projectile-command-map)
  	:config
  	(setq projectile-project-search-path '(("~/programming/" . 2) ("~/gitpacks" . 1) ("~/design_patterns_rust/" . 2)))
  	;(define-key projectile-mode-map (kbd "SPC p") 'projectile-command-map)
  	(projectile-mode))
    

      ;(define-key evil-normal-state-map " " nil)
(define-key evil-motion-state-map " " nil)

	(use-package treemacs-projectile
  	:ensure t)



(use-package treemacs
  :ensure t)
(use-package lsp-treemacs
  :ensure t)

(font-lock-add-keywords 'rustic-mode
       '(("\\<\\([a-zA-Z_]*\\) *("  1 font-lock-function-name-face)))

(use-package tree-sitter
  :ensure t
  :hook
  (
   (rustic-mode . tree-sitter-mode)
   (rustic-mode . tree-sitter-hl-mode)))
    

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
      :background nil))

(use-package yasnippet
  :ensure t
  :config
  (yas-global-mode))
(use-package yasnippet-snippets
  :ensure t)
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

(setq dap-cpptools-extension-version "1.12.1")
(use-package dap-mode
  :ensure t)

(require 'dap-gdb-lldb)
(require 'dap-lldb)
(require 'dap-cpptools)

(dap-register-debug-template "Rust::GDB Run Configuration"
           (list :type "gdb"
                 :request "launch"
                 :name "GDB::Run"
                 :gdbpath "rust-gdb"
                 :target nil
                 :cwd nil))
           ;;:target "${workspaceFolder}/target/debug/examples/logger"
           ;;:cwd "${workspaceFolder}"))
(with-eval-after-load 'dap-cpptools
  ;; Add a template specific for debugging Rust programs.
  ;; It is used for new projects, where I can M-x dap-edit-debug-template
  (dap-register-debug-template "Rust::CppTools Run Configuration"
        				  (list :type "cppdbg"
        				      :request "launch"
        				      :name "Rust::Run"
        				      :MIMode "gdb"
        				      :miDebuggerPath "rust-gdb"
        				      :environment []
        				      :program "${workspaceFolder}/target/debug/hello / replace with binary"
        				      :cwd "${workspaceFolder}"
        				      :console "external"
        				      :dap-compilation "cargo build"
        				      :dap-compilation-dir "${workspaceFolder}")))

(use-package rustic
 :ensure t
 :bind(
       (:map rust-mode-map
	 ("<f6>" . rustic-format-buffer)
	 ("<f5>" . my-cargo-run)))


 :config
 (require 'lsp-rust)
 (setq lsp-rust-analyzer-completion-add-call-parenthesis nil))

(defun my-cargo-run ()
 "Build and run Rust code."
 	(interactive)
 	(no-confirm #'rustic-cargo-run-rerun)
 	(let (
       (orig-win (selected-window))
       (run-win (display-buffer (get-buffer "*cargo-run*") nil 'visible)))
  	    
  	    (select-window run-win)
  	    (comint-mode)
  	    (read-only-mode 0)
  	    (select-window orig-win)))

(use-package aggressive-indent
  :ensure t
  :hook prog-mode)

(use-package parinfer-rust-mode
  :ensure t
  :hook emacs-lisp-mode)

(use-package request
  :ensure t)

(defun no-confirm (fun &rest args)
    "Apply FUN to ARGS, skipping user confirmations."
    (cl-flet ((always-yes (&rest _) t))
     (cl-letf (((symbol-function 'y-or-n-p) #'always-yes)
               ((symbol-function 'yes-or-no-p) #'always-yes))
      (apply fun args))))

(defun my/rust-playground ()
  (interactive)
  (let ((path (concat "/tmp/rust-playground-" (format-time-string "%Y%m_%H-%M-%S-%N/"))))
    (shell-command (concat "cargo new " path))
    (find-file (concat path "src/main.rs"))))

(load "scrot")

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

(use-package org-modern
  :ensure t
  :config
  (global-org-modern-mode))

(use-package visual-fill-column
 	:ensure t
 	:hook
 	(org-mode . visual-line-mode)
 	(visual-line-mode . visual-fill-column-mode)
 	(dired-mode . visual-fill-column-mode)
 	:config
 	(setq-default visual-fill-column-center-text t)
 	(setq-default fill-column 120))

(use-package org-roam
  :ensure t
  :custom
  (org-roam-directory "~/RoamNotes")
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n i" . org-roam-node-insert))
  :config
  (org-roam-setup))

(use-package org-roam-ui
  :ensure t
  :after org-roam
  :config
  (setq org-roam-ui-sync-theme t
	org-roam-ui-follow t
	org-roam-ui-update-on-save t
	org-roam-ui-open-on-start t))

(add-hook 'projectile-after-switch-project-hook 'treemacs-add-and-display-current-project-exclusively)

(use-package dired
  :ensure nil
  :commands (dired dired-jump)
  :bind (("C-x C-j" . dired-jump))
  :custom ((dired-listing-switches "-aghov --group-directories-first"))
  :config
  (evil-collection-define-key 'normal 'dired-mode-map
    "h" 'dired-single-up-directory
    "l" 'dired-single-buffer)
  (setq dired-dwim-target t))
    

(with-eval-after-load 'dired
  (require 'dired-x))
  ;; Set dired-x global variables here.  For example:
  ;; (setq dired-guess-shell-gnutar "gtar")
  ;; (setq dired-x-hands-off-my-keys nil)
  



(use-package dired-single
  :ensure t)


(use-package all-the-icons-dired
  :ensure t
  :hook (dired-mode . all-the-icons-dired-mode))

(use-package dired-open
  :ensure t
  :commands (dired dired-jump)
  :config
  ;; Doesn't work as expected!
  ;;(add-to-list 'dired-open-functions #'dired-open-xdg t)
  (setq dired-open-extensions '(("png" . "feh")
                                ("mkv" . "mpv")
                                ("mp4" . "mpv")
                                ("pdf" . "chr")
                                ("docx" . "libreoffice")
                                ("html" . "chr"))))
        				  

(use-package dired-hide-dotfiles
  :ensure t
  :hook (dired-mode . dired-hide-dotfiles-mode)
  :config
  (evil-collection-define-key 'normal 'dired-mode-map
    "H" 'dired-hide-dotfiles-mode))

(use-package dired-narrow
  :ensure t)

(use-package dired-du
  :ensure t
  ;:hook
  ;(dired-mode . dired-du-mode)
  :config
  (setq dired-du-size-format t))

(recentf-mode 1)
(setq recentf-max-menu-items 25)
(setq recentf-max-saved-items 25)
(run-at-time nil (* 5 60) 'recentf-save-list)

(use-package auctex
  :ensure t)
