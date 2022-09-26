(require 'package)
  (package-initialize)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(unless (package-installed-p 'use-package)
  (package-install 'use-package))
  (require 'use-package)

  (setq custom-file "~/.my_programming_emacs/custom.el")
  (load custom-file t)

  (setq auto-save-file-name-transforms
      '(("." "~/.my_programming_emacs/" t))
      backup-directory-alist
      '(("." . "~/.my_programming_emacs/backups")))

;; scroll one line at a time (less "jumpy" than defaults)
(pixel-scroll-precision-mode)
(setq pixel-scroll-precision-large-scroll-height 40.0)
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq scroll-step           1
	scroll-conservatively 10000)

(set-face-attribute 'region nil :background "deep sky blue")
(set-face-attribute 'default nil :height 120)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)

(use-package evil :ensure t :config (evil-mode)
  (evil-set-undo-system 'undo-redo))

  (use-package evil-org
      :ensure t
:hook org-mode)

   (require
	'evil-magit)

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

(use-package projectile
:ensure t)

(use-package treemacs-projectile
:ensure t)

(use-package treemacs
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

(use-package lsp-mode
:ensure t
:bind (:map lsp-mode-map
	    ("C-c d" . lsp-describe-thing-at-point)
	    ("C-c a" . lsp-execute-code-action))
:bind-keymap ("C-c l" . lsp-command-map)
:config
(lsp-enable-which-key-integration t))

(use-package company
:ensure t
:hook ((emacs-lisp-mode . (lambda ()
			    (setq-local company-backends '(company-elisp))))
	(emacs-lisp-mode . company-mode))
:bind(
	(:map company-active-map ("<tab>" . company-complete-selection))
	("C-j" . company-select-next-or-abort)
	("C-k" . company-select-previous-or-abort)

	)
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
	("<f6>" . rustic-format-buffer)
	("<f5>" . my-cargo-run)
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
		      (expand-file-name "~/.my_programming_emacs/config.org"))
    ;; Dynamic scoping to the rescue
    (let ((org-confirm-babel-evaluate nil))
      (org-babel-tangle))))

(add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'org-babel-tangle-config)))

(setq org-confirm-babel-evaluate nil)
(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (python . t)))

(add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
(add-to-list 'org-structure-template-alist '("py" . "src python"))
(add-to-list 'org-structure-template-alist '("sh" . "src sh"))
(add-to-list 'org-structure-template-alist '("rs" . "src rust"))
