; -*- coding: utf-8; mode: lisp -*-

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(inhibit-startup-screen t)
 '(tool-bar-mode nil)
 '(menu-bar-mode nil))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 )

;; stack trace on errors
(setq debug-on-error t)

;; never use tabs!
(setq-default indent-tabs-mode nil)

;; but show me if tabs or whitespace exists
(show-ws-toggle-show-trailing-whitespace)
(show-ws-toggle-show-tabs)

;; show line and column numbers
(setq line-number-mode t)
(setq column-number-mode t)

;; start server after initialization
(add-hook 'after-init-hook 'server-start)

;; package management
(require 'cl-lib)
(require 'package)
(package-initialize)

(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/"))

(defvar dev-packages
  '(company
    anaconda-mode
    company-anaconda
    flycheck
    flx
    flx-ido
    ido
    jedi
    jedi-core
    json-mode
    php-mode
    projectile
    python)
  "A list of packages to ensure are installed at launch.")

(defun dev-packages-installed-p ()
  (cl-loop for p in dev-packages
        if (not (package-installed-p p)) return nil
        finally (return t)))

(unless (dev-packages-installed-p)
  ;; check for new packages (package versions)
  (message "%s" "Emacs is now refreshing its package database...")
  (package-refresh-contents)
  (message "%s" " done.")
  ;; install the missing packages
  (dolist (p dev-packages)
    (when (not (package-installed-p p))
      (package-install p))))

(provide 'dev-packages)

;; theme
(load-theme 'tango-dark t)

;; Set the font we should use
(set-default-font "Inconsolata-10")

;; enable ido-mode
(require 'ido)
(ido-mode t)

;; enable php-mode
(require 'php-mode)

;; json mode width
(add-hook 'json-mode-hook
          (lambda ()
            (make-local-variable 'js-indent-level)
            (setq js-indent-level 2)))

;; Display ido results vertically, rather than horizontally
(setq ido-decorations (quote ("\n-> " "" "\n   " "\n   ..." "[" "]" " [No match]" " [Matched]" " [Not readable]" " [Too big]" " [Confirm]")))

(defun ido-disable-line-truncation () (set (make-local-variable 'truncate-lines) nil))
(add-hook 'ido-minibuffer-setup-hook 'ido-disable-line-truncation)
(defun ido-define-keys () ;; C-n/p is more intuitive in vertical layout
  (define-key ido-completion-map (kbd "C-n") 'ido-next-match)
  (define-key ido-completion-map (kbd "C-p") 'ido-prev-match))
(add-hook 'ido-setup-hook 'ido-define-keys)

;; flx-ido
(require 'flx-ido)
(flx-ido-mode 1)

;; disable ido faces to see flx highlights.
(setq ido-enable-flex-matching t)
(setq ido-use-faces nil)

;; always use flycheck
(add-hook 'after-init-hook 'global-flycheck-mode)

;; always use company mode
(add-hook 'after-init-hook 'global-company-mode)

;; always use projectile
(projectile-global-mode)

;; use anaconda-mode with python
(add-hook 'python-mode-hook 'anaconda-mode)

;; single window mode and double window mode
(defun sw ()
  (interactive)
  (delete-other-windows)
  (set-frame-size (selected-frame) 80 65))

(defun dw ()
  (interactive)
  (delete-other-windows)
  (set-frame-size (selected-frame) 167 65)
  (split-window (selected-window) 85 t))

;; Clear echo area
(princ "" t)
