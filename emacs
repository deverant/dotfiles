; -*- coding: utf-8; mode: lisp -*-

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(inhibit-startup-screen t)
 '(menu-bar-mode nil)
 '(org-agenda-files (quote ("~/Dropbox/org")))
 '(package-selected-packages
   (quote
    (ido-completing-read+ rust-mode puppet-mode projectile json-mode jedi flycheck flx-ido company-anaconda))))
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

;; show line and column numbers
(setq line-number-mode t)
(setq column-number-mode t)

;; only needed pre-26.3
(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")

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
    ido-completing-read+
    jedi
    jedi-core
    json-mode
    puppet-mode
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
(ido-everywhere t)
(require 'ido-completing-read+)
(ido-ubiquitous-mode t)

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
(projectile-mode +1)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

;; use anaconda-mode with python
(add-hook 'python-mode-hook 'anaconda-mode)

;; try harder to find include files in flymake
(defvar mh-erlang-flymake-code-path-dirs (list "../../*/ebin")
  "List of directories to add to code path for Erlang Flymake.
Wildcards are expanded.")

(defun mh-simple-get-deps-code-path-dirs ()
  ;; Why complicate things?
  (and (buffer-file-name)
       (let ((default-directory (file-name-directory (buffer-file-name))))
         (apply 'append
                (mapcar
                 (lambda (wildcard)
                   ;; If the wild card expands to a directory you
                   ;; don't have read permission for, this would throw
                   ;; an error.
                   (ignore-errors
                     (file-expand-wildcards wildcard)))
                 mh-erlang-flymake-code-path-dirs)))))

(defun mh-simple-get-deps-include-dirs ()
  (list "../include" "../src" ".."))

(setq erlang-flymake-get-code-path-dirs-function 'mh-simple-get-deps-code-path-dirs
      erlang-flymake-get-include-dirs-function 'mh-simple-get-deps-include-dirs)

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

;; Clear whitespace on save
(add-hook 'before-save-hook 'whitespace-cleanup)

;; Add org-mode keybindings
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)

;; org-mode default capture location
(setq org-capture-templates
      '(("t" "Todo" entry (file "~/Dropbox/org/refile.org")
         "* TODO %?\n%U\n%a\n")
        ("j" "Journal" entry (file+datetree "~/Dropbox/org/journal.org")
         "* %?\nEntered on %U\n  %i\n  %a")))

; Targets include this file and any file contributing to the agenda - up to 9 levels deep
(setq org-refile-targets (quote ((nil :maxlevel . 9)
                                 (org-agenda-files :maxlevel . 9))))

; Use full outline paths for refile targets - we file directly with IDO
(setq org-refile-use-outline-path 'file)

; Targets complete directly with IDO
(setq org-outline-path-complete-in-steps nil)

; Allow refile to create parent tasks with confirmation
(setq org-refile-allow-creating-parent-nodes (quote confirm))

; Use IDO for both buffer and file completion and ido-everywhere to t
(setq ido-everywhere t)
(setq ido-max-directory-size 100000)
(ido-mode (quote both))
; Use the current window when visiting files and buffers with ido
(setq ido-default-file-method 'selected-window)
(setq ido-default-buffer-method 'selected-window)
; Use the current window for indirect buffer display
(setq org-indirect-buffer-display 'current-window)

;;;; Refile settings
; Exclude DONE state tasks from refile targets
(defun bh/verify-refile-target ()
  "Exclude todo keywords with a done state from refile targets"
  (not (member (nth 2 (org-heading-components)) org-done-keywords)))

(setq org-refile-target-verify-function 'bh/verify-refile-target)

;; Overwrite the current window with the agenda
(setq org-agenda-window-setup 'current-window)

;; Enforce todo dependencies
(setq org-enforce-todo-dependencies t)

;; Hide scheduled items from global todo list
(setq org-agenda-todo-ignore-scheduled t)
(setq org-agenda-todo-ignore-deadlines t)

;; Do not dim blocked tasks
(setq org-agenda-dim-blocked-tasks nil)

;; Compact the block agenda view
(setq org-agenda-compact-blocks t)

;; Use dashes for second level of lists
(setq org-list-demote-modify-bullet (quote (("+" . "-")
                                            ("*" . "-")
                                            ("1." . "-")
                                            ("1)" . "-")
                                            ("A)" . "-")
                                            ("B)" . "-")
                                            ("a)" . "-")
                                            ("b)" . "-")
                                            ("A." . "-")
                                            ("B." . "-")
                                            ("a." . "-")
                                            ("b." . "-"))))

;; Don't set archives tasks as DONE
(setq org-archive-mark-done nil)
(setq org-archive-location "%s_archive::* Archived Tasks")

;; Use xdg-open to open URLs
(setq browse-url-browser-function 'browse-url-xdg-open)

;; Clear echo area
(princ "" t)
