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

;; set load path to libraries
(add-to-list 'load-path "~/.emacs.d/lib/")

;; byte-compile anything in lib/ that has updated since last time
(byte-recompile-directory (expand-file-name "~/.emacs.d/lib/") 0)

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

;; single window mode and double window mode
(defun sw ()
  (interactive)
  (delete-other-windows)
  (set-frame-size (selected-frame) 80 65))

(defun dw ()
  (interactive)
  (delete-other-windows)
  (set-frame-size (selected-frame) 163 65)
  (split-window (selected-window) 83 t))

(dolist (file (directory-files "~/.emacs.d/init.d" t ".elc?$"))
  (load (file-name-sans-extension file)))

;; Clear echo area
(princ "" t)
