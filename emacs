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

;; set load paths
(add-to-list 'load-path "~/.emacs.d/")

;; byte-compile anything that has updated since last time
(byte-recompile-directory (expand-file-name "~/.emacs.d") 0)

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

;; by default use postgresql highlighting for sql mode and load plsql mode
(add-hook 'sql-mode-hook 'sql-highlight-postgres-keywords)
(load "~/.emacs.d/plsql.elc")

;; load theme
(load "~/.emacs.d/base16-emacs/base16-default-theme.el")

;; tabulation on top with M-left and M-right navigation
(tabbar-mode)

(defun my-tabbar-buffer-groups () ;; customize to show all normal files in one group
   "Returns the name of the tab group names the current buffer belongs to.
 There are two groups: Emacs buffers (those whose name starts with “*”, plus
 dired buffers), and the rest.  This works at least with Emacs v24.2 using
 tabbar.el v1.7."
   (list (cond ((string-equal "*" (substring (buffer-name) 0 1)) "emacs")
               ((eq major-mode 'dired-mode) "emacs")
               (t "user"))))
(setq tabbar-buffer-groups-function 'my-tabbar-buffer-groups)

(global-set-key [M-left] 'tabbar-backward-tab)
(global-set-key [M-right] 'tabbar-forward-tab)


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


;; Clear echo area
(princ "" t)
