;; flycheck

(el-get-install 'flycheck)

(add-hook 'after-init-hook #'global-flycheck-mode)