;; load php-mode libs

;; Make sure buffers are cleaned up when the windows is closed
(add-hook 'server-switch-hook
	  (lambda ()
	    (menu-bar-mode -1)))
 
(add-hook 'server-done-hook (lambda nil (kill-buffer nil)))
