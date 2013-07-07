;; by default use postgresql highlighting for sql mode and load plsql mode
(add-hook 'sql-mode-hook 'sql-highlight-postgres-keywords)
(load "plsql")
