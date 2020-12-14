(defun c++-conf--find-clangd ()
  "Finds the location of clangd on the system for use by LSP."
  (let ((found-clangd (locate-file "clangd" exec-path)))
    (if found-clangd
        found-clangd
      "/usr/local/opt/llvm/bin/clangd")))

(defun c++-conf--setup ()
  (setq-default lsp-clients-clangd-executable (c++-conf--find-clangd)))

(add-hook 'c++-mode-hook 'c++-conf--setup)

;; open .h files in c++-mode instead of c-mode
(add-to-list
 'auto-mode-alist
 '("\\.h\\'" . c++-mode))
