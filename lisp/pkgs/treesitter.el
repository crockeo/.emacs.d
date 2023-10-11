(ch/pkg treesitter
  (setq treesit-language-source-alist
	'((tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
	  (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
          (elixir "https://github.com/elixir-lang/tree-sitter-elixir" "main" "src")
          (heex "https://github.com/phoenixframework/tree-sitter-heex" "main" "src")))

  (defun ch/treesitter/install (grammar)
    (let* ((emacs-dir (expand-file-name user-emacs-directory))
	   (treesitter-dir (concat emacs-dir "tree-sitter/"))
	   (grammar-file (concat treesitter-dir (format "libtree-sitter-%s.dylib" grammar))))
      (unless (file-exists-p grammar-file)
	(treesit-install-language-grammar grammar))))

  (mapc
   #'ch/treesitter/install
   (mapcar #'car treesit-language-source-alist))

  (add-to-list 'auto-mode-alist '("\\.tsx\\'" . tsx-ts-mode))
  (add-hook 'tsx-ts-mode-hook #'lsp-deferred)

  (add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-ts-mode))
  (add-hook 'typescript-ts-mode-hook #'lsp-deferred)

  )
