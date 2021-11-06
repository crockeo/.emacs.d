(ch/pkg swift
  (use-package swift-mode
    :hook (swift-mode . (lambda () (lsp-deferred))))

  (use-package lsp-sourcekit
    :after lsp-mode
    :config
    (setq lsp-sourcekit-executable
	  (string-trim (shell-command-to-string "xcrun --find sourcekit-lsp")))))
