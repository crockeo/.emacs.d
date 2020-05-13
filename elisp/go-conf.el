;; Installing packages
(use-package go-mode)

(defun go-conf--ensure-lsp ()
  "Ensures that we have the golang language server available."
  (unless (executable-find "gopls")
    (start-process "ensure-lsp" "ensure-lsp"
                   "go"
                   "get"
                   "golang.org/x/tools/gopls")
    (message "gopls installed")))

(defun go-conf--custom-indentation ()
  "Sets Golang tab width to 4"
  (setq indent-tabs-mode 1)
  (setq tab-width 4))

(defun go-conf--setup-go-mode ()
  "Ensures that go-mode is set up correctly."
  (ensure-lsp)
  (setq lsp-enable-snippet nil)
  (lsp-deferred)

  (custom-indentation))

(remove-hook 'go-mode-hook 'go-conf--setup-go-mode)
(add-hook 'go-mode-hook 'go-conf--setup-go-mode)
