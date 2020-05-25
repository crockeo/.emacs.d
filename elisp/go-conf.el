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
  (setq-default indent-tabs-mode 1)
  (setq indent-tabs-mode 1)

  (setq-default tab-width 4)
  (setq tab-width 4))

(defun go-conf--setup-go-mode ()
  "Ensures that go-mode is set up correctly."
  (go-conf--ensure-lsp)
  (setq lsp-enable-snippet nil)
  (lsp-deferred)

  (go-conf--custom-indentation))

(add-hook 'go-mode-hook 'go-conf--setup-go-mode)
