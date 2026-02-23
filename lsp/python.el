;; Install eglot if not already installed
(unless (package-installed-p 'eglot)
  (package-refresh-contents)
  (package-install 'eglot))

;; Enable eglot for Python
(add-hook 'python-mode-hook 'eglot-ensure)

;; Specify the server to use for Python (e.g., pyright)
(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               `(python-mode . ("pyright-langserver" "--stdio"))))

;; Optional: Install pyright easily if it's not installed
(defun ensure-pyright-installed ()
  "Ensure the pyright language server is installed."
  (unless (executable-find "pyright-langserver")
    (message "Pyright is not installed. Installing via npm...")
    (call-process-shell-command "npm install -g pyright")))

(use-package reformatter)

(reformatter-define ruff-format
  :program "ruff"
  :args (list "format"
              "--stdin-filename"
              (or (buffer-file-name) "temp.py")  ;; use actual file path if available
              "-")
  :stdin t)

(reformatter-define ruff-fix
  :program "ruff"
  :args (list "--fix"
              "--stdin-filename"
              (or (buffer-file-name) "temp.py")
              "-")
  :stdin t)

(defun my-python-ruff-formatting ()
  "Apply Ruff lint fixes and format using Ruff."
  (interactive)
  (when (eq major-mode 'python-mode)
    (let ((pt (point)))
      (ruff-fix-buffer)
      (ruff-format-buffer)
      (goto-char pt))))

(defun my-setup-python-formatting ()
  "Set up custom formatting passes for Python."
  (add-hook 'before-save-hook #'my-python-ruff-formatting nil t))

;; Add the setup to Python mode
(add-hook 'python-mode-hook 'my-setup-python-formatting)
(add-hook 'python-mode-hook 'ensure-pyright-installed)
