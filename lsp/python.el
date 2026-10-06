;;; python.el --- Python language tools -*- lexical-binding: t; -*-

(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               '((python-mode python-ts-mode) . ("pyright-langserver" "--stdio"))))

(use-package reformatter
  :config
  (reformatter-define ruff-format
    :program "ruff"
    :args (list "format" "--stdin-filename"
                (or buffer-file-name "temp.py") "-"))
  (reformatter-define ruff-fix
    :program "ruff"
    :args (list "check" "--fix" "--exit-zero" "--stdin-filename"
                (or buffer-file-name "temp.py") "-")))

(defun my-python-ruff-formatting ()
  "Apply Ruff fixes and formatting when Ruff is available locally."
  (when (and (derived-mode-p 'python-mode 'python-ts-mode)
             (not (file-remote-p default-directory))
             (executable-find "ruff"))
    (save-excursion
      (ruff-fix-buffer)
      (ruff-format-buffer))))

(defun my-setup-python-formatting ()
  "Set up buffer-local Python formatting."
  (add-hook 'before-save-hook #'my-python-ruff-formatting nil t))

(dolist (hook '(python-mode-hook python-ts-mode-hook))
  (add-hook hook #'my-setup-python-formatting))
