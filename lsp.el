;;; lsp.el --- Moritz Scherer's Emacs setup.  -*- lexical-binding: t; -*-

(use-package xref
  :pin gnu
  :custom (xref-auto-jump-to-first-xref t)
  :bind (("C-c q" . #'xref-find-definitions)
         ("C-c w" . #'xref-find-references)
         ("s-[" . #'xref-go-back)
         ("s-]" . #'xref-go-forward)))

(use-package eldoc
  :pin gnu
  :diminish
  :bind ("s-d" . #'eldoc)
  :custom
  (eldoc-echo-area-prefer-doc-buffer t)
  (eldoc-echo-area-use-multiline-p t))

(use-package eglot
  :hook ((python-mode . eglot-ensure)
         (verilog-mode . eglot-ensure)
         (rust-mode . eglot-ensure)
         (c-mode . eglot-ensure)
         (cmake-mode . eglot-ensure)
         )
  :bind (:map eglot-mode-map
         ("C-c q" . #'xref-find-definitions)
         ("C-c w" . #'xref-find-references)
         ("C-c a" . #'eglot-rename)
         ("C-c C-c" . #'eglot-code-actions)
         ("C-c C-l" . #'eglot-format-buffer)
         ("C-c C-r" . #'my-eglot-camel-to-snake))
  :custom
  (eglot-confirm-server-initiated-edits nil)
  (eglot-autoshutdown t)
  (eglot-send-changes-idle-time 0.1)
  )

(fset #'jsonrpc--log-event #'ignore)

(use-package flymake
  :config
  (setq elisp-flymake-byte-compile-load-path load-path)
  :hook ((emacs-lisp-mode . flymake-mode)))

(use-package numpydoc
  :custom
  (numpydoc-insert-examples-block nil)
  (numpydoc-template-long nil)
  :bind (:map python-mode-map
              ("C-c C-n" . numpydoc-generate)))

;; Provide drop-down completion.
(use-package company
  :custom
  ;; Search other buffers with the same modes for completion instead of
  ;; searching all other buffers.
  (company-dabbrev-other-buffers t)
  (company-dabbrev-code-other-buffers t)
  ;; M-<num> to select an option according to its number.
  (company-show-numbers t)
  ;; Only 2 letters required for completion to activate.
  (company-minimum-prefix-length 3)
  ;; Do not downcase completions by default.
  (company-dabbrev-downcase nil)
  ;; Even if I write something with the wrong case,
  ;; provide the correct casing.
  (company-dabbrev-ignore-case t)
  ;; company completion wait
  (company-idle-delay 0.2)
  ;; No company-mode in shell & eshell
  (company-global-modes '(not eshell-mode shell-mode))
  ;; Use company with text and programming modes.
    :hook ((text-mode . company-mode)
           (prog-mode . company-mode)))

(defun run-formatter-on-buffer (command args)
  "Run a formatter COMMAND with ARGS on the current buffer content.
The formatter is run in a temporary buffer, and the result is applied back to the current buffer
only if no conflicts with unsaved changes are detected."
  (let ((original-content (buffer-string)) ;; Save current content
        (formatted-content nil))
    ;; Run the formatter in a temporary buffer
    (with-temp-buffer
      (insert original-content)
      (let ((exit-code (apply #'call-process-region (point-min) (point-max)
                              command t t nil args)))
        (if (zerop exit-code)
            (setq formatted-content (buffer-string))
          (message "Formatter %s failed with exit code %d" command exit-code))))
    ;; Only apply changes if the content has not been modified
    (when (and formatted-content
               (string= original-content (buffer-string)))
      (erase-buffer)
      (insert formatted-content))))

(load-user-file "lsp/verilog.el")
(load-user-file "lsp/c.el")
(load-user-file "lsp/cmake.el")
(load-user-file "lsp/python.el")
(load-user-file "lsp/lisp.el")
