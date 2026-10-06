;;; mlir.el --- Optional local LLVM tooling -*- lexical-binding: t; -*-

(defcustom my-mlir-emacs-directory
  (expand-file-name "~/devel/expanse_iree/third_party/iree/third_party/llvm-project/mlir/utils/emacs")
  "Directory containing the optional LLVM mlir-mode.el."
  :type 'directory :group 'languages)

(defcustom my-mlir-language-server
  (expand-file-name "~/devel/expanse_iree/install/bin/iree-mlir-lsp-server")
  "MLIR language server executable."
  :type 'file :group 'languages)

(when (file-readable-p (expand-file-name "mlir-mode.el" my-mlir-emacs-directory))
  (add-to-list 'load-path my-mlir-emacs-directory)
  (autoload 'mlir-mode "mlir-mode" nil t)
  (add-to-list 'auto-mode-alist '("\\.mlir\\'" . mlir-mode))
  (with-eval-after-load 'eglot
    (add-to-list 'eglot-server-programs
                 `(mlir-mode . (,my-mlir-language-server)))))
