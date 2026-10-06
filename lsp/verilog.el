;;; verilog.el --- Moritz Scherer's Emacs setup.  -*- lexical-binding: t; -*-

;;; VERILOG

(require 'eglot)
(require 'treesit nil t)

(use-package verilog-ts-mode)
(add-to-list 'load-path (expand-file-name "veb" user-init-dir))
(require 'verilog-eglot-bender)

;; Prefer verilog-ts-mode for Verilog / SystemVerilog sources
(when (and (fboundp 'verilog-ts-mode)
           (fboundp 'treesit-ready-p)
           (treesit-ready-p 'verilog t))
  ;; Remap verilog-mode to verilog-ts-mode
  (add-to-list 'major-mode-remap-alist '(verilog-mode . verilog-ts-mode))

  ;; Explicitly register extensions
  (dolist (pattern '("\\.v\\'" "\\.sv\\'" "\\.svh\\'"))
    (add-to-list 'auto-mode-alist (cons pattern 'verilog-ts-mode))))

;; (use-package verilog-eglot-bender
;;   :ensure nil
;;   :hook ((verilog-ts-mode . veb/on-verilog-buffer)
;;          (verilog-mode . veb/on-verilog-buffer))
;;   :custom
;;   (veb-filelist-name "target/slang/build/slang-flist-simulation.f")
;;   (veb-lsp-executable "circt-verilog-lsp-server")
;;   (veb-debug 0))

(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               '((verilog-mode verilog-ts-mode) . ("verible-verilog-ls"))))
