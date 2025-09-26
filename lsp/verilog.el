;;; verilog.el --- Moritz Scherer's Emacs setup.  -*- lexical-binding: t; -*-

;;; VERILOG

(require 'eglot)
(require 'treesit)

(use-package verilog-ts-mode)

;; Prefer verilog-ts-mode for Verilog / SystemVerilog sources
(when (fboundp 'verilog-ts-mode)
  ;; Remap verilog-mode to verilog-ts-mode
  (add-to-list 'major-mode-remap-alist '(verilog-mode . verilog-ts-mode))

  ;; Explicitly register extensions
  (dolist (pattern '("\\.v\\'" "\\.sv\\'" "\\.svh\\'"))
    (add-to-list 'auto-mode-alist (cons pattern 'verilog-ts-mode))))

;; (with-eval-after-load 'eglot
;;   (add-to-list 'eglot-server-programs
;;                `((verilog-mode)
;;                  . ,(list "circt-verilog-lsp-server"
;;                           "--command-file"
;;                           (expand-file-name "/Users/scheremo/devel/snitch_upstream/compile_arcilator.f")))))


(use-package verilog-eglot-bender
  :load-path "~/.emacs.d/veb"
  :hook (verilog-ts-mode . veb/on-verilog-buffer)
  :custom
  (veb-filelist-name ".circt-verilog.f")
  (veb-lsp-executable "circt-verilog-lsp-server")
  (veb-debug 0)
  )
