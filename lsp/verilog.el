;;; verilog.el --- Moritz Scherer's Emacs setup.  -*- lexical-binding: t; -*-

;;; VERILOG

(require 'eglot)
(require 'treesit)

(use-package verilog-ts-mode)

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
