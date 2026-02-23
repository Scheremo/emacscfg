;;; c.el --- Moritz Scherer's Emacs setup.  -*- lexical-binding: t; -*-

(require 'eglot)

(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               '((c-mode c++-mode)
                 . ("clangd"
                    "-j=8"
                    "--log=verbose"
                    "--pretty"
                    "--background-index"
                    "--completion-style=detailed"
                    "--pch-storage=memory"
                    "--header-insertion-decorators"
                    "--header-insertion=iwyu"
                    "--query-driver=/opt/riscv/bin/riscv32-corev-elf-gcc, /opt/riscv/bin/riscv32-corev-elf-g++"))))
