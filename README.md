# Moritz' Emacs config

Tested with Emacs 30.1. Restart Emacs after changing package initialization;
`early-init.el` disables automatic activation so `init.el` can load packages
in the correct order (including the Nael/nael-lsp autoload workaround).

Most packages use package.el/use-package; gptel uses the existing straight.el
checkout. Missing Emacs packages may be downloaded on first startup. Icon
fonts are installed explicitly with `M-x all-the-icons-install-fonts`.

## Optional external tools

Install the tools for the languages you use and make them visible on Emacs's
`exec-path`. Opening files does not install external software.

| Language / feature | Executable |
| --- | --- |
| Python completion | `pyright-langserver` |
| Python fixes and formatting on save | `ruff` |
| C/C++ | `clangd` |
| Verilog / SystemVerilog | `verible-verilog-ls` |
| CMake | `cmake-language-server` |
| Rust (when rust-mode is installed) | `rust-analyzer` |
| Local chat | Ollama at `localhost:11434`, with model `phi4` |

Language servers start automatically only when their executable is available.
Ruff formatting is local only and is skipped when Ruff is missing.
Verilog uses `verilog-ts-mode` when the `verilog` grammar is available, falling
back to `verilog-mode` otherwise. MLIR support is optional; customize
`my-mlir-emacs-directory` and `my-mlir-language-server` for your LLVM checkout.
The Lean input method loads its translation data when first selected.

`C-c m` compiles the current project; `C-c M` keeps your side of a merge
conflict. The project menu uses built-in grep and shell commands, and
`C-c R` selects a recent file in the current project. Scheme formatting is
manual (`C-c C-l`), since automatic whitespace rewriting can change strings.

## Verification

With the packages already installed, run from this directory:

```sh
emacs -Q --batch -l tests/config-tests.el
```

The tests load the configuration and theme, check language-mode hooks and
missing-tool behavior, and exercise the Lean input method. Package downloads
are blocked, recent/history state is redirected to temporary files, and
language servers are stubbed during mode tests. Live server connections and
GUI rendering require a separate interactive check.
