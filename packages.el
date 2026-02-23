;;; packages.el --- Moritz Scherer's Emacs setup.  -*- lexical-binding: t; -*-

(use-package yaml-mode)

(use-package tree-sitter
  :config (global-tree-sitter-mode))

(use-package tree-sitter-langs)

(let ((installed (package-installed-p 'all-the-icons)))
  (use-package all-the-icons)
  (unless installed (all-the-icons-install-fonts)))

(use-package all-the-icons-dired
  :after all-the-icons
  :hook (dired-mode . all-the-icons-dired-mode))

;; Let the OS determine what monospace means...
(set-face-attribute 'default nil :font "monospace")

(add-to-list 'default-frame-alist '(fullscreen . maximized))

(use-package diminish
  :config
  (diminish 'visual-line-mode))

(use-package rainbow-delimiters
  :disabled
  :hook ((prog-mode . rainbow-delimiters-mode)))

(use-package centered-window
  :custom
  (cwm-centered-window-width 180))

(use-package undo-tree)
(global-undo-tree-mode)

(use-package multiple-cursors
  :defer 1
  )

(use-package ws-butler
  :ensure t :hook (prog-mode . ws-butler-mode))

(use-package magit)


(use-package markdown-mode
  :hook (gfm-mode . visual-line-mode)
  :bind (:map markdown-mode-map ("C-c C-s a" . markdown-table-align))
  :mode ("\\.md$" . gfm-mode))

(use-package apheleia
  :custom (apheleia-remote-algorithm 'local)
  )

(use-package doxymacs
  :vc (:url "https://github.com/pniedzielski/doxymacs.git"
            :rev :newest
            :lisp-dir "lisp/")
  :hook (c-mode-common-hook . doxymacs-mode)
  :bind (:map c-mode-base-map
              ;; Lookup documentation for the symbol at point.
              ("C-c d ?" . doxymacs-lookup)
              ;; Rescan your Doxygen tags file.
              ("C-c d r" . doxymacs-rescan-tags)
              ;; Prompt you for a Doxygen command to enter, and its
              ;; arguments.
              ("C-c d RET" . doxymacs-insert-command)
              ;; Insert a Doxygen comment for the next function.
              ("C-c d f" . doxymacs-insert-function-comment)
              ;; Insert a Doxygen comment for the current file.
              ("C-c d i" . doxymacs-insert-file-comment)
              ;; Insert a Doxygen comment for the current member.
              ("C-c d ;" . doxymacs-insert-member-comment)
              ;; Insert a blank multi-line Doxygen comment.
              ("C-c d m" . doxymacs-insert-blank-multiline-comment)
              ;; Insert a blank single-line Doxygen comment.
              ("C-c d s" . doxymacs-insert-blank-singleline-comment)
              ;; Insert a grouping comments around the current region.
              ("C-c d @" . doxymacs-insert-grouping-comments)))

(use-package cmake-mode)

;; (use-package direnv
;;   :config (direnv-mode)
;;   :custom (direnv-always-show-summary nil))

(use-package dracula-theme)
(defun dracula()
  (interactive)
  (load-theme 'dracula t))

(add-hook 'after-init-hook 'dracula)

(add-to-list 'load-path "~/devel/axir/third_party/llvm-project/mlir/utils/emacs")
(require 'mlir-mode)
