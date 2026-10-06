;;; lsp.el --- Moritz Scherer's Emacs setup.  -*- lexical-binding: t; -*-

(straight-use-package 'gptel)

(require 'gptel)
(require 'gptel-ollama)

;; OPTIONAL configuration
(setq
 gptel-model 'phi4
 gptel-backend (gptel-make-ollama "Ollama"
                 :host "localhost:11434"
                 :stream t
                 :models '(phi4)))

(setq gptel-use-context 'user)

(global-unset-key (kbd "M-l"))

(global-set-key (kbd "M-l r") 'gptel-rewrite)
(global-set-key (kbd "M-l m") 'gptel-menu)
(global-set-key (kbd "M-l b") 'gptel)
(global-set-key (kbd "M-l M-l") 'gptel-abort)
(global-set-key (kbd "M-l a") 'gptel-add)
(global-set-key (kbd "M-l s") 'gptel-send)
