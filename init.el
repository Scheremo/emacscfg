;;; init.el --- Moritz Scherer's Emacs setup.  -*- lexical-binding: t; -*-
;;
;;; Commentary:
;; Activate packages and load the configuration modules.
;;
;;; Code:


(when (window-system)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (tooltip-mode -1))

(require 'package)
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("nongnu" . "https://elpa.nongnu.org/nongnu/")
                         ("melpa" . "https://melpa.org/packages/")))
(setq package-native-compile t)

;; nael-lsp's generated autoloads access nael-mode-map before loading Nael.
;; Activate everything else first, so Nael sees the installed dependencies.
(let ((package-load-list (cons '(nael-lsp nil) package-load-list)))
  (package-initialize))
(when (and (assq 'nael-lsp package-alist)
           (or (equal (assq 'nael-lsp package-load-list) '(nael-lsp t))
               (and (memq 'all package-load-list)
                    (not (assq 'nael-lsp package-load-list)))))
  (require 'nael)
  (package-activate 'nael-lsp))

;; use-package is built into Emacs 29 and newer.
(unless (require 'use-package nil t)
  (unless package-archive-contents (package-refresh-contents))
  (package-install 'use-package)
  (require 'use-package))
(setq use-package-always-ensure t)

(setq max-lisp-eval-depth 2000) 

(defconst user-init-dir
  (cond ((boundp 'user-emacs-directory)
         user-emacs-directory)
        ((boundp 'user-init-directory)
         user-init-directory)
        (t "~/.emacs.d/")))

(defun load-user-file (file)
  "Load FILE from the user configuration directory."
  (interactive "f")
  (load-file (expand-file-name file user-init-dir)))

(load-user-file "bootstrap-straight.el")
(load-user-file "config.el")
(load-user-file "snippets.el")
(load-user-file "packages.el")
(load-user-file "project.el")
(load-user-file "lsp.el")
(load-user-file "keybindings.el")
(load-user-file "gpt.el")


(provide 'init)
