;;; init.el --- Moritz Scherer's Emacs setup.  -*- lexical-binding: t; -*-
;;
;;; Commentary:
;; This file loads use-package, org-mode, and compiles and executes readme.org
;;
;;; Code:


(when (window-system)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (tooltip-mode -1))

(require 'package)
;; Workaround the TLS problems with ELPA in older versions
(unless '(version> emacs-version "29.0")
  (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3"))
(unless (package-installed-p 'gnu-elpa-keyring-update)
  (setq package-check-signature nil))
(unless '(version> emacs-version "29.0")
  (package-install 'gnu-elpa-keyring-update))


(setq package-install-upgrade-built-in t)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("ublt" . "https://elpa.ubolonton.org/packages/") t)
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t) ; Org-mode's repository
(setq package-native-compile t)
(setq use-package-always-ensure t)
(unless (package-installed-p 'use-package)
  (message "refreshing contents")
  (unless package-archive-contents (package-refresh-contents))
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package)
  )

(setq max-lisp-eval-depth 2000)

(defconst user-init-dir
  (cond ((boundp 'user-emacs-directory)
         user-emacs-directory)
        ((boundp 'user-init-directory)
         user-init-directory)
        (t "~/.emacs.d/")))

(defun load-user-file (file)
  (interactive "f")
  "Load a file in current user's configuration directory"
  (load-file (expand-file-name file user-init-dir)))

(load-user-file "bootstrap-straight.el")
(load-user-file "config.el")
(load-user-file "snippets.el")
(load-user-file "packages.el")
(load-user-file "project.el")
(load-user-file "lsp.el")
(load-user-file "keybindings.el")
(load-user-file "gpt.el")

(defun my-magit/delete-merged-branches ()
  (interactive)
  (magit-fetch-all-prune)
  (let* ((default-branch
           (read-string "Default branch: " (magit-get-current-branch)))
         (merged-branches
          (magit-git-lines "branch"
                           "--format" "%(refname:short)"
                           "--merged"
                           default-branch))
         (branches-to-delete
          (remove default-branch merged-branches)))
    (if branches-to-delete
        (if (yes-or-no-p (concat "Delete branches? ["
                                 (mapconcat 'identity branches-to-delete ", ") "]"))
            (magit-branch-delete branches-to-delete))
      (message "Nothing to delete"))))

(transient-append-suffix 'magit-branch "C"
    '("K" "delete all merged" my-magit/delete-merged-branches))


(provide 'init)
