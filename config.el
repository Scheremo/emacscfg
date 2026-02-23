;;; config.el --- Moritz Scherer's Emacs setup.  -*- lexical-binding: t; -*-

;;; General purpose configuration

(set-frame-font "monospace 16" nil t)

(setq gc-cons-threshold 100000000)
(setq max-specpdl-size 5000)
(setq read-process-output-max (* 1024000 1024)) ;; 1mb

(setq
 ;; No need to see GNU agitprop.
 inhibit-startup-screen t
 ;; No need to remind me what a scratch buffer is.
 initial-scratch-message nil
 ;; Double-spaces after periods is morally wrong.
 sentence-end-double-space nil
 ;; Never ding at me, ever.
 ring-bell-function 'ignore
 ;; Save existing clipboard text into the kill ring before replacing it.
 save-interprogram-paste-before-kill t
 ;; Prompts should go in the minibuffer, not in a GUI.
 use-dialog-box nil
 ;; Fix undo in commands affecting the mark.
 mark-even-if-inactive t
 ;; Let C-k delete the whole line.
 kill-whole-line t
 ;; search should be case-sensitive by default
 case-fold-search nil
 ;; accept 'y' or 'n' instead of yes/no
 ;; the documentation advises against setting this variable
 ;; the documentation can get bent imo
 use-short-answers t
 ;; eke out a little more scrolling performance
 fast-but-imprecise-scrolling t
 ;; prefer newer elisp files
 load-prefer-newer t
 ;; when I say to quit, I mean quit
 confirm-kill-processes nil
 ;; if native-comp is having trouble, there's not very much I can do
 native-comp-async-report-warnings-errors 'silent
 ;; unicode ellipses are better
 truncate-string-ellipsis "…"
 ;; I want to close these fast, so switch to it so I can just hit 'q'
 help-window-select t
 ;; keep the point in the same place while scrolling
 scroll-preserve-screen-position t
 ;; more info in completions
 completions-detailed t
 ;; highlight error messages more aggressively
 next-error-message-highlight t
 ;; don't let the minibuffer muck up my window tiling
 read-minibuffer-restore-windows t
 ;; scope save prompts to individual projects
 save-some-buffers-default-predicate 'save-some-buffers-root
 ;; don't keep duplicate entries in kill ring
 kill-do-not-save-duplicates t
 )

;; Never mix tabs and spaces. Never use tabs, period.
;; We need the setq-default here because this becomes
;; a buffer-local variable when set.
(setq-default indent-tabs-mode nil)

(set-charset-priority 'unicode)
(prefer-coding-system 'utf-8-unix)

(setq column-number-mode t)
(setq isearch-lazy-count t)

(delete-selection-mode t)
(global-display-line-numbers-mode t)
(column-number-mode)
(savehist-mode)

(require 'hl-line)
(add-hook 'prog-mode-hook #'hl-line-mode)
(add-hook 'text-mode-hook #'hl-line-mode)

(setq
 make-backup-files nil
 auto-save-default nil
 create-lockfiles nil)

(setq custom-file (make-temp-name "/tmp/"))
(setq custom-safe-themes t)

(setq require-final-newline t)

(defalias 'view-emacs-news 'ignore)
(defalias 'describe-gnu-project 'ignore)
(defalias 'describe-copying 'ignore)

(setq-default fill-column 100)

(require 'tramp)
(customize-set-variable 'tramp-default-remote-shell "/bin/bash")
(add-to-list 'tramp-connection-properties
             (list (regexp-quote "/ssh:moritz@mosaic-soc.ch:")
                   "remote-shell" "/bin/bash"))

(setq remote-file-name-inhibit-locks t)

;; Needs to be called from recentf's :init
;; todo: make this into a use-package invocation
(defun pt/customize-tramp ()

  (setq tramp-default-method "ssh"
        tramp-verbose 1
        remote-file-name-inhibit-cache nil)
  )

;; (setopt tramp-remote-path '(tramp-own-remote-path))

(use-package recentf
  :pin gnu
  :after dash
  :init (pt/customize-tramp) ;; so that tramp urls work ok in recentf
  :custom
  ;; (recentf-exclude (-concat recentf-exclude '("\\elpa"
  ;;                                             "private/tmp" ; to avoid custom files
  ;;                                             "txt/roam"
  ;;                                             "type-break"
  ;;                                             )))
  (recentf-max-saved-items 50)
  (recentf-max-menu-items 30)
  :config (recentf-mode))

(add-to-list 'auto-mode-alist '("\\.hjson\\'" . json-mode))
(add-to-list 'auto-mode-alist '("\\.json\\'" . json-mode))
(add-to-list 'auto-mode-alist '("\\.sdc\\'" . tcl-mode))
