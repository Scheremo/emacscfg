;;; verilog-eglot-bender.el --- Verilog TS + Bender + Verible via Eglot  -*- lexical-binding: t; -*-

(require 'eglot)
(require 'project)
(require 'subr-x)
(require 'cl-lib)

;;;; Debugging
(defgroup veb nil
  "Verilog + Eglot + Bender integration."
  :group 'tools
  :prefix "veb-")

(defcustom veb-debug t
  "When non-nil, emit verbose debug messages to *Messages*."
  :type 'boolean :group 'veb)

(defmacro veb--log (fmt &rest args)
  "Internal logger. Uses message when veb-debug is non-nil."
  `(when veb-debug
     (message (concat "[VEB] " ,fmt) ,@args)))

;;;; Customization
(defcustom veb-filelist-name ".circt-verilog.f"
  "Name of the file list created in the project root for the LSP."
  :type 'string :group 'veb)

(defcustom veb-lsp-executable "circt-verilog-lsp-server"
  "Path/name of the CIRCT Verilog LSP server (remote if TRAMP)."
  :type 'file :group 'veb)

(defcustom veb-lsp-extra-args nil
  "Extra arguments to pass to the LSP server."
  :type '(repeat string) :group 'veb)

(defcustom veb-enable-fallback t
  "Whether to enable any fallback behavior (placeholder knob)."
  :type 'boolean :group 'veb)

;;;; Tree-sitter associations
(when (fboundp 'verilog-ts-mode)
  (veb--log "Enabling verilog-ts-mode associations")
  ;; Prefer verilog-ts-mode
  (add-to-list 'major-mode-remap-alist '(verilog-mode . verilog-ts-mode))
  ;; Correct regexes (no stray quotes)
  (dolist (pair '(("\\.v\\'"   . verilog-ts-mode)
                  ("\\.sv\\'"  . verilog-ts-mode)
                  ("\\.svh\\'" . verilog-ts-mode)))
    (veb--log "auto-mode-alist add: %S" pair)
    (add-to-list 'auto-mode-alist pair)))

;;;; Project helpers
(defun veb--project-root (&optional dir)
  "Return best-effort project root for DIR (or `default-directory`)."
  (let* ((dir (file-name-as-directory (or dir default-directory)))
         (_   (veb--log "Resolving project root from: %s" dir))
         (git (locate-dominating-file dir ".git"))
         (proj (ignore-errors (project-current nil dir)))
         (root (or git (and proj (project-root proj)) dir)))
    (veb--log "Project root -> %s (git=%s proj=%s)"
              (or root "<nil>") (and git t) (and proj t))
    root))

(defun veb--filelist-path (&optional root)
  (let* ((r (or root (veb--project-root)))
         (p (expand-file-name veb-filelist-name r)))
    (veb--log "Filelist path for root %s -> %s" r p)
    p))

;;;; Eglot + server wiring
(defun veb--lsp-command (&optional _project)
  "Return list (PROGRAM ARGS...) for `eglot-server-programs`."
  (let* ((root (veb--project-root))
         (fl   (veb--filelist-path root))
         (cmd  (list veb-lsp-executable))
         (args (copy-sequence veb-lsp-extra-args))
         (full (append cmd args (when (file-exists-p fl) (list "--command-file" fl)))))
    (veb--log "Computed LSP command:")
    (veb--log "  root: %s" root)
    (veb--log "  filelist: %s (exists=%s)" fl (file-exists-p fl))
    (veb--log "  exec: %s" veb-lsp-executable)
    (veb--log "  extra-args: %S" veb-lsp-extra-args)
    (veb--log "  final argv: %S" full)
    full))

(defun veb--install-eglot-mapping ()
  "Map verilog modes to the dynamic LSP command."
  (veb--log "Installing eglot-server-programs mappings")
  (let ((before eglot-server-programs))
    (setq eglot-server-programs
          (cons '(verilog-ts-mode . veb--lsp-command)
                (assq-delete-all 'verilog-ts-mode eglot-server-programs)))
    (setq eglot-server-programs
          (cons '(verilog-mode . veb--lsp-command)
                (assq-delete-all 'verilog-mode eglot-server-programs)))
    (veb--log "eglot-server-programs (before): %S" before)
    (veb--log "eglot-server-programs (after):  %S" eglot-server-programs)))

(defun veb-start-eglot-if-needed ()
  (veb--log "Ensuring Eglot is started for buffer %s (mode=%s default-dir=%s)"
            (buffer-name) major-mode default-directory)
  (veb--install-eglot-mapping)
  (condition-case err
      (progn
        (veb--log "Calling eglot-ensure …")
        (eglot-ensure)
        (veb--log "eglot-ensure done. Server: %S" (eglot-current-server)))
    (error
     (veb--log "eglot-ensure failed: %S" err)
     (signal (car err) (cdr err)))))

;;;; Minor mode & hooks
(define-minor-mode veb-project-mode
  "Keep file list up-to-date and Eglot configured."
  :lighter " VEB"
  (veb--log "veb-project-mode %s (buffer=%s dir=%s)"
            (if veb-project-mode "ENABLED" "DISABLED")
            (buffer-name) default-directory)
  (when veb-project-mode
    (veb-start-eglot-if-needed)))

(defun veb/on-verilog-buffer ()
  (veb--log "verilog buffer hook: %s (mode=%s)" (buffer-name) major-mode)
  (veb-project-mode 1))

;; Optional: disable fallback on remote (placeholder setting)
(defun veb--maybe-disable-fallback-remote ()
  (when (file-remote-p default-directory)
    (veb--log "Remote directory detected (%s) → disabling fallback" default-directory)
    (setq veb-enable-fallback nil)))

(add-hook 'verilog-ts-mode-hook #'veb--maybe-disable-fallback-remote)
(add-hook 'verilog-mode-hook    #'veb--maybe-disable-fallback-remote)
(add-hook 'verilog-ts-mode-hook #'veb/on-verilog-buffer)
(add-hook 'verilog-mode-hook    #'veb/on-verilog-buffer)

(provide 'verilog-eglot-bender)
;;; verilog-eglot-bender.el ends here
