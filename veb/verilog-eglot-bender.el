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
;; Remote-safe helpers (TRAMP-friendly)

(defun veb--dir-as-directory (d)
  (file-name-as-directory (expand-file-name d)))

(defun veb--parent (d)
  "Return parent of D, preserving TRAMP prefixes.
Stops at the root when parent == d."
  (let* ((abs (expand-file-name d))
         (nd  (directory-file-name abs)))
    (file-name-directory nd)))

(defun veb--ancestor-parent-of-component (dir names)
  "If DIR is inside a directory whose basename is in NAMES
(e.g. …/working_dir/x → NAMES=(\"working_dir\")), return the parent
of that component. Works with TRAMP. Otherwise nil."
  (let ((cur (veb--dir-as-directory dir))
        (res nil))
    (while (and cur (not res))
      (let* ((base   (file-name-nondirectory (directory-file-name cur)))
             (parent (veb--parent cur)))
        (when (member base names)
          (setq res parent))
        ;; Stop at filesystem root (remote-safe): parent == cur.
        (setq cur (unless (equal parent cur) parent))))
    res))

(defun veb--localname (path)
  "Strip TRAMP prefix; return PATH as seen on the remote host (or unchanged)."
  (or (file-remote-p path 'localname) path))

(defun veb--project-root (&optional dir)
  "Return best-effort project root for DIR (or `default-directory`).

If inside a vendored tree like `working_dir/...` or `.bender/...`,
escape to the parent *of that component* first, then search for a .git
or project.el root from there. Remote/TRAMP paths are handled."
  (let* ((dir   (veb--dir-as-directory (or dir default-directory)))
         (_     (veb--log "Resolving project root from: %s (remote=%s)"
                          dir (file-remote-p dir)))
         (escape (veb--ancestor-parent-of-component
                  dir '("working_dir" ".bender")))
         (base  (or escape dir))
         ;; Prefer a .git at/above BASE (remote-safe).
         (git   (locate-dominating-file base ".git"))
         ;; Fall back to project.el on BASE.
         (proj  (ignore-errors (project-current nil base)))
         (root  (or git (and proj (project-root proj)) base)))
    (veb--log "Project root -> %s  (escaped=%s  git=%s  proj=%s)"
              (or root "<nil>")
              (and escape escape) (and git t) (and proj t))
    root))

(defun veb--filelist-path (&optional root)
  (let* ((r (or root (veb--project-root)))
         (p (expand-file-name veb-filelist-name r)))
    (veb--log "Filelist path for root %s -> %s" r p)
    p))

(defun veb--lsp-command (&optional _project)
  "Return list (PROGRAM ARGS...) for `eglot-server-programs`."
  (let* ((root   (veb--project-root))
         (fl     (veb--filelist-path root))
         (fl-arg (veb--localname fl))          ;; <- strip /ssh:…:
         (cmd    (list veb-lsp-executable))
         (args   (copy-sequence veb-lsp-extra-args))
         (argv   (append cmd args (when (file-exists-p fl)
                                    (list "--command-file" fl-arg)))))
    (veb--log "Computed LSP command:")
    (veb--log "  root: %s" root)
    (veb--log "  filelist: %s  (localname=%s  exists=%s)"
              fl fl-arg (file-exists-p fl))
    (veb--log "  final argv: %S" argv)
    argv))

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
