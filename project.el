; project.el

(use-package compile
  :ensure nil
  :custom
  (compilation-read-command nil "Don't prompt every time.")
  (compilation-scroll-output 'first-error))

(use-package project
  :ensure nil
  :bind (("C-c k" . #'project-kill-buffers)
         ("C-c m" . #'project-compile)
         ("C-x f" . #'find-file)
         ("C-c F" . #'project-switch-project)
         ("C-c R" . #'pt/recentf-in-project)
         ("C-c f" . #'project-find-file))
  :custom
  ;; This is one of my favorite things: you can customize
  ;; the options shown upon switching projects.
  (project-switch-commands
   '((project-find-file "Find file")
     (magit-project-status "Magit" ?g)
     (project-find-regexp "Grep" ?h)
     (project-shell "Shell" ?t)
     (project-dired "Dired" ?d)
     (pt/recentf-in-project "Recently opened" ?r)))
  (compilation-always-kill t)
  (project-vc-merge-submodules nil)
  )

(defun pt/recentf-in-project ()
  "Visit a recent file belonging to the current project."
  (interactive)
  (require 'recentf)
  (let* ((root (project-root (project-current t)))
         (files (seq-filter
                 (lambda (file) (string-prefix-p root (expand-file-name file)))
                 recentf-list)))
    (unless files
      (user-error "No recent files in this project"))
    (find-file (completing-read "Recent project file: " files nil t))))
