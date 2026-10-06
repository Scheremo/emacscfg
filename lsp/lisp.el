(defun treesit-query-format-buffer ()
  "Pretty-print a Tree-sitter query buffer with (), [], {} block awareness,
and avoid breaking lines before `@capture` tokens."
  (interactive)
  (save-excursion
    ;; Normalize whitespace
    (goto-char (point-min))
    (while (re-search-forward "[\t ]+" nil t)
      (replace-match " "))

    ;; Add newline after closing delimiter if NOT followed by @capture
    (goto-char (point-min))
    (while (re-search-forward "\\([])}]\\)[ \t]*\\([^@\n]\\|$\\)" nil t)
      (replace-match "\\1\n\\2"))

    ;; Add newline before opening delimiter if not preceded by newline
    (goto-char (point-min))
    (while (re-search-forward "\\([^ \t\n]\\)[ \t]*\\([({[]\\)" nil t)
      (replace-match "\\1\n\\2"))

    ;; Clean up blank lines
    (goto-char (point-min))
    (delete-blank-lines)

    ;; Re-indent the result
    (indent-region (point-min) (point-max))

    (message "Tree-sitter query formatted.")))
(add-to-list 'auto-mode-alist '("\\.scm\\'" . scheme-mode))
(use-package aggressive-indent
  :hook (scheme-mode . aggressive-indent-mode))

(eval-after-load 'scheme
  '(define-key scheme-mode-map (kbd "C-c C-l") #'treesit-query-format-buffer))

;; Formatting is an explicit command: .scm also contains ordinary Scheme,
;; where whitespace inside strings must never be rewritten on save.
