;;; snippets.el --- Moritz Scherer's Emacs setup.  -*- lexical-binding: t; -*-

(defun scheremo/insert_author ()
  "Insert the author header snippet for the current mode."
  (interactive)
  (require 'yasnippet)
  (yas-expand-snippet (yas-lookup-snippet "author_header"))
  )


(use-package yasnippet
  :commands (yas-insert-snippet yas-expand-snippet yas-lookup-snippet)
  :hook ((prog-mode . yas-minor-mode)
         (text-mode . yas-minor-mode))
  :diminish yas-minor-mode
  :custom (yas-prompt-functions '(yas-completing-prompt)))
(eval-after-load 'yasnippet
  '(progn
     (define-key yas-keymap (kbd "TAB") 'yas-next-field)))
