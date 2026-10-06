;;; keybindings.el --- Moritz Scherer's emacs setup

(global-unset-key (kbd "M-u"))

;; Multi-Cursor keybinds
(global-set-key (kbd "C-,") 'mc/mark-next-like-this)
(global-set-key (kbd "C-.") 'mc/mark-previous-like-this)

;; Magit keybinds
(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C-c b") 'smerge-keep-all)
(global-set-key (kbd "C-c M") 'smerge-keep-mine)
(global-set-key (kbd "C-c n") 'smerge-next)
(global-set-key (kbd "C-c p") 'smerge-previous)
(global-set-key (kbd "C-c o") 'smerge-keep-other)

(bind-key "C-s" #'isearch-forward-regexp)
(bind-key "C-c s" #'isearch-forward-symbol)


(defun pt/indent-just-yanked ()
  "Re-indent whatever you just yanked appropriately."
  (interactive)
  (exchange-point-and-mark)
  (indent-region (region-beginning) (region-end))
  (deactivate-mark))

(bind-key "C-c I" #'pt/indent-just-yanked)

;; YAS keybinds
(global-set-key (kbd "C-x C-a") 'scheremo/insert_author)
(global-set-key (kbd "C-x y") 'yas-insert-snippet)

(global-set-key (kbd "C-<tab>") 'completion-at-point)
(global-set-key (kbd "s-u") 'revert-buffer)
