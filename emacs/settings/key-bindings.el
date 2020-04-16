(global-set-key (kbd "C-.") 'hippie-expand-no-case-fold)
(global-set-key (kbd "C-:") 'hippie-expand-lines)
(global-set-key (kbd "C-,") 'completion-at-point)

(global-set-key [f8] 'neotree-toggle)

;; Use right option key as ALT instead of Meta
;;(setq ns-right-alternate-modifier nil)

(when (eq system-type 'darwin)
  (setq mac-right-option-modifier 'none)
  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier nil))

(provide 'key-bindings)
