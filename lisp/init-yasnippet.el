;;----------------------------------------------------------------------------
;; yasnippet
;;----------------------------------------------------------------------------


(require-package 'yasnippet)
(yas-global-mode 1)

(add-hook 'org-mode-hook
          '(lambda ()
             (make-variable-buffer-local 'yas/trigger-key)
             (setq yas/trigger-key [tab])))

(provide 'init-yasnippet)
