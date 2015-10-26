(ignore-errors
  (require-package 'erlang))

(when (package-installed-p 'erlang)
  (require 'erlang-start))

(add-to-list 'ac-modes 'erlang-mode)


;; (setq erlang-mode-hook
;;       (function (lambda ()
;;                   (setq indent-tabs-mode nil))))

;; (setq erlang-tab-always-indent nil)

(provide 'init-erlang)
