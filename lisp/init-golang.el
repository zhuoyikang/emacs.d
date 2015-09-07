(require-package 'go-mode)
(require-package 'go-autocomplete)

(require 'go-autocomplete)
(require 'auto-complete-config)
(ac-config-default)


(setenv "GOROOT" "/usr/local/go")
(setenv "GOPATH" "/Users/zhuoyikang/go:/Users/zhuoyikang/Source/pf/chat/_vendor:./")

(provide 'init-golang)
