;;----------------------------------------------------------------------------
;; 界面上的通用配置
;;----------------------------------------------------------------------------

(setq-default cursor-type 'bar)
;; 去掉蜂鸣,否则会显示难看的疤痕
(setq ring-bell-function 'ignore)

;; 按DEl键删除选中区域
(delete-selection-mode 1)

;;按键映射修改，对Meta和Command交换.
(setq mac-option-modifier 'meta)
(setq mac-command-modifier 'super)


;;----------------------------------------------------------------------------
;; 让你的上下左右键可以用来切换窗口
;;----------------------------------------------------------------------------

(when (fboundp 'winner-mode)
  (winner-mode 1))
(windmove-default-keybindings)
(global-set-key (kbd "<f7> ") 'winner-undo)


;;----------------------------------------------------------------------------
;; Grep
;;----------------------------------------------------------------------------

(require 'grep)
(grep-apply-setting 'grep-command  "grep -nr ")



;;----------------------------------------------------------------------------
;; org-mode
;;----------------------------------------------------------------------------

(setq org-agenda-files (list "~/project/gtd/"
                             "~/project/gtd/Daily"
                             ))

;;显示不够的内容。自动换行.
(add-hook 'org-mode-hook (lambda () (setq truncate-lines nil)))


;;org-mode 和winner模式冲突.
(add-hook 'org-shiftup-final-hook 'windmove-up)
(add-hook 'org-shiftleft-final-hook 'windmove-left)
(add-hook 'org-shiftdown-final-hook 'windmove-down)
(add-hook 'org-shiftright-final-hook 'windmove-right)


;;----------------------------------------------------------------------------
;; c/c++
;;----------------------------------------------------------------------------

(defun my-c-mode-common-hook ()
  (setq c-basic-offset 4)
  (c-set-offset 'substatement-open 0))

(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)
(setq-default c-basic-offset 4)

(setq semantic-default-submodes '(global-semantic-mru-bookmark-mode
                                  global-semanticdb-minor-mode
                                  global-semantic-idle-scheduler-mode
                                  global-semantic-idle-summary-mode
                                  global-semantic-decoration-mode
                                  global-semantic-tag-folding-mode
                                  ;;global-semantic-idle-tag-highlight-mode
                                  global-semantic-mru-bookmark-mode
                                  global-semantic-stickyfunc-mode))
(semantic-mode 1)


;; 导入高级的名字补全，信息显示等
(require 'semantic/ia)

;; 使用semantic的跳入和跳出功能
(defadvice push-mark (around semantic-mru-bookmark activate)
  (semantic-mrub-push semantic-mru-bookmark-ring (point) 'mark)
  ad-do-it)

(setq zyk-include-path-list (list "/usr/local/include/python2.7"
                                  "/Users/zhuoyikang/Source/games/T4/cocos2d/cocos"
                                  "/Users/zhuoyikang/Source/games/T4/cocos2d/cocos/ui"
                                  "/Users/zhuoyikang/Source/games/T4/cocos2d/cocos/editor-support/"
                                  "/Users/zhuoyikang/Source/games/T4/cocos2d/external/glfw3/include/mac"
                                  "/Users/zhuoyikang/Source/games/T4/cocos2d/external/"
                                  "../"
                                  ))


(defun semantic-ia-fast-jump-back ()
  (interactive)
  (if (ring-empty-p (oref semantic-mru-bookmark-ring ring))
      (error "Semantic Bookmark ring is currently empty"))
  (let* ((ring (oref semantic-mru-bookmark-ring ring))
         (alist (semantic-mrub-ring-to-assoc-list ring))
         (first (cdr (car alist))))
    (if (semantic-equivalent-tag-p (oref first tag) (semantic-current-tag))
        (setq first (cdr (car (cdr alist)))))
    (semantic-mrub-switch-tags first)))


;; 绑定到按键.
(global-set-key (kbd "M-m")  'semantic-ia-fast-jump)
;;(global-set-key (kbd "M-n")  'semantic-ia-fast-jump-back)
(global-set-key (kbd "M-0")  'semantic-ia-fast-jump)
(global-set-key (kbd "M-1")  'semantic-ia-fast-jump-back)


;; 增加自定义的搜索路径
(defun my-semantic-hook ()
  (let ((list-directory zyk-include-path-list))
    (while (car list-directory)
      (progn
        (semantic-add-system-include (car list-directory) 'c-mode)
        (semantic-add-system-include (car list-directory) 'c++-mode)
        )
      (setq list-directory (cdr list-directory)))
    ))

(add-hook 'semantic-init-hooks 'my-semantic-hook)



;;----------------------------------------------------------------------------
;; 习惯按键设置
;;----------------------------------------------------------------------------

;;(global-set-key (kbd "C-f") 'set-mark-command)
(global-set-key (kbd "C-c C-c")   'comment-region)
(global-set-key (kbd "C-c C-u")   'uncomment-region)
(global-set-key (kbd "C-q") 'backward-kill-word)
(global-set-key (kbd "C-c C-f") 'goto-line)
(global-set-key (kbd "M-<left>") 'previous-buffer)
(global-set-key (kbd "M-<right>") 'next-buffer)
(global-set-key (kbd "M-|") 'indent-region)
(global-set-key (kbd "M-h") 'mark-paragraph)
(global-set-key (kbd "C-x C-k") 'kill-region)
(global-set-key (kbd "C-c C-f") 'goto-line)


(global-set-key (kbd "C-<up>") 'backward-paragraph)
(global-set-key (kbd "C-<down>") 'forward-paragraph)


;;----------------------------------------------------------------------------
;; erlang配置
;;----------------------------------------------------------------------------


(add-to-list 'load-path "/usr/local/share/distel/elisp")
(require 'distel)
(distel-setup)

(setq erlang-indent-level 2)

;; erlang模式设置
(add-hook 'erlang-mode-hook
          (lambda ()
            (modify-syntax-entry ?_ "w") ;;让this_good连成一个单词处理
            (setq-default indent-tabs-mode nil)))

(setq derl-cookie "abc")
(setq erl-nodename-cache 'develop@127.0.0.1)
(setq flycheck-erlang-include-path  (list "../include" "../../include"  "../../../include"))



;;----------------------------------------------------------------------------
;; 其他一些自定义配置
;;----------------------------------------------------------------------------

(defun gentoo()
  (interactive)
  (find-file-existing "/root@127.0.0.1#4022:/root/"))


(provide 'init-zyk)
