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
;; rust
;;----------------------------------------------------------------------------


(require-package 'rust-mode)
(require-package 'toml-mode)
(require-package 'protobuf-mode)



;;----------------------------------------------------------------------------
;; org-mode
;;----------------------------------------------------------------------------

(setq org-agenda-files (list "~/project/gtd/"
                             "~/project/gtd/Daily"
                             ))

;; color by the code lang.
(setq org-src-fontify-natively t)


;;显示不够的内容。自动换行.
(add-hook 'org-mode-hook (lambda () (setq truncate-lines nil)))


;;org-mode 和winner模式冲突.
(add-hook 'org-shiftup-final-hook 'windmove-up)
(add-hook 'org-shiftleft-final-hook 'windmove-left)
(add-hook 'org-shiftdown-final-hook 'windmove-down)
(add-hook 'org-shiftright-final-hook 'windmove-right)


;; 归档已完成的任务
(defun my-org-archive-done-tasks ()
  (interactive)
  (org-map-entries 'org-archive-subtree "/DONE" 'file))


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

(setq lua-indent-level 4)


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
(global-set-key (kbd "C-c u")   'uncomment-region)
(global-set-key (kbd "C-q") 'backward-kill-word)
(global-set-key (kbd "C-c C-f") 'goto-line)
(global-set-key (kbd "M-<left>") 'previous-buffer)
(global-set-key (kbd "M-<right>") 'next-buffer)
(global-set-key (kbd "M-[") 'previous-buffer)
(global-set-key (kbd "M-]") 'next-buffer)
(global-set-key (kbd "M-|") 'indent-region)
(global-set-key (kbd "M-h") 'mark-paragraph)

;; 多个windows窗口切换.
(global-set-key (kbd "s-[") 'ns-prev-frame)
(global-set-key (kbd "s-]") 'ns-next-frame)

(global-set-key (kbd "C-x C-k") 'kill-region)
(global-set-key (kbd "C-c C-f") 'goto-line)


(global-set-key (kbd "C-<up>") 'backward-paragraph)
(global-set-key (kbd "C-<down>") 'forward-paragraph)

(global-set-key (kbd "C-c v") 'ff-find-other-file)


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
;; erlang specific
(setq flycheck-erlang-include-path  (list "../include"
                                          "../../include"
                                          "../../../include"
                                          "/Users/zhuoyikang/Source/pf/ejabberd-contrib/deps_ejabberd/include"
                                          "/lib/ejabberd/include"
                                          "/Users/zhuoyikang/Source/pf/ejabberd-contrib/deps_common_pf/include"
                                          "/Users/zhuoyikang/Source/pf/ejabberd-contrib/deps_eredis/include"
                                          "/Users/zhuoyikang/Source/pf/ejabberd-contrib/deps_erlcloud/deps/lhttpc/include"
                                          "/Users/zhuoyikang/Source/pf/ejabberd-contrib/deps_erlcloud/deps/meck/test/include"
                                          "/Users/zhuoyikang/Source/pf/ejabberd-contrib/deps_erlcloud/include"
                                          "/Users/zhuoyikang/Source/pf/ejabberd-contrib/deps_erlzk/include"
                                          "/Users/zhuoyikang/Source/pf/ejabberd-contrib/mod_pf_admin/include"
                                          "/Users/zhuoyikang/Source/pf/ejabberd-contrib/mod_pf_admin_muc/include"
                                          "/Users/zhuoyikang/Source/pf/ejabberd-contrib/mod_pf_user_mute/include"
                                          ))

(setq flycheck-erlang-library-path (list "ebin" "../ebin"  "../../ebin"  "../../../ebin"  "/lib/ejabberd/ebin/"))


(defvar rebar-directory "gl_online"
  "匹配这个正则表达式的路径将会认为是工作文件夹,工作文件夹下的erlang文件被保存时触发自动编译.")

;; 打印函数
(setq rebar-directory  (list "gl_online"
                             "galaxy-empire-hub-2"
                             "gl_999"
                             "6"
                             "5"))

(defun rebar-compile-source ()
  "编译当前缓冲区的文件，在文件保存后执行."
  (let ((buffer-name (buffer-file-name (current-buffer)))
        (match-pos nil)(full-path nil)
        (compile-cmd nil)(list-directory rebar-directory)
        (match-directory nil))
    (if (string-equal "erl" (file-name-extension buffer-name))
        (progn
          (while (and  (car list-directory)  (equal nil match-directory))
            (if (string-match (car list-directory) buffer-name)
                (setq  match-directory (car list-directory)))
            (setq list-directory (cdr list-directory)))
          (if (and match-directory (string-match match-directory buffer-name))
              (progn
                (setq match-pos (string-match match-directory buffer-name))
                (setq full-path (substring buffer-name 0 match-pos))
                (setq compile-cmd  (concat  "erlc " "-o " full-path
                                            match-directory "/ebin/" " -Wall "
                                            "-I" full-path match-directory "/include "
                                            "-pa" full-path match-directory "/ebin "
                                            buffer-name
                                            ))
                (shell-command compile-cmd "*Messages")
                ;;(message compile-cmd)
                )
            )
          )
      )
    )
  )


(add-hook 'after-save-hook 'rebar-compile-source)



;;----------------------------------------------------------------------------
;; 其他一些自定义配置
;;----------------------------------------------------------------------------

(defun gentoo()
  (interactive)
  (find-file-existing "/root@127.0.0.1#4022:/root/"))

(defun gendis1()
  (interactive)
  (find-file-existing "/root@192.168.56.3:/root/"))




;;----------------------------------------------------------------------------
;; 交换两个窗口的内容
;;----------------------------------------------------------------------------

(defun swap-buffers-in-windows ()
  "Put the buffer from the selected window in next window, and vice versa."
  (interactive)
  (let* ((this (selected-window))
         (other (next-window))
         (this-buffer (window-buffer this))
         (other-buffer (window-buffer other)))
    (set-window-buffer other this-buffer)
    (set-window-buffer this other-buffer)
    )
  )


;; 打开api文件
(defun api()
  (interactive)
  (find-file-existing "~/Project/GLD/gl_online/api/api.txt"))


;; 打开api文件
(defun pro()
  (interactive)
  (find-file-existing "~/Project/GLD/gl_online/api/protocal.txt"))




(defun etags-create-current()
  "Create etags"
  (interactive)
  (message (shell-command-to-string "find . -name \"*.[chCHp]*\" -print | etags -"  )))

(provide 'init-zyk)
