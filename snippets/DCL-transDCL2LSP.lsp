;;;=================================================================*
;;;                                                                 *
;;; 功能：将dcl文件转换为一个lisp函数。                               *
;;;      函数的功能是，生成临时dcl，返回临时文件的路径全名。            *
;;;      摆脱dcl文件位置的束缚。                                      *
;;;                                                                 *
;;;=================================================================*
;;;测试
(defun c:tt () (c:dcl2lsp))
;;;=================================================================*
(defun c:dcl2lsp (/ filename_dcl lst_str filename_lsp f str)
  (and
    ;; 1、选择原始dcl文件
    (setq filename_dcl
	   (getfiled "选择dcl文件"
		     ""
		     "dcl"
		     4
	   )
    )
    ;; 2、生成lisp源码
    (setq lst_str (zl-dcl->lsp filename_dcl))
    ;; 3、输出lsp文件
    (setq filename_lsp (strcat filename_dcl ".lsp"))
    (setq f (open filename_lsp "w"))
    (foreach str lst_str (princ "\n" f) (princ str f))
    (not(close f))
    ;; 4、用记事本显示，lsp文件内容
    (startapp "notepad" filename_lsp)
  )
  (princ)
)
;;;=================================================================*
;;;      通用函数                                                   *
;;;功能：
;;;参数：filename_dcl  -----原始dcl文件名。                         *
;;;返回：字符串表。内容组合为lsp函数定义。                          *
(defun zl-dcl->lsp (filename_dcl / f lst_str0 lst_str1 lst_str2)

  ;; 1、读取dcl文件内容
  (if (setq f (open filename_dcl "r"))
    (progn
      (setq lst_str1 '())
      (while (setq str (read-line F))
	(setq str      (vl-prin1-to-string str)
	      lst_str1 (cons str lst_str1)
	)
      )
      (setq lst_str1 (reverse lst_str1))
      (close f)
    )
  )
  ;; 2、定义前缀
  (setq	lst_str0
	  (list
	    ";;;=================================================================*"
	    (strcat ";;;生成日期：" (rtos (getvar "cdate") 2 6))
	    ";;;本文件由程序自动生成。                                           *"
	    ";;;程序生成完成后需将主代码“*.lsp”文件中的语句中的     *"
	    ";;; (load_dialog 双引号*.Dcl双引号)改为(load_dialog (make-dcl)) 方可用            *"
	    ";;;修改后的代码可编辑到主LISP程序后方运行                                                                 *"
	    ";;;=================================================================*"
	    ";;;为能让多个有本程序生成的DCL.lsp可以同时使用，生成程序后应将对话框名 (make-dcl)改名   *"
	    ";;;供需修改两处地方，一处为加载的地方(load_dialog (？？？-make-dcl)) ，另一处为       *"
	    ";;;对话框主程序名(defun ？？？-make-dcl    ，一定要一致                      *"
	    ";;;示例：(make-dcl)                                                 *"
	    "(defun make-dcl  (/ lst_str str file f)"
	    "\t\t(setq lst_str '("
	  )
  )
  ;; 3、定义后缀
  (setq	lst_str2
	 '("		    )"
	   "    )"
	   "    (setq file (vl-filename-mktemp \"DclTemp.dcl\"))"
	   "    (setq f (open file \"w\"))"
	   "    (foreach str lst_str"
	   "	(princ \"\\n\" f)"
	   "	(princ str f)"
	   "    )"
	   "    (close f)"
	   "    ;;返回"
	   "    file"
	   ")"
	   ";;;=================================================================*"
	   "(princ)"
	  )
  )
  ;; 4、返回
  (append lst_str0 lst_str1 lst_str2)
)
