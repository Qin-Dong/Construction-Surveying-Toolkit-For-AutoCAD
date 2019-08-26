;函数：rv 
;功能：采用一个点或由0开始的索引数从细多义线上移去选定的顶点 
;函数代码：
(defun rv (e pt)
  (if (= (cdr (assoc 0 (setq e (entget e)))) "LWPOLYLINE")
    (progn
      (setvar "cmdecho" 0)
      (command "undo" "begin")
      (entmod
	(drop e
	      (cons 10
		    (if	(= (type pt) 'LIST)
		      (progn
			(setq pt (trans pt 1 (cdr (assoc -1 e))))
			(list (car pt) (cadr pt))
		      )
		      (nth pt (massoc 10 e))
		    )
	      )
	)
      )
      (command "undo" "end")
      (setvar "cmdecho" 1)
      (princ)
    )
  )
)
;语法：
(rv e pt) 
;参数：一个细多义线的图元名称和一个点或一个代表所要移去的顶点的索引号
;返回值：
(rv (car (entsel)) 2) ;<- 在此使用的是索引号
;说明：;如果选定的点不是顶点，在多义线上不会产生改动，RV也不会产生错误。
;AutoCAD 2000用户可用本地的vl-remove函数替换drop函数。
