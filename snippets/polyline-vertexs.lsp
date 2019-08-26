;函数：vertexs 
;功能：返回多段线的各顶点 
;函数代码：
(defun vertexs (ename / plist pp n)        
  (setq obj (vlax-ename->vla-object ename))
  (setq plist (vlax-safearray->list
		(vlax-variant-value
		  (vla-get-coordinates obj))))
  (setq n 0)
  (repeat (/ (length plist) 2)
    (setq pp (append pp (list (list (nth n plist)(nth (1+ n) plist)))))
    (setq n (+ n 2))
  )
  pp
)
;语法：(vertexs ename) 
;参数：ename:图元名
;返回值：各顶点形成的列表
