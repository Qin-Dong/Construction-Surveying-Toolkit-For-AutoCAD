;函数：xd-modent 
;功能：同时修改多个属性的Entmod 
;函数代码：
(defun xd-modent (el tylst / c)
  (foreach n tylst
    (if (setq c (assoc (car n) el))
      (setq el (subst n c el))
      (setq el (append el (list n)))
    )
  )
  (entmod el)
)

;语法：
(xd-modent el tylst) 
;参数：el 实体dxf 表 tylst 新的值
;示例：
(xd-modent el '((8 . "1") (62 . 1))) 
