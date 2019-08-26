;函数：numbersofseg 
;功能：返回多段线子段的数量 
;函数代码：
(defun numbersofseg (ename)		
  (setq obj (vlax-ename->vla-object ename))
  (setq plist (vlax-safearray->list
		(vlax-variant-value
		  (vla-get-coordinates obj))))
  (1- (/ (length plist) 2))
)
;语法：(numbersofseg ename) 
;参数：ename:图元名
;返回值：子段数量的整数
