;函数：coodsofnumpoint 
;功能：返回多段线第n点的坐标 
;函数代码：
(defun coodsofnumpoint (ename n)
  (setq obj (vlax-ename->vla-object ename))
  (setq plist (vlax-safearray->list
		(vlax-variant-value
		  (vla-get-coordinates obj))))
  (list (nth (* n 2) plist)(nth (1+ (* n 2)) plist))
)
;语法：
(coodsofnumpoint ename n) 
;参数：ename:图元名 n:点位置
;返回值：坐标列表
