函数：
checkarc 
功能：
判断多段线是否有圆弧(凸度/=0)的子段 
函数代码：
(defun checkarc (ename)			
  (setq obj (vlax-ename->vla-object ename))
  (setq plist (vlax-safearray->list
		(vlax-variant-value
		  (vla-get-coordinates obj))))
  (setq n 0 bu nil)
  (repeat (/ (length plist) 2)
    (if (/= (vla-getbulge obj n) 0)
      (setq bu T)
     )
    (setq n (+ n 1))
  )
  bu
)语法：
(checkarc ename) 
参数：
ename:图元名
返回值：
T：有圆弧段
nil:无圆弧段
