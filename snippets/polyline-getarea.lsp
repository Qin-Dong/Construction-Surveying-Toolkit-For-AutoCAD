函数：getarea 
功能：返回多义线的面积 
函数代码：
(defun getarea (ename)	
  (setq obj (vlax-ename->vla-object ename))
  (vla-get-area obj)
)语法：
(getarea ename) 
参数：ename:图元名
返回值：面积
