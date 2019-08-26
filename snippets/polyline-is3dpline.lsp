函数：
is3dpline 
功能：
判断多段线是否三维多段线 
函数代码：
(defun is3dpline (ename)		
  (setq obj (vlax-ename->vla-object ename))
  (if (= (vla-get-objectname obj) "AcDb3dPolyline")
    (setq 3d T)
    (setq 3d nil)
  )
  3d
)语法：
(is3dpline ename) 
参数：
ename:图元名
返回值：
T：为三维多段线
nil：不是三维多段线
