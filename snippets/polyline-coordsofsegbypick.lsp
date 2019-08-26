函数：coordsofsegbypick 
功能：返回多义线所点击子段的端点坐标 
函数代码：
(defun coordsofsegbypick (ename p)
  (setq obj (vlax-ename->vla-object ename)
         pp (vlax-curve-getclosestpointto obj (trans p 1 0))
          n (fix (vlax-curve-getparamatpoint obj pp)))
  (segcoord obj n)
)语法：
(coordsofsegbypick ename p) 
参数：
ename:图元名
p:点
返回值：
坐标列表
