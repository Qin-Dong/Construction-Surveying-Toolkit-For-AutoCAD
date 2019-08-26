函数：
getseg 
功能：
获取包含选定细多义线线段的两个端点列表 
函数代码：
(defun getseg (poly pt / pts i)
  (setq	pts (massoc 10 (entget poly))
	i   (caddar (ssnamex (ssget pt)))
  )
  (list	(nth (1- i) pts)
	(if (and (isclosed poly)
		 (= i (length pts))
	    ) 	  (car pts)
	  (nth i pts)
	)
  )
)语法：
(getseg poly pt ) 
参数：
细多义线的图元名称和所选定的点
返回值：
返回包含选定细多义线线段的两个端点列表
示例：
(apply 'getseg (entsel)) 
说明：
该程序不是很稳定，所以请进行严密的测试 
