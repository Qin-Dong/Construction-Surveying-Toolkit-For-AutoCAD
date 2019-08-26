函数：
isclosed 
功能：
检查细多义线是否闭合 
函数代码：
(defun isclosed (poly)
  (= 1 (logand 1 (cdr (assoc 70 (entget poly)))))
)

语法：
(isclosed poly) 
参数：
细多义线图元名称
返回值：
如果细多义线闭合，则返回T，否则返回nil。
示例：
(isclosed (car (entsel)))
