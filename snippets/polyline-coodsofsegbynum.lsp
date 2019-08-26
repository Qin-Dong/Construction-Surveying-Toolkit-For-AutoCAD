;函数：coodsofsegbynum 
;功能：返回多段线第n子段的端点坐标 
;函数代码：
(defun coodsofsegbynum (obj n)		
  (vlax-safearray->list
              (vlax-variant-value
                (vla-get-coordinate obj 2)))
)
;语法：
(coodsofsegbynum obj n) 
;参数：obj:图元名 n:代表子段位置的整数
;返回值：坐标列表 
