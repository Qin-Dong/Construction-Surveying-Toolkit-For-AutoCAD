;函数：ax:Centroid 
;功能：返回闭合多义线的质心 
;函数代码：
(defun ax:Centroid (poly / pl ms va reg cen)
  (setq	pl (vlax-ename->vla-object poly)
	ms (vla-get-modelspace
	     (vla-get-activedocument (vlax-get-acad-object))
	   )
	va (vlax-make-safearray vlax-vbObject '(0 . 0))
  )
  (vlax-safearray-put-element va 0 pl)
  (setq	reg (car (vlax-safearray->list
		   (vlax-variant-value (vla-addregion ms va))
		 )
	    )
	cen (vla-get-centroid reg)
  )
  (vla-delete reg)
  (vlax-safearray->list (vlax-variant-value cen))
)

;语法：
(ax:Centroid poly) 
;参数：一个闭合的平面的多义线的图元名称
;示例：
(ax:Centroid (car (entsel)))
