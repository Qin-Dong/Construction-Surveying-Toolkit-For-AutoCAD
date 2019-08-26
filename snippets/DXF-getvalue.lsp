 ;;;取组码值
  (defun dxf (ent i)
    (cond ((= (type ent) 'ename)
	    (cdr (assoc i (entget ent '("*"))))
	     )
	  ((= (type ent) 'list)
	   (cdr (assoc i ent))
	   )
    ) ;_ if
  )
