(defun C:zzExport2Dat ()
  (vl-load-com)
  (setq	filename
	 (getfiled "保存为..."
		   (getvar "dwgprefix")
		   "dat;csv"
		   1
	 )
  )
;;;选择所有地形点图块
  (SETQ	SS (ssget "x"
		  (list
		    '(0 . "INSERT")
		    (cons 8 "gcd")
		  )
	   )
  )
;;;提取块坐标即地形点坐标
  (if (and (/= ss nil) (/= filename nil)) ;if1
    (progn				;progn1
      (setq fileId (open filename "w"))
      (setq i 0)
      (repeat (sslength ss)
	(setq ssn (ssname ss i))
	(setq endata (assoc '10 (entget ssn)))
	(if (/= endata nil)
	  (progn
	    ;;读取颜色值
	    (setq pcolor (cdr (assoc '62 (entget ssn))))
	    (if	(/= pcolor nil)
	      (setq pcolor_str (strcat "co" (itoa pcolor)))
	      (setq pcolor_str "")
	    )
	    (setq pxyz (cdr endata))
	    (setq px (car pxyz))
	    (setq py (cadr pxyz))
	    (setq pz (caddr pxyz))
	    (setq pxyz_str (strcat (itoa (+ i 1))
				   ","
				   pcolor_str
				   ","
				   (rtos px 2 3)
				   ","
				   (rtos py 2 3)
				   ","
				   (rtos pz 2 3)
			   )
	    )
	    (write-line pxyz_str fileId)
	  )
	)
	(setq i (1+ i))
      )
      (setq fileId (close fileId))
      (princ (strcat "共导出"
		     (itoa i)
		     "个CASS地形点！ (C)覃东 201709 QQ:61902475"
	     )
      )
    )					;end progn1
    (princ
      "图中没有找到CASS地形点或未输入文件名！(C)覃东 201709 QQ:61902475"
    )
  )					;end if1
  (princ)
)
;;;CASS地形图中点对象属性
;;;((-1 . <图元名: 7ffffb491b0>) (0 . "INSERT") (330 . <图元名: 7ffffb6d980>) (5 . "9A4AB") (100 . "AcDbEntity")
;;(67 . 0) (410 . "Model") (8 . "GCD") (6 . "Continuous") (100 . "AcDbBlockReference") (66 . 1) (2 . "GC200")
;;(10 269544.0 3.74404e+006 2713.01) (41 . 0.5) (42 . 0.5) (43 . 0.5) (50 . 0.0) (70 . 0) (71 . 0) (44 . 0.0)
;;(45 . 0.0) (210 0.0 0.0 1.0))