;函数：xl:PutXData 
;功能：把扩展数据附着到ACAD图元上 
;函数代码：
(defun xl:PutXData (Obj Data / dxf n i data_i temp1 appid)
  (setq dxf  (entget obj)
   N      (LENgth data)
 i      0
 data_i '(-3)
  )
  (repeat n
    (setq temp1  (nth i data)
   appid (car temp1)
   data_i (append data_i (list temp1))
   i  (1+ i)
    )
    (regapp appid)
    (entmod (append dxf (list data_i)))
  )

)
;语法：
(ax:PUTXData Obj DATA) 
;参数：obj:图元名 XDATA:扩展数据
;如：
(("south" (1000 . "204201") (1040 . 1.0))
      ("AAAA" (1041 . 562.307)  (1000 . "aaaaa"))
      ("BBBB" (1000 . "bbbbbbb"))
      ("CCCC" (1041 . 752.569))
)
;示例：
(xl:putXData myVlaObj '(("south" (1000 . "204201") (1040 . 1.0))))
;说明：对于图元上已经有的相同注册程序名的XDATA数据会覆盖
