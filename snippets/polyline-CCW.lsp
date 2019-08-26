函数：
xl:PlineCCW 
功能：
判断多义线或者坐标例表是否逆时针 
函数代码：
(defun xl:PlineCCW (pline /)
  (if (= (type pline) 'LIST)
    (PlineCCW_list pline)  ;如果pline为坐标例表，则调用LIST处理函数
    (PlineCCW_obj pline)  ;否则调用obj处理函数
  )
)

;;;;;主程序
(defun xl:GE_WhatPoly (ptlist / nverts cnt area tmp pt1 pt2)
  (setq
    cnt    0
    nverts (length ptlist)
    area   0.0
  )

  (while (< cnt (1- nverts))
    (setq
      pt1  (nth cnt ptlist)
      pt2  (nth (1+ cnt) ptlist)
      area (+ area (* (cadr pt1) (car pt2)))
      cnt  (1+ cnt)
    )
  )

  (setq
    pt1  (nth (1- nverts) ptlist)
    pt2  (nth 0 ptlist)
    area (+ area (* (cadr pt1) (car pt2)))
    cnt  0
    tmp  0.0
  )

  (while (< cnt (1- nverts))
    (setq
      pt1 (nth cnt ptlist)
      pt2 (nth (1+ cnt) ptlist)
      tmp (+ tmp (* (cadr pt2) (car pt1)))
      cnt (1+ cnt)
    )
  )

  (setq
    pt1  (nth 0 ptlist)
    pt2  (nth (1- nverts) ptlist)
    tmp  (+ tmp (* (cadr pt1) (car pt2)))
    area (* 0.5 (- area tmp))
  )

  (cond
    ((< area 0.0) (setq area t))
    ((> area 0.0) (setq area nil))
    (T (setq area nil))
  )
  area
)
;;;;;图元名的处理函数
(defun PlineCCW_obj
      (pline /    type_pline obj
       step lenparam    pt       plist
       starpt endpt　endparam       len
      )
  　
  ;;把曲线模拟成多折线
  (setq type_pline (cdr (assoc 0 (entget pline))))
  (cond ((OR (= type_pline "POLYLINE") (= type_pline "LWPOLYLINE"))
  (SETQ PLIST (XL:COORDS PLINE));;（xl:coords pline）为取得多义线顶点表函数，可以用别的函数替换
 )
  (T
  (setq obj (vlax-ename->vla-object pline)
        starpt (vlax-curve-getstartpoint obj)
        endpt (vlax-curve-getendpoint obj)
        endparam (vlax-curve-getEndParam pline)
        len      (vlax-curve-getDistAtParam obj endparam)
        step (/ len 100)
        lenparam 0
        plist (list starpt)
  )
  (while (< (setq lenparam (+ lenparam step)) len)
    (setq pt (vlax-curve-getPointAtdist obj lenparam))
    (if pt
      (setq plist (append plist (list pt)))
    )
  )
  (if (not (vlax-curve-isClosed obj))
    (setq plist (append plist (list endpt)))
  )
 )
  )
  (xl:GE_WhatPoly plist)
)
;;;;;;坐标例表的处理函数
(defun plineccw_list (plist / temp)
  (if (> (length plist) 2)
    (setq temp (xl:GE_WhatPoly plist))
  )
  temp
)语法：
(xl:PlineCCW pline) 
参数：
pline:多义线图元名 或者坐标例表如((x1 y1 z1) (x2 y2 z2))\((x1 y1) (x2 y2))
返回值：
返回值:T OR nil
T:逆时针
NIL:顺时针
