;;;****************************************************
;;; No.23-4 返回多段线(*POLYLINE)的所有顶点坐标  函数  
;;;****************************************************
(defun ayGetPLineVTX (EntName1 / Obj1 vtx vtxlst PtsList i)
	(cond
		((= (cdr (assoc 0 (entget EntName1))) "LWPOLYLINE")
		 (setq PtsList (ayGetLWPolyLineVTX EntName1))
		);end_switch
		((= (cdr (assoc 0 (entget EntName1))) "POLYLINE")
		 (setq PtsList (ayGetPolyLineVTX EntName1))
		);end_switch
	);end_cond
	(setq PtsList PtsList)
);end_defun
