;;;-----------------------------------------------
;;; No.23-4-1 获取 LWPOLYLINE 对象所有顶点坐标    
;;;-----------------------------------------------
(defun ayGetLWPolyLineVTX (EntName1 / Obj1 vtx vtxlst PtsList i)
  (vl-load-com)
	(setq Obj1 (vlax-ename->vla-object EntName1))
	(setq vtx (vla-get-Coordinates Obj1))
	(setq vtxLst (vlax-safearray->list (vlax-variant-value vtx)))
	(setq i 0)
	(setq PtsList nil)
	(repeat (/ (length vtxLst) 2)
		(setq PtsList (append PtsList (list (list (nth i vtxLst) (nth (1+ i) vtxLst)))))
		(setq i (+ i 2))
	);end_repeat
	(setq PtsList PtsList)
);end_defun
