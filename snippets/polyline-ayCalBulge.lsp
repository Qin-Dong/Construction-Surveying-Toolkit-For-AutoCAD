;;;*********************************
;;; No.48 凸度计算函数              
;;; 返回表: (圆心坐标 半径 包含角)  
;;;*********************************
(defun ayCalBulge (sVertexPt eVertexPt xBulge / R cenAngle xAngle) 
	(setq xAngle (* 4 (atan xBulge))
				cenAngle ((if (< xBulge 0) - + ) (- (angle sVertexPt eVertexPt) (/ xAngle 2.0)) (/ PI 2))
				R (abs (/ (/ (distance sVertexPt eVertexPt) 2.0) (sin (/ xAngle 2.0))))
	);end_setq
	(list (polar sVertexPt cenAngle R) R (abs xAngle)) 
);end_defun
