;;;---------------------------------------------
;;; No.23-4-2 获取 POLYLINE 对象所有顶点坐标    
;;;---------------------------------------------
(Defun ayGetPolyLineVTX (LwPolyEntName / entData1 entName1 pel ptp wpl wpll plp par ct
													             pen rl pn clk pt al gx bj np xc gg rr cp retList)
	(setq entName1 LwPolyEntName)
	(setq retList nil)
	(setq entData1 (entget entName1))
	(if (= "POLYLINE" (Cdr (Assoc 0 entData1)))
		(progn
			(setq pel  entData1             ;取出对象表.
			      ptp  (Cdr (Assoc 70 pel)) ;取出结束片段型.
			      wpl  '()                  ;自建的点位数表.
						wpll '()
						entName1 (EntNext entName1)
						pen entName1
			);end_setq
			(While (/= "SEQEND" (Cdr (Assoc 0 (entget pen))));如果没束.
				(setq pel (entget pen)               ;取得顶点对象数据表.
			        plp (Cdr (Assoc 10 pel))       ;取出控制点点位.
			        par (Cdr (Assoc 42 pel))       ;取出弓弦比.
			        wpl (Cons (List plp par) wpl)  ;将数据加到WPL表中.
							wpll (cons plp wpll)
				);end_setq 
			  (setq pen (EntNext pen));搜索下一个对象.
			);end_while
			(setq wpll (Reverse wpll))

			;以下代码暂时没有用！
			(setq ct (If (= 0 (Cadr (Car wpl))) "直线片段封闭" "弧片段封闭")) 
			(setq wpl (Cons (Last wpl) wpl);加入封闭点.
				    wpl (Reverse wpl)        ;整理WPL表.
			      rl (Length wpl) 
			      pn 0 
			);end_setq 
			(setq clk (If (Or (= 0 ptp) (= 128 ptp)) "开口" "封闭"))  
			(Repeat (1- rl)          ;逐点分析.
			  (setq al (Nth pn wpl)  ;取出点数据表.
			        pt (Car al)      ;取出点位.
			  );end_setq 
			  (If (And (/= 0.0 (Cadr al)) (Nth pn wpl)) ;如果是断.
			 	  (Progn (setq gx (Cadr al)               ;取出弓比.
			                 bj (* (ATAN (ABS gx)) 4)   ;计算包角.
			                 np (Car (Nth (1+ pn) wpl)) ;取出下一点位.
			                 xc (* 0.5 (Distance pt np));半弦长计算.
			                 gg (* gx xc)               ;弓高计算.
			                 rr (/ (+ (* xc xc)(* gg gg)) (* 2 gg)) 
			            );end_setq  
			            (setq cp (Polar pt (setq pa (Angle pt np)) xc)  
			                  cp (Polar cp (+ pa (* 0.5 PI)) (- rr gg)) 
			            );end_setq 
			    );end_progn 
			  );end_if
		    (setq pn (1+ pn))
			);end_repeat
			
			(setq retList wpll)
		);end_progn
	);end_if
);end_defun
