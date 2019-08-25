(defun c:BAR ()
  (command "color" 7)
 
  (setq i 0)
  (setq judge 0)
  (grtext -1 "绘制标尺 覃东 按 F1 获取帮助")
  (princ "\n")
  (setq o (getpoint "请输入一点："))
  (princ "\n")
  (if (/= H nil)(progn (setq Tmp_H H)(setq PromptTmp (strcat "该点高程" "<" (rtos H 2 3) ">:")))(setq PromptTmp "该点高程："))
  (if (= (setq H (getreal PromptTmp)) nil)(setq H Tmp_H))

  (if (/= Tmp_Di nil)(setq PromptTmp (strcat "标尺距离" "<" (rtos Tmp_Di 2 3) ">:"))(setq PromptTmp "标尺距离："))
  (if (= (setq Di (getint PromptTmp)) nil)(setq Di Tmp_Di))
  (setq Tmp_Di Di)

  (if (/= Tmp_L nil)(setq PromptTmp (strcat "标尺长度" "<" (rtos Tmp_L 2 3) ">:"))(setq PromptTmp "标尺长度："))
  (if (= (setq L (getint PromptTmp)) nil)(setq L Tmp_L))
  (setq Tmp_L L)

  (if (/= Tmp_Sc nil)(setq PromptTmp (strcat "比例尺分母" "<" (rtos Tmp_Sc 2 3) ">:"))(setq PromptTmp "比例尺分母："))
  (if (= (setq Sc (getint PromptTmp)) nil)(setq Sc Tmp_Sc))
  (setq Tmp_Sc Sc)

  (setq sc (/ sc 100))
  (setq TtH (* (/ sc 10.0) 2.3))
 (command "-style" "BarSt" "宋体" TtH 1 0 "n" "n" "n")
  (princ)
  (if (< Di 0)
    (setq Di (* (- (fix (/ Di sc)) 1) sc))
    (setq Di (* (+ (fix (/ Di sc)) 1) sc))
  )
  (setq Lj Di)
  (setq intH  (atoi (setq iH (rtos H 2 0))))


  (setq dy (- H intH))
  (if (> dy 0)(setq n (- (cadr o) dy))(setq n (+ (cadr o) dy)))
  

  ;(setq n (cadr o))  
  (setq m (+ (car o) Di))
  (setq Posi (list m n))
;  (if (< Di 0)
;    (setq LevelBarStartPointX (+ m (/ sc 2.0)))
;    (setq LevelBarStartPointX (- m (* 0.2 sc)))
;  )
;  (setq LevelBarStartPoint (list LevelBarStartPointX n))
;  (setq LevelBarEndPointX (- (car o)  Di))
;  (setq LevelBarEndPoint (list LevelBarEndPointX n))
;  (command "line" LevelBarStartPoint LevelBarEndPoint "")

;  (setq Di (atoi (rtos Di 2 0)))

;  (setq j sc)
;  (if (< Di 0)
;    (setq Di (+ Di sc))
;    (setq Di (- Di sc))
;  )
;  (setq Di (abs Di))
;  (while (/= Di 0)
;    (setq RDX (+ (car o) j))
;      (setq RDY (- (cadr o) (abs dy)))
;      (setq RUY (+ n (* sc 0.1)))
;    (command "line" (list RDX RDY) (list RDX RUY) "")
;(if (= sc 1) 
;   (command "text"
;	     (list (- RDX 0.09) (+ RUY 0.05))
;	     "0"
;	     (rtos j 2 0)
;
;    )
;   (command "text"
;	     (list (- RDX 0.2) (+ RUY 0.05))
;	     "0"
;	     (rtos j 2 0)
;
;    )
;
;)
;
;    (setq LDX (- (car o) j))
;      (setq LDY (- (cadr o) (abs dy)))
;      (setq LUY (+ n (* sc 0.1)))
;
;    (command "line" (list LDX LDY) (list LDX LUY) "")
;(if (= sc 1)
;    (command "text"
;	     (list (- LDX 0.2) (+ LUY 0.05))
;	     "0"
;	     (strcat "-" (rtos j 2 0))
;    )
;
;    (command "text"
;	     (list (- LDX 0.5) (+ LUY 0.05))
;	     "0"
;	     (strcat "-" (rtos j 2 0))
;    )


;)
;    (setq j (+ j sc))
;    (setq Di (- Di sc))
;  )


;  (setq MDX (car o))
;  (setq MDY (- (cadr o) (abs dy)))
;  (setq MUY (+ n (* sc 0.1)))

;  (command "line" (list MDX MDY) (list MDX MUY) "")
;  (command "text" (list (- MDX 0.1) (+ MUY 0.05)) "0" "0")

;  (if (> Lj 0)
;    (progn
;
;      (setq LDX (- (car o) j))
;      (setq LDY (- (cadr o) (abs dy)))
;      (setq LUY (+ n (* sc 0.1)))

;      (command "line" (list LDX LDY) (list LDX LUY) "")
;(if (= sc 1)
;      (command "text"
;	       (list (- LDX 0.1) (+ LUY 0.05))
;	       "0"
;	       (strcat "-" (rtos j 2 0))
;      )
;      (command "text"
;	       (list (- LDX 0.5) (+ LUY 0.05))
;	       "0"
;	       (strcat "-" (rtos j 2 0))
;      )
;
;
;)
;    )
;    (progn
;
;      (setq RDX (+ (car o) j))
;      (setq RDY (- (cadr o) (abs dy)))
;      (setq RUY (+ n (* sc 0.1)))
;      (command "line" (list RDX RDY) (list RDX RUY) "")
;      (command "text"
;	       (list (- RDX (/ sc 5.0)) (+ RUY 0.05))
;	       "0"
;	       (rtos j 2 0)
;      )
;    )
;  )

  (setq Nubr (+ (atoi (rtos (/ L sc) 2 0)) 1))
  (command "setvar" "CMDECHO" "0")
  (princ)
  (if (> dy 0)(setq intH (+ intH sc)))
(if (< dy 0)(setq intH (+ (- intH 1) sc)))
  (if (= dy 0)(setq intH (+ intH sc)))
  
 (while (/= NuBr 0)

    (setq Ex (- m (/ sc 10.0)))

    (setq Fy (+ n (* i sc)))
    (setq Ey (+ n (* (+ i 1) sc)))

    (setq Fp (list m Fy))
    (setq Ep (list Ex Ey))

    (setq Lfp (list m Ey))
    (setq Lep (list (+ m (/ sc 10.0)) Ey))

    (setq Txtp (list (+ m (* (/ sc 10.0) 1.5)) Ey))

    (setq Hi (+ intH (* sc i)))

    (setq Loe (list (+ (/ sc 10.0) m) n))


    (command "rectangle" Fp Ep)

    (if	(= judge 0)
      (progn
	(command "hatch" "s" "l" "")
	(setq judge 1)
      )
      (setq judge 0)
    )

    (command "line" Lfp Lep "")

    (command "text" Txtp "0" Hi)

    (setq i (+ i 1))
    (setq Nubr (- Nubr 1))
  )
  (setq judge 0)
  (command "line" (list m n) loe "")
 
(if (> dy 0)(command "text" (list (+ m (/ sc 10.0)) n) "0"  (- intH sc))(command "text" (list (+ m (/ sc 10.0)) n) "0" (- intH sc)))
  (princ)
  (command "redraw")
  (command "setvar" "CMDECHO" "1")
  (grtext)
  (princ)

)

