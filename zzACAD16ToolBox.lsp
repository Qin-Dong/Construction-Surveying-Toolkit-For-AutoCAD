;;---------------------=={ Area Label }==---------------------;;
;;                                                            ;;
;;  Allows the user to label picked areas or objects and      ;;
;;  either display the area in an ACAD Table (if available),  ;;
;;  optionally using fields to link area numbers and objects; ;;
;;  or write it to file.                                      ;;
;;------------------------------------------------------------;;
;;  Author: Lee Mac, Copyright ?2011 - www.lee-mac.com       ;;
;;------------------------------------------------------------;;
;;  Version 1.9    -    29-10-2011                            ;;
;;------------------------------------------------------------;;

(defun c:zzArea nil (AreaLabel 3))
(defun c:zzA nil (AreaLabel 3))
;; Areas to Text
(defun c:zzArea2Table nil (AreaLabel 1))
;; Areas to Table
(defun c:zzArea2File nil (AreaLabel 2))
					;点抽稀VBA宏,将VBA宏命名为acad.dvb并放到AutoCAD安装目录下自动加载
(defun c:zzDcx ()
  (command "_-VBARUN" "vba_zzDcx")
)
;; Areas to File
;;------------------------------------------------------------;;

(defun AreaLabel (flag	   /	    *error*  _startundo	       _endundo
		  _centroid	    _text    _open    _select
		  _getobjectid	    _isannotative     acdoc    acspc
		  ap	   ar	    as	     cf	      cm       el
		  fd	   fl	    fo	     n	      of       om
		  p1	   pf	    pt	     sf	      st       t1
		  t2	   tb	    th	     ts	      tx       ucsxang
		  ucszdir
		 )

  ;;------------------------------------------------------------;;
  ;;                         Adjustments                        ;;
  ;;------------------------------------------------------------;;

  (setq	h1 "断 面 面 积 统 计 表"
	;; Heading
	t1 "序号"
	;; Number Title
	t2 "面积"
	;; Area Title
	pf ""
	;; Number Prefix (optional, "" if none)
	sf ""
	;; Number Suffix (optional, "" if none)
	ap ""
	;; Area Prefix (optional, "" if none)
	as ""
	;; Area Suffix (optional, "" if none)
	cf 1.0
	;; Area Conversion Factor (e.g. 1e-6 = mm2->m2)
	fd t
	;; Use fields to link numbers/objects to table (t=yes, nil=no)
	fo "%lu6%qf1"
	   ;; Area field formatting
  )


  (if (= nil areaName)
    (setq areaName "")
  )
  ;;------------------------------------------------------------;;

  (defun *error* (msg)
    (if	cm
      (setvar 'CMDECHO cm)
    )
    (if	el
      (progn (entdel el) (setq el nil))
    )
    (if	acdoc
      (_EndUndo acdoc)
    )
    (if	(and of (eq 'FILE (type of)))
      (close of)
    )
    (if	(and Shell (not (vlax-object-released-p Shell)))
      (vlax-release-object Shell)
    )
    (if	(null (wcmatch (strcase msg) "*BREAK,*CANCEL*,*EXIT*"))
      (princ (strcat "\n--> Error: " msg))
    )
    (princ)
  )

  ;;------------------------------------------------------------;;

  (defun _StartUndo (doc)
    (_EndUndo doc)
    (vla-StartUndoMark doc)
  )

  ;;------------------------------------------------------------;;

  (defun _EndUndo (doc)
    (if	(= 8 (logand 8 (getvar 'UNDOCTL)))
      (vla-EndUndoMark doc)
    )
  )

  ;;------------------------------------------------------------;;

  (defun _centroid (space objs / reg cen)
    (setq reg (car (vlax-invoke space 'addregion objs))
	  cen (vlax-get reg 'centroid)
    )
    (vla-delete reg)
    (trans cen 1 0)
  )

  ;;------------------------------------------------------------;;

  (defun _text (space point string height rotation / text)
    (setq text (vla-addtext space string (vlax-3D-point point) height))
    (vla-put-alignment text acalignmentmiddlecenter)
    (vla-put-textalignmentpoint text (vlax-3D-point point))
    (vla-put-rotation text rotation)
    text
  )

  ;;------------------------------------------------------------;;

  (defun _Open (target / Shell result)
    (if	(setq Shell (vla-getInterfaceObject
		      (vlax-get-acad-object)
		      "Shell.Application"
		    )
	)
      (progn
	(setq result
	       (and
		 (or (eq 'INT (type target)) (setq target (findfile target)))
		 (not
		   (vl-catch-all-error-p
		     (vl-catch-all-apply
		       'vlax-invoke
		       (list Shell 'Open target)
		     )
		   )
		 )
	       )
	)
	(vlax-release-object Shell)
      )
    )
    result
  )

  ;;------------------------------------------------------------;;

  (defun _Select (msg pred func init / e)
    (setq pred (eval pred))
    (while
      (progn (setvar 'ERRNO 0)
	     (apply 'initget init)
	     (setq e (func msg))
	     (cond
	       ((= 7 (getvar 'ERRNO))
		(princ "\nMissed, try again.")
	       )
	       ((eq 'STR (type e))
		nil
	       )
	       ((vl-consp e)
		(if (and pred (not (pred (setq e (car e)))))
		  (princ "\nInvalid Object Selected.")
		)
	       )
	     )
      )
    )
    e
  )

  ;;------------------------------------------------------------;;

  (defun _GetObjectID (doc obj)
    (if	(vl-string-search "64" (getenv "PROCESSOR_ARCHITECTURE"))
      (vlax-invoke-method
	(vla-get-Utility doc)
	'GetObjectIdString
	obj
	:vlax-false
      )
      (itoa (vla-get-Objectid obj))
    )
  )

  ;;------------------------------------------------------------;;

  (defun _isAnnotative (style / object annotx)
    (and
      (setq object (tblobjname "STYLE" style))
      (setq
	annotx (cadr (assoc -3 (entget object '("AcadAnnotative"))))
      )
      (= 1 (cdr (assoc 1070 (reverse annotx))))
    )
  )

  ;;------------------------------------------------------------;;

  (setq	acdoc	(vla-get-activedocument (vlax-get-acad-object))
	acspc	(vlax-get-property
		  acdoc
		  (if (= 1 (getvar 'CVPORT))
		    'Paperspace
		    'Modelspace
		  )
		)

	ucszdir	(trans '(0. 0. 1.) 1 0 t)
	ucsxang	(angle '(0. 0. 0.) (trans (getvar 'UCSXDIR) 0 ucszdir))
  )
  (_StartUndo acdoc)
  (setq cm (getvar 'CMDECHO))
  (setvar 'CMDECHO 0)
  (setq	om (eq "1"
	       (cond ((getenv "LMAC_AreaLabel"))
		     ((setenv "LMAC_AreaLabel" "0"))
	       )
	   )
  )

  (setq	ts
	 (/ (getvar 'TEXTSIZE)
	    (if	(_isAnnotative (getvar 'TEXTSTYLE))
	      (cond ((getvar 'CANNOSCALEVALUE))
		    (1.0)
	      )
	      1.0
	    )
	 )
  )

  (cond
    ((not (vlax-method-applicable-p acspc 'addtable))

     (princ "\n--> Table Objects not Available in this Version.")
    )
    ((=	4
	(logand	4
		(cdr (assoc 70 (tblsearch "LAYER" (getvar 'CLAYER))))
	)
     )

     (princ "\n--> Current Layer Locked.")
    )
    ((not
       (setq *al:num
	      (cond
		((= flag 3) 1)
		(
		 (getint
		   (strcat "\n面积序号起始值 <"
			   (itoa (setq *al:num (1+ (cond (*al:num)
							 (0)
						   )
					       )
				 )
			   )
			   ">: "
		   )
		 )
		)
		(*al:num)
	      )
       )
     )
    )
    ((= flag 1)

     (setq th
	    (* 2.
	       (if
		 (zerop
		   (setq th
			  (vla-gettextheight
			    (setq st
				   (vla-item
				     (vla-item
				       (vla-get-dictionaries acdoc)
				       "ACAD_TABLESTYLE"
				     )
				     (getvar 'CTABLESTYLE)
				   )
			    )
			    acdatarow
			  )
		   )
		 )
		  ts
		  (/ th
		     (if (_isAnnotative (vla-gettextstyle st acdatarow))
		       (cond ((getvar 'CANNOSCALEVALUE))
			     (1.0)
		       )
		       1.0
		     )
		  )
	       )
	    )
     )

     (if
       (cond
	 (
	  (progn (initget "Add")
		 (vl-consp (setq pt
				  (getpoint "\n输入放置面积表的位置 <选择现有面积表>: "
				  )
			   )
		 )
	  )
	  (setq	tb
		 (vla-addtable
		   acspc
		   (vlax-3D-point (trans pt 1 0))
		   2
		   2
		   th
		   (* 1.5 th (max (strlen t1) (strlen t2)))
					;表格宽度在这设置
		 )
	  )
	  (vla-put-direction tb (vlax-3D-point (getvar 'UCSXDIR)))
	  (vla-settext tb 0 0 h1)
	  (vla-settext tb 1 0 t1)
	  (vla-settext tb 1 1 t2)

	  (while
	    (progn
	      (if om
		(setq p1
		       (_Select
			 (strcat "\n选择下一个对象[拾点]<退出>: ")
			 '(lambda (x)
			    (and
			      (vlax-property-available-p
				(vlax-ename->vla-object x)
				'area
			      )
			      (not (eq "HATCH" (cdr (assoc 0 (entget x)))))
			      (or (eq "REGION" (cdr (assoc 0 (entget x))))
				  (vlax-curve-isclosed x)
			      )
			    )
			  )
			 entsel
			 '("Pick")
		       )
		)
		(progn (initget "Object")
		       (setq p1 (getpoint "\n选择区域[对象]<退出>: "))
		)
	      )
	      (cond
		((null p1)

		 (vla-delete tb)
		)
		((eq "Pick" p1)

		 (setq om nil)
		 t
		)
		((eq "Object" p1)

		 (setq om t)
		)
		((eq 'ENAME (type p1))

		 (setq tx
			(cons
			  (_text acspc
				 (_centroid
				   acspc
				   (list (setq p1 (vlax-ename->vla-object p1)))
				 )
				 (strcat pf (itoa *al:num) sf)
				 ts
				 ucsxang
			  )
			  tx
			)
		 )
		 (vla-insertrows tb (setq n 2) th 1)
		 (vla-settext
		   tb
		   n
		   1
		   (if fd
		     (strcat "%<\\AcObjProp Object(%<\\_ObjId "
			     (_GetObjectID acdoc p1)
			     ">%).Area \\f \""
			     fo
			     "\">%"
		     )
		     (strcat ap (rtos (* cf (vla-get-area p1)) 2 2) as)
		   )
		 )
		 (vla-settext
		   tb
		   n
		   0
		   (if fd
		     (strcat "%<\\AcObjProp Object(%<\\_ObjId "
			     (_GetObjectID acdoc (car tx))
			     ">%).TextString>%"
		     )
		     (strcat pf (itoa *al:num) sf)
		   )
		 )
		 nil
		)
		((vl-consp p1)

		 (setq el (entlast))
		 (vl-cmdf "_.-boundary"	    "_A"     "_I"     "_N"
			  ""	   "_O"	    "_P"     ""	      "_non"
			  p1	   ""
			 )

		 (if (not (equal el (setq el (entlast))))
		   (progn
		     (setq tx
			    (cons
			      (_text
				acspc
				(_centroid acspc
					   (list (vlax-ename->vla-object el))
				)
				(strcat pf (itoa *al:num) sf)
				ts
				ucsxang
			      )
			      tx
			    )
		     )
		     (vla-insertrows tb (setq n 2) th 1)
		     (vla-settext
		       tb
		       n
		       1
		       (strcat ap
			       (rtos (* cf (vlax-curve-getarea el)) 2 2)
			       as
		       )
		     )
		     (vla-settext
		       tb
		       n
		       0
		       (if fd
			 (strcat "%<\\AcObjProp Object(%<\\_ObjId "
				 (_GetObjectID acdoc (car tx))
				 ">%).TextString>%"
			 )
			 (strcat pf (itoa *al:num) sf)
		       )
		     )
		     (redraw el 3)
		     nil
		   )
		   (vla-delete tb)
		 )
		)
	      )
	    )
	  )
	  (not (vlax-erased-p tb))
	 )
	 (
	  (and
	    (setq tb
		   (_Select "\n选择现有表加入: "
			    '(lambda (x)
			       (eq "ACAD_TABLE" (cdr (assoc 0 (entget x))))
			     )
			    entsel
			    nil
		   )
	    )
	    (< 1
	       (vla-get-columns (setq tb (vlax-ename->vla-object tb)))
	    )
	  )
	  (setq	n	(1- (vla-get-rows tb))
		*al:num	(1- *al:num)
	  )
	 )
       )
	(progn
	  (while
	    (if	om
	      (setq p1
		     (_Select (strcat "\nSelect Object ["
				      (if tx
					"Undo/"
					""
				      )
				      "Pick] <Exit>: "
			      )
			      '(lambda (x)
				 (and
				   (vlax-property-available-p
				     (vlax-ename->vla-object x)
				     'area
				   )
				   (not (eq "HATCH" (cdr (assoc 0 (entget x)))))
				   (or (eq "REGION" (cdr (assoc 0 (entget x))))
				       (vlax-curve-isclosed x)
				   )
				 )
			       )
			      entsel
			      (list (if	tx
				      "Undo Pick"
				      "Pick"
				    )
			      )
		     )
	      )
	      (progn (initget (if tx
				"Undo Object"
				"Object"
			      )
		     )
		     (setq p1 (getpoint	(strcat	"\n选择区域["
						(if tx
						  "Undo/"
						  ""
						)
						"Object] <Exit>: "
					)
			      )
		     )
	      )
	    )
	     (cond
	       ((and tx (eq "Undo" p1))

		(if el
		  (progn (entdel el) (setq el nil))
		)
		(vla-deleterows tb n 1)
		(vla-delete (car tx))
		(setq n	      (1- n)
		      tx      (cdr tx)
		      *al:num (1- *al:num)
		)
	       )
	       ((eq "Undo" p1)

		(princ "\n--> 啥也没干.")
	       )
	       ((eq "Object" p1)

		(if el
		  (progn (entdel el) (setq el nil))
		)
		(setq om t)
	       )
	       ((eq "Pick" p1)

		(setq om nil)
	       )
	       ((and om (eq 'ENAME (type p1)))

		(setq tx
		       (cons
			 (_text	acspc
				(_centroid
				  acspc
				  (list (setq p1 (vlax-ename->vla-object p1)))
				)
				(strcat pf (itoa (setq *al:num (1+ *al:num))) sf)
				ts
				ucsxang
			 )
			 tx
		       )
		)
		(vla-insertrows tb (setq n (1+ n)) th 1)
		(vla-settext
		  tb
		  n
		  1
		  (if fd
		    (strcat "%<\\AcObjProp Object(%<\\_ObjId "
			    (_GetObjectID acdoc p1)
			    ">%).Area \\f \""
			    fo
			    "\">%"
		    )
		    (strcat ap (rtos (* cf (vla-get-area p1)) 2 2) as)
		  )
		)
		(vla-settext
		  tb
		  n
		  0
		  (if fd
		    (strcat "%<\\AcObjProp Object(%<\\_ObjId "
			    (_GetObjectID acdoc (car tx))
			    ">%).TextString>%"
		    )
		    (strcat pf (itoa *al:num) sf)
		  )
		)
	       )
	       ((vl-consp p1)

		(if el
		  (progn (entdel el) (setq el nil))
		)
		(setq el (entlast))
		(vl-cmdf "_.-boundary"	   "_A"	    "_I"     "_N"
			 ""	  "_O"	   "_P"	    ""	     "_non"
			 p1	  ""
			)

		(if (not (equal el (setq el (entlast))))
		  (progn
		    (setq tx
			   (cons
			     (_text
			       acspc
			       (_centroid acspc
					  (list (vlax-ename->vla-object el))
			       )
			       (strcat pf (itoa (setq *al:num (1+ *al:num))) sf)
			       ts
			       ucsxang
			     )
			     tx
			   )
		    )
		    (vla-insertrows tb (setq n (1+ n)) th 1)
		    (vla-settext
		      tb
		      n
		      1
		      (strcat ap
			      (rtos (* cf (vlax-curve-getarea el)) 2 2)
			      as
		      )
		    )
		    (vla-settext
		      tb
		      n
		      0
		      (if fd
			(strcat	"%<\\AcObjProp Object(%<\\_ObjId "
				(_GetObjectID acdoc (car tx))
				">%).TextString>%"
			)
			(strcat pf (itoa *al:num) sf)
		      )
		    )
		    (redraw el 3)
		  )
		  (princ "\n--> Error Retrieving Area.")
		)
	       )
	     )
	  )
	  (if el
	    (progn (entdel el) (setq el nil))
	  )
	)
     )
    )
    ((= flag 2)
     (and
       (setq fl	(getfiled "创建面积统计文件"
			  (cond	(*file*)
				("")
			  )
			  "txt;csv;xls"
			  1
		)
       )
       (setq of (open fl "w"))
     )
     (setq *file*  (vl-filename-directory fl)
	   de	   (cdr
		     (assoc (strcase (vl-filename-extension fl) t)
			    '((".txt" . "\t") (".csv" . ",") (".xls" . "\t"))
		     )
		   )
	   *al:num (1- *al:num)
     )
     (write-line h1 of)
     (write-line (strcat t1 de t2) of)

     (while
       (if om
	 (setq p1
		(_Select (strcat "\n选择对象[拾取]<退出>: ")
			 '(lambda (x)
			    (and
			      (vlax-property-available-p
				(vlax-ename->vla-object x)
				'area
			      )
			      (not (eq "HATCH" (cdr (assoc 0 (entget x)))))
			      (or (eq "REGION" (cdr (assoc 0 (entget x))))
				  (vlax-curve-isclosed x)
			      )
			    )
			  )
			 entsel
			 '("Pick")
		)
	 )
	 (progn
	   (initget "Object")
	   (setq p1 (getpoint (strcat "\n选择区域[对象]<退出>: ")))
	 )
       )
	(cond
	  ((eq "Object" p1)

	   (if el
	     (progn (entdel el) (setq el nil))
	   )
	   (setq om t)
	  )
	  ((eq "Pick" p1)

	   (setq om nil)
	  )
	  ((eq 'ENAME (type p1))

	   (_text
	     acspc
	     (_centroid	acspc
			(list (setq p1 (vlax-ename->vla-object p1)))
	     )
	     (strcat pf (itoa (setq *al:num (1+ *al:num))) sf)
	     ts
	     ucsxang
	   )
	   (write-line
	     (strcat pf
		     (itoa *al:num)
		     sf
		     de
		     ap
		     (rtos (* cf (vla-get-area p1)) 2)
		     as
	     )
	     of
	   )
	  )
	  ((vl-consp p1)

	   (if el
	     (progn (entdel el) (setq el nil))
	   )
	   (setq el (entlast))
	   (vl-cmdf "_.-boundary"     "_A"     "_I"	"_N"
		    ""	     "_O"     "_P"     ""	"_non"
		    p1	     ""
		   )

	   (if (not (equal el (setq el (entlast))))
	     (progn
	       (_text
		 acspc
		 (_centroid acspc (list (vlax-ename->vla-object el)))
		 (strcat pf (itoa (setq *al:num (1+ *al:num))) sf)
		 ts
		 ucsxang
	       )
	       (write-line
		 (strcat pf
			 (itoa *al:num)
			 sf
			 de
			 ap
			 (rtos (* cf (vlax-curve-getarea el)) 2 2)
			 as
		 )
		 of
	       )
	       (redraw el 3)
	     )
	     (princ "\n--> Error Retrieving Area.")
	   )
	  )
	)
     )
     (if el
       (progn (entdel el) (setq el nil))
     )
     (setq of (close of))
     (_Open (findfile fl))
    )
    ((= flag 3)
					;为面积指定编号或名称，以便与方量表对应
					;(setq areaName
					;      (cond
					;	(
					;	 (getstring  (strcat "\n面积编号 <\042" areaName  "\042>: " ) )
					;	)
					; 	 (areaName)
					;    )
					;       )
     (princ
       "功能：标注封闭区域的面积。(C)中国电建一二·五联合体测量队 覃东 2017.05 cehui@139.com\n"
     )
     (if (= areaName "zz")
       (setq areaName "")
     )
     (setq areaName_old areaName)
     (setq areaName
	    (getstring (strcat "\n面积编号 <\042" areaName "\042>: ")
	    )
     )
     (if (= areaName "")
       (setq areaName areaName_old)
     )


     (while
       (if om
	 (setq p1
		(_Select (strcat "\n选择对象[拾取]<退出>: ")
			 '(lambda (x)
			    (and
			      (vlax-property-available-p
				(vlax-ename->vla-object x)
				'area
			      )
			      (not (eq "HATCH" (cdr (assoc 0 (entget x)))))
			      (or (eq "REGION" (cdr (assoc 0 (entget x))))
				  (vlax-curve-isclosed x)
			      )
			    )
			  )
			 entsel
			 '("Pick")
		)
	 )
	 (progn
	   (initget "Object")
	   (setq p1 (getpoint (strcat "\n选择区域<退出>: ")))
	 )
       )
	(cond
	  ((eq "Object" p1)

	   (if el
	     (progn (entdel el) (setq el nil))
	   )
	   (setq om t)
	  )
	  ((eq "Pick" p1)

	   (setq om nil)
	  )
	  ((eq 'ENAME (type p1))

	   (_text
	     acspc
	     (_centroid	acspc
			(list (setq p1 (vlax-ename->vla-object p1)))
	     )
	     (strcat pf (rtos (* cf (vla-get-area p1)) 2) sf)
	     ts
	     ucsxang
	   )
	  )
	  ((vl-consp p1)
	   (setq el (entlast))
	   (vl-cmdf "_.-boundary"     "_A"     "_I"	"_N"
		    ""	     "_O"     "_P"     ""	"_non"
		    p1	     ""
		   )

	   (if (not (equal el (setq el (entlast))))
	     (progn
	       (_text
		 acspc
		 (_centroid acspc (list (vlax-ename->vla-object el)))
		 (strcat (if (and (/= areaName "zz") (/= areaName ""))
			   (strcat areaName ":")
			   (strcat "" "")
			 )
			 (rtos (* cf (vlax-curve-getarea el)) 2 2)
			 sf
		 )
		 ts
		 ucsxang
	       )
	       (redraw el 1)
	     )
	     (princ "\n--> Error Retrieving Area.")
	   )
	  )
	)
     )
    )
  )
  (setenv "LMAC_AreaLabel"
	  (if om
	    "1"
	    "0"
	  )
  )
  (setvar 'CMDECHO cm)
  (_EndUndo acdoc)
  (princ)
)

;;生成方格网数据文件------------------------------------------------------------;;
;;;求模
(defun mod (numA numB)
  (- (/ (* 1.0 numA) (* numB 1.0))
     (fix (/ (* numA 1.0) (* 1.0 numB)))
  )
)

(defun C:zzFGW (/	  mscale    gridDistCm		pointId
		datFile	  pointA    pointB    pA	pB
		gridDist  zx_x	    zx_y      zs_x	zs_y
		fileId	  gridPointX	      gridPointY
	       )
  (princ
    "功能：生成地形图方格网数据文件。(C)中国电建一二·五联合体测量队 覃东 2017.05 cehui@139.com\n"
  )

  (setq	mscale
	 (getint "请输入成图比例尺分母(100/200/500/1000/2000/5000)："
	 )
  )
  (while (or (/= (mod mscale 100) 0) (<= mscale 0))
    (setq mscale
	   (getint "请输入成图比例尺分母100/200/500/1000/2000/5000：")
    )
  )

;;;(setq gridDistCm (getint "请输入格网距离(cm):"))
  (setq gridDistCm 10)
;;;默认10cm
  (setq pointId 0)
;;;(setq	datFile
;;; (getfiled "请输入格网坐标文件名："
;;;	   (cond (*file*)
;;;		 ("")
;;;	   )
;;;	   "dat"
;;;	   1
;;;)
;;;)
  (setq datFile "D:\\地形图坐标方格网数据.dat")

  (setq pointA (getpoint "请输入第一点："))
  (princ (strcat "N:"
		 (rtos (cadr pointA) 2 3)
		 " E:"
		 (rtos (car pointA) 2 3)
		 "\n"
	 )
  )
  (setq pointB (getpoint "请输入第二点："))
  (princ (strcat "N:"
		 (rtos (cadr pointB) 2 3)
		 " E:"
		 (rtos (car pointB) 2 3)
		 "\n"
	 )
  )
  (setq	pA pointA
	pB pointB
  )
  (setq	pointA (list (min (car pA) (car pB))
		     (min (cadr pA) (cadr pB))
	       )
  )

  (setq	pointB (list (max (car pA) (car pB))
		     (max (cadr pA) (cadr pB))
	       )
  )
;;;格网距离：米
  (setq gridDist (* (/ mscale 100) gridDistCm))
  (setq zx_x (* (+ (fix (/ (car pointA) gridDist)) 1) gridDist))
  (setq zx_y (* (+ (fix (/ (cadr pointA) gridDist)) 1) gridDist))

  (setq zs_x (* (fix (/ (car pointB) gridDist)) gridDist))
  (setq zs_y (* (fix (/ (cadr pointB) gridDist)) gridDist))

  (setq gridPointX zx_x)

  (setq fileId (open datFile "w"))
  (while (<= gridPointX zs_x)
    (setq gridPointY zx_y)
    (while (<= gridPointY zs_y)
      (setq pointId (+ pointId 1))
      (write-line
	(strcat	(itoa pointId)
		",+,"
		(itoa gridPointX)
		","
		(itoa gridPointY)
		",0.0"
	)
	fileId
      )
      (setq pointId (+ pointId 1))
      (write-line
	(strcat	(itoa pointId)
		",NE,"
		(itoa gridPointX)
		","
		(itoa gridPointY)
		",0.0"
	)
	fileId
      )

      '(command "_point" (list gridPointX gridPointY))
      (setq gridPointY (+ gridPointY gridDist))
    )
;;;end while y

    (setq gridPointX (+ gridPointX gridDist))
  )
;;;end while x

  (setq fileId (close fileId))
  (princ)
)
;;生成方格网数据文件------------------------------------------------------------;;
;;提取指定区域内的点，删除区域外地形点------------------------------------------------------------;;
(defun C:zzQydx
       (/ ptName ptSign ptE ptN ptH ptCount filename1 filename2)
  (princ
    "功能：删除指定区域外的点。(C)中国电建一二·五联合体测量队 覃东 2017.05 cehui@139.com\n"
  )
  (setq	filename1
	 (getfiled "提示字符串"
		   "D:\\"
		   "dat"
		   2
	 )
  )
  (if filename1
    (progn
      (setq
	filename2 (strcat (car (splitx filename1 ".dat"))
			  "-区域提取-"
			  (date)
			  "-"
			  (time)
			  ".dat"
		  )
      )
      (setq regionObj (entsel))
      (setq f1 (open filename1 "r"))
      (if (and f1 regionObj)
	(progn
	  (setq f2 (open filename2 "w"))
	  (setq rIndex 0)
	  (setq ptCount 0)
	  (while (setq lineStr (read-line f1))
	    (setq lineStrs (splitX lineStr ","))
	    (setq ptName (cons (nth 0 lineStrs) ptName))
	    ;;将点编号或点名生成表
	    (setq ptE (cons (nth 2 lineStrs) ptE))
	    ;;将E生成表
	    (setq ptN (cons (nth 3 lineStrs) ptN))
	    ;;将N生成表
	    (setq ptH (cons (nth 4 lineStrs) ptH))
	    ;;将H生成表
	    (setq ptSign (cons (itoa rIndex) ptSign))
	    ;;生成记号表
	    (setq rIndex (+ rIndex 1))
	  )
	  (setq ptName (reverse ptName))
	  ;;反序
	  (setq ptSign (reverse ptSign))
	  ;;反序
	  (setq ptE (reverse ptE))
	  ;;反序
	  (setq ptN (reverse ptN))
	  ;;反序
	  (setq ptH (reverse ptH))
	  ;;反序
	  (close f1)
	  ;;表处理
	  (setq rIndex 0)
	  (while (setq tmpPtname (nth rIndex ptName))
	    (setq tmpE (nth rIndex ptE))
	    (setq tmpN (nth rIndex ptN))
	    (setq tmpH (nth rIndex ptH))
	    (setq tmpSign (nth rIndex ptSign))
	    (if	(pt_inorout regionObj (list (atof tmpE) (atof tmpN)))
	      (progn
		(write-line
		  (strcat tmpPtname ",," tmpE "," tmpN "," tmpH)
		  f2
		)
		(setq ptCount (+ 1 ptCount))
	      )
	    )
	    (setq rIndex (+ rIndex 1))
	  )
	  ;;表处理
	  (princ
	    (strcat
	      "共提取"
	      (itoa ptCount)
	      "个点。(C)中国电建一二·五联合体测量队 覃东 2017.05 cehui@139.com\n"
	    )
	  )
	  ;;(princ "\n")
	  (close f2)
	)
	;;End progn
	(princ
	  "用户取消操作！(C)中国电建一二·五联合体测量队 覃东 2017.05 cehui@139.com\n"
	)
      )
      ;;end if

    )
    ;; end progn1
    (princ
      "用户取消操作！(C)中国电建一二·五联合体测量队 覃东 2017.05 cehui@139.com\n"
    )
  )
  ;;end if1
  (princ)
)
;;提取指定区域内的点，删除区域外地形点------------------------------------------------------------;;

;;区域外地形点生成文件，删除区域内地形点------------------------------------------------------------;;
(defun C:zzQywdx
       (/ ptName ptSign ptE ptN ptH ptCount filename1 filename2)
  (princ
    "功能：删除指定区域内的点，取区域外的地形点。(C)中国电建一二·五联合体测量队 覃东 2017.05 cehui@139.com\n"
  )
  (setq	filename1
	 (getfiled "提示字符串"
		   "D:\\"
		   "dat"
		   2
	 )
  )
  (if filename1
    (progn
      (setq
	filename2 (strcat (car (splitx filename1 ".dat"))
			  "-区域外提取-"
			  (date)
			  "-"
			  (time)
			  ".dat"
		  )
      )
      (setq regionObj (entsel))
      (setq f1 (open filename1 "r"))
      (if (and f1 regionObj)
	(progn
	  (setq f2 (open filename2 "w"))
	  (setq rIndex 0)
	  (setq ptCount 0)
	  (while (setq lineStr (read-line f1))
	    (setq lineStrs (splitX lineStr ","))
	    (setq ptName (cons (nth 0 lineStrs) ptName))
	    ;;将点编号或点名生成表
	    (setq ptE (cons (nth 2 lineStrs) ptE))
	    ;;将E生成表
	    (setq ptN (cons (nth 3 lineStrs) ptN))
	    ;;将N生成表
	    (setq ptH (cons (nth 4 lineStrs) ptH))
	    ;;将H生成表
	    (setq ptSign (cons (itoa rIndex) ptSign))
	    ;;生成记号表
	    (setq rIndex (+ rIndex 1))
	  )
	  (setq ptName (reverse ptName))
	  ;;反序
	  (setq ptSign (reverse ptSign))
	  ;;反序
	  (setq ptE (reverse ptE))
	  ;;反序
	  (setq ptN (reverse ptN))
	  ;;反序
	  (setq ptH (reverse ptH))
	  ;;反序
	  (close f1)
	  ;;表处理
	  (setq rIndex 0)
	  (while (setq tmpPtname (nth rIndex ptName))
	    (setq tmpE (nth rIndex ptE))
	    (setq tmpN (nth rIndex ptN))
	    (setq tmpH (nth rIndex ptH))
	    (setq tmpSign (nth rIndex ptSign))
	    (if
	      (not (pt_inorout regionObj (list (atof tmpE) (atof tmpN)))
	      )
	       (progn
		 (write-line
		   (strcat tmpPtname ",," tmpE "," tmpN "," tmpH)
		   f2
		 )
		 (setq ptCount (+ 1 ptCount))
	       )
	    )
	    (setq rIndex (+ rIndex 1))
	  )
	  ;;表处理
	  (princ
	    (strcat
	      "共提取"
	      (itoa ptCount)
	      "个点。(C)中国电建一二·五联合体测量队 覃东 2017.05 cehui@139.com\n"
	    )
	  )
	  ;;(princ "\n")
	  (close f2)
	)
	;;End progn
	(princ
	  "用户取消操作！(C)中国电建一二·五联合体测量队 覃东 2017.05 cehui@139.com\n"
	)
      )
      ;;end if

    )
    ;; end progn1
    (princ
      "用户取消操作！(C)中国电建一二·五联合体测量队 覃东 2017.05 cehui@139.com\n"
    )
  )
  ;;end if1
  (princ)
)
;;区域外地形点生成文件，删除区域内地形点------------------------------------------------------------;;


;;绘横断面高程标尺----------------------------------------------------------------------------------;;
(defun c:zzBC (/		  i		     judge
	       varBarPosition	  varOrigin	     varBarPosition_tmp
	       varTextHeight	  varBarPositionLevel
	       varLength_tmp	  varLength	     varBarStartLevel
	       varBarEndPositionLevel		     varBarEndLevel
	       varBarStartX	  varBarStartY	     varBarsCount
	       varStartY	  varEndX	     varEndY
	       varPts
	      )
  (setq i 0)
  (setq judge 0)
  (grtext -1 "绘制标尺 覃东")
  (princ "\n")
  (setq varOrigin (getpoint "请输入一点："))
  (princ "\n")
  (if (/= varLevel nil)
    (progn (setq varLevel_tmp varLevel)
	   (setq PromptTmp (strcat "该点高程" "<" (rtos varLevel 2 3) ">:"))
    )
    (setq PromptTmp "该点高程：")
  )
  ;;(while (<= (setq varLevel (getreal PromptTmp)) 0))
  (if (= (setq varLevel (getreal PromptTmp)) nil)
    (setq varLevel varLevel_tmp)
  )
  (princ "\n")
  (if (/= varBarPosition_tmp nil)
    (setq PromptTmp (strcat "标尺距离"
			    "<"
			    (rtos (car varBarPosition_tmp) 2 3)
			    ","
			    (rtos (cadr varBarPosition_tmp) 2 3)
			    ">:"
		    )
    )
    (setq PromptTmp "标尺距离：")
  )
  (princ "\n")
  (if (= (setq varBarPosition (getpoint PromptTmp)) nil)
    (setq varBarPosition varBarPosition_tmp)
  )
  (setq varBarPosition_tmp varBarPosition)
  ;;选择标尺的顶点
  (if (/= varLength_tmp nil)
    (setq PromptTmp (strcat "标尺长度"
			    "<"
			    (rtos (car varLength_tmp) 2 3)
			    ","
			    (rtos (cadr varLength_tmp) 2 3)
			    ">:"
		    )
    )
    (setq PromptTmp "标尺长度：")
  )
  (princ "\n")
  (if (= (setq varLength (getpoint PromptTmp)) nil)
    (setq varLength varLength_tmp)
  )
  (setq varLength_tmp varLength)

  (princ "\n")
  (if (/= varScale_tmp nil)
    (setq
      PromptTmp	(strcat "比例尺分母" "<" (rtos varScale_tmp 2 3) ">:")
    )
    (setq PromptTmp "比例尺分母：")
  )
  (if (= (setq varScale (getint PromptTmp)) nil)
    (setq varScale varScale_tmp)
  )

  (if (and (/= nil varScale)
	   (/= nil varScale)
	   (/= nil varBarPosition)
	   (/= nil varLength)
	   (/= nil varOrigin)
      )
    ;;if main
    (progn
      ;;progn main

      ;;当确定标尺长度时若点在下方进行改正
      (if (<= (cadr varLength) (cadr varBarPosition))
	(setq varLength
	       (list (car varLength)
		     (+	(cadr varBarPosition)
			(abs (- (cadr varLength) (cadr varBarPosition)))
		     )
	       )
	)

      )

      (setq varScale_tmp varScale)
      (setq varScale (/ varScale 100))
      ;;比例下每cm长度
      (setq varTextHeight (* (/ varScale 10.0) 1.5))
      ;;文字高度及宽度为1.5mm
      (EntMakeTextStyle
	"LevelBar" varTextHeight 1 "simhei.ttf"	"")
      (EntMakeLayer "2-断面-标尺" 1)
      ;;确定标尺起点高程
      (setq varBarPositionLevel
	     (+	(- (cadr varBarPosition) (cadr varOrigin))
		varLevel
	     )
      )
      (setq varBarStartLevel (fix (+ varBarPositionLevel 0.5)))
      ;;四舍五入求标尺起点整高程
      ;;确定标尺终点高程
      (setq varBarEndPositionLevel
	     (+	(- (cadr varLength) (cadr varOrigin))
		varLevel
	     )
      )
      (setq varBarEndLevel (fix (+ varBarEndPositionLevel 0.5)))
      ;;四舍五入求标尺起点整高程
      ;;确定标尺起点坐标
      (setq varBarStartX (car varBarPosition))
      (setq varBarStartY
	     (+	(cadr varBarPosition)
		(- varBarStartLevel varBarPositionLevel)
	     )
      )
      (setq varBarsCount
	     (+	(atoi
		  (rtos
		    (/ (- varBarEndLevel varBarStartLevel) varScale)
		    2
		    0
		  )
		)
		1
	     )
      )

      (setq varBarsCount (* (fix (+ (/ varBarsCount 2) 0.5)) 2))

      (while (/= varBarsCount 0)
	(setq varStartY (+ varBarStartY (* i varScale)))

	(setq varEndX (- varBarStartX (* (/ varScale 10.0) 1.5)))
	(setq varEndY (+ varBarStartY (* (+ i 1) varScale)))


	(setq Fp (list varBarStartX varStartY))
	(setq Ep (list varEndX varEndY))

	(setq Lfp (list varBarStartX varStartY))
	(setq Lep (list (+ varBarStartX (/ varScale 10.0)) varStartY))

	(setq
	  Txtp
	   (list (+ varBarStartX (* (/ varScale 10.0) 2.0)) varEndY)
	)
	(setq Hi (+ varBarStartLevel (* varScale i)))
	(setq
	  Loe (list (+ (/ varScale 10.0) varBarStartX) varBarStartY)
	)
	(if (= judge 0)
	  (progn
	    (setq SolidBarFp
		   (list (/ (+ (car Fp) (car Ep)) 2) (cadr Fp))
	    )
	    ;;实心标尺起点
	    (setq SolidBarEp
		   (list (/ (+ (car Fp) (car Ep)) 2) (cadr Ep))
	    )
	    ;;实心标尺终点
	    (entMakePLineThick
	      (list SolidBarFp SolidBarEp)
	      varTextHeight
	      "2-断面-标尺"
	    )
	    (EntMakeLine
	      (car lfp)
	      (cadr Lfp)
	      (car Lep)
	      (cadr Lep)
	      "2-断面-标尺"
	    )
	    (EntMakeText
	      (+ varBarStartX (* (/ varScale 10.0) 1.1))
	      varStartY
	      (itoa Hi)
	      varTextHeight
	      "LevelBar"
	      "2-断面-标尺"
	    )
	    (setq judge 1)
	  )
	  (progn
	    (setq varPts nil)
	    (setq varPts (cons (list varBarStartX varStartY) varPts))
	    (setq varPts (cons (list varBarStartX varEndY) varPts))
	    (setq varPts (cons (list varEndX varEndY) varPts))
	    (setq varPts (cons (list varEndX varStartY) varPts))
	    (entMakePLine varPts "2-断面-标尺")
	    (setq judge 0)
	  )
	)
	(setq i (+ i 1))
	(setq varBarsCount (- varBarsCount 1))
      )
      (if (= judge 0)
	(progn
	  (setq Lfp (list varBarStartX varEndY))
	  (setq Lep (list (+ varBarStartX (/ varScale 10.0)) varEndY))
	  (setq Hi (+ varBarStartLevel (* varScale i)))
	  (EntMakeLine
	    (car lfp)
	    (cadr Lfp)
	    (car Lep)
	    (cadr Lep)
	    "2-断面-标尺"
	  )
	  (EntMakeText
	    (+ varBarStartX (* (/ varScale 10.0) 1.1))
	    varEndY
	    (itoa Hi)
	    varTextHeight
	    "LevelBar"
	    "2-断面-标尺"
	  )
	)
      )
      (princ
	"\n(C)中国电建一二·五联合体测量队 覃东 cehui@139.com"
      )
      (vl-cmdf "regen")
      (princ)
    )
    ;;end progn main
    (progn
      (princ
	"\n输入错误！请按提示输入！ (C)中国电建一二·五联合体测量队 覃东 cehui@139.com"
      )
      (princ)
    )
  )
  ;;end if main

)

;;绘横断面高程标尺----------------------------------------------------------------------------------;;

(defun C:zzHelp ()
  (alert
    "    中国电建一二·五联合体测量队\n\n\n    :: zzFGW               -生成方格网坐标数据文件(D:盘)\n    :: zzArea2Table     -输出面积到AutoCAD表格\n    :: zzArea2File        -输出面积到文件\n    :: zzA                    -标注面积到区域中心\n    :: zzQydx               -区域地形：删除指定封闭区域外的地形点\n    :: zzQywdx            -区域外地形：删除指定封闭区域内的地形点\n    :: zzBC                  -横断面高程标尺：绘制横断面图的高程标尺\n    :: zzExport2Dat      -CASS地形图导出无编码数据文件(.dat)\n    :: zzCass2CadPoint      -将图成CASS地形点生成CAD点对象用于生成C3D曲面\n    ::zzHelp                -查看命令帮助提示信息\n\n\n                       (C) 覃东 20170928 QQ:61902475"  )
  (princ)
)

;;CASS地形图生成CAD点对象用于生成C3D曲面--------------------------------------------------------------------;;
(defun C:zzCass2CadPoint ()
  (setq ss (ssget "X" '((0 . "insert") (8 . "GCD"))))

  (setq i 0)
  (repeat (sslength ss)
    (setq ssn (ssname ss i))
    (setq endata (entget ssn))
    (setq blockXYZ (assoc 10 endata))
    (print blockXYZ)
    (entmakeX (list '(0 . "POINT") blockXYZ))
    (setq i (1+ i))
  )

)



;;CASS地形图导出无编码数据文件----------------------------------------------------------------------------------;;
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
;;CASS地形图导出无编码数据文件----------------------------------------------------------------------------------;;


;;自定义通用函数----------------------------------------------------------------------------------;;

;;用法：(EntMakeText 点X 点Y 文本内容 文本高度)
(defun EntMakeText (px py str tHeight styleName layerName / pt)
  (setq pt (list px py))
  (entmakeX
    (list '(0 . "TEXT")
	  (cons 1 str)
	  (cons 10 pt)
	  (cons 7 styleName)
	  (cons 8 layerName)
	  (cons 40 tHeight)
    )
  )
)

;;用法：(EntMakeLine 起点X 起点Y 终点X 终点Y)
(defun EntMakeLine (xa ya xb yb layerName / p1 p2)
  (setq	p1 (list xa ya)
	p2 (list xb yb)
  )
  (entmakeX (list '(0 . "LINE")
		  '(370 . 0)
		  (cons 10 p1)
		  (cons 11 p2)
		  (cons 8 layerName)
	    )
  )
)

(defun entMakePLineThick (pts weight layerName)
  (entmake (append
	     (list '(0 . "LWPOLYLINE")
		   '(100 . "AcDbEntity")
		   '(100 . "AcDbPolyline")
		   (cons 8 layerName)
		   ;;层
		   ;;'(62 . 7)
		   ;;颜色：7-白色
		   '
		    (370 . 0)
		   ;;线宽0，0.20值为(370 . 20)
		   (cons 90 (length pts))
		   (cons 43 weight)
		   ;;全局宽
		   '
		    (70 . 0)
	     )				;list
	     (mapcar '(lambda (x)
			(cons 40 weight)
			;;起点宽
			(cons 41 weight)
			;;终点宽
			(cons 42 0.0)
			(cons 10 x)
		      )
		     pts
	     )
	   )				;append
  )
)


(defun entMakePLine (pts layerName)
  (entmake (append
	     (list '(0 . "LWPOLYLINE")
		   '(100 . "AcDbEntity")
		   '(100 . "AcDbPolyline")
		   '(370 . 0)
		   (cons 8 layerName)
		   (cons 90 (length pts))
		   '(70 . 0)
	     )				;list
	     (mapcar '(lambda (x) (cons 10 x)) pts)
	   )				;append
  )
)

(defun EntMakeTextStyle	(tStyleName	  tStyleHeight
			 tStyleWeight	  tFontName
			 tBigFontName
			)
  (if (not (tblsearch "style" tStyleName))
    (entmakeX
      (list
	'(0 . "STYLE")
	'(100 . "AcDbSymbolTableRecord")
	'(100 . "AcDbTextStyleTableRecord")
	(cons 2 tStyleName)
	'(70 . 0)
	(cons 40 tStyleHeight)
	(cons 41 tStyleWeight)
	(cons 3 tFontName)
	(cons 4 tBigFontName)
      )
    )
  )
)

(defun EntMakeLayer (layname color / nlay)
  (vl-load-com)
  (or (tblsearch "layer" layname)
      (or (not (setq nlay
		      (vla-add (vla-get-layers
				 (vla-get-activedocument (vlax-get-acad-object))
			       )
			       layname
		      )
	       )
	  )
	  (vla-put-color nlay color)	;vla-put-返回值为nil
					;(vla-put-plottable nlay :vlax-false) ;设为不打印层
					;(vla-put-activelayer
					;  (vla-get-activedocument (vlax-get-acad-object))
					;  nlay
					;)
	  ;;设为当前层
      )
  )
)


;;;功能：字符串按指定分隔符分隔,分隔符可以是字串，用于取文件名，用扩展名作为分隔符
;;;(splitX "C:\\Users\\....25～K1+013.52；EL.2776.73～EL.2804.74）.dat" ".dat")
;;;返回：(C:\\Users\\....25～K1+013.52；EL.2776.73～EL.2804.74）)
(defun splitX (str delim / LST POS)
  (while (setq pos (vl-string-search delim str))
    (setq lst (append lst (list (substr str 1 pos))))
    (setq str (substr str (+ (+ pos (strlen delim)) 1)))
  )
  (if (> (strlen str) 0)
    (append lst (list str))
    lst
  )
)


(defun pt_inorout (regionObj pt / pt_list e1 pt n i j va va_count)
  (setq	pt_list	(mapcar	'cdr
			(vl-remove-if
			  '(lambda (x) (/= 10 (car x)))
			  (entget (car regionObj))
			)
		)
  )

  (setq	i	 0
	va_count 0
	n	 (length pt_list)
	pt_list	 (append pt_list (list (car pt_list)))
  )
  (repeat n
    (setq va (-	(angle pt (nth i pt_list))
		(angle pt (nth (1+ i) pt_list))
	     )
    )
    (cond ((> va pi) (setq va (- va pi)))
	  ((< va (* -1 pi)) (setq va (+ va pi)))
    )
    (setq va_count (+ va_count va)
	  i	   (1+ i)
    )
  )
  (if (< (abs (- (abs va_count) pi)) 0.000001)
    't
    'nil
  )
)


(defun date ()
  (setq datetime (rtos (getvar "cdate") 2 6))
  (car (splitx datetime "."))
)

(defun time ()
  (setq datetime (rtos (getvar "cdate") 2 6))
  (cadr (splitx datetime "."))
)
;;删除区域外地形点------------------------------------------------------------;;


(vl-load-com)
(princ)
(princ
  "\n:: 一二·五测量工具箱 | Ver:20170520 | 覃东 中国电建一二·五联合体测量队::"
)
(princ "\n:: zzFGW       -生成方格网坐标数据文件(D:盘)")
(princ "\n:: zzArea2Table-输出面积到AutoCAD表格")
(princ "\n:: zzArea2File -输出面积到文件")
(princ "\n:: zzA         -标注面积到区域中心")
(princ
  "\n:: zzQydx      -区域地形：删除指定封闭区域外的地形点"
)
(princ
  "\n:: zzQywdx     -区域外地形：删除指定封闭区域内的地形点"
)
(princ
  "\n:: zzBC        -横断面高程标尺：绘制横断面图的高程标尺"
)
(princ
  "\n:: zzExport2Dat        -CASS地形图导出无编码数据文件(.dat)"
)
(princ "\n:: zzHelp         -查看命令帮助提示信息")
(princ)

;;------------------------------------------------------------;;
;;                         End of File                        ;;
;;------------------------------------------------------------;;
