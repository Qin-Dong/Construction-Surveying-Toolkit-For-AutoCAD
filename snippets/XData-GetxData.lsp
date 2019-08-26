;获得对象扩展数据
(defun Object_GetxData(obj / xtypeOut xdataOut)
(vla-getxdata obj "" 'xtypeOut 'xdataOut)
(cdr (mapcar 'vlax-variant-value (vlax-safearray->list xdataOut)))
)
