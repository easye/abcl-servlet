(defgeneric org.example.normalizeURI (url))

;;(defun trace 

#+nil
(defmethod :around org.example.normalizeURI (url url)
  (push args)
  (call-next-method)
  (push result)
  (return result))
