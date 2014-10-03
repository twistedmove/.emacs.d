(defun nifty-func (arg1 &optional arg2)
  (or (listp arg1)
      (and
       (stringp arg1)
       (concat arg1 "bells"))
      (message "Hi, Mom!")))
