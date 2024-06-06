(defun chkCharSet (/ sys_var)
  (if (setq sys_var (getvar "LISPSYS"))
    (if (> sys_var 0) 
      T;Unicode mode, Utf-8 for text literal.
      nil;Non-Unicode mode, GB Charset for CN.
    )
    T;SysVar "LISPSYS" not exist
  )
)

(defun getSouthCode (eName)
  (if (and
        eName
        (equal 'ENAME (type eName))
        (equal 'STR (type (setq ret (cdr (assoc 1000 (cdr (car (cdr (assoc -3 (entget eName '("SOUTH")))))))))))
      )
    ret
    nil;Invalid
  )
)

(defun statVertex (eName)
  (if (and
        eName
        (equal 'ENAME (type eName))
        (setq eLst (entget eName))
        (setq eType (cdr (assoc 0 eLst)))
      )
    (cond
      ((= eType "INSERT") 1);Point Features
      ((= eType "LWPOLYLINE");Line&Area Feature
        (apply '+ (mapcar '(lambda (field) (if (= 10 (car field)) 1 0)) eLst))
      )
      ((= eType "POLYLINE");Line Feature
        (setq
          ctr 0
          cur (entnext eName)
        )
        (while (= (cdr (assoc 0 (entget cur))) "VERTEX")
          (setq
            ctr (1+ ctr)
            cur (entnext cur)
          )
        )
        (if (= (cdr (assoc 0 (entget cur))) "SEQEND") ctr nil)
      )
      (T nil);Unsupported Type
    );Count
    nil;Invalid
  )
)

(defun chkClosed (eName)
  nil
)

(defun SouthEntDispatch (ss /)
  (setq
    codeDict nil
    ctr 0
  )
  (repeat (sslength ss)
    (setq
      eName (ssname ss ctr)
      ctr (1+ ctr)
      curCode (getSouthCode eName)
    )
    (if curCode
      (if (setq tempLst (assoc curCode codeDict))
        (setq
          codeDict (subst (cons curCode (cons eName (cdr tempLst))) tempLst codeDict)
        );Append entities to a existing key
        (setq codeDict (cons (cons curCode (cons eName nil)) codeDict));New key
      );Entity with SouthCode
    )
  )
  codeDict
)

(defun c:SouthVtxStat ()
  
)

(defun c:SouthVtxStatX ()
  
)

(defun c:SouthVtxStatHelp ()
  
)
