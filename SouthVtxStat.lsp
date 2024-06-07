(defun chkCharSet (/ sys_var)
  (if (setq sys_var (getvar "LISPSYS"))
    (if (> sys_var 0) 
      T;Unicode mode, Utf-8 for text literal.
      nil;Non-Unicode mode, GB Charset for CN.
    )
    T;SysVar "LISPSYS" not exist
  )
)

(defun str2strLst (str)
  (if (equal 'STR (type str))
    (progn
      (setq
        ctr 1
        strLst nil
      )
      (repeat (strlen str)
        (setq
          strLst (cons (substr str ctr 1) strLst)
          ctr (1+ ctr)
        )
      )
      (reverse strLst)
    )
    nil;Invalid input
  )
)

(defun str2intLst (str)
  (mapcar 'ascii (str2strLst str))
)

(defun plural (num strNoun strPlural)
  (strcat (itoa num) " " (if (= 1 (abs num)) strNoun strPlural))
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

(defun isPrimeSouthCode (southCode)
  (if (member "-" (str2strLst southCode)) nil T)
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
      (T 0);Unsupported Type
    );Count
    nil;Invalid
  )
)

(defun chkClosed (eName)
  nil
)

(defun SouthEntDispatch (ss)
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

(defun main (ss)
  ;(setq isUnicode (chkCharSet))
  (prompt
    (strcat
      "\n================================================================"
      "\nInputted selection-set contain a total of " (itoa (sslength ss)) " entities."
    )
  )
  (if (setq codeDict (SouthEntDispatch ss))
    (progn
      (setq
        countLst
        (mapcar
          '(lambda (lst)
            (setq
              southCode (car lst)
              entLst (cdr lst)
              entNum (length entLst)
              vtxNum (apply '+ (mapcar 'statVertex entLst))
            )
            (prompt
              (strcat
                "\n+ Count SOUTH(" southCode ") " (plural entNum "Entity" "Entities") ", " (plural vtxNum "Vertex" "Vertexes") "."
              )
            )
            (cons southCode (cons entNum vtxNum))
          )
          codeDict
        )
      )
      (prompt
        (strcat
          "\n================================================================"
          "\n* Total SOUTH Codes: " (itoa (length countLst))
          "\n* Total SOUTH Entities: "
          (itoa (apply '+ (mapcar '(lambda (lst) (car (cdr lst))) countLst)))
          "\n* Total SOUTH Prime Entities: "
          (itoa
            (apply
              '+
              (mapcar
                '(lambda (lst)
                  (if (isPrimeSouthCode (car lst))
                    (car (cdr lst))
                    0;non-Prime
                  )
                )
                countLst
              )
            )
          )
          "\n* Total Vertexes: "
          (itoa (apply '+ (mapcar '(lambda (lst) (cdr (cdr lst))) countLst)))
          "\n* Total Significant Vertexes: "
          (itoa
            (apply
              '+
              (mapcar
                '(lambda (lst)
                  (if (isPrimeSouthCode (car lst))
                    (cdr (cdr lst))
                    0;non-Prime
                  )
                )
                countLst
              )
            )
          )
          "\n================================================================\n"
        )
      )
    );Count&Report
    (prompt "\nNo valid or significant SOUTH Code Entity!\n");Invalid codeDict
  )
  (princ)
)

(defun c:SouthVtxStat ()
  (main (ssget ":N"))
)

(defun c:SouthVtxStatX ()
  (main (ssget "X"))
)

(defun c:SouthVtxStatHelp ()
  
)
