(defun chkCharSet (/ sys_var)
  (if (setq sys_var (getvar "LISPSYS"))
    (if (> sys_var 0) 
      T;Unicode mode, Utf-8 for text literal.
      nil;Non-Unicode mode, GB Charset for CN.
    )
    nil;SysVar "LISPSYS" not exist
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

(defun intLst2str (intLst)
  (apply 'strcat (mapcar 'chr intLst))
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

(defun alterLang ()
  (mapcar
    '(lambda (keyLst)
      (cons
        (car keyLst)
        (apply
          '(lambda (textRes) (if (listp textRes) (intLst2str textRes) textRes))
          (cons
            ;switch lang
            (cond
              (g_isCHS
                (apply
                  '(lambda (lstCharset) (if g_isUnicode (cdr lstCharset) (car lstCharset)))
                  (cons (cdr (assoc "CHS" (cdr keyLst))) nil)
                )
              )
              (T (car (cdr (assoc "ASCII" (cdr keyLst)))))
            )
            nil;for constructing a list needed by apply
          )
        )
      );key-text pair
    )
    ;Declare
    '(
      ("i_input_ent"
        ("ASCII" "The total of entities that inputted selection-set contains")
        ("CHS" "输入选择集包含的总实体数量" 36755 20837 36873 25321 38598 21253 21547 30340 24635 23454 20307 25968 37327)       
      )
      ("i_total_codes"
        ("ASCII" "Total SOUTH Codes")
        ("CHS" "总计SOUTH编码数量" 24635 35745 83 79 85 84 72 32534 30721 25968 37327)
      )
      ("i_total_ent"
        ("ASCII" "Total SOUTH Entities")
        ("CHS" "总计SOUTH实体数量" 24635 35745 83 79 85 84 72 23454 20307 25968 37327)
      )
      ("i_total_ent_"
        ("ASCII" "Total SOUTH Prime Entities")
        ("CHS" "总计SOUTH非符号化实体数量" 24635 35745 83 79 85 84 72 38750 31526 21495 21270 23454 20307 25968 37327)
      )
      ("i_total_vtx"
        ("ASCII" "Total SOUTH Vertexes")
        ("CHS" "总计SOUTH实体顶点数量" 24635 35745 83 79 85 84 72 23454 20307 39030 28857 25968 37327)
      )
      ("i_total_vtx_"
        ("ASCII" "Total Significant SOUTH Vertexes")
        ("CHS" "总计SOUTH非符号化实体顶点数量" 24635 35745 83 79 85 84 72 38750 31526 21495 21270 23454 20307 39030 28857 25968 37327)
      )
      ("e_invalid_ss"
        ("ASCII" "Invalid inputted selection-set")
        ("CHS" "输入的选择集无效!" 36755 20837 30340 36873 25321 38598 26080 25928 33)
      )
      ("e_no_SOUTH_ent"
        ("ASCII" "No valid or significant SOUTH Code Entity!")
        ("CHS" "无合法有效的SOUTH编码实体!" 26080 21512 27861 26377 25928 30340 83 79 85 84 72 32534 30721 23454 20307 33)
      )
      ("i_copyright"
        ("ASCII" 67 111 112 121 114 105 103 104 116 32 40 67 41 32 50 48 50 52 32 89 105 102 101 110 103 32 70 97 110 46 32 65 108 108 32 114 105 103 104 116 115 32 114 101 115 101 114 118 101 100 46)
        ("CHS" (67 111 112 121 114 105 103 104 116 32 40 67 41 32 50 48 50 52 32 89 105 102 101 110 103 32 70 97 110 46 32 65 108 108 32 114 105 103 104 116 115 32 114 101 115 101 114 118 101 100 46) 29256 26435 25152 26377 169 32 50 48 50 52 32 33539 26131 20016 128521)
      )
      ("i_self_statement"
        ("ASCII" "There are two command \"SouthVtxStat\" and \"SouthVtxStatX\". \"SouthVtxStat\" is for interactively creating CAD selection-set by mouse, the other \"SouthVtxStatX\" select directly all entities of DWG database. Both of them filter SOUTH entities from inputted Selection-set.")
        ("CHS" "有两个命令分别是 \"SouthVtxStat\" 和 \"SouthVtxStatX\"。其中前者通过鼠标交互创建CAD选择集，后者则选取DWG数据库中所有实体建立选择集，两者皆从输入的选择集中筛选SOUTH编码实体。" 26377 20004 20010 21629 20196 20998 21035 26159 32 34 83 111 117 116 104 86 116 120 83 116 97 116 34 32 21644 32 34 83 111 117 116 104 86 116 120 83 116 97 116 88 34 12290 20854 20013 21069 32773 36890 36807 40736 26631 20132 20114 21019 24314 67 65 68 36873 25321 38598 65292 21518 32773 21017 36873 21462 68 87 71 25968 25454 24211 20013 25152 26377 23454 20307 24314 31435 36873 25321 38598 65292 20004 32773 30342 20174 36755 20837 30340 36873 25321 38598 20013 31579 36873 83 79 85 84 72 32534 30721 23454 20307 12290)       
      )
    )
  )
)

(defun textDict(key)
  (cdr (assoc key g_textDict))
)

(defun main (ss)
  (if (and ss (equal 'PICKSET (type ss)))
    (progn
      (prompt
        (strcat
          "\n================================================================"
          "\n* " (textDict "i_input_ent") ": " (itoa (sslength ss))
        )
      )
      (if (setq codeDict (SouthEntDispatch ss))
        (apply
          '(lambda (countLst)
            (prompt
              (strcat
                "\n================================================================"
                "\n* " (textDict "i_total_codes") ": " (itoa (length countLst))
                "\n* " (textDict "i_total_ent") ": "
                (itoa (apply '+ (mapcar '(lambda (lst) (car (cdr lst))) countLst)))
                "\n* " (textDict "i_total_ent_") ": "
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
                "\n* " (textDict "i_total_vtx") ": "
                (itoa (apply '+ (mapcar '(lambda (lst) (cdr (cdr lst))) countLst)))
                "\n* " (textDict "i_total_vtx_") ": "
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
          )
          (cons
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
              );Per SOUTH code statistics
              codeDict
            );codeDict->countLst
            nil
          )
        );Count&Report
        (prompt 
          (strcat
            "\n" (textDict "e_no_SOUTH_ent")
            "\n================================================================\n"
          )
        );Invalid codeDict
      )
    );Valid inputted ss
    (prompt (strcat "\n" (textDict "e_invalid_ss") "\n"));Invalid inputted ss
  ) 
  (princ)
)

(defun c:SouthVtxStat ()
  (main (ssget ":N"))
)

(defun c:SouthVtxStatX ()
  (main (ssget "X"))
)

(defun c:SouthVtxStat_Help ()
  (prompt
    (strcat
      "\n================================================================\n"
      (textDict "i_copyright") "\n" (textDict "i_self_statement")
      "\n================================================================\n"
    )
  )
  (princ)
)

;Global Initializing
(setq
  g_isUnicode (chkCharSet)
  g_isCHS (if (or (= "ANSI_936" (getvar "SYSCODEPAGE")) (= "ANSI_936" (getvar "DWGCODEPAGE"))) T nil)
  g_textDict (alterLang)
)
