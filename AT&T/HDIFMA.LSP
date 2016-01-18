;hdifma
;
;
;--------* Stone & Webster Engineering Corp
;--------* CAD/CAE
;--------* Mechanical Application
;--------* 031296                       jdc
;
;---------* Square Diffuser
;
(defun sqr_diff (/ pt1 pt2 pt3 pt4 pt5)
 (_chkdsc)
 (inpnt)
 (setq pt1 (polar inp (* pi 1.25)(/ (/ sz 2.0)(sin (* pi 0.25))))
       pt2 (polar pt1 (* pi 2.0) sz)
       pt3 (polar inp (* pi 0.5)(+ (/ sz 2.0)(* 0.083 #dsc)))
       pt4 (polar pt3 (* pi 0.5)(* 0.40625 #dsc))
       pt5 (polar pt4 (* pi 0.5)(* 0.15625 #dsc))
 )
 (command "polygon" "" inp "c" (/ sz 2))
 (setq ent (entlast))
 (command "line" pt1 (polar pt2 (* pi 0.5) sz) "")
 (cond
      ((= ftype "sqr_p")(command "line" pt2 (polar pt1 (* pi 0.5) sz) ""))
      ((= ftype "sqr_s")(progn (command "line" pt2 (polar pt1 (* pi 0.5) sz) "")
                        (command "pline" pt3 pt4 "w" plw "0" pt5 "")
                        (ssadd (entlast) ttmp)
                        (command "array" ttmp "" "p" inp "4" "" "y")))
 )
 (rotent)
 (f:modr)
 (princ)
)
;
;---------* Round Diffuser
;
(defun rnd_diff (/ pt1 pt2 pt3 pt4 pt5)
 (_chkdsc)
 (inpnt)
 (setq pt1 (polar inp (* pi 1.25)(/ sz 2.0))
       pt2 (polar inp (* pi 1.75)(/ sz 2.0))
       pt3 (polar inp (* pi 1.25)(+ (/ sz 2.0)(* 0.083 #dsc)))
       pt4 (polar pt3 (* pi 1.25)(* 0.40625 #dsc))
       pt5 (polar pt4 (* pi 1.25)(* 0.15625 #dsc))
 )
 (command "circle" inp "d" sz)
 (setq ent (entlast))
 (command "line" pt1 (polar pt1 (* pi 0.25) sz) "")
 (cond
      ((= ftype "rnd_p")(command "line" pt2 (polar pt2 (* pi 0.75) sz) ""))
      ((= ftype "rnd_s")(progn(command "line" pt2 (polar pt2 (* pi 0.75) sz) "")
                        (command "pline" pt3 pt4 "w" plw "0" pt5 "")
                        (ssadd (entlast) ttmp)
                        (command "array" ttmp "" "p" inp "4" "" "y")))
 )
 (rotent)
 (f:modr)
 (princ)
)
;
;---------* Side Diffuser
;
(defun sid_diff (/ pt1 pt2 pt3 pt4 pt5 pt6 pt7 pt8 pt9 pt10 pt11
                     pt12 pt13 pt14)
 (_chkdsc)
 (inpnt)
 (setq pt1 (polar inp pi (/ sz 2.0))
       pt2 (polar pt1 (* pi 0.50) 4)
       pt3 (polar pt2 (* pi 2.00) sz)
       pt4 (polar pt3 (* pi 1.50) 4)
       pt5 (polar pt4 (* pi 0.25)(/ 4 (sin (* pi 0.25))))
       pt6 (polar inp (* pi 0.5)(+ 4 (* 0.083 #dsc)))
       pt7 (polar pt6 (* pi 0.5)(* 0.40625 #dsc))
       pt8 (polar pt7 (* pi 0.5)(* 0.15625 #dsc))
       pt9 (polar pt6 (* pi 0.5)(* 0.15625 #dsc))
       pt10 (polar pt9 (* pi 0.5)(* 0.125 #dsc))
       pt11 (polar pt10 (/ pi 6.0)(* 0.078125 #dsc))
       pt12 (polar pt11 (/ pi 1.2)(* 0.156250 #dsc))
       pt13 (polar pt12 (/ pi 6.0)(* 0.078125 #dsc))
       pt14 (polar pt13 (/ pi 2.0)(* 0.125 #dsc))
 )
 (command "pline" pt5 (polar pt5 pi (+ sz 8.0)) "")
 (setq ent (entlast))
 (command "pline" pt3 pt4 pt1 pt2 "")
 (cond
      ((= ftype "sid_s")
                        (command "pline" pt6 pt7 "w" plw "0" pt8 ""))
      ((= ftype "sid_e")
                        (command "pline" pt6 "w" "0" plw pt9 "w"
                                 "0" "0" pt10 pt11 pt12 pt13 pt14 ""))
 )
 (rotent)
 (f:modr)
 (princ)
)
(princ)
