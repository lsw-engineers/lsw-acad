;sec_sym.lsp                          `
;
;--------* Stone & Webster Engineering Corp
;--------* CAD/CAE
;--------* mechanical Application
;--------* 031396                       jdc
;
;
;--------* Contains the code to draw symbology used for sections
;
;--------* flag left
;
(defun flagl (/ in1 in2 bor pinb h_ce h_lt args)
;
;--------* initialize error functions
;
(setq h_ce (getvar "cecolor")
      h_lt (getvar "celtype")
)
(adderr
       '(setvar "cecolor" h_ce)
)
(adderr
       '(setvar "celtype" h_lt)
)
 (_chkdsc)
 (setq pinb (findfile "gpfl200f.dwg") ;---------* flag block used by lisp routine
       in1 (getpoint "\nSection Flag line start: ")
       in2 (getpoint in1 "\nSection Flag line end: ")
       bor (angtoc (+(angle in1 in2) pi))
 );end setq
       (command "linetype" "s" "continuous" "")
       (command "color" "bylayer")
       (command "line" in1 in2 "")
       (command "insert" pinb in2 #dsc "" bor)
 (reset nil)
 (princ)
 )
;
;-------* flag right
;
(defun flagr (/ in1 in2 bor pinb h_ce h_lt args)
; 
;--------* initialize error functions
;
(setq h_ce (getvar "cecolor")
      h_lt (getvar "celtype")
)
(adderr
       '(setvar "cecolor" h_ce)
)
(adderr
       '(setvar "celtype" h_lt)
)
 (_chkdsc)
 (setq pinb (findfile "gpfl100f.dwg")
       in1 (getpoint "\nSection Flag line start: ")
       in2 (getpoint in1 "\nSection Flag line end: ")
       bor (angtoc (angle in1 in2))
  );end setq
       (command "linetype" "s" "continuous" "")
       (command "color" "bylayer")
       (command "line" in1 in2 "")
       (command "insert" pinb in2 #dsc "" bor)
 (reset nil)
 (princ)
 );end defun
;
;--------* section cut 1 field, left
;
(defun scut1l (/ in1 in2 bor pinb h_ce h_lt args)
;
;--------* initialize error functions
;
(setq h_ce (getvar "cecolor")
      h_lt (getvar "celtype")
)
(adderr
       '(setvar "cecolor" h_ce)
)
(adderr
       '(setvar "celtype" h_lt)
)
 (_chkdsc)
 (setq pinb (findfile "gpsc100f.dwg")
       in1 (getpoint "\nSection cut line start: ")
       in2 (getpoint in1 "\nSection cut line end: ")
       bor (ANGTOC (+ (angle in1 in2) pi))
 );end setq
 (command "linetype" "s" "continuous" "")
 (command "color" "bylayer")
 (command "line" in1 (polar in1 (angle in1 in2)(- (distance in1 in2)(* 0.28125 #dsc))) "")
 (command "style" "title" "simplex" (* (/ 1.0 4.0) #dsc) 0.9 "" "N" "N" "N")
 (command "color" "bylayer")
 (setq fxt (getstring "\nSection designation: "))
 (command "text" "j" "MCenter" in2 0 fxt)
 (command "INSERT" pinb in2 #dsc "" bor)
 (reset nil)
 (princ)
);end defun
;
;--------* section cut 1 field, right
;
(defun scut1r (/ in1 in2 bor pinb h_ce h_lt args)
;
;--------* initialize error functions
;
(setq h_ce (getvar "cecolor")
      h_lt (getvar "celtype")
)
(adderr
       '(setvar "cecolor" h_ce)
)
(adderr
       '(setvar "celtype" h_lt)
)
 (_chkdsc)
 (setq pinb (findfile "gpsc100f.dwg")
       in1 (getpoint "\nSection cut line start: ")
       in2 (getpoint in1 "\nSection cut line end: ")
       bor (ANGTOC (angle in1 in2))
 );end setq
 (command "linetype" "s" "continuous" "")
 (command "color" "bylayer")
 (command "line" in1 (polar in1 (angle in1 in2)(- (distance in1 in2)(* 0.28125 #dsc))) "")
 (command "style" "title" "simplex" (* (/ 1.0 4.0) #dsc) 0.9 "" "N" "N" "N")
 (command "color" "bylayer")
 (setq fxt (getstring "\nSection designation: "))
 (command "text" "j" "MCenter" in2 0 fxt)
 (command "INSERT" pinb in2 #dsc "" bor)
 (reset nil)
 (princ)
);end defun
;
;--------* section cut 2 fields, left
;
(defun scut2l (/ in1 in2 in3 in4 in5 in6 bor pinb h_ce h_lt args)
;
;--------* initialize error functions
;
(setq h_ce (getvar "cecolor")
      h_lt (getvar "celtype")
      h_osm (getvar "osmode")
)
(adderr
       '(setvar "cecolor" h_ce)
)
(adderr
       '(setvar "celtype" h_lt)
)
(adderr
       '(setvar "osmode" h_osm)
)
 (_chkdsc)
 (setq pinb (findfile "gpsc100f.dwg")
       in1 (getpoint "\nSection cut line start: ")
       in2 (getpoint in1 "\nSection cut line end: ")
       in3 (polar in2 0 (* (/ 9.0 32.0) #dsc))
       in4 (polar in2 (* pi 1.0) (* (/ 9.0 32.0) #dsc))
       in5 (polar in2 (* pi 0.5) (* (/ 3.0 32.0) #dsc))
       in6 (polar in2 (* pi 1.5) (* (/ 3.0 32.0) #dsc))
       bor (ANGTOC (+(angle in1 in2) pi))
 );end setq
 (command "linetype" "s" "continuous" "")
 (command "style" "notes" "simplex" (* (/ 1.0 8.0) #dsc) 0.9 "" "N" "N" "N")
 (command "color" "bylayer")
 (command "line" in1 (polar in1 (angle in1 in2)(- (distance in1 in2)(* 0.28125 #dsc))) "")
 (command "line" in3 in4 "")
 (command "color" "bylayer")
 (setq fxt (getstring "\nSection designation: "))
 (command "_.text" "j" "MCenter" in5 0 fxt)
 (setq fxt (getstring "\nReference: "))
 (command "_.text" "j" "MCenter" in6 0 fxt)
 (command "INSERT" pinb in2 #dsc "" bor)
 (reset nil)
 (princ)
);end defun
;
;--------* section cut 2 fields, right
;
(defun scut2r (/ in1 in2 in3 in4 in5 in6 bor pinb h_ce h_lt args)
;
;--------* initialize error functions
;
(setq h_ce (getvar "cecolor")
      h_lt (getvar "celtype")
      h_osm (getvar "osmode")
)
(adderr
       '(setvar "cecolor" h_ce)
)
(adderr
       '(setvar "celtype" h_lt)
)
(adderr
       '(setvar "osmode" h_osm)
)
 (_chkdsc)
 (setq pinb (findfile "gpsc100f.dwg")
       in1 (getpoint "\nSection cut line start: ")
       in2 (getpoint in1 "\nSection cut line end: ")
       in3 (polar in2 0 (* (/ 9.0 32.0) #dsc))
       in4 (polar in2 (* pi 1.0) (* (/ 9.0 32.0) #dsc))
       in5 (polar in2 (* pi 0.5) (* (/ 3.0 32.0) #dsc))
       in6 (polar in2 (* pi 1.5) (* (/ 3.0 32.0) #dsc))
       bor (ANGTOC (angle in1 in2))
 );end setq
 (command "linetype" "s" "continuous" "")
 (command "style" "notes" "simplex" (* (/ 1.0 8.0) #dsc) 0.9 "" "N" "N" "N")
 (command "color" "bylayer")
 (command "line" in1 (polar in1 (angle in1 in2)(- (distance in1 in2)(* 0.28125 #dsc))) "")
 (command "line" in3 in4 "")
 (command "color" "bylayer")
 (setq fxt (getstring "\nSection designation: "))
 (command "_.text" "j" "MCenter" in5 0 fxt)
 (setq fxt (getstring "\nReference: "))
 (command "_.text" "j" "MCenter" in6 0 fxt)
 (command "INSERT" pinb in2 #dsc "" bor)
 (reset nil)
 (princ)
);end defun
;
;--------* ingpb inserts general blocks from dialog
;
(defun ingpb (b_name1 /)
 (setq b_name (findfile (strcat b_name1 ".dwg"))
       h_ada (getvar "attdia")
 )
 (setvar "attreq" 0)
 (setvar "attdia" 1)
 (adderr '(setvar "attdia" h_ada))
 (adderr '(setvar "attreq" 1))
 (setvar "cmdecho" 1)
 (command "_.insert" b_name)
;
;--------* if interested this is an infinite pause while a
;--------* command is active
;
    (while
       (eq 1 (logand 1 (getvar "cmdactive")))
       (command pause)
    )
    (if (and (/= b_name1 "gpsb1000")(/= b_name1 "gpsb2000")(/= b_name1 "gpna100f"))
        (progn
              (command "_.ddatte" "l")
              (while
                   (eq 1 (logand 1 (getvar "cmdactive")))
                   (command pause)
              )
        )
    )   
   (reset nil)
   (princ)
)   
(princ)
