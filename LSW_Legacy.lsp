;;
;;  lsw_legacy - Retired LSW legacy lisp routines.
;;
;;  Copyright © 2003 by LSW Engineers Arizona, Inc.
;;
;;  
;;-------------------------------------------------------------------------
;;  DESCRIPTION
;;      Commands found herein have been removed from LSW's acad.lsp during
;;      the AutoCAD 2000 -> AutoCAD 2004 upgrade cycle.  These lisp
;;      routines have been retained for optional use by employees.  Add
;;      a line to your personal aset.lsp file as follows to have access
;;      to these commands:
;;                            (load "lsw_legacy")
;;-------------------------------------------------------------------------
;;

(setq v:acal "L:/lisp/")

(DEFUN FLOAD (QQO)
(setvar "menuecho" 1)
(load (strcat v:acal qqo))
(setq qqo nil))

(DEFUN F:LODF (QQO)
 (setvar "menuecho" 3)
 (cond 
      ( (null (eval (read qqo)))
        (setvar "menuecho" 1)
        (terpri)
        (load (strcat (substr qqo 1 1) (substr qqo 3 (strlen qqo))))
       )
 )
 (setvar "menuecho" 1)
 (setq qqo nil)
 )

(DEFUN C:4 (/ SS1)
(setq ss1 (ssget))
(command "change" ss1 "" "p" "c" "4" "")
(princ))

(DEFUN C:7 (/ SS1)
(setq ss1 (ssget))
(command "change" ss1 "" "p" "c" "7" "")
(princ))

(DEFUN C:8 (/ SS1)
(setq ss1 (ssget))
(command "change" ss1 "" "p" "c" "8" "")
(princ))

(DEFUN C:9 (/ SS1)
(setq ss1 (ssget))
(command "change" ss1 "" "p" "c" "9" "")
(princ))

(DEFUN C:10 (/ SS1)
(setq ss1 (ssget))
(command "change" ss1 "" "p" "c" "10" "")
(princ))

(DEFUN C:12 (/ SS1)
(setq ss1 (ssget))
(command "change" ss1 "" "p" "c" "12" "")
(princ))

(DEFUN C:14 (/ SS1)
(setq ss1 (ssget))
(command "change" ss1 "" "p" "c" "14" "")
(princ))

(DEFUN C:15 (/ SS1)
(setq ss1 (ssget))
(command "change" ss1 "" "p" "c" "15" "")
(princ))

(DEFUN C:146 (/ SS1)
(setq ss1 (ssget))
(command "change" ss1 "" "p" "c" "146" "")
(princ))

(DEFUN C:214 (/ SS1)
(setq ss1 (ssget))
(command "change" ss1 "" "p" "c" "214" "")
(princ))

(DEFUN C:12H (/ SS1)
(setq ss1 (ssget))
(command "change" ss1 "" "p" "c" "12" "lt" "hidden" "")
(princ))

(DEFUN C:12H1 (/ SS1)
(setq ss1 (ssget))
(command "change" ss1 "" "p" "c" "12" "lt" "hidden2" "")
(princ))

(DEFUN C:A ()
(setvar "pickbox" 5)
(setvar "aperture" 5)
(princ))

(DEFUN C:AC ()
  (setq defltAC (cond ((> defltAC "") defltAC)(T "")))
  (setq in1 (getstring T
      (strcat "\nNew Attribute Value <" defltAC ">: "))
    defltAC (cond ((= in1 " ") "")((> in1 "") in1)(T defltAC)))
(prompt "Pick Attribute to Modify: ")
(command "attedit" "Y" "*" "*" "*" pause "" "v" "r" defltac "")
(princ))

(DEFUN C:ARR ()
(setvar "cmdecho" 0)
(command "array")
(menucmd "s=opt")
(princ))

(DEFUN AUTOSAVE ()
(setq time (getvar "tdusrtimer"))
(if (> time 0.0105) (c:save))
(princ))

(DEFUN C:CHT ()
  (setq str (cond ((> str "") str)(T "")))
  (setq in1 (getstring T
      (strcat "\nWhat is the New String to be?  <" str ">: "))
    str (cond ((= in1 " ") "")((> in1 "") in1)(T str)))
(prompt "Select Object: ")
(command "select" "single" pause)
(setvar "cmdecho" 0)
(command "change" "p" "" "" "" "" "" "" str)
(princ))

(DEFUN C:CM ()
(prompt "\nSelect objects for Multiple Copy: ")
(setq cm (ssget))
(prompt "\nBase point of displacement: ")
(command "copy" cm "" "m" pause pause)
(princ))

(defun C:CT (/ p l n e os as ns st s nsl osl sl si chf chm cont)
(MENUCMD "S=OPT")
(SETQ chm 0 p (SSGET)) 
(MENUCMD "S=AU001S1")
   (if p (progn
      (SETQ cont t) 
      (while cont
         (SETQ osl (strlen (SETQ os (getstring "\nOld string: " t))))
         (if (= osl 0)
            (princ "Null input invalid")
            (SETQ cont nil)))
      (SETQ nsl (strlen (SETQ ns (getstring "\nNew string: " t))))
      (SETQ l 0 n (sslength p))
      (while (< l n)      
         (if (= "TEXT" (cdr (assoc 0 (SETQ e (entget (ssname p l))))))
            (progn
               (SETQ chf nil si 1)
               (SETQ s (cdr (SETQ as (assoc 1 e))))
               (while (= osl (SETQ sl (strlen
                             (SETQ st (substr s si osl)))))
                  (if (= st os)
                      (progn (SETQ s (strcat (substr s 1 (1- si)) ns
                                        (substr s (+ si osl))))
                        (SETQ chf t) 
                        (SETQ si (+ si nsl)))
                      (SETQ si (1+ si))))
               (if chf (progn (SETQ e (subst (cons 1 s) as e))
                  (entmod e) 
                  (SETQ chm (1+ chm))))))
         (SETQ l (1+ l)))))
(princ "Changed ") (princ chm) (princ " text lines.") 
(terpri)
(PRINC))

(DEFUN C:CTS ()
(fload "#load")
(#load "cts")
(princ))

(DEFUN C:DIV ()
(setvar "orthomode" 1)
(command "layer" "m" "const" "")
(prompt "\nDraw line to DIV: ")
(command "line" pause pause "")
(prompt "\nSelect object to Divide: ")
(setq ss (entsel))
;(setq div1 (getreal "\nNumber of Lights: (input lights at every other node) "))
;(setq div2 (* div1 2))
(setvar "pdmode" 3)
(prompt "\nEnter number of Lights multiplied by 2: ")
(prompt "\n<input lights at every other node>. ")
(command "divide" ss pause)
;(setvar "pdmode" 0)
(princ))

(DEFUN C:DL ()
(fload "dline")
(c:dline)
(princ))

(DEFUN C:DNP ()
(setvar "cmdecho" 1)
(command "dist" "qui,nea" pause "qui,perp")
(setvar "cmdecho" 0)
(princ))

(DEFUN C:EA ()
(command "arc" "qui,end" pause "e" "qui,end" pause "d")
(princ))

(DEFUN C:EL ()
(setvar "cmdecho" 0)
(command "erase" "l" "")
(princ))

(DEFUN C:ELEC ()
(load "elec")
(princ))

(DEFUN C:EON ()
(setvar "osmode" 1025)
(princ))

(DEFUN C:EOF ()
(setvar "osmode" 0)
(princ))

(DEFUN C:EPT () ;this will erase points input with C:DIV
(setq eptA (ssget "x" '((8 . "const"))))
(command "erase" eptA "")
(princ))

(DEFUN C:FC ()
(setvar "cmdecho" 0)
(command "fillet" "c")
(princ))

(DEFUN C:GR (/ SS1)
(setq ss1 (ssget))
(command "change" ss1 "" "p" "c" "3" "")
(princ))

(DEFUN C:HATC ()
(setvar "cmdecho" 0)
(if c:hat nil (fload "hat"))
(graphscr)
(c:hat)
(princ))

(DEFUN C:HPROP (P V)
(prompt "Select objects: ")
(menucmd "s=opt")
(command "select" "auto" pause)
(command "change" "p" "" "p" p v "")
(menucmd "p5=p5a")
(princ))

(DEFUN C:HVY (/ SS)
(Setq ss (ssget))
(Command "change" ss "" "p" "la" "heavy" "c" "bylayer" "")
(Princ))

(DEFUN C:IO()
(if(=(getvar "snapstyl")0)
(setvar "snapstyl" 1)
(setvar "snapstyl" 0))
(princ (strcat "\nsnapstyl set at "
(rtos(getvar "snapstyl")20)))
(princ))

(DEFUN C:LEX () (fload "lexplode") (c:lex))

(DEFUN C:LLS () (IF LLS NIL (FLOAD "LYR-LIST")) (C:LLS) (PRINC))

(DEFUN C:LAS (/ LAS)
(setvar "cmdecho" 0)
(setq las (getstring "\nWhat is to be the New Current Layer? "))
(command "layer" "s" las "")
(princ))

(defun C:LC (/ #A #B HOLDECHO k sig)
   (repeat 3 (print))
   (setq holdecho (getvar "cmdecho"))
   (setvar "cmdecho" 0)
   (menucmd "s=opt")
   (setq #b (ssget))
   (setq #a (cdr (assoc 8
   (entget (car (entsel "\nPick LAYER SELECTION Entity: "))))))
   (command "change" #b "" "p" "la" #a "")
   (print)
   (prompt (strcat "\nSelected entity has been changed to " #A " layer."))
   (setvar "cmdecho" holdecho)
(princ))

(DEFUN C:LCC () (IF LCC NIL (FLOAD "LYR-LCC")) (C:LCC) (PRINC))

(DEFUN C:LCX () (IF LCX NIL (FLOAD "LYR-LCX")) (C:LCX) (PRINC))

(DEFUN C:LE ()
(Fload "dellayer.lsp")
(C:le))

(DEFUN C:LGT (/ SS)
(setq ss (ssget))
(command "change" ss "" "p" "la" "light" "c" "bylayer" "")
(princ))

(defun C:LF (/ #a #b holdecho k sig)
(setq holdecho (getvar "cmdecho"))
(setvar "cmdecho" 0)
(setq #a (cdr (assoc 8
(entget (car (entsel "\nPick entity on Layer to FREEZE: "))))))
(command "layer" "f" #a "")
(prompt (strcat "\nLayer " #A " has been FROZEN."))
(princ))

(DEFUN C:LK ()
(setvar "cmdecho" 0)
(setq cl (getvar "clayer"))
(command "layer" "lock" "*" "unlock" cl "")
(princ))

(defun C:LL (/ #a #b holdecho k sig)
(setq holdecho (getvar "cmdecho"))
(setvar "cmdecho" 0)
(setq #a (cdr (assoc 8
(entget (car (entsel "\nPick entity on Layer to LOCK: "))))))
(command "layer" "lock" #a "")
(prompt (strcat "\nLayer " #A " has been LOCKED."))
(princ))

(DEFUN C:LM ()
(fload "loadmac")
(c:loadmac)
(princ))

(DEFUN C:LMC () (IF LMC NIL (FLOAD "LYR-LMC")) (C:LMC) (PRINC))

(DEFUN C:LMX () (IF LMX NIL (FLOAD "LYR-LMX")) (C:LMX) (PRINC))

(defun C:LO (/ #A #B HOLDECHO k sig)
(setq holdecho (getvar "cmdecho"))
(setvar "cmdecho" 0)
(setq #a (cdr (assoc 8
(entget (car (entsel "\nPick entity on Layer to turn OFF: "))))))
(command "layer" "off" #a "")
(prompt (strcat "\nLayer "#A" has been turned OFF."))
(princ))

(defun C:LS (/ #a holdecho)
  (setq holdecho (getvar "cmdecho"))
  (setvar "cmdecho" 0)
  (setq	#a
	 (cdr
	   (assoc 8
		  (entget (car (entsel "\nPick entity to set LAYER : ")))
	   )
	 )
  )
  (command "layer" "set" #a "")
  (eval #a)
  (prompt (strcat "\nACTIVE layer has been set to " #A "."))
  (setvar "cmdecho" holdecho)
  (princ)
)


(defun C:LU (/ #a #b holdecho k sig)
(setq holdecho (getvar "cmdecho"))
(setvar "cmdecho" 0)
(setq #a (cdr (assoc 8
(entget (car (entsel "\nPick entity on Layer to UNLOCK: "))))))
(command "layer" "unlock" #a "")
(prompt (strcat "\nLayer " #A " has been UNLOCKED."))
(princ))

(DEFUN C:MA ()
(command "layer" "s" "matchline" "")
(setq plw (* (getvar "dimscale") 0.09375))
(prompt "\nPick first point of Matchline: ")
(command "pline" pause "w" plw plw)
(princ))

(DEFUN C:MAG (/ SS1)
(setq ss1 (ssget))
(command "change" ss1 "" "p" "c" "6" "")
(princ))

(DEFUN C:MED (/ SS)
(setq ss (ssget))
(command "change" ss "" "p" "la" "medium" "c" "bylayer" "")
(princ))

(defun MW ()
   (setq a (getpoint "\nMidway between point: "))
   (polar
     (setq b (getpoint a "    and point: "))
     (angle b a)
     (/ (distance b a) 2)
   )
)

(DEFUN C:N ()
(f:slay "note-new")
(c:dt)
(c:sl)
(princ))

(DEFUN C:ON ()
(setvar "cmdecho" 0)
(command "layer" "on" "*" "")
(princ))

(DEFUN C:P0 ()
  (setq distm0 (cond ((> distm0 "") distm0)(t "")))
  (setq in1 (getstring
      (strcat "\nDistance of displacement <" DISTM0 "> ? "))
    distm0 (cond ((= in1 " ") "")((> in1 "") in1)(t distm0)))
(command "PAN" "@" (strcat "@" distm0 "<0"))
(princ))

(DEFUN C:P1 ()
  (setq distm0 (cond ((> distm0 "") distm0)(t "")))
  (setq in1 (getstring
      (strcat "\nDistance of displacement <" DISTM0 "> ? "))
    distm0 (cond ((= in1 " ") "")((> in1 "") in1)(t distm0)))
(command "PAN" "@" (strcat "@" distm0 "<90"))
(princ))

(DEFUN C:P2 ()
  (setq distm0 (cond ((> distm0 "") distm0)(t "")))
  (setq in1 (getstring
      (strcat "\nDistance of displacement <" DISTM0 "> ? "))
    distm0 (cond ((= in1 " ") "")((> in1 "") in1)(t distm0)))
(command "PAN" "@" (strcat "@" distm0 "<180"))
(princ))

(DEFUN C:P3 ()
  (setq distm0 (cond ((> distm0 "") distm0)(t "")))
  (setq in1 (getstring
      (strcat "\nDistance of displacement <" DISTM0 "> ? "))
    distm0 (cond ((= in1 " ") "")((> in1 "") in1)(t distm0)))
(command "PAN" "@" (strcat "@" distm0 "<270"))
(princ))

(DEFUN C:PHA (/ SS1)
(setq ss1 (ssget))
(command "change" ss1 "" "p" "lt" "phantom" "")
(princ))

(DEFUN C:PU ()
(command "purge" "a" "" "n")
(command "purge" "a" "" "n")
(command "purge" "a" "" "n")
;  Redundant purges added for better cleanup - AJB [01/28/03]
(princ))

(DEFUN C:PLOTDATE ()
(fload "plotdate")
(c:plotdate)
(princ))

(DEFUN C:PLDT ()
(c:plotdate)
(princ))

(DEFUN C:RA ()
(COMMAND "REGENALL")
(PRINC))

(DEFUN C:RB ()
(command "change" "single" pause "" "" "drag" pause)
(princ))

(DEFUN C:RED (/ SS1)
(setq ss1 (ssget))
(command "change" ss1 "" "p" "c" "1" "")
(princ))

(DEFUN C:S ()
(fload "s.lsp")
(princ))

(DEFUN C:S12 ()
(setvar "cmdecho" 0)
(prompt "\nSaving Drawing as Release 12.")
(Prompt "\nPlease Wait...." )
(f:slay "0")
(command ".time" "r" "") (graphscr)
(setq fname (getvar "dwgname"))
(command ".qsave")
(command ".saveasr12" fname "y")
(princ))

(DEFUN C:S13 ()
(command ".saveas" "r13" "" "y")
(PROMPT "\nDrawing saved in R13 Format")
(princ))


(DEFUN C:SA ()
(COMMAND "QSAVE")
(prompt "\nSaving Drawing. Please Wait....")
(princ))

(DEFUN C:SAVE ()
(C:SA)
(prompt "\nSaving Drawing. Please Wait....")
(princ))

(DEFUN C:SCR ()
(COMMAND ".SCRIPT")
(princ))

(DEFUN C:SHV ()
(command "layer" "s" "heavy" "")
(princ))

(DEFUN C:SK ()
(setvar "skpoly" 1)
(command "sketch")
(princ))

(DEFUN C:SLT ()
(command "layer" "s" "light" "")
(princ))

(DEFUN C:SMED ()
(command "layer" "s" "medium" "")
(princ))

(DEFUN C:SPELL ()
(fload "spell")
(c:spell)
(princ))

(DEFUN C:T0 ()
(command "change" pause "" "" "" "" "" 0 "")
(princ))

(DEFUN C:T90 ()
(command "change" pause "" "" "" "" "" 90 "")
(princ))

(DEFUN C:TR ()
(setvar "cmdecho" 0)
(command "trim" "auto")
(princ))

(DEFUN C:TH ()
(setvar "cmdecho" 0)
(command "layer" "t" "*" "")
(princ))

(DEFUN C:V ()
(command "view" "r")
(princ))

(DEFUN C:VLC ()
  (setq
    pfxVLC (cond ((> pfxVLC "") pfxVLC)(T "")))  ; Callout Prefix Character(s)
  (setq
    in1 (getstring
      (strcat "\nEnter Layer to change items to <" pfxVLC ">: ") T)
    pfxVLC (cond ((= in1 " ") "")((> in1 "") in1)(T pfxVLC)))
(setq ss1 (ssget))
(command "change" ss1 "" "p" "la" pfxVLC "")
(princ))

(defun C:VLF ()
(setq holdecho (getvar "cmdecho"))
(setvar "cmdecho" 0)
(SETQ #A (cdr (assoc 8
(entget (car (entsel "\nPick entity on Layer to FREEZE in the CURRENT viewport: "))))))
(command "vplayer" "f" #a "" "")
(prompt (strcat "\nlayer " #a " has been frozen in the current viewport."))
(princ))

(DEFUN C:VZ ()
(COMMAND "VISRETAIN")
(PRINC))

(DEFUN C:WEND ()
(c:wsave)
(command "close")
)

(DEFUN C:WSAVE ()
(setvar "cmdecho" 0)
(Prompt "\nSaving Drawing.")
(Prompt "\nPlease Wait...." )
(f:slay "0")
(command ".time" "r" "") (graphscr)
(setq fname (getvar "dwgname"))
(c:pu)
(command ".qsave")
(princ))

(DEFUN C:WSA ()
(c:wsave))

(DEFUN C:WS13 ()
(setvar "cmdecho" 0)
(Prompt "\nSaving Drawing 'Wblocked as R13 file'.")
(Prompt "\nPlease Wait...." )
(f:slay "0")
(command ".time" "r" "") (graphscr)
(setq fname (getvar "dwgname"))
(command "purge" "a" "*" "n")
(c:s13)
(princ))

(DEFUN C:XR()
(COMMAND "XREF")
(PRINC))

(DEFUN C:XREN ()
(command "xref" "?" "")
(setq old (getstring "\nWhat is the name of the existing XREF? "))
(SETQ NEW (GETSTRING "\nWhat is the name of the new XREF? "))
(command "rename" "bl" old new)
(command "xref" "path" new new)
(graphscr)
(princ))

(DEFUN C:Y (/ SS1)
(setq ss1 (ssget))
(command "change" ss1 "" "p" "c" "y" "")
(princ))

(DEFUN C:YH (/ SS1)
(setq ss1 (ssget))
(command "change" ss1 "" "p" "c" "y" "lt" "hidden" "")
(princ))

(DEFUN C:YH1 (/ SS1)
(setq ss1 (ssget))
(command "change" ss1 "" "p" "c" "y" "lt" "hidden2" "")
(princ))(DEFUN C:Z1 ()
(command "zoom" "w" (getvar "extmin") (getvar "extmax"))
(princ))

(DEFUN C:Z2 ()
(command "zoom" 2)
(princ))

(DEFUN C:Z3 ()
(command "zoom" 4)
(princ))

(DEFUN C:Z4 ()
(command "zoom" 10)
(princ))

(DEFUN C:Z5 ()
(command "zoom" 20)
(princ))

(DEFUN C:Z6 ()
(command "zoom" 40)
(princ))

(DEFUN C:ZU ()
(command "zoom" "p" "zoom" "w" "")
(princ))

(DEFUN C:ZV ()
(command "zoom" 2.75)
(princ))

