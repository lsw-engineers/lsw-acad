(defun c:J ()
(command "insert" "EPJBOX00" PAUSE dwgsc dwgsc 0)
(princ))

(DEFUN C:TD ()
     (prompt "\nNow Inserting 'telephone/data receptacle <one half solid filled>:' ")
     (setvar "orthomode" 1)
     (f:slay "E-POWR-WALL-NEWW")
     (command "insert" "pow63" "s" dwgsc "qui,nea" pause pause)
(f:rlay)
(princ))

(defun c:OUT ()
(c:E065)
(princ))

(defun c:QUAD ()
(c:E072)
(princ))

(defun c:SW ()
(c:E084)
(princ))

(defun c:SW3 ()
(c:E077)
(princ))

(DEFUN C:1N ()
     (COMMAND "INSERT" "POW1N" "S" dwgsc "QUI,NEA" PAUSE PAUSE)
(PRINC))

(DEFUN C:1H ()
     (COMMAND "INSERT" "POW1H" "S" dwgsc "QUI,NEA" PAUSE PAUSE)
(PRINC))

(DEFUN C:1G ()
     (COMMAND "INSERT" "POW1G" "S" DWGSC "MID" PAUSE "CEN" "@")
     (COMMAND "ROTATE" "L" "" "@" "90")
(PRINC))

(DEFUN C:2 ()
     (COMMAND "INSERT" "POW2" "S" DWGSC "MID" PAUSE "CEN" "@")
     (COMMAND "ROTATE" "L" "" "@" "90")
(PRINC))

(DEFUN C:2G ()
     (COMMAND "INSERT" "POW2G" "S" DWGSC "MID" PAUSE "CEN" "@")
     (COMMAND "ROTATE" "L" "" "@" "90")
(PRINC))

(DEFUN C:2G2 ()
     (COMMAND "INSERT" "POW2G2" "S" DWGSC "MID" PAUSE "CEN" "@")
     (COMMAND "ROTATE" "L" "" "@" "90")
(PRINC))

(DEFUN C:3 ()
     (COMMAND "INSERT" "POW3" "S" DWGSC "MID" PAUSE "CEN" "@")
     (COMMAND "ROTATE" "L" "" "@" "90")
(PRINC))

(DEFUN C:3G ()
     (COMMAND "INSERT" "POW3G" "S" DWGSC "MID" PAUSE "CEN" "@")
     (COMMAND "ROTATE" "L" "" "@" "90")
(PRINC))

(DEFUN C:3G3 ()
     (COMMAND "INSERT" "POW3G3" "S" DWGSC "MID" PAUSE "CEN" "@")
     (COMMAND "ROTATE" "L" "" "@" "90")
(PRINC))

(DEFUN C:4 ()
     (COMMAND "INSERT" "POW4" "S" DWGSC "MID" PAUSE "CEN" "@")
     (COMMAND "ROTATE" "L" "" "@" "90")
(PRINC))

(DEFUN C:4G ()
     (COMMAND "INSERT" "POW4G" "S" DWGSC "MID" PAUSE "CEN" "@")
     (COMMAND "ROTATE" "L" "" "@" "90")
(PRINC))

(DEFUN C:5G ()
     (COMMAND "INSERT" "POW5G" "S" DWGSC "MID" PAUSE "CEN" "@")
     (COMMAND "ROTATE" "L" "" "@" "90")
(PRINC))

(defun c:sed2 (/ pnt1 pnt2 cir1 cir2 om)
     (setq om (getvar "orthomode"))
     (setvar "orthomode" 0)
     (command "osnap" "qua")
     (setq pnt1 (getpoint "\nbeginning arc point: "))
     (setq pnt2 (getpoint "\nending arc point: "))
     (command "osnap" "non")
     (command "circle" "cen" pnt1 "qua" pnt1)
     (setq cir1 (entlast))
     (command "circle" "cen" pnt2 "qua" pnt2)
     (setq cir2 (entlast))
     (command "arc" "cen" pnt1 "e" "cen" pnt2 "d" pause)
     (command "trim" "auto" cir1 "" "cen" pnt1 "")
     (command "trim" "auto" cir2 "" "cen" pnt2 "")
     (command "erase" cir1 cir2 "")
     (command "redraw")
     (setvar "orthomode" om)
(f:rlay)
(princ))
