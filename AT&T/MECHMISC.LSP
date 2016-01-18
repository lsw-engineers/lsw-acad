;misc
;
;--------* Stone & Webster Engineering Corp.
;--------* CAD/CAE
;--------* Mechanical application
;--------* 022896                        jdc
;
;--------* miscellaneous mechnical pieces
;
;
;--------* pipe break plan
;
(defun c:yy ( / sz inp pt1 pt2 stmp osm aun)
 (if (not initerr)
     (load (findfile "errcntl.lsp"))
 )
 (initerr)
 (setq args '(sz inp pt1 pt2 stmp osm aun args))
;
;--------* store variables for error control
; 
 (setq osm (getvar "osmode")
       aun (getvar "aunits")
 )
 (adderr '(setvar "osmode" osm))
 (adderr '(setvar "aunits" aun))
 (setvar "osmode" 0)
 (setvar "aunits" 0)
 (setq sz (getreal "\nPipe Diameter : ")
       inp (getpoint "\nInsertion point: ")
       pt1 (polar inp (* pi 0.5) (/ sz 2.0))
       pt2 (polar pt1 (* pi 1.5) sz)
 )
 (setq stmp (ssadd))
 (command "pline" pt1 "a" "d" "180" pt2 inp pt1 "")
 (ssadd (entlast) stmp)
 (command "hatch" "u" "45" "0.25" "n" stmp "")
 (ssadd (entlast) stmp)
 (command "rotate" stmp "" inp)
 (while
       (eq 1 (logand 1 (getvar "cmdactive")))
       (command pause)
 )
 (setq stmp nil)
 (reset nil)
 (princ)
)
;
;--------* pipend section
;
(defun c:pipend ( / args osm aun sz inp cp1 cp2 pt1 pt2 pt3
                    pt4 pt5 pt6 pt7 pt8 pt9 pt10 pt11 pt12
                    pt13 pt14 pt15 pt16 pt17 pt18 pt19 pt20
                    pt21 pt22 pt23)
  (if (not initerr)
     (load (findfile "errcntl.lsp"))
 )
 (initerr)
 (setq args '(osm aun sz inp cp1 cp2 pt1 pt2 pt3
              pt4 pt5 pt6 pt7 pt8 pt9 pt10 pt11 pt12
              pt13 pt14 pt15 pt16 pt17 pt18 pt19 pt20
              pt21 pt22 pt23 args)
 )
;
;--------* store variables for error control
;
 (setq osm (getvar "osmode")
       aun (getvar "aunits")
 )
 (adderr '(setvar "osmode" osm))
 (adderr '(setvar "aunits" aun))
 (setvar "osmode" 0)
 (setvar "aunits" 0)
 (setq sz (getreal "\nPipe Diameter : ")
       inp (getpoint "\nInsertion point: ")
       cp1 (polar inp (* pi 1.5)(/ sz 4.0))
       pt1 (list (car (polar cp1 (* pi 0.375)(/ sz 8.0)))
                 (cadr (polar cp1 (* pi 0.375)(/ sz 4.0)))
           )
       pt2 (list (car (polar cp1 (* pi 0.25)(/ sz 8.0)))
                 (cadr (polar cp1 (* pi 0.25)(/ sz 4.0)))
           )
       pt3 (list (car (polar cp1 (* pi 0.125)(/ sz 8.0)))
                 (cadr (polar cp1 (* pi 0.125)(/ sz 4.0)))
           )
       pt4 (polar cp1 (* pi 2.0)(/ sz 8.0))
       pt5 (list (car (polar cp1 (* pi 1.875)(/ sz 8.0)))
                 (cadr (polar cp1 (* pi 1.875)(/ sz 4.0)))
           )
       pt6 (list (car (polar cp1 (* pi 1.75)(/ sz 8.0)))
                 (cadr (polar cp1 (* pi 1.75)(/ sz 4.0)))
           )
       pt7 (list (car (polar cp1 (* pi 1.625)(/ sz 8.0)))
                 (cadr (polar cp1 (* pi 1.625)(/ sz 4.0)))
           )
       pt8 (polar cp1 (* pi 1.5)(/ sz 4.0))
       cp2 (polar pt8 (* pi 1.5)(/ sz 4.0))
       pt9 (list (car (polar cp2 (* pi 0.625)(/ sz 8.0)))
                 (cadr (polar cp2 (* pi 0.625)(/ sz 4.0)))
           )
       pt10 (list (car (polar cp2 (* pi 0.75)(/ sz 8.0)))
                  (cadr (polar cp2 (* pi 0.75)(/ sz 4.0)))
           )
       pt11 (list (car (polar cp2 (* pi 0.875)(/ sz 8.0)))
                  (cadr (polar cp2 (* pi 0.875)(/ sz 4.0)))
           )
       pt12 (polar cp2 pi (/ sz 8.0))
       pt13 (list (car (polar cp2 (* pi 1.125)(/ sz 8.0)))
                  (cadr (polar cp2 (* pi 1.125)(/ sz 4.0)))
           )
       pt14 (list (car (polar cp2 (* pi 1.25)(/ sz 8.0)))
                  (cadr (polar cp2 (* pi 1.25)(/ sz 4.0)))
           )
       pt15 (list (car (polar cp2 (* pi 1.375)(/ sz 8.0)))
                  (cadr (polar cp2 (* pi 1.375)(/ sz 4.0)))
           )
       pt16 (polar cp2 (* pi 1.5)(/ sz 4.0))
       pt17 (list (car (polar cp2 (* pi 1.625)(/ sz 8.0)))
                  (cadr (polar cp2 (* pi 1.625)(/ sz 4.0)))
           )
       pt18 (list (car (polar cp2 (* pi 1.75)(/ sz 8.0)))
                  (cadr (polar cp2 (* pi 1.75)(/ sz 4.0)))
           )
       pt19 (list (car (polar cp2 (* pi 1.875)(/ sz 8.0)))
                  (cadr (polar cp2 (* pi 1.875)(/ sz 4.0)))
           )
       pt20 (polar cp2 (* pi 2.0)(/ sz 8.0))
       pt21 (list (car (polar cp2 (* pi 0.125)(/ sz 8.0)))
                  (cadr (polar cp2 (* pi 0.125)(/ sz 4.0)))
           )
       pt22 (list (car (polar cp2 (* pi 0.25)(/ sz 8.0)))
                  (cadr (polar cp2 (* pi 0.25)(/ sz 4.0)))
           )
       pt23 (list (car (polar cp2 (* pi 0.375)(/ sz 8.0)))
                  (cadr (polar cp2 (* pi 0.375)(/ sz 4.0)))
           )
 )
 (command "pline" inp "a" "s" pt1 pt2 "s" pt3 pt4 "s" pt5 pt6 "s" pt7 pt8
                  "s" pt9 pt10 "s" pt11 pt12 "s" pt13 pt14 "s" pt15 pt16
                  "s" pt17 pt18 "s" pt19 pt20 "s" pt21 pt22 "s" pt23 pt8 "")
 (command "rotate" "l" "" inp)
 (while
       (eq 1 (logand 1 (getvar "cmdactive")))
       (command pause)
 )
 (reset nil)
 (princ)
)
;
;--------* pipe double line
;
(defun c:pipe ( / sz sz2 ang pt1 pt2 osm aun args)
 (if (not initerr)
     (load (findfile "errcntl.lsp"))
 )
 (initerr)
 (setq args '(sz sz2 ang pt1 pt2 osm aun args))
;
;--------* store variables for error control
; 
 (setq osm (getvar "osmode")
       aun (getvar "aunits")
 )
 (adderr '(setvar "osmode" osm))
 (adderr '(setvar "aunits" aun))
 (setvar "osmode" 0)
 (setvar "aunits" 0)
      (setq sz (getreal "\nPipe Diameter : ")
            pt1 (getpoint "\nFrom point: ")
            pt2 (getpoint "\nTo point: " pt1)  ;Second point
            ang (angle pt1 pt2)              ;Calc the angle
            sz2 (/ sz 2.0)
        );setq
        ;; Draw parallel lines for the double line pipe
        (command "line" (polar pt1 (+ ang (* pi 0.50)) sz2)
                 (polar pt2 (+ ang (* pi 0.50)) sz2) ""
                 "line" (polar pt1 (+ ang (* pi 1.50)) sz2)
                 (polar pt2 (+ ang (* pi 1.50)) sz2) "")
 (reset nil)
(princ)
)
