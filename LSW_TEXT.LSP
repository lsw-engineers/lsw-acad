;;;;;
;;  LSW_TEXT.LSP - LSW TEXT COMMANDS AND FUNCTIONS
;;
;;  Copyright © 2008 by LSW Engineers Arizona, Inc.
;;
;;  
;;-------------------------------------------------------------------------
;;  DESCRIPTION
;;	Custom LSW commands related to text and it's properties.
;;	Broken into sections: support functions, commands that call support
;;	functions, and commands.
;;
;;
;;-------------------------------------------------------------------------
;;


;;;  /----------------------------------------------------------\
;;;  | General text support functions.				|
;;;  \----------------------------------------------------------/

 ;Add standard fonts to drawing
;;;(defun ADD_FONTS ()
;;;  (setvar "cmdecho" 0)
;;;;; commented out the proceeding line -- how can SF be set before S:STARTUP runs?
;;;;;(setvar "textsize" (* (SCALEFACTOR) 0.09375))
;;;  (command "-style" "standard" "simplex.shx" 0.0 0.85 0.0 "n" "n" "n")
;;;  (command "-style" "standard_bold" "romand.shx" 0.0 0.85 0.0 "n" "n" "n")
;;;  (command "-style" "standard_italic" "simplex.shx" 0.0 0.85 15.0 "n" "n" "n")
;;;  (command "-style" "standard_bolditalic" "romand.shx" 0.0 0.85 15.0 "n" "n" "n")
;;;  (setvar "textstyle" "Standard")
;;;  (princ)
;;;)

 ;RUNS DTEXT COMMAND ON CORRECT LAYER
(defun NOTE_TEXT (layer)
  (F:SLAY layer)
  (TEXT_SET)
  (prompt "\nText Insertion Point: ")
  (command "dtext" pause "" "")
  (F:RLAY)
  (princ)
)

 ;RUNS DTEXT COMMAND ON CORRECT LAYER, CENTERS TEXT BETWEEN TWO LINES OR INTERSECTIONS
(defun NOTE_TEXT_MIDDLE	(layer rotate osm_1 osm_2 / pt1 pts pt2 ang1 ang2)
  (F:SLAY layer)
  (F:MODS (quote ("orthomode" "osmode" "cmdecho")))
  (setvar "orthomode" 0)
  (setvar "osmode" osm_1)
  (setq pt1 (getpoint "\nEnter first point:"))
  (setvar "osmode" osm_2)
  (setq pt2 (getpoint pt1 "\nEnter second point:"))
  (setvar "osmode" 0)
  (setq pt3 (polar pt1 (angle pt1 pt2) (/ (distance pt1 pt2) 2)))
  (setq ang1 (angle pt1 pt2))
  (cond ((= rotate nil)
         (setq ang2 0)
        )
        ((and (<= (R->D ang1) 180) (> (R->D ang1) 0))
         (setq ang2 (+ (R->D ang1) 270))
        )
        (t (setq ang2 (+ (R->D ang1) 90))
        )
  )
  (TEXT_SET)
  (command "dtext" "j" "mc" pt3 "" ang2)
  (TEXT_ANG_RESET)
  (F:RLAY)
  (F:MODR)
)

 ;RUNS MTEXT COMMAND ON CORRECT LAYER
(defun NOTE_MTEXT (layer)
  (F:SLAY layer)
  (TEXT_SET)
  (initdia)
  (command "mtext" pause pause)
  (F:RLAY)
  (princ)
)

 ;reloads all block in a drawing from the file search path (network)
(defun RELOAD_BLOCKS (/ ss i ent blnm)
  (F:MODS (quote ("regenmode")))
  (setvar "regenmode" 0)
  (if (setq ss (ssget "_X" (list (cons 0 "INSERT"))))
    (progn
      (setq i -1)
      (repeat (sslength ss)
	(setq i (1+ i))
	(setq ent (entget (ssname ss i)))
	(setq blnm (cdr (assoc 2 ent)))
	(if (findfile (strcat blnm ".dwg"))
	  (progn
	    (princ (strcase blnm))
	    (command ".insert" (strcat blnm "="))
	    (command)
	    (command "attsync" "n" blnm)
	  );progn
	);if
      );repeat
    );progn
  );if
  (F:MODR)
)

 ;Set selected text to new justification
(defun SET_JUSTIFY (new_justify	   /		  select_set
		    counter	   ent_list	  new_justify_code
		    ins_pt
		   )
  (setvar "cmdecho" 0)
  (prompt "\nSelect text: ")
  (setq select_set (ssget '((0 . "TEXT"))))
  (setq counter -1)
  (repeat (sslength select_set)
    (setq ent_list
	   (entget (ssname select_set (setq counter (1+ counter)))
	   )
    )
    (if	(equal (cdr (assoc 0 ent_list)) "TEXT")
      (progn
	(setq ins_pt	       (cdr (assoc
				      (if (member (cdr (assoc 72 ent_list)) '(0 3 5))
					10
					11
				      )
				      ent_list
				    )
			       )
	      new_justify_code (if (member new_justify '(0 3 5))
				 10
				 11
			       )
	      ent_list	       (subst (cons new_justify_code ins_pt)
				      (assoc new_justify_code ent_list)
				      ent_list
			       )
	      ent_list	       (subst (cons 72 new_justify)
				      (assoc 72 ent_list)
				      ent_list
			       )
	)
	(entmod ent_list)
      ) ;progn
    ) ;if
  ) ;repeat
  (princ)
)

 ;RESET TEXT ANGLE TO 0
(defun TEXT_ANG_RESET nil
  (F:SLAY "0")
  (command "text" "0,0,0" "" 0 "ERROR")
  (command "erase" (entlast) "")
)


 ;SET TEXT STYLE and HEIGHT
(defun TEXT_SET	nil
  (setvar "cmdecho" 0)
  (cond
;;;    	((= (strcase (getvar "textstyle")) "SIMPLEX")
;;;	 (setvar "textstyle" "standard")
;;;	 (prompt "\nText style set to \"Standard.\"")
;;;	 (setvar "textsize" (* (SCALEFACTOR) 0.09375))
;;;	 (command "-style" "" "SIMPLEX.SHX" 0.0 0.85 0.0 "N" "N" "N")
;;;	)
	((= (strcase (getvar "textstyle")) "STANDARD")
	 (setvar "textsize" (* (SCALEFACTOR) 0.09375))
	 ;(command "-style" "" "SIMPLEX.SHX" 0.0 0.85 0.0 "N" "N" "N")
	)
	(t
	 (prompt
	   "\nNon-standard text style in use.  Text height not set."
	 )
	)
  )
)

 ;Updates all the width factors for text/mtext in a drawing.
(defun UPDATE_WF_and_STYLE (wf / ss i ent entsty edit1 edit2)
  (if (setq ss (ssget "_X" (list '(0 . "TEXT,MTEXT,ATTDEF,ATTRIB"))))
    (progn
      (setq i -1)
      (repeat (sslength ss)
	(setq i (1+ i))
	(setq ent (entget (ssname ss i)))
	(setq entsty (strcase (cdr (assoc 7 ent))))
	(if (and (/= entsty "MEDIUM") (/= entsty "TITLE"))
	  (progn
	    (setq ent1  (subst (cons 41 wf) (assoc 41 ent) ent))
	    (entmod ent1)
	    (setq ent2  (subst (cons 7 "standard") (assoc 7 ent1) ent1))
	    (entmod ent2)
	  )
	)
	(princ)
      )
    )
    (princ)
  )
)

 ;REDEFINE TEXT SIZE (USED IN MENU COMMAND)
(defun F:TXTSZ (/ S1)
  (setvar "CMDECHO" 0)
  (prompt "\nSelect text objects: ")
  (setq S1 (ssget '((0 . "TEXT"))))
  (setq CNT -1)
  (repeat (sslength S1)
    (setq Elist (entget (ssname S1 (setq CNT (1+ CNT)))))
    (entmod
      (cond
	((null (assoc 40 Elist))
	 (append Elist (list (cons 40 TXSZ)))
	)
	(t
	 (subst (cons 40 TXSZ) (assoc 40 Elist) Elist)
	)
	(TERPRI)
	(TERPRI)
      )
    )
  )
)

 ;REDEFINE TEXT STYLE (USED IN MENU COMMAND)
(defun F:TXTST (/ S1)
  (setvar "CMDECHO" 0)
  (COMMAND "-STYLE"
	   TXSTY
	   (strcat TXSTY ".SHX")
	   ""
	   ""
	   ""
	   ""
	   ""
  )
  (prompt "\nSelect text objects: ")
  (setq S1 (ssget '((0 . "TEXT"))))
  (setq CNT -1)
  (repeat (sslength S1)
    (setq Elist (entget (ssname S1 (setq CNT (1+ CNT)))))
    (entmod
      (cond
	((null (assoc 7 Elist))
	 (append Elist (list (cons 7 TXSTY)))
	)
	(t
	 (subst (cons 7 TXSTY) (assoc 7 Elist) Elist)
	)
	(TERPRI)
	(TERPRI)
      )
    )
  )
)


;;;  /----------------------------------------------------------\
;;;  | General text commands that use support functions.	|
;;;  \----------------------------------------------------------/

(defun C:DT () (NOTE_TEXT "NOTE-NEW"))
(defun C:EDT () (NOTE_TEXT "NOTE-EXST"))
(defun C:DDT () (NOTE_TEXT "NOTE-DEMO"))
(defun C:NT () (NOTE_TEXT "_NPLT"))

(defun C:DTM () (NOTE_TEXT_MIDDLE "NOTE-NEW" "rotate" 512 128))
(defun C:EDTM () (NOTE_TEXT_MIDDLE "NOTE-EXST" "rotate" 512 128))
(defun C:DDTM () (NOTE_TEXT_MIDDLE "NOTE-DEMO" "rotate" 512 128))
(defun C:SDT () (NOTE_TEXT_MIDDLE "NOTE-NEW" nil 32 32))

(defun C:MTEXT () (NOTE_MTEXT "NOTE-NEW"))
(defun C:EMT () (NOTE_MTEXT "NOTE-EXST"))
(defun C:DMT () (NOTE_MTEXT "NOTE-DEMO"))

(defun C:JL ()
  (SET_JUSTIFY 0) ; 0 is dxf code for left
  (princ)
)

(defun C:JC ()
  (SET_JUSTIFY 1) ; 1 is dxf code for center
  (princ)
)

(defun C:JR ()
  (SET_JUSTIFY 2) ; 2 is dxf code for right
  (princ)
)


;;;  /----------------------------------------------------------\
;;;  | General text commands.					|
;;;  \----------------------------------------------------------/

 ;Text editing tool
(defun C:CHTXT (/ a f item b e)
  (setvar "cmdecho" 0)
  (prompt "\nPick Text Entities in the same order that they will be changed: ")
  (setq a (ssget))
  (setq numstr (sslength a))
  (setq f (ssname a 0))
  (setq i 1)
  (while (<= i numstr)
    (setq item (entget f))
    (setq b (Getstring 1 "\nNew Text: "))
    (setq e (assoc 1 item))
    (setq item (subst (cons 1 b) e item))
    (entmod item)
    (setq i (1+ i))
    (ssdel f a)
    (setq f (ssname a 0))
  )
  (princ)
)

 ;Reset North direction variable for wall angle commands
(defun C:LCN () (setq load_calc_north nil))

 ;Set text style to Arial Narrow (current 2012 LSW Standard).
(defun C:NEWSTYLE ()
  (command "-style" "Standard" "Arial Narrow" 0.0 1.0 0.0 "n" "n")
  (UPDATE_WF_and_STYLE 1.0) ;for "Standard" only
  (command "-style" "MEDIUM" "Arial.ttf" 0.0 1.0 0.0 "n" "n")
  (COMMAND "-RENAME" "STYLE" "MEDIUM" "TITLE")
  (setvar "textstyle" "Standard")
  (if (/= (getvar "dimtxsty") "Standard")
    (setvar "dimtxsty" "Standard")
  )
  (RELOAD_BLOCKS)
  (command "-purge" "ST" "" "n")
  (prompt "\nThe Standard text style has been set to the current font styles (Arial Narrow and Arial).")
  (princ)
)

 ;Alias for NEWSTYLE
(defun C:NS () (C:NEWSTYLE))

 ;Set text style to Simplex (pre-2012 LSW Standard).
(defun C:OLDSTYLE ()
  (command "-style" "Standard" "Simplex.shx" 0.0 0.85 0.0 "n" "n" "n")
  (UPDATE_WF_and_STYLE 0.85) ;for "Standard" only
  (if (tblsearch "style" "TITLE")
    (COMMAND "-RENAME" "STYLE" "TITLE" "MEDIUM")
  )
  (command "-style" "MEDIUM" "Medium.shx" 0.0 1.0 0.0 "n" "n")
  (setvar "textstyle" "Standard")
  (RELOAD_BLOCKS)
  (command "-purge" "ST" "" "n")
  (prompt "\nThe Standard text style has been set to the Legacy font styles (Simplex and Medium).")
  (princ)
)

 ;Draw Pline or Rectangle and insert it's area as text
(defun C:SFT (/ start_point sqft_ob sqft)
  (F:SLAY "_NPLT")
  (F:MODS (quote ("osmode" "orthomode")))
  (setvar "orthomode" 0)
  (setq	start_point
	 (getpoint
	   "\nPick first point of Pline/Rectangle or hit <Enter> to pick a pline: "
	 )
  )
  (if (null start_point)
    (progn
      (setq sqft_ob (ssget :S))
      (command "area" "o" sqft_ob)
    )
    (progn
      (setq second_point
	     (getpoint start_point
	       "\nPick second point of pline or opposite corner of rectangle: "
	     )
      )
      (command "line" start_point second_point "")
      (setq third_point
	     (getpoint second_point
	       "\nPick third point of pline or hit <Enter> for rectangle: "
	     )
      )
      (command "erase" "l" "")
      (setq points (list start_point second_point third_point))
      (if (null third_point)
	(progn
	  (SET_OSMODE "non")
	  (command "rectangle" start_point second_point)
	)
	(PAUSE_FOR_COMMAND "pline" points)
      )
      (command "area" "o" (entlast))
    )
  )
  (setq sqft (strcat (rtos (/ (getvar "area") 144) 2 0) " SQFT"))
  (SET_OSMODE "non")
  (TEXT_SET)
  (command "text" "0,0,0" "" "" sqft)
  (setvar "orthomode" 0)
  (command "move" "l" "" "0,0,0" pause)
  (F:MODR)
  (F:RLAY)
)

 ;Inserts text in a line and breaks line
(defun C:TL (/ q@ qj q?@ qqj qjj qj@ ql q% worktype)
  ;q@=text to be inserted,
  ;qj=point on line,
  ;q?@=selection set of line,
  ;qqj=line entity,
  ;qjj=layer of line entity,
  ;qj@=angle of line in radians,
  ;ql=length of text/break in line devided by 2,
  ;q%=angle of text in degrees.
  (F:MODS (quote ("osmode" "blipmode" "highlight")))
  (setvar "blipmode" 0)
  (setvar "highlight" 0)
  (TEXT_SET)
  (setq q@ (getstring "\nEnter text to insert: "))
  (while (= q@ "")
    (setq
      q@ (getstring "\nError: No text input.  Enter text to insert: "
	 )
    )
  )
  (SET_OSMODE "nea")
  (while (setq qj
		(getpoint
		  "\nEnter line location at which to insert text <nea to>: "
		)
	 )
    (command "undo" "group")
    (SET_OSMODE "non")
    (command "select" "Box" qj qj "")
    (setq q?@ (ssget "p"))
    (cond
      ((= q?@ nil) (princ "\nNo line found. \n"))
      ((> (sslength q?@) 1) (princ "\nToo many lines found. \n"))
      ((equal
	 "LINE"
	 (cdr (assoc (quote 0) (setq qqj (entget (ssname q?@ 0)))))
       )
       (SET_OSMODE "non")
       (setq qjj (strcase (cdr (assoc (quote 8) qqj)))
	     qj@ (angle	(cdr (assoc (quote 10) qqj))
			(cdr (assoc (quote 11) qqj))
		 )
	     ql	 (* (1+ (strlen q@)) (SCALEFACTOR) 0.09375 0.5)
       )
       (if (or (<= (distance qj (cdr (assoc (quote 10) qqj))) ql)
	       (<= (distance qj (cdr (assoc (quote 11) qqj))) ql)
	   )
	 (progn (SET_OSMODE "nea")
	        (prompt (strcat "\nThe line is too short to contain '" q@ "'. "))
	 )
	 (progn	(if (and (>= qj@ (/ pi 2)) (<= qj@ (* (/ pi 2) 3)))
		  (setq q% (R->D (+ qj@ pi)))
		  (setq q% (R->D qj@))
		)
	   	(if (= (ROUND q%) 270)
		  (setq q% 90)
		)	   
		(F:SLAY qjj)
		(command "break"
			 (cdr (assoc (quote -1) qqj))
			 (polar qj qj@ ql)
			 (polar qj (+ qj@ pi) ql)
		)
		(command "text" "m" qj "" q% q@)
	   	(cond ((= "NEW" (substr qjj (- (strlen qjj) 2))) (setq color 3))
		      ((= "EXST" (substr qjj (- (strlen qjj) 3))) (setq color 194))
		      ((= "DEMO" (substr qjj (- (strlen qjj) 3))) (setq color 150))
		      ((= "FTR" (substr qjj (- (strlen qjj) 2))) (setq color 2))
		      (t (setq color "bylayer"))
		)
	        (command "change" "l" "" "p" "c" color "")
		(SET_OSMODE "nea")
	 )
       )
      )
      (t
       (terpri)
       (prompt "Must select a LINE.")
       (terpri)
       (SET_OSMODE "nea")
      )
    )
    (command "undo" "end")
  )
  (TEXT_ANG_RESET)
  (F:RLAY)
  (F:MODR)
  (princ)
)

 ;insert WALL LENGTH TEXT
(defun C:WLEN (/ pt1 pt2 len)
  (F:SLAY "_NPLT")
  (F:MODS (quote ("osmode" "orthomode")))
  (SET_OSMODE "end")
  (setvar "orthomode" 0)
  (setq pt1 (getpoint "\nFirst Point:"))
  (setq pt2 (getpoint pt1 "\nSecond Point:"))
  (command "dist" pt1 pt2)
  (setq len (strcat (rtos (/ (getvar "distance") 12) 2 1) "'"))
  (TEXT_SET)
  (command "text" "0,0,0" "" "" len)
  (SET_OSMODE "non")
  (command "move" "l" "" "0,0,0" pause)
  (F:MODR)
  (F:RLAY)
)



 ;insert WALL angle TEXT
(defun C:WA (/ pt1 pt2 pt3 pt4 ang1 ang2 ang3 ang4 ang_factor)
  (F:SLAY "_NPLT")
  (F:MODS (quote ("osmode" "orthomode" "snapang")))
  (SET_OSMODE "end")
  (setvar "orthomode" 0)
  (if (= load_calc_north nil)
    (setq load_calc_north
	   (getreal
	     "\nInput North direction in degrees East of North or hit enter (0 - North is up): "
	   )
    )
    ()
  )
  (if (= load_calc_north nil)
    (setq load_calc_north 0)
    ()
  )
  (setq pt1 (getpoint "\nFirst Point on Wall:"))
  (setq pt2 (getpoint pt1 "\nSecond Point on Wall:"))
  (SET_OSMODE "non")
  (setvar "orthomode" 1)
  (setq ang1 (angle pt1 pt2))
  (setq pt3 (polar pt1 ang1 (/ (distance pt1 pt2) 2)))
  (setvar "snapang" ang1)
  (setq pt4 (getpoint pt3 "\nPick Exterior Side of Wall:"))
  (setvar "orthomode" 0)
  (setvar "snapang" 0)
  (setq ang2 (R->D (angle pt3 pt4)))
  (if (<= load_calc_north 90)
    (setq ang_factor (- 90 load_calc_north))
    (setq ang_factor (- 450 load_calc_north))
  )
  (if (<= ang2 ang_factor)
    (setq ang3 (strcat (rtos (- ang_factor ang2) 2 0) "%%D"))
    (setq ang3 (strcat (rtos (- (+ ang_factor 360) ang2) 2 0) "%%D"))
  )
  (if (and (<= (R->D ang1) 270) (> (R->D ang1) 90))
    (setq ang4 (+ (R->D ang1) 180))
    (setq ang4 (R->D ang1))
  )
  (TEXT_SET)
  (command "text" "0,0,0" "" ang4 ang3)
  (command "move" "l" "" "0,0,0" pause)
  (TEXT_ANG_RESET)
  (F:MODR)
  (F:RLAY)
)

 ;insert WALL LENGTH and angle TEXT
(defun C:WLA (/ len pt1 pt2 pt3 pt4 ang1 ang2 ang3 ang4 ang_factor)
  (F:SLAY "_NPLT")
  (F:MODS (quote ("osmode" "orthomode" "snapang")))
  (SET_OSMODE "end")
  (setvar "orthomode" 0)
  (if (= load_calc_north nil)
    (setq load_calc_north
	   (getreal
	     "\nInput North direction in degrees East of North or hit enter (0 - North is up): "
	   )
    )
    ()
  )
  (if (= load_calc_north nil)
    (setq load_calc_north 0)
    ()
  )
  (setq pt1 (getpoint "\nFirst Point on Wall:"))
  (setq pt2 (getpoint pt1 "\nSecond Point on Wall:"))
  (SET_OSMODE "non")
  (command "dist" pt1 pt2)
  (setq len (strcat (rtos (/ (getvar "distance") 12) 2 1) "'"))
  (TEXT_SET)
  (command "text" "0,0,0" "" "" LEN)
  (command "move" "l" "" "0,0,0" pause)
  (setvar "orthomode" 1)
  (setq ang1 (angle pt1 pt2))
  (setq pt3 (polar pt1 ang1 (/ (distance pt1 pt2) 2)))
  (setvar "snapang" ang1)
  (setq pt4 (getpoint pt3 "\nPick Exterior Side of Wall:"))
  (setvar "orthomode" 0)
  (setvar "snapang" 0)
  (setq ang2 (R->D (angle pt3 pt4)))
  (if (<= load_calc_north 90)
    (setq ang_factor (- 90 load_calc_north))
    (setq ang_factor (- 450 load_calc_north))
  )
  (if (<= ang2 ang_factor)
    (setq ang3 (strcat (rtos (- ang_factor ang2) 2 0) "%%D"))
    (setq ang3 (strcat (rtos (- (+ ang_factor 360) ang2) 2 0) "%%D"))
  )
  (if (and (<= (R->D ang1) 270) (> (R->D ang1) 90))
    (setq ang4 (+ (R->D ang1) 180))
    (setq ang4 (R->D ang1))
  )
  (TEXT_SET)
  (command "text" "0,0,0" "" ang4 ang3)
  (command "move" "l" "" "0,0,0" pause)
  (TEXT_ANG_RESET)
  (F:MODR)
  (F:RLAY)
)