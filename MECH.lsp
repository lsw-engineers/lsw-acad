;;;;;
;;  MECH.LSP - MECHANICAL LISP COMMANDS
;;
;;  Copyright © 2008 by LSW Engineers Arizona, Inc.
;;
;;  
;;-------------------------------------------------------------------------
;;  DESCRIPTION
;;	All mechanical and plumbing specific lisps.
;;	Broken into sections: support functions, commands that call support
;;	functions, commands, and layer changes.
;;
;;
;;-------------------------------------------------------------------------
;;

(setq mech 1)


; ------------  PLACE THE LSWMECH PULL-DOWN FUNCTION -------------
;  This function places the LSWMECH pull-down to the left of the
;  Window pull-down on the acad menu. If FLG is true the current
;  LSWMECH pull-down if present will be removed and placed again.
;  If FLG is nil and the pull-down is present, nothing will be done
;
;  When placing the LSWMECH menu, acad's Window menu is used to
;  determine the pull-down location. if the Window pull-down is
;  missing, the Help pull-down will be used instead. If that too
;  is missing, it will place the LSWMECH pull-down after the last
;  menu location.
; ----------------------------------------------------------------

; Place the LSWMECH Tools pulldown to the left of the last pulldown already loaded
(defun LSWMECH-placemenu( / n )
  (if (menugroup "LSWMECH")
      (progn
       (setq n 1)
       (while (< n 24)
        (if (menucmd (strcat "P" (itoa n) ".1=?"))
	    (setq n (+ n 1))
            (progn
             (if (> n 3)
                 (setq n (- n 2))
                 (setq n 3)
             );if
             (menucmd (strcat "p" (itoa n) "=+LSWMECH.pop5"))
             (menucmd (strcat "p" (itoa n) "=+LSWMECH.pop4"))
             (menucmd (strcat "p" (itoa n) "=+LSWMECH.pop3"))
             (menucmd (strcat "p" (itoa n) "=+LSWMECH.pop2"))
             (menucmd (strcat "p" (itoa n) "=+LSWMECH.pop1"))
             (setq n 25)
            );progn
        );if
       );while
      );progn
  );if
);defun LSWMECH-placemenu
 
(LSWMECH-placemenu)

;;;  /----------------------------------------------------------\
;;;  | Mechanical and plumbing specific support functions.	|
;;;  \----------------------------------------------------------/

 ;Calculations and duct drawing for SD command (4 diffusers only)
(defun DIFFUSER_DUCT (/			  pt1
		      pt2		  pt3
		      pt4		  ang1
		      ang2		  pt_center
		      pt_upstream_raw	  pt_downstream_diff_1
		      pt_downstream_diff_2
		      pt_upstream_diff_1  pt_upstream_diff_2
		      main_len		  pt_upstream
		      pt_downstream	  width_len
		      pt_takeoff_upstream pt_upstream_1
		      pt_upstream_2	  pt_upstream_3
		      pt_upstream_4	  pt_takeoff_downstream
		      downstream_len	  pt_downstream_1
		      pt_downstream_2
		     )
  (setq
    pt1	(list (nth 0 pts_diffs) (nth 1 pts_diffs) (nth 2 pts_diffs))
  )
  (setq
    pt2	(list (nth 3 pts_diffs) (nth 4 pts_diffs) (nth 5 pts_diffs))
  )
  (setq
    pt3	(list (nth 6 pts_diffs) (nth 7 pts_diffs) (nth 8 pts_diffs))
  )
  (setq	pt4 (list (nth 9 pts_diffs)
		  (nth 10 pts_diffs)
		  (nth 11 pts_diffs)
	    )
  )
  (setq ang1 (angle pt1 pt4))
  (setq
    pt_center (polar pt1 ang1 (/ (distance pt1 pt4) 2))
  )
  (setq	pt_upstream_raw
	 (getpoint
	   "\nSelect approximate location of HVAC equipment: "
	 )
  )
  (setq ang2 (R->D (angle pt_center pt_upstream_raw)))
  (cond	((and (> ang2 45) (<= ang2 135))
	 (setq ang2 (/ pi 2))
	 (setq pt_downstream_diff_1 pt1)
	 (setq pt_downstream_diff_2 pt3)
	 (setq pt_upstream_diff_1 pt4)
	 (setq pt_upstream_diff_2 pt2)
	)
	((and (> ang2 135) (<= ang2 225))
	 (setq ang2 pi)
	 (setq pt_downstream_diff_1 pt3)
	 (setq pt_downstream_diff_2 pt4)
	 (setq pt_upstream_diff_1 pt2)
	 (setq pt_upstream_diff_2 pt1)
	)
	((and (> ang2 225) (<= ang2 315))
	 (setq ang2 (* (/ pi 2) 3))
	 (setq pt_downstream_diff_1 pt4)
	 (setq pt_downstream_diff_2 pt2)
	 (setq pt_upstream_diff_1 pt1)
	 (setq pt_upstream_diff_2 pt3)
	)
	(t
	 (setq ang2 0)
	 (setq pt_downstream_diff_1 pt2)
	 (setq pt_downstream_diff_2 pt1)
	 (setq pt_upstream_diff_1 pt3)
	 (setq pt_upstream_diff_2 pt4)
	)
  )
  ;;Calculate points for main duct and draw
  (setq main_len (distance pt_downstream_diff_1 pt_upstream_diff_2))
  (setq pt_upstream (polar pt_center ang2 (+ (/ main_len 2) 12)))
  (setq	pt_downstream
	 (polar pt_center (+ ang2 pi) (+ (/ main_len 2) 12))
  )
  (F:SLAY "MH-DUCT-NEW")
  (command "line" pt_downstream pt_upstream "")
  (command "insert"
	   "d-pcap"
	   "s"
	   (SCALEFACTOR)
	   pt_downstream
	   pt_upstream
  )
  ;;Calculate points for upstream takeoffs and draw
  (setq	width_len
	 (/ (distance pt_downstream_diff_1 pt_downstream_diff_2) 2)
  )
  (setq	pt_takeoff_upstream
	 (polar pt_center ang2 (- (/ main_len 2) 42))
  )
  (setq	pt_upstream_1
	 (polar	pt_takeoff_upstream
		(+ ang2 (/ pi 2))
		width_len
	 )
  )
  (setq	pt_upstream_2
	 (polar	pt_takeoff_upstream
		(- ang2 (/ pi 2))
		width_len
	 )
  )
  (setq pt_upstream_3 (polar pt_upstream_1 ang2 24))
  (setq pt_upstream_4 (polar pt_upstream_2 ang2 24))
  (DRAW_MECH_TAKEOFF "MH-DUCT-NEW"
    pt_takeoff_upstream	   pt_upstream_1
    pt_upstream_3	   pt_upstream_diff_2
    pt_upstream
   )
  (DRAW_MECH_TAKEOFF "MH-DUCT-NEW"
    pt_takeoff_upstream	   pt_upstream_2
    pt_upstream_4	   pt_upstream_diff_1
    pt_upstream
   )
  ;;Calculate points for downstream takeoffs and draw
  (setq	pt_takeoff_downstream
	 (polar pt_center (+ ang2 pi) (/ main_len 2))
  )
  (setq downstream_len (- width_len 18))
  (setq	pt_downstream_1
	 (polar	pt_takeoff_downstream
		(+ ang2 (/ pi 2))
		downstream_len
	 )
  )
  (setq	pt_downstream_2
	 (polar	pt_takeoff_downstream
		(- ang2 (/ pi 2))
		downstream_len
	 )
  )
  (DRAW_MECH_TAKEOFF "MH-DUCT-NEW"
    pt_takeoff_downstream  pt_downstream_1
    nil			   pt_downstream_diff_1
    pt_upstream
   )
  (DRAW_MECH_TAKEOFF "MH-DUCT-NEW"
    pt_takeoff_downstream  pt_downstream_2
    nil			   pt_downstream_diff_2
    pt_upstream
   )
) ;End of DIFFUSER_DUCT

 ;FUNCTION FOR C:SD FOR DEFINING DIFFUSER LAYOUT
(defun DIFFUSER_INSERT (pt_room1 pt_room2 pt_grid  /	    xgrid
			xmax	 xmin	  ygrid	   ymax	    ymin
			avg_spacing	  room_x   room_y   pts_x
			pts_y	 nx	  ny	   n	    pt
		       )
  (setq xmin (min (nth 0 pt_room1) (nth 0 pt_room2)))
  (setq xmax (max (nth 0 pt_room1) (nth 0 pt_room2)))
  (setq ymin (min (nth 1 pt_room1) (nth 1 pt_room2)))
  (setq ymax (max (nth 1 pt_room1) (nth 1 pt_room2)))
  (setq xgrid (+ (nth 0 pt_grid) 12))
  (setq ygrid (+ (nth 1 pt_grid) 12))

  (setq avg_spacing (* 18 12))
  (setq room_x (abs (- xmax xmin)))
  (setq room_y (abs (- ymax ymin)))
  (setq num_diff_x (1+ (fix (/ room_x avg_spacing))))
  (setq num_diff_y (1+ (fix (/ room_y avg_spacing))))
  (setq pts_x (DIFFUSER_POINTS num_diff_x room_x xmin xgrid))
  (setq pts_y (DIFFUSER_POINTS num_diff_y room_y ymin ygrid))

  (setq nx 1)
  (setq ny 1)
  (repeat num_diff_x
    (repeat num_diff_y
      (setq pts_diffs (append pts_diffs
			      (list (nth nx pts_x) (nth ny pts_y) 0.0)
		      )
      )
      (setq ny (1+ ny))
    )
    (setq nx (1+ nx))
    (setq ny 1)
  )
  (F:SLAY "MH-DIFF-NEW")
  (setq n 0)
  (repeat (* num_diff_x num_diff_y)
    (setq pt (list (nth n pts_diffs)
		   (nth (+ n 1) pts_diffs)
		   (nth (+ n 2) pts_diffs)
	     )
    )
    (command "insert" "supdif" "s" 24 "r" 0 pt)
    (setq n (+ n 3))
  )
) ;End of DIFFUSER_INSERT

 ;Function called in DIFFUSER_INSERT
(defun DIFFUSER_POINTS
       (num_diff room_dim min_value grid / n points raw temp)
  (setq n 1)
  (setq points (list 0))
  (repeat num_diff
    (setq raw (+ min_value (/ (* room_dim n) (* 2 num_diff))))
    (setq temp
	   (if (<= raw grid)
	     (- grid (* (ROUND (/ (- grid raw) 24)) 24))
	     (+ grid (* (ROUND (/ (- raw grid) 24)) 24))
	   )
    )
    (setq points (append points (list temp)))
    (setq n (+ n 2))
  )
  points
) ;End of DIFFUSER_POINTS

 ;INSERTS ROUND DUCT TAKEOFF FROM THE MAIN DUCT TO THE DIFFUSER.
(defun DRAW_MECH_TAKEOFF (layer	   main_duct	     branch_pt2
			  branch_pt3	    diffuser upstream /
			  pt1	   pt2	    pt3	     pt4      pt5
			  pt6	   pt7	    pt8	     pt9      pt10
			  pt11	   pt12	    ang1     ang2     ang3
			  ang4	   ang5	    ang6     ang7
			 )
  (command "undo" "group")
 ;SAVE CURRENT SYSTEM VARIABLES and SWITCH TO DUCT LAYER
 ;ORTHO ON, OSNAP NEAREST/INTERSECTION, and SET pt3 AS nil FOR 2-POINT TAKEOFF
  (F:SLAY layer)
  (setvar "orthomode" 1)
  (setvar "plinewid" 0)
  (setq pt3 "")
 ;GET POINT 1 ON MAIN DUCT and POINTS 2 and 3 FOR ROUND DUCT AS NEEDED
  (if (= main_duct "user")
    (progn
      (SET_OSMODE "int nea")
      (setq pt1 (getpoint "\nSelect Point on Duct:"))
    )
    (setq pt1 main_duct)
  )
  (if (= branch_pt2 "user")
    (progn
      (SET_OSMODE "non")
      (setq pt2 (getpoint pt1 "\nSelect Next Point:"))
    )
    (setq pt2 branch_pt2)
  )
  (if (= branch_pt3 "user")
    (setq pt3 (getpoint	pt2
			"\nSelect Third Point (or enter to continue):"
	      )
    )
    (setq pt3 branch_pt3)
  )
 ;CALC POINT 11 AS START OF FLEX DEPENDING ON 2 or 3 POINT DUCT
 ;DRAW Pline, insert DUCTCAP, and FILLET
  (cond	((= pt3 nil)
	 (setq pt11 pt2)
	 (command "-insert" "D-PCAP" pt2 (SCALEFACTOR) "" pt1)
	 (command "pline" pt1 pt2 "")
	)
	(t
	 (setq pt11 pt3)
	 (command "-insert" "D-PCAP" pt3 (SCALEFACTOR) "" pt2)
	 (command "pline" pt1 pt2 pt3 "")
;;;	 (setvar "filletrad" 8)
;;;	 (command "fillet" "p" (entlast))
	)
  )
 ;SET POINT 4 AT INCERTION POINT OF DIFFUSER
  (setvar "orthomode" 0)
  
  (if (= diffuser "user")
    (progn
      (SET_OSMODE "ins")
      (setq pt4 (getpoint pt11 "\nSelect Insertion Point of Diffuser (or enter for NO flex):"))
    )
    (setq pt4 diffuser)
  )
  (if (= pt4 nil)
    (setq pt4 pt11)
  )
  (SET_OSMODE "non")
 ;CALCULATE ANGLES 1, 2 and 3 TO DETERMIN ORIENTATION FOR DRAWING SPline FLEX DUCT
 ;CALC POINTS 6 and 7 FOR LEFT TURN FLEX
 ;CALC POINTS 8 and 9 FOR RIGHT TURN FLEX
 ;DRAW SPline FLEX DUCT
 (if (= pt3 nil)
   (setq ang1 (R->D (angle pt1 pt2)))
   (setq ang1 (R->D (angle pt2 pt3)))
 );end
 (setq ang2 (R->D (angle pt11 pt4)))
 (if (and (>= ang1 270) (>= ang2 0) (<= ang2 90))
   (setq ang3 (+ ang2 360))
   (setq ang3 ang2)
 );end
 (setq
   pt6 (polar pt11 (D->R (- ang2 30)) (* (distance pt4 pt11) 0.32))
 )
 (setq pt7 (polar pt11
		  (D->R (+ ang2 8.4))
		  (* (distance pt4 pt11) 0.62)
	   )
 )
 (setq
   pt8 (polar pt11 (D->R (+ ang2 30)) (* (distance pt4 pt11) 0.32))
 )
 (setq pt9 (polar pt11
		  (D->R (- ang2 8.4))
		  (* (distance pt4 pt11) 0.62)
	   )
 )
  (if (/= pt4 pt11)
    (progn
      (if (/= pt3 nil)
	(progn
	  (setvar "filletrad" 8)
	  (command "fillet" "p" (entlast))
	)
      )
      (if (and (>= (- ang3 ang1) 0) (<= (- ang3 ang1) 180))
	(command "spline" pt11 pt6 pt7 pt4 "" "" "") ;TURNING LEFT
	(command "spline" pt11 pt8 pt9 pt4 "" "" "") ;TURNING RIGHT
      ) ;end if (spline)
    );end PROGN
  );end IF
      
 ;CALC POINT 5 FOR VOLUME DAMPER LOCATION
 ;GET POINT 10 AT POINT UPSTREAM ON MAIN DUCT TO DETERMINE DIRECTION OF TAKEOFF
 ;CALC ANGLES 4, 5 6, and 7 TO DETERMINE ORIENTATION
 ;insert DUCT CAP and VOLUME DAMPER
  (setq pt5 (polar pt1 (angle pt1 pt2) (* 18 (/ (SCALEFACTOR) 96))))
 ;set pt5 18"ISH from pt1
  (setvar "orthomode" 1)
  (if (= upstream "user")
    (progn
      (SET_OSMODE "nea")
      (setq pt10 (getpoint pt1 "\nPick a point upstream on duct:"))
    )
    (setq pt10 upstream)
  )
  (setvar "orthomode" 0)
  (SET_OSMODE "non")
  (setq ang4 (R->D (angle pt1 pt5))) ;angle OF TAKEOFF
  (setq ang5 (R->D (angle pt1 pt10))) ;angle OF MAINDUCT
  (if (> (- (- ang5 ang4) 180) 0)
    (setq ang6 (+ ang5 90))
    (setq ang6 ang4)
  ) ;end
  (if (< (+ (- ang5 ang4) 180) 0)
    (setq ang7 (- ang6 360))
    (setq ang7 ang6)
  ) ;end
  (cond	((>= (- ang7 ang5) 0) ;TURNING RIGHT
	 (command "-insert" "TAKEOFF" pt1 (SCALEFACTOR) "" ang4)
	 (command "-insert" "VDR" pt5 (SCALEFACTOR) "" (+ ang4 180))
	)
	(t ;TURNING LEFT
	 (command "-insert" "TAKEOFF" pt1 (SCALEFACTOR) "" ang5)
	 (command "-insert" "VDL" pt5 (SCALEFACTOR) "" (+ ang5 90))
	)
  )
 ;RESET SYSTEM VARIABLES TO PRE-COMMAND SETTINGS
  (command "undo" "end")
  (princ)
) ;end of DRAW_MECH_TAKEOFF

 ;insert SUPPLY, RETURN, and EXHAUST DIFFUSERS FROM THE CORNER
(defun INS_MECH_DIFF (layer    block_name	 osm	  xdim
		      ydim     grid	/	 xdim	  ydim
		      zdim     pt1	pt2	 pt3	  diffuser
		      temp_xdim
		     )
  (F:SLAY layer)
  (F:MODS (quote ("osmode" "orthomode")))
  (SET_OSMODE osm)
  (setvar "orthomode" 1)
  (setq zdim xdim)
  (cond	((= xdim 0)
	 (setq xdim (getreal "\nEnter Grille/Diffuser Length (in inches): ")
	       ydim (getreal "\nEnter Grille/Diffuser Width (in inches): ")
	       zdim xdim
	 )
	)

	((= xdim 1)
	 (setq default_xdim
		(cond ((> default_xdim "") default_xdim)
		      (t "")
		)
	 )
	 (setq temp_xdim    (getstring
			      (strcat
				"\nEnter the Size of the Grille/Diffuser (in inches) or <"
				default_xdim
				"> :"
			      )
			    )
	       xdim	    (cond ((= temp_xdim " ") "")
				  ((> temp_xdim "") temp_xdim)
				  (t default_xdim)
			    )
	       ydim	    xdim
	       zdim	    xdim
	       default_xdim xdim
	 )
	)
	(t (setq zdim xdim))
  )
  (cond	((= grid "grid")
	 (setq pt1 (getpoint "\nPick RCP intersection: "))
	 (setq pt2 (polar pt1 0 12))
	 (setq pt3 (polar pt2 (/ pi 2) 12))
	 (SET_OSMODE "non")
	 (command "-insert"	  block_name	  "x"	  xdim
		  "y"	  ydim	  "z"	  zdim	  "r"	  0
		  pt3	  ""
		 )
	 (setq diffuser (ssget "l"))
	 (command "rotate" diffuser "" pt1 pause)
	)
	(t
	 (command "-insert" block_name "x" xdim	"y" ydim "z" zdim "r" 0
		  pause)
	)
  )
  (F:RLAY)
  (F:MODR)
  (princ)
)


 ;insert SIDE WALL DIFFUSER/GRILLE INS_BLOCK_WDIFF
(defun INS_MECH_WALL_DIFF (layer   supply  /	   width   block_name
			   pt1	   pt2	   pt3	   pt4	   pt5
			   pt6	   pt7	   pt8	   pt9	   pt10
			   ang1	   ang2	   defined wdiff1  wdiff2
			   wdiff3
			  )
  (command "undo" "group")
  (F:SLAY "0")
  (F:MODS (quote ("osmode" "orthomode")))
  (SET_OSMODE "end nea int")
  (setvar "orthomode" 1)
  (if (= duct_width nil)
    (setq duct_width 1)
    ()
  )
  (setq	width (getint (strcat "\nInput Duct Width in inches <"
			      (rtos duct_width 2 0)
			      ">: "
		      )
	      )
  )
  (if (= width nil)
    (setq width duct_width)
    ()
  )
  (if (= supply "supply")
    (setq block_name
	   (strcat "SWD_"
		   (rtos width 2 0)
		   "_@"
		   (rtos (SCALEFACTOR) 2 0)
	   )
    )
    (setq block_name
	   (strcat "SWR_"
		   (rtos width 2 0)
		   "_@"
		   (rtos (SCALEFACTOR) 2 0)
	   )
    )
  )
  (setq pt1 (getpoint "\nInsertion Point: "))
  (SET_OSMODE "non")
  (setq	pt2  (getpoint pt1 "\nDirection of Duct: ")
	ang1 (angle pt1 pt2)
	ang2 (+ (R->D ang1) 180)
	pt3  (polar pt1 pi 2.5)
	pt4  (polar pt3 (/ pi 2) (/ width 2))
	pt5  (polar pt1 (* pi 1.5) (/ width 2))
	pt6  (polar pt1 0 0.75)
	pt7  (polar pt1 (/ pi 2) (+ (/ width 2) 1.5))
	pt8  (polar pt6 (* pi 1.5) (+ (/ width 2) 1.5))
	pt9  (polar pt1 0 (/ (SCALEFACTOR) 16))
	pt10 (polar pt1 0 (/ (* (SCALEFACTOR) 5) 16))
  )
  (setq defined (tblsearch "block" block_name))
  (cond	((= defined nil)
	 (command "rectangle" pt4 pt5)
	 (setq wdiff1 (ssget "l"))
	 (command "rectangle" pt7 pt8)
	 (setq wdiff2 (ssget "l"))
	 (if (= supply "supply")
	   (command "insert" "flowa" "s" (SCALEFACTOR) "r" 0 pt9)
	   (command "insert" "flowa" "s" (SCALEFACTOR) "r" 180 pt10)
	 )
	 (setq wdiff3 (ssget "l"))
	 (command "scale"
		  wdiff1
		  wdiff2
		  wdiff3
		  ""
		  pt1
		  (/ 1 (SCALEFACTOR))
	 )
	 (command "-block" block_name pt1 wdiff1 wdiff2 wdiff3 "")
	)
	(t
	 ()
	)
  )
  (F:SLAY layer)
  (command "insert" block_name "s" (SCALEFACTOR) "r" ang2 pt1)
  (setq duct_width width)
  (F:RLAY)
  (F:MODR)
  (command "undo" "end")
  (princ)
)

 ;insert ISOMETRIC FIXTURES
(defun INS_MECH_ISO (fixture_type	 iso_type  cleanout  /
		     pt1       pt2	 pt3	   pt4	     pt5
		     pt6       pt7	 ang1	   ang2	     iso_fixt
		     iso_wco   wco_dir	 fixt_dir  trim_line
		    )
  (command "undo" "group")
  (F:SLAY "0")
  (F:MODS (quote ("orthomode" "osmode" "snapstyl")))
  (setvar "orthomode" 1)
  (SET_OSMODE "nea end");
  (setvar "snapstyl" 1)
  (setq pt1 (getpoint "\nSelect Point on Waste line"))
  (SET_OSMODE "non")
  (setq pt2 (polar pt1 (/ pi 2) (* (SCALEFACTOR) 0.125)))
  (setq pt3 (polar pt2 (/ pi 2) (* (SCALEFACTOR) 0.5)))
  (setq pt4 (polar pt3 (/ pi 2) (* (SCALEFACTOR) 0.5)))
  (F:SLAY "PS-VENT-NEW")
  (command "line" pt3 pt4 "")
  (F:SLAY "PS-WSTE-NEW")
  (command "line" pt2 pt3 "")
  (setq pt5 (getpoint pt1 "\nSelect Direction of Fixture:"))
  (setq ang1 (angle pt1 pt5))
  (cond	((= iso_type "double")
	 (cond ((or (< ang1 (/ pi 2))
		    (and (< ang1 (* 3 (/ pi 2))) (> ang1 pi))
		)
		(setq fixt_dir "_30")
	       )
	       (t (setq fixt_dir "_150"))
	 )
	 (cond ((< ang1 (/ pi 2)) (setq wco_dir "_30"))
	       ((< ang1 pi) (setq wco_dir "_150"))
	       ((< ang1 (* 3 (/ pi 2))) (setq wco_dir "_210"))
	       (t (setq wco_dir "_330"))
	 )
	)
	((= iso_type "fag")
	 (cond ((< ang1 (/ pi 2))
		(setq fixt_dir "_30"
		      wco_dir "_150"
		)
	       )
	       ((< ang1 pi)
		(setq fixt_dir "_150"
		      wco_dir "_30"
		)
	       )
	       ((< ang1 (* 3 (/ pi 2)))
		(setq fixt_dir "_210"
		      wco_dir "_330"
		)
	       )
	       (t
		(setq fixt_dir "_330"
		      wco_dir "_210"
		)
	       )
	 )
	)
	((= iso_type "typical")
	 (cond ((< ang1 (/ pi 2)) (setq fixt_dir "_30"))
	       ((< ang1 pi) (setq fixt_dir "_150"))
	       ((< ang1 (* 3 (/ pi 2))) (setq fixt_dir "_210"))
	       (t (setq fixt_dir "_330"))
	 )
	 (setq wco_dir fixt_dir)
	)
  )
  (setq iso_fixt (strcat "ISO" fixture_type fixt_dir))
  (setq iso_wco (strcat "ISOWCO" wco_dir))
  (command "-insert" iso_fixt pt2 (SCALEFACTOR) "" "")
  (F:SLAY "PE-FIXT-NEW")
  (if (= cleanout 1)
    (command "-insert" iso_wco pt2 (SCALEFACTOR) "" "")
    ()
  )
  (setq pt6 (getpoint pt1 "\nSelect Direction of Waste:"))
  (setq ang2 (angle pt1 pt6))
  (setq pt7 (polar pt1 ang2 (* (SCALEFACTOR) 0.125)))
  (F:SLAY "PS-WSTE-NEW")
  (command "line" pt7 pt2 "")
  
  (setq trim_line (ssget "l"));;
  (ISO_END_TRIM pt1 trim_line);;
  
  
  (F:RLAY)
  (F:MODR)
  (command "undo" "end")
  (princ)
)

 ;insert PLUMBING ISOMETRIC FITTINGS
(defun INS_MECH_ISO_BLOCK (fitt_type	 catagory      trim_scale
			   /		 temp_fitt_name
			   temp_fitt	 iso_fitt
			  )
  (F:MODS (quote ("osmode" "orthomode" "snapstyl")))
  (SET_OSMODE "end nea")
  (setvar "orthomode" 1)
  (setvar "snapstyl" 1)
  (setq	temp_fitt_name
	 (strcat "ISO"
		 fitt_type
		 (if (= category "vertical")
		   "_90A"
		   "_30"
		 )
	 )
  )
  (prompt "Insertion Point: ")
  (command "insert"
	   temp_fitt_name
	   "s"
	   (SCALEFACTOR)
	   "r"
	   0
	   pause
  )
  (setq temp_fitt (ssget "l"))
  (setq insertion_point (getvar "lastpoint"))
  (command "erase" temp_fitt "")
  (setq pt2 (getpoint insertion_point "\nSelect Direction of Fitting:"))
  (setq ang1 (angle insertion_point pt2))
  (cond	((= catagory "hb")
	 (cond ((< ang1 (/ pi 2)) (setq fitt_dir "_30"))
	       ((< ang1 pi) (setq fitt_dir "_150"))
	       ((< ang1 (* 3 (/ pi 2))) (setq fitt_dir "_210"))
	       (t (setq fitt_dir "_330"))
	 )
	)
	((= catagory "typical")
	 (cond ((or (< ang1 (/ pi 2))
		    (and (< ang1 (* (/ pi 2) 3)) (> ang1 pi))
		)
		(setq fitt_dir "_30")
	       )
	       (t (setq fitt_dir "_150"))
	 )
	)
	((= catagory "vertical")
	 (cond ((< ang1 pi) (setq fitt_dir "_90A"))
	       (t (setq fitt_dir "_90B"))
	 )
	)
  )
  (setq iso_fitt (strcat "ISO" fitt_type fitt_dir))
  (command "-insert"
	   iso_fitt
	   insertion_point
	   (SCALEFACTOR)
	   ""
	   ""
  )
  (cond	((/= trim_scale nil)
	 (SET_OSMODE "non")
	 (setq insertion_point (getvar "lastpoint"))
	 (command "SELECT" "Box" insertion_point insertion_point "")
	 (setq
	   trimee_ss (ssget "p")
	   trimee_ls (sslength trimee_ss)
	   no
		     -1
	 )
	 (command "circle"
		  insertion_point
		  (* (SCALEFACTOR) trim_scale)
	 )
	 (setq trim_entity (ssget "l"))
	 (command "trim" trim_entity "")
	 (repeat trimee_ls
	   (setq no (1+ no))
	   (command (list (ssname trimee_ss no) insertion_point))
	 )
	 (command "")
	 (command "erase" trim_entity "")
	)
	(t ())
  )
  (F:MODR)
  (princ)
)

 ;insert ISOMETRIC SCO
(defun INS_MECH_ISO_SCO	(double / pt1 pt2 pt3 pt4 pt5 pt6 pt7 pt8 ang1 trim_line)
  (command "undo" "group")
  (F:SLAY "0")
  (F:MODS (quote ("orthomode" "osmode" "snapstyl")))
  (setvar "orthomode" 1)
  (SET_OSMODE "nea end");
  (setvar "snapstyl" 1)
  (setq pt1 (getpoint "\nSelect Point on Waste line"))
  (SET_OSMODE "non")
  (setq pt2 (polar pt1 (/ pi 2) (* (SCALEFACTOR) 0.125)))
  (setq pt3 (polar pt2 (/ pi 2) (* (SCALEFACTOR) 0.125)))
  (F:SLAY "PS-WSTE-NEW")
  (command "line" pt2 pt3 "")
  (setq	pt4  (getpoint pt1 "\nSelect Direction of Waste:")
	ang1 (angle pt1 pt4)
	pt5  (polar pt1 ang1 (* (SCALEFACTOR) 0.125))
  )
  (command "line" pt5 pt2 "")
  
  (setq trim_line (ssget "l"));;
  (ISO_END_TRIM pt1 trim_line);;

  (SET_OSMODE "non")
  (if (= double 1)
    (progn (setq pt6 (polar pt5 ang1 (* (SCALEFACTOR) 0.03125))
		 pt7 (polar pt2 ang1 (* (SCALEFACTOR) 0.28125))
		 pt8 (polar pt3 ang1 (* (SCALEFACTOR) 0.28125))
	   )
	   (command "line" pt6 pt7 "")
	   (command "line" pt7 pt8 "")
	   (F:SLAY "PE-FIXT-NEW")
	   (command "-insert" "ISOSCO" pt7 (SCALEFACTOR) "" "")
    )
  )
  (F:SLAY "PE-FIXT-NEW")
  (command "-insert" "ISOSCO" pt2 (SCALEFACTOR) "" "")
  (F:RLAY)
  (F:MODR)
  (command "undo" "end")
  (princ)
)

 ;INSERTS ISOMETRIC VTR
(defun INS_MECH_ISO_VTR	(block_name / pt1 pt2 pt3)
  (command "undo" "group")
  (F:SLAY "PS-VENT-NEW")
  (F:MODS (quote ("orthomode" "osmode" "snapstyl")))
  (setvar "orthomode" 0)
  (SET_OSMODE "end")
  (setvar "snapstyl" 1)
  (setq pt1 (getpoint "\nSelect the TOP of WASTE Riser:"))
  (setq pt2 "")
  (setq	pt2
	 (getpoint
	   "\nSelect where the VENT STARTS (or press enter for same point):"
	 )
  )
  (SET_OSMODE "non")
  (setq pt3 (polar pt1 (/ pi 2) (* (SCALEFACTOR) 1.5)))
  (if (= pt2 nil)
    (command "line" pt1 pt3 "")
    (command "line" pt2 pt3 "")
  )
  (command "-insert" block_name pt3 (SCALEFACTOR) "" "")
  (F:RLAY)
  (F:MODR)
  (command "undo" "end")
  (princ)
)

 ;trims a the waste line when iso fixture snaps to end point
(defun ISO_END_TRIM (pt trim / selection_set line start end)
  (SET_OSMODE "end")
  (command "select" "box" pt pt "")
  (setq selection_set (ssget "p")
	line (cdr (assoc (quote -1) (entget (ssname selection_set 0))))
	start (cdr (assoc (quote 10) (entget (ssname selection_set 0))))
	end (cdr (assoc (quote 11) (entget (ssname selection_set 0))))
  )
  (if (or (equal pt start 0.01) (equal pt end 0.01));;
    (command "trim" trim "" (list line pt1)  "")
  )
)

 ;Command to insert Rooftop Heat Pump or Gas Pack
(defun ROOFTOPUNIT (suffix / tonnage carrierrange tranegprange tranehprange range visname blockname)
  (initget 1 "Carrier Trane")
  (if (= manufacturer nil)
    (setq manufacturer (getkword "\nCarrier or Trane? "))
  )
  (setq tonnage (getreal "\nInput nominal tonnage (or enter for default size): "))
  (setq carrierrange (list 2 6 7 10 12 15 20 25))
  (setq tranegprange (list 3 5 6 10 12 20 51 51))
  (setq tranehprange (list 3 4 5 10 12 20 51 51))
  (cond ((and (= manufacturer "Carrier") (= suffix "_HP"))
	 (if (or (< tonnage 2) (> tonnage 25))
	   (progn
	     (prompt "\nTonnage outside of expected range, default size will be used instead.")
	     (setq visname "default")
	   )
	   (VISNAMEFUNCT carrierrange) 
	 )
	)
	((and (= manufacturer "Carrier") (= suffix "_GP"))
	 (if (or (< tonnage 2) (> tonnage 25))
	   (progn
	     (prompt "\nTonnage outside of expected range, default size will be used instead.")
	     (setq visname "default")
	   )
	   (VISNAMEFUNCT carrierrange) 
	 )
	)
	((and (= manufacturer "Trane") (= suffix "_HP"))
	 (if (or (< tonnage 3) (> tonnage 20))
	   (progn
	     (prompt "\nTonnage outside of expected range, default size will be used instead.")
	     (setq visname "default")
	   )
	   (VISNAMEFUNCT tranehprange) 
	 )
	)
	((and (= manufacturer "Trane") (= suffix "_GP"))
	 (if (or (< tonnage 3) (> tonnage 20))
	   (progn
	     (prompt "\nTonnage outside of expected range, default size will be used instead.")
	     (setq visname "default")
	   )
	   (VISNAMEFUNCT tranegprange) 
	 )
	)
  )
  (setq blockname (strcat manufacturer suffix))
  (F:SLAY "ME-EQPM-NEW")
  (F:MODS (quote ("orthomode" "osmode")))
  (setvar "orthomode" 0)
  (SET_OSMODE "non")
  (command "-insert" blockname "s" 1 "r" 0 "0,0,0")
  (CHGDYNPROP (entlast) "Visibility" visname)
  (command "move" (entlast) "" "0,0,0" pause)
  (setvar "orthomode" 1)
  (command "rotate" (entlast) "" (getvar "lastpoint") pause)
  (F:RLAY)
  (F:MODR)
  (princ)
)

 ;defines visibility name for rooftop unit dynamic block insert
(defun VISNAMEFUNCT (range /)
  (cond ((>= tonnage (nth 6 range))
	 (setq visname (strcat (rtos (nth 6 range) 2 0) "-" (rtos (nth 7 range) 2 0) " Ton Down"))
	)
	((>= tonnage (nth 4 range))
	 (setq visname (strcat (rtos (nth 4 range) 2 0) "-" (rtos (nth 5 range) 2 0) " Ton Down"))
	)
	((>= tonnage (nth 2 range))
	 (setq visname (strcat (rtos (nth 2 range) 2 0) "-" (rtos (nth 3 range) 2 0) " Ton Down"))
	)
	(t (setq visname "fail")
	   (prompt "Tonnage not recognized, default will be used.")
	)
  )
)


;;;  /----------------------------------------------------------\
;;;  | Mech and plumb commands that use support functions.	|
;;;  \----------------------------------------------------------/

 ;ALIAS'
(defun C:BALL () (C:BAV))
(defun C:MT () (C:MTEXT))
(defun C:WL () (C:WLEN))

 ;INS_BLOCK
(defun C:CAP ()
  (INS_BLOCK "CAP" "current_layer" "end" "NEA" 0 "rotate" 0)
)
(defun C:CO ()
  (INS_BLOCK "CLEANOUT"	"current_layer" "end" "NEA" 0 "rotate" 0)
)
(defun C:DC ()
  (INS_BLOCK "D-PCAP" "current_layer" "end" "NEA" 0 "rotate" 0)
)
(defun C:DN ()
  (INS_BLOCK "DN" "current_layer" "end" "NEA" 0 "rotate" 0)
)
(defun C:DSD ()
  (INS_BLOCK "DSD" "ME-DSD-NEW" "nea" "per" 0 "rotate" 0)
)
(defun C:EB ()
  (INS_BLOCK "EB" "FS-PIPE-NEW" "nea end" "NEA" 0 "rotate" 0)
)
(defun C:ELL ()
  (INS_BLOCK "ELBOW" "current_layer" "int" "NEA" 0 "rotate" 0)
)
(defun C:FD ()
  (INS_BLOCK "FD" "PE-FIXT-NEW" "non" "NON" 0 0 96)
)
(defun C:FDC ()
  (INS_BLOCK "FDC" "FS-PIPE-NEW" "nea" "NEA" 0 "rotate" 0)
)
(defun C:FL ()
  (INS_BLOCK "FLOWA" "MH-DIFF-NEW" "non" "NON" 1 "rotate" 0)
)
(defun C:FL1 ()
  (INS_BLOCK "FLOW" "current_layer" "end nea" "NEA" 0 "rotate" 0)
)
(defun C:FLUE ()
  (INS_BLOCK "FLUE" "PE-EQPM-NEW" "non" "NON" 0 0 "scale")
)
(defun C:FMP1 ()
  (INS_BLOCK "PUMP_FLOOR1" "PE-EQPM-NEW" "non" "NON" 1 "rotate"	1)
)
(defun C:FMP2 ()
  (INS_BLOCK "PUMP_FLOOR2" "PE-EQPM-NEW" "non" "NON" 1 "rotate"	1)
)
(defun C:FRISE ()
  (INS_BLOCK "FRISER" "FS-PIPE-NEW" "non" "NON" 0 0 0)
)
(defun C:FSK ()
  (INS_BLOCK "FS" "PE-FIXT-NEW" "non" "NON" 0 0 96)
)
(defun C:GAUGE ()
  (INS_BLOCK "GAUGE" "current_layer" "nea" "NEA" 0 "rotate" 0)
)
(defun C:GS ()
  (INS_BLOCK "GS" "NOTE-NEW" "non" "NON" 0 0 0)
)
(defun C:HB ()
  (INS_BLOCK "hb" "current_layer" "end" "NEA" 0 "rotate" 0)
)
(defun C:IE ()
  (INS_BLOCK "IE" "NOTE-NEW" "non" "NON" 0 0 0)
)
(defun C:IEC ()
  (INS_BLOCK "IEC" "NOTE-NEW" "non" "NON" 0 0 0)
)
(defun C:MB ()
  (INS_BLOCK "MB" "current_layer" "NEA" "NEA" 0 "rotate" 0)
)
(defun C:MGO ()
  (INS_BLOCK "MG_OUTLET" "PE-EQPM-NEW" "non" "NON" 0 0 0)
)
(defun C:OD ()
  (INS_BLOCK "OVERFLOWDRAIN" "PS-RFDN-NEW" "cen" "NON" 0 0 0)
)
;;;(defun C:PCAP ()
;;;  (INS_BLOCK "D-PCAP" "current_layer" "end" "NEA" 0 "rotate" 0)
;;;)
(defun C:PETE ()
  (INS_BLOCK "PETESPLUG" "current_layer" "nea" "NEA" 0 "rotate"	0)
)
(defun C:PUMP ()
  (INS_BLOCK "PUMP_DIAGRAM" "PE-EQPM-NEW" "end nea" "NEA" 0 "rotate" 0)
)
(defun C:RD ()
  (INS_BLOCK "ROOFDRAIN" "PS-RFDN-NEW" "cen" "NON" 0 0 0)
)
(defun C:RFF ()
  (INS_BLOCK "RFF" "NOTE-NEW" "non" "NON" 0 0 0)
)
(defun C:SFD ()
  (INS_BLOCK "SFD" "ME-FSD-NEW" "nea" "NEA" 0 "rotate" 0)
)
;;;SOV, COULD BE IMPROVED UPON
(defun C:SOVR ()
  (INS_BLOCK "SHUTOFFRISER" "current_layer" "CEN END" "QUA" 1 "rotate" 0)
)
(defun C:T ()
  (INS_BLOCK "t" "current_layer" "nea" "NEA" 0 "rotate" 0)
)
(defun C:T45 ()
  (INS_BLOCK "TAKEOFF" "current_layer" "int" "NEA" 1 "rotate" 0)
)
(defun C:TAP ()
  (INS_BLOCK "PIPETAP" "current_layer" "nea" "NEA" 0 "rotate" 0)
)
(defun C:TEE ()
  (INS_BLOCK "TEE" "current_layer" "int" "NEA" 0 "rotate" 0)
)
(defun C:THERM ()
  (INS_BLOCK "THERMO" "current_layer" "nea" "NEA" 0 "rotate" 0)
)
(defun C:VD ()
  (INS_BLOCK "VDL" "current_layer" "nea" "NEA" 0 "rotate" 0)
)
(defun C:VDR ()
  (INS_BLOCK "VDR" "current_layer" "nea" "NEA" 0 "rotate" 0)
)
(defun C:WHA ()
  (INS_BLOCK "WHA" "current_layer" "nea" "NON" 0 0 0)
)
(defun C:WS ()
  (INS_BLOCK "WS" "NOTE-NEW" "non" "NON" 0 0 0)
)
(defun C:WVS ()
  (INS_BLOCK "WVS" "NOTE-NEW" "non" "NON" 0 0 0)
)

 ;INS_BLOCK_DL
(defun C:SFDD () (INS_BLOCK_DL "SFD" "ME-FSD-NEW" 0))
(defun C:VDD () (INS_BLOCK_DL "VDL" "current_layer" 0))
(defun C:VDDR () (INS_BLOCK_DL "VDR" "current_layer" 0))

 ;INS_BLOCK_TRIM
(defun C:BAV ()
  (INS_BLOCK_TRIM "VBA" "current_layer" 0.0625 "rotate")
)
(defun C:BFV ()
  (INS_BLOCK_TRIM "VBF" "current_layer" 0.0625 "rotate")
)
(defun C:CKV ()
  (INS_BLOCK_TRIM "VCK" "current_layer" 0.0625 "rotate")
)
(defun C:FCO ()
  (INS_BLOCK_TRIM "SCO" "current_layer" 0.0625 0)
)
(defun C:FCV ()
  (INS_BLOCK_TRIM "FCV" "current_layer" 0.0625 "rotate")
)
(defun C:GBV ()
  (INS_BLOCK_TRIM "VGB" "current_layer" 0.0625 "rotate")
)
(defun C:GC ()
  (INS_BLOCK_TRIM "VGC" "current_layer" 0.0625 "rotate")
)
(defun C:ILP ()
  (INS_BLOCK_TRIM
    "PUMP_INline"
    "PE-EQPM-NEW"
    0.21875
    "rotate"
  )
)
(defun C:RPBFP ()
  (INS_BLOCK_TRIM "RPBFP" "current_layer" 0.296875 "rotate")
)
(defun C:SCO ()
  (INS_BLOCK_TRIM "SCO" "current_layer" 0.0625 0)
)
(defun C:SOL ()
  (INS_BLOCK_TRIM "VSOL" "current_layer" 0.0625 "rotate")
)
(defun C:SOV ()
  (INS_BLOCK_TRIM "VSO" "current_layer" 0.0625 "rotate")
)
(defun C:STRAIN	()
  (INS_BLOCK_TRIM
    "STRAIN"
    "current_layer"
    0.1484375
    "rotate"
  )
)
(defun C:U1 ()
  (INS_BLOCK_TRIM "UN" "current_layer" 0.03125 "rotate")
)
(defun C:UP ()
  (INS_BLOCK_TRIM "UP" "current_layer" 0.0390625 0)
)
(defun C:VM2 ()
  (INS_BLOCK_TRIM "VM2" "current_layer" 0.0625 "rotate")
)
(defun C:VM3 ()
  (INS_BLOCK_TRIM "VM3" "current_layer" 0.0625 "rotate")
)
(defun C:VMB ()
  (INS_BLOCK_TRIM "VMB" "current_layer" 0.0625 "rotate")
)

 ;INS_MECH_DIFF
(defun C:EDF () (INS_MECH_DIFF "MH-DIFF-NEW" "EXHDIF" "non" 24 24 nil))
(defun C:EDFG () (INS_MECH_DIFF "MH-DIFF-NEW" "EXHDIF" "int" 24 24 "grid"))
(defun C:EDF1 () (INS_MECH_DIFF "MH-DIFF-NEW" "EXHDIF" "non" 1 nil nil))
(defun C:EDF2 () (INS_MECH_DIFF "MH-DIFF-NEW" "EXHDIF" "non" 0 nil nil))
(defun C:RDF () (INS_MECH_DIFF "MH-DIFF-NEW" "RETDIF" "non" 24 24 nil))
(defun C:RDFG () (INS_MECH_DIFF "MH-DIFF-NEW" "RETDIF" "int" 24 24 "grid"))
(defun C:RDF1 () (INS_MECH_DIFF "MH-DIFF-NEW" "RETDIF" "non" 1 nil nil))
(defun C:RDF2 () (INS_MECH_DIFF "MH-DIFF-NEW" "RETDIF" "non" 0 nil nil))
(defun C:SDF () (INS_MECH_DIFF "MH-DIFF-NEW" "SUPDIF" "non" 24 24 nil))
(defun C:SDFG () (INS_MECH_DIFF "MH-DIFF-NEW" "SUPDIF" "int" 24 24 "grid"))
(defun C:SDF1 () (INS_MECH_DIFF "MH-DIFF-NEW" "SUPDIF" "non" 1 nil nil))
(defun C:SDF2 () (INS_MECH_DIFF "MH-DIFF-NEW" "SUPDIF" "non" 0 nil nil))

 ;INS_MECH_ISO
(defun C:IDF () (INS_MECH_ISO "DF" "typical" 1))
(defun C:IDF2 () (INS_MECH_ISO "DF2" "double" 1))
(defun C:IFAG () (INS_MECH_ISO "fag" "fag" 1))
(defun C:IFD () (INS_MECH_ISO "FD" "typical" 1))
(defun C:IFD2 () (INS_MECH_ISO "FD2" "double" 1))
(defun C:IFS () (INS_MECH_ISO "FS" "typical" 1))
(defun C:IFS2 () (INS_MECH_ISO "FS2" "double" 1))
(defun C:ILAV () (INS_MECH_ISO "LAV" "typical" 1))
(defun C:ILAV2 () (INS_MECH_ISO "LAV2" "double" 1))
(defun C:IMS () (INS_MECH_ISO "MS" "typical" 1))
(defun C:IMS2 () (INS_MECH_ISO "MS2" "double" 1))
(defun C:IUR () (INS_MECH_ISO "UR" "typical" 1))
(defun C:IUR2 () (INS_MECH_ISO "UR2" "double" 1))
(defun C:IWC () (INS_MECH_ISO "WC" "typical" 0))
(defun C:IWC2 () (INS_MECH_ISO "WC2" "double" 0))
(defun C:IWCW () (INS_MECH_ISO "WCW" "typical" 0))
(defun C:IWCW2 () (INS_MECH_ISO "WCW2" "double" 0))

 ;INS_MECH_ISO_BLOCK
(defun C:IBF () (INS_MECH_ISO_BLOCK "BF" "typical" 0.0625))
(defun C:IBFV ()
  (INS_MECH_ISO_BLOCK "BF" "vertical" 0.0625)
)
(defun C:IBV () (INS_MECH_ISO_BLOCK "BV" "typical" 0.0625))
(defun C:IBVV ()
  (INS_MECH_ISO_BLOCK "BV" "vertical" 0.0625)
)
(defun C:IGC () (INS_MECH_ISO_BLOCK "GC" "typical" 0.0625))
(defun C:IGCV ()
  (INS_MECH_ISO_BLOCK "GC" "vertical" 0.0625)
)
(defun C:IHB () (INS_MECH_ISO_BLOCK "hb" "hb" nil))
(defun C:ISO ()
  (INS_MECH_ISO_BLOCK "SO" "typical" 0.0703125)
)
(defun C:ISOV ()
  (INS_MECH_ISO_BLOCK "SO" "vertical" 0.0703125)
)
(defun C:ITP () (INS_MECH_ISO_BLOCK "TP" "typical" nil))
(defun C:IUN () (INS_MECH_ISO_BLOCK "UN" "typical" 0.0625))
(defun C:IUNV ()
  (INS_MECH_ISO_BLOCK "UN" "vertical" 0.0625)
)
(defun C:IWHA () (INS_MECH_ISO_BLOCK "WHA" "typical" nil))

 ;INS_MECH_ISO_SCO
(defun C:IFCO () (INS_MECH_ISO_SCO 0))
(defun C:ISCO () (INS_MECH_ISO_SCO 0))
(defun C:ISCO2 () (INS_MECH_ISO_SCO 1))

 ;INS_MECH_ISO_VTR
(defun C:IVTR () (INS_MECH_ISO_VTR "ISOVTR_30"))
(defun C:IVTR1 () (INS_MECH_ISO_VTR "ISOVTR_150"))

 ;INS_MECH_WALL_DIFF
(defun C:SWD () (INS_MECH_WALL_DIFF "MH-DIFF-NEW" "supply"))
(defun C:SWR () (INS_MECH_WALL_DIFF "MH-DIFF-NEW" "return"))

 ;INS_SENSOR
(defun C:CST () (INS_SENSOR "CSTAT" 0.078125))
(defun C:HST () (INS_SENSOR "HSTAT" 0.078125))
(defun C:SST () (INS_SENSOR "SSTAT" 0.078125))
(defun C:TST () (INS_SENSOR "TSTAT" 0.078125))

 ;ROOFTOPUNIT
(defun C:GP () (ROOFTOPUNIT "_GP"))
(defun C:HP () (ROOFTOPUNIT "_HP"))



;;;  /----------------------------------------------------------\
;;;  | Mechanical and plumbing commands.			|
;;;  \----------------------------------------------------------/

 ;INSERTS ISOMETRIC WASTE RISER
(defun C:ILVL (/      pt1    pt2    pt3	   pt4	  pt5	 pt6	pt7
	       pt8    pt9    pt10   ang1   ang2	  ang3	 wco_dir
	       iso_wco trim_line
	      )
  (command "undo" "group")
  (F:SLAY "0")
  (F:MODS (quote ("orthomode" "osmode" "snapstyl")))
  (setvar "orthomode" 1)
  (SET_OSMODE "nea end")
  (setvar "snapstyl" 1)
  (setq pt1 (getpoint "\nSelect Point on Waste line:"))
  (SET_OSMODE "non")
  (setq pt2 (polar pt1 (/ pi 2) (* (SCALEFACTOR) 0.125)))
  (setq pt3 (polar pt2 (/ pi 2) (* (SCALEFACTOR) 1.625)))
  (setq pt4 (polar pt3 (/ pi 2) (* (SCALEFACTOR) 0.125)))
  (setq pt5 (polar pt4 (/ pi 2) (* (SCALEFACTOR) 0.125)))
  (F:SLAY "PS-WSTE-NEW")
  (command "line" pt2 pt4 "")
  (setq pt6 (getpoint pt1 "\nSelect Direction of Waste:"))
  (setq ang1 (angle pt1 pt6))
  (setq pt7 (polar pt1 ang1 (* (SCALEFACTOR) 0.125)))
  (command "line" pt7 pt2 "")

  (setq trim_line (ssget "l"));;
  (ISO_END_TRIM pt1 trim_line);;
  (SET_OSMODE "non");;

  (setq pt8 (getpoint pt5 "\nSelect Direction of Next Level Waste:"))
  (setq ang2 (angle pt5 pt8))
  (setq pt9 (polar pt5 ang2 (* (SCALEFACTOR) 0.125)))
  (command "line" pt9 pt4 "")
  (if (or (< ang2 (/ pi 2)) (> ang2 (* (/ pi 2) 3)))
    (command "-insert" "ISOLVL_30" pt3 (SCALEFACTOR) "" "")
    (command "-insert" "ISOLVL_150" pt3 (SCALEFACTOR) "" "")
  )
  (command "explode" "l" "")
  (setq pt10 (getpoint pt2 "\nSelect Direction of Cleanout:"))
  (setq ang3 (angle pt2 pt10))
  (cond	((< ang3 (/ pi 2)) (setq wco_dir "_30"))
	((< ang3 pi) (setq wco_dir "_150"))
	((< ang3 (* 3 (/ pi 2))) (setq wco_dir "_210"))
	(t (setq wco_dir "_330"))
  )
  (setq iso_wco (strcat "ISOWCO" wco_dir))
  (F:SLAY "PE-FIXT-NEW")
  (command "-insert" iso_wco pt2 (SCALEFACTOR) "" "")
  (F:RLAY)
  (F:MODR)
  (command "undo" "end")
  (princ)
)
 
 ;Rotate floor plan piping to isometric view from left
(defun C:ISOL (/ pt1 pt2 selection ang1 center center_prime)
  (load "flattensup.lsp")
  (F:MODS (quote ("osmode" "attreq")))
  (setvar "attreq" 0)
  (SET_OSMODE "non")
  (setq pt1 (getpoint "\nWINDOW AROUND OBJECTS - FIRST POINT"))
  (setq pt2 (getcorner  pt1 "\nSECOND POINT"))
  (prompt "\nPlease Wait...\n")
  (setq selection (ssget "c" pt1 pt2))
  (setq ang1 (angle pt1 pt2))
  (setq center (polar pt1 ang1 (/ (distance pt1 pt2) 2)))
  (setq center_prime (list (nth 2 center) (nth 1 center) (nth 0 center)))
  (command "rotate" selection "" center 45)
  (command "ucs" "y" -90)
  (command "rotate" selection "" center_prime 54.7355)
  (command "ucs" "y" 90)
  (ACET-FLATN selection nil);ACET-FLATN is an express lisp on the C: drive
  (prompt "\nDone.")
  (F:MODR)
  (princ)
)


 ;Rotate floor plan piping to isometric view from right
(defun C:ISOR (/ pt1 pt2 selection ang1 center center_prime)
  (load "flattensup.lsp")
  (F:MODS (quote ("osmode" "attreq")))
  (setvar "attreq" 0)
  (SET_OSMODE "non")
  (setq pt1 (getpoint "\nWINDOW AROUND OBJECTS - FIRST POINT"))
  (setq pt2 (getcorner  pt1 "\nSECOND POINT"))
  (prompt "\nPlease Wait...")
  (setq selection (ssget "c" pt1 pt2))
  (setq ang1 (angle pt1 pt2))
  (setq center (polar pt1 ang1 (/ (distance pt1 pt2) 2)))
  (setq center_prime (list (nth 2 center) (nth 1 center) (nth 0 center)))
  (command "rotate" selection "" center -45)
  (command "ucs" "y" -90)
  (command "rotate" selection "" center_prime 54.7355)
  (command "ucs" "y" 90)
  (ACET-FLATN selection nil);ACET-FLATN is an express lisp on the C: drive
  (prompt "\nDone.")
  (F:MODR)
  (princ)
)

 ;DRAW_MECH_TAKEOFF
(defun C:RDT ()
  (F:MODS (quote ("orthomode" "osmode" "filletrad" "plinewid")))
  (DRAW_MECH_TAKEOFF "MH-DUCT-NEW" "user" "user" "user" "user" "user")
  (F:RLAY)
  (F:MODR)
)

 ;Quick layout of supply diffusers for a rectangular room
(defun C:SD (/		 pt_grid     pt_room1	 pt_room2
	     pts_diffs	 num_diff_x  num_diff_y	 yes
	     room_ang	 room_dist
	    )
  (command "undo" "group")
  (F:MODS (quote ("osmode" "orthomode" "filletrad")))
  (SET_OSMODE "end int")
  (setvar "orthomode" 0)
  (setq pt_room1 (getpoint "\nSelect first corner of room: "))
  (prompt "\nSelect second corner of room: ")
  (SET_OSMODE "end int")
  (command "rectangle" pt_room1 pause)
  (setq pt_room2 (getvar "lastpoint"))
  (SET_OSMODE "int")
  (setq pt_grid (getpoint "\nSelect RCP grid intersection of <Enter> for center: "))
  (SET_OSMODE "non")
  (command "erase" (entlast) "")
  (if (= pt_grid nil)
    (progn
      (setq room_ang (angle pt_room1 pt_room2)
	    room_dist (distance pt_room1 pt_room2)
	    pt_grid (polar pt_room1 room_ang (/ room_dist 2))
	    pt_grid (polar pt_grid 0 12)
	    pt_grid (polar pt_grid (/ pi 2) 12)
      )
    )
  )
  (DIFFUSER_INSERT pt_room1 pt_room2 pt_grid)
  (if (and (= num_diff_x 2) (= num_diff_y 2))
    (setq
      yes (getstring "\nWould you like to draw the ductwork? <Y>: ")
    )
    (setq yes "n")
  )
  (if (/= (strcase yes) "Y" (strcase yes) "YES" (strcase yes) "")
    ()
    (DIFFUSER_DUCT)
  )
 ;Close out
  (F:RLAY)
  (F:MODR)
  (command "undo" "end")
  (princ)
) ;End of SD

 ;Draws double line duct turning vanes
(defun C:TV (/	       pt1	pt2	 dist	  dist_x   dist_y
	      ang      pt_x1	pt_x2	 pt_y1	  pt_y2	   place
	      number_tv		n	 entities
	     )
  (F:SLAY "0")
  (F:MODS (quote ("osmode" "orthomode")))
  (setvar "orthomode" 0)
  (SET_OSMODE "int")
  (setq pt1 (getpoint "\nPick inside corner of turn."))
  (setq pt2 (getpoint pt1 "\nPick outside corner of turn."))
  (SET_OSMODE "non")
  (setq dist (distance pt1 pt2))
  (setq dist_x (abs (- (nth 0 pt1) (nth 0 pt2))))
  (setq dist_y (abs (- (nth 1 pt1) (nth 1 pt2))))
  (setq ang (angle pt1 pt2))
  (setq	block_name
	 (strcat "Turning_Vanes_"
		 (rtos dist_x 2 0)
		 "_"
		 (rtos dist_y 2 0)
		 "_"
		 (rtos (R->D ang) 2 0)
	 )
  )
  (if (/= (tblsearch "block" block_name) nil)
    (command "insert" block_name "s" 1 "r" 0 pt1)
    (progn
      (cond ((< ang (/ pi 2))
	     (setq pt_x1 (polar pt1 (* (/ pi 2) 3) 4))
	     (setq pt_x2 (polar pt_x1 0 dist_x))
	     (setq pt_y1 (polar pt1 0 -4))
	     (setq pt_y2 (polar pt_y1 (/ pi 2) dist_y))
	    )
	    ((< ang pi)
	     (setq pt_x1 (polar pt1 (* (/ pi 2) 3) 4))
	     (setq pt_x2 (polar pt_x1 pi dist_x))
	     (setq pt_y1 (polar pt1 0 4))
	     (setq pt_y2 (polar pt_y1 (/ pi 2) dist_y))
	    )
	    ((< ang (* (/ pi 2) 3))
	     (setq pt_x1 (polar pt1 (* (/ pi 2) 3) -4))
	     (setq pt_x2 (polar pt_x1 pi dist_x))
	     (setq pt_y1 (polar pt1 0 4))
	     (setq pt_y2 (polar pt_y1 (* (/ pi 2) 3) dist_y))
	    )
	    ((< ang (* pi 2))
	     (setq pt_x1 (polar pt1 (* (/ pi 2) 3) -4))
	     (setq pt_x2 (polar pt_x1 0 dist_x))
	     (setq pt_y1 (polar pt1 0 -4))
	     (setq pt_y2 (polar pt_y1 (* (/ pi 2) 3) dist_y))
	    )
      )
      (setq place pt1)
      (setq number_tv (/ (- dist 5) 4.5))
      (setq n 0)
      (while (< n number_tv)
	(command "insert" "turning_vane" "s" 1 "r" (R->D ang) place)
	(setq entities (append entities (list (entlast))))
	(setq place (polar place ang 4.5))
	(setq n (1+ n))
      )
      (command "-color" 51)
      (command "line" pt_x1 pt_x2 "")
      (setq entities (append entities (list (entlast))))
      (command "line" pt_y1 pt_y2 "")
      (setq entities (append entities (list (entlast))))
      (command "-color" "bylayer")
      (command "-block" block_name pt1)
      (setq n 0)
      (repeat (length entities)
	(command (nth n entities))
	(setq n (1+ n))
      )
      (command "")
      (F:SLAY "0")
      (command "insert" block_name "s" 1 "r" 0 pt1)
      (MATCH_ENTITY_LAYER nil)
    ) ;end progn
  ) ;end if
  (F:RLAY)
  (F:MODR)
  (princ)
);end of TV


;;;  /------------------------------------------------------------------\
;;;  | The following function and command definitions allow for the     |
;;;  | drawing of flexible ductwork (for double-line duct drawings).    |
;;;  | The function is based on a lisp routine by Jim Weisbin.          |
;;;  | NOT STANDARD LSW FLEX DUCT, USE FOR EXISTING CONDITIONS	|
;;;  \------------------------------------------------------------------/

(DEFUN flexduct	(layer	/      flexduct_radius	    d3	   w
		 ribsp	p1     p2     p3     p4	    p5	   count
		 s1	s2     a1     a2     a90    a270   a360
		 a3	a4     mid1   mid2   sang   eang   temp
		 a1	a2     x      w	     radius c1
		)
  (F:MODS (QUOTE ("cmdecho" "plinewid" "celtscale" "cecolor")))
  (f:slay layer)
  (setvar "cecolor" "bylayer")
  (setvar "celtscale" 0.5)
  (graphscr)
  (setq	flexduct_diameter
	 (if (or (not (numberp flexduct_diameter))
		 (zerop flexduct_diameter)
	     )
	   12.0
	   flexduct_diameter
	 )
	ribsp 3
  )
  (if
    (setq p1 (getpoint "\nStart point: "))
     (progn
       (initget 1)
       (setq p2 (getpoint p1 "\nPoint on arc: "))
       (while (equal p1 p2)
	 (initget 1)
	 (setq
	   p2 (getpoint p1 "\nSame point not allowed - Point on arc: ")
	 )
       )				; end while
       (grdraw p1 p2 -1)
       (setq p3 (getpoint p2 "\nEnd point/<straight line>: "))
       (if (null p3)
	 (setq p3 p2)
       )
       (grdraw p2 p3 -1)
       (initget 6)
       (setq d3		       (getdist	p3
					(strcat	"\nFlexduct Diameter <"
						(rtos flexduct_diameter 2 2)
						">: "
					)
			       )
	     flexduct_diameter (if (null d3)
				 flexduct_diameter
				 d3
			       )
	     flexduct_radius   (* 0.5 flexduct_diameter)
	     mid1	       (list (/ (+ (car p1) (car p2)) 2)
				     (/ (+ (cadr p1) (cadr p2)) 2)
			       )
	     mid2	       (list (/ (+ (car p2) (car p3)) 2)
				     (/ (+ (cadr p2) (cadr p3)) 2)
			       )
	     a1		       (angle p1 p2)
	     a2		       (angle p2 p3)
;;; check if angle 1 is 180 degrees from angle 2
;;; if yes, then this is a straight duct:
	     p3		       (if (= (angle p1 p2) (angle p3 p2))
				 p2
				 p3
			       )
	     a90	       (/ pi 2.0)
	     a270	       (* pi 1.5)
	     a360	       (* pi 2.0)
       )
;;;
;;; start the pline:

       (command "pline" p1 "w" 0 0)
;;; if a1=a2 or p2=p3 then this is a straight line, else draw curved
       (if (or (equal a1 a2) (equal p2 p3))
	 (progn
	   (if (< (distance p1 p3) (distance p1 p2))
	     (setq p3 p2)
	   )				; end if
;;; ensure that count is rounded up to an even number:
	   (setq count (* 2 (fix (+ (/ (distance p1 p3) ribsp 2) 0.5)))
;;; divide distance by count to get each segment:
		 seg   (/ (distance p1 p3) count)
;;;; get starting point:
		 temp  (polar p1 (- a1 pi) (/ seg 2.0))
	   )
	   (repeat (/ count 2)
	     (command (polar (setq temp (polar temp a1 seg))
			     (+ a1 a90)
			     flexduct_radius
		      )
	     )
	     (command (polar (setq temp (polar temp a1 seg))
			     (- a1 a90)
			     flexduct_radius
		      )
	     )
	   )
	   end
	   repeat
	 )				; end progn (if)
	 (progn
	   (setq p4 (polar mid1 (- a1 a90) flexduct_radius))
	   (setq p5 (polar mid2 (- a2 a90) flexduct_radius))
	   (setq c1	(inters mid1 p4 mid2 p5 nil)
		 radius	(distance c1 p1)
		 sang	(angle c1 p1)
		 eang	(angle c1 p3)
		 a3	(abs (- (angle p2 p1) (angle p2 p3)))
		 a3	(if (>= a3 pi)
			  (- a360 a3)
			  a3
			)
		 a4	(- (angle p1 p2) (angle p2 p3))
		 a1	(abs (- sang eang))
		 a2	(if (> a1 pi)
			  (- a360 a1)
			  a1
			)
		 a2	(if (< a3 a90)
			  (- a360 a2)
			  a2
			)
		 count	(* 2 (fix (+ (/ (* radius a2) ribsp 2) 0.5)))
		 s1	(/ a2 count)
		 s2	(if (> sang eang)
			  (- 0.0 s1)
			  s1
			)
		 s2	(if (< a3 a90)
			  (- 0.0 s2)
			  s2
			)
		 s2	(if (> a1 pi)
			  (- 0.0 s2)
			  s2
			)
		 s2	(if (or (= a4 a90) (= a4 (- 0.0 a270)))
			  (- 0.0 s1)
			  s2
			)
		 s2	(if (or (= a4 a270) (= a4 (- 0.0 a90)))
			  s1
			  s2
			)
		 temp	(- sang (/ s2 2.0))
	   )
	   (repeat (/ count 2)
	     (command (polar c1
			     (setq temp (+ temp s2))
			     (+ radius flexduct_radius)
		      )
	     )
	     (command (polar c1
			     (setq temp (+ temp s2))
			     (- radius flexduct_radius)
		      )
	     )
	   )				; end repeat
	 )				; end progn (else)
       )				; end if
					; now finish the polyline:
       (command p3 "")
					; clean up the drawing:
       (grdraw p1 p2 -1)
       (grdraw p2 p3 -1)
       (redraw (entlast) 1)
     )					; end main progn
  )					; end main if
  (f:modr)
  (f:rlay)
  (princ)
)					; end function

(defun c:EF () (flexduct "MH-EXHA-NEW"))
(defun c:RF () (flexduct "MH-RETN-NEW"))
(defun c:SF () (flexduct "MH-SPLY-NEW"))


;;;  /----------------------------------------------------------\
;;;  | Commands to set mech and plumb layers.			|
;;;  \----------------------------------------------------------/

 ;NEW LAYERS
(defun C:CHWR () (LAYERSET "MP-CHWR-NEW") (princ))
(defun C:CHWS () (LAYERSET "MP-CHWS-NEW") (princ))
(defun C:CLDW () (LAYERSET "PB-CLDW-NEW") (princ))
(defun C:CNDR () (LAYERSET "MP-CNDR-NEW") (princ))
(defun C:COND () (LAYERSET "MP-COND-NEW") (princ))
(defun C:CONT () (LAYERSET "ME-CONT-NEW") (princ))
(defun C:CWR () (LAYERSET "MP-CWR-NEW") (princ))
(defun C:CWS () (LAYERSET "MP-CWS-NEW") (princ))
(defun C:MDEMO () (LAYERSET "MH-DEMO") (princ))
(defun C:DIFF () (LAYERSET "MH-DIFF-NEW") (princ))
(defun C:DIR () (LAYERSET "PL-DIR-NEW") (princ))
(defun C:DIS () (LAYERSET "PL-DIS-NEW") (princ))
(defun C:DUCT () (LAYERSET "MH-DUCT-NEW") (princ))
(defun C:EQPM () (LAYERSET "ME-EQPM-NEW") (princ))
(defun C:EXHA () (LAYERSET "MH-EXHA-NEW") (princ))
(defun C:FIXT () (LAYERSET "PE-FIXT-NEW") (princ))
(defun C:GWST () (LAYERSET "PS-GWST-NEW") (princ))
(defun C:HEAD () (LAYERSET "FS-HEAD-NEW") (princ))
(defun C:HHWR () (LAYERSET "MP-HHWR-NEW") (princ))
(defun C:HHWS () (LAYERSET "MP-HHWS-NEW") (princ))
(defun C:HOTW () (LAYERSET "PB-HOTW-NEW") (princ))
(defun C:HTWR () (LAYERSET "PB-HTWR-NEW") (princ))
(defun C:LGAS () (LAYERSET "PF-LGAS-NEW") (princ))
(defun C:MAIR () (LAYERSET "PM-MAIR-NEW") (princ))
(defun C:MVAC () (LAYERSET "PM-MVAC-NEW") (princ))
(defun C:NGAS () (LAYERSET "PF-NGAS-NEW") (princ))
(defun C:NIOX () (LAYERSET "PM-NIOX-NEW") (princ))
(defun C:NTGN () (LAYERSET "PM-NTGN-NEW") (princ))
(defun C:OAIR () (LAYERSET "MH-OAIR-NEW") (princ))
(defun C:OIL () (LAYERSET "PF-OIL-NEW") (princ))
(defun C:OXGN () (LAYERSET "PM-OXGN-NEW") (princ))
(defun C:PIPE () (LAYERSET "FS-PIPE-NEW") (princ))
(defun C:PDEMO () (LAYERSET "PB-DEMO") (princ))
(defun C:PEQPM () (LAYERSET "PE-EQPM-NEW") (princ))
(defun C:RETN () (LAYERSET "MH-RETN-NEW") (princ))
(defun C:RFDN () (LAYERSET "PS-RFDN-NEW") (princ))
(defun C:RFGT () (LAYERSET "MP-RFGT-NEW") (princ))
(defun C:ROR () (LAYERSET "PL-ROR-NEW") (princ))
(defun C:ROS () (LAYERSET "PL-ROS-NEW") (princ))
(defun C:SPLY () (LAYERSET "MH-SPLY-NEW") (princ))
(defun C:STAN () (LAYERSET "FS-STAN-NEW") (princ))
(defun C:STEM () (LAYERSET "MP-STEM-NEW") (princ))
(defun C:VENT () (LAYERSET "PS-VENT-NEW") (princ))
(defun C:WSTE () (LAYERSET "PS-WSTE-NEW") (princ))

(defun C:CW () (C:CLDW))
(defun C:EQ () (C:EQPM))
(defun C:EXH () (C:EXHA))
(defun C:GW () (C:GWST))
(defun C:HW () (C:HOTW))
(defun C:HWR () (C:HTWR))
(defun C:OSA () (C:OAIR))
(defun C:PEQ () (C:PEQPM))
(defun C:PFIX () (C:FIXT))
(defun C:RET () (C:RETN))
(defun C:SUP () (C:SPLY))
(defun C:VTR () (C:VENT))
(defun C:W () (C:WSTE))

 ;EXISTING LAYERS
(defun C:ECHWR () (LAYERSET "MP-CHWR-EXST") (princ))
(defun C:ECHWS () (LAYERSET "MP-CHWS-EXST") (princ))
(defun C:ECLDW () (LAYERSET "PB-CLDW-EXST") (princ))
(defun C:ECNDR () (LAYERSET "MP-CNDR-EXST") (princ))
(defun C:ECOND () (LAYERSET "MP-COND-EXST") (princ))
(defun C:ECONT () (LAYERSET "ME-CONT-EXST") (princ))
(defun C:ECWR () (LAYERSET "MP-CWR-EXST") (princ))
(defun C:ECWS () (LAYERSET "MP-CWS-EXST") (princ))
(defun C:EDIFF () (LAYERSET "MH-DIFF-EXST") (princ))
(defun C:EDUCT () (LAYERSET "MH-DUCT-EXST") (princ))
(defun C:EEQPM () (LAYERSET "ME-EQPM-EXST") (princ))
(defun C:EEXHA () (LAYERSET "MH-EXHA-EXST") (princ))
(defun C:EFIXT () (LAYERSET "PE-FIXT-EXST") (princ))
(defun C:EGWST () (LAYERSET "PS-GWST-EXST") (princ))
(defun C:EHEAD () (LAYERSET "FS-HEAD-EXST") (princ))
(defun C:EHHWR () (LAYERSET "MP-HHWR-EXST") (princ))
(defun C:EHHWS () (LAYERSET "MP-HHWS-EXST") (princ))
(defun C:EHOTW () (LAYERSET "PB-HOTW-EXST") (princ))
(defun C:EHTWR () (LAYERSET "PB-HTWR-EXST") (princ))
(defun C:ELGAS () (LAYERSET "PF-LGAS-EXST") (princ))
(defun C:EMAIR () (LAYERSET "PM-MAIR-EXST") (princ))
(defun C:EMVAC () (LAYERSET "PM-MVAC-EXST") (princ))
(defun C:ENGAS () (LAYERSET "PF-NGAS-EXST") (princ))
(defun C:ENIOX () (LAYERSET "PM-NIOX-EXST") (princ))
(defun C:ENTGN () (LAYERSET "PM-NTGN-EXST") (princ))
(defun C:EOAIR () (LAYERSET "MH-OAIR-EXST") (princ))
(defun C:EOIL () (LAYERSET "PF-OIL-EXST") (princ))
(defun C:EOXGN () (LAYERSET "PM-OXGN-EXST") (princ))
(defun C:EPIPE () (LAYERSET "FS-PIPE-EXST") (princ))
(defun C:EPEQPM () (LAYERSET "PE-EQPM-EXST") (princ))
(defun C:ERETN () (LAYERSET "MH-RETN-EXST") (princ))
(defun C:ERFDN () (LAYERSET "PS-RFDN-EXST") (princ))
(defun C:ERFGT () (LAYERSET "MP-RFGT-EXST") (princ))
(defun C:ESPLY () (LAYERSET "MH-SPLY-EXST") (princ))
(defun C:ESTAN () (LAYERSET "FS-STAN-EXST") (princ))
(defun C:ESTEM () (LAYERSET "MP-STEM-EXST") (princ))
(defun C:EVENT () (LAYERSET "PS-VENT-EXST") (princ))
(defun C:EWSTE () (LAYERSET "PS-WSTE-EXST") (princ))

(defun C:ECW () (C:ECLDW))
(defun C:EEQ () (C:EEQPM))
(defun C:EEXH () (C:EEXHA))
(defun C:EGW () (C:EGWST))
(defun C:EHW () (C:EHOTW))
(defun C:EHWR () (C:EHTWR))
(defun C:EPEQ () (C:EPEQPM))
(defun C:EPFIX () (C:EFIXT))
(defun C:ERET () (C:ERETN))
(defun C:ESUP () (C:ESPLY))
(defun C:EVTR () (C:EVENT))
(defun C:EW () (C:EWSTE))

 ;DEMO LAYERS
(defun C:DCHWR () (LAYERSET "MP-CHWR-DEMO") (princ))
(defun C:DCHWS () (LAYERSET "MP-CHWS-DEMO") (princ))
(defun C:DCLDW () (LAYERSET "PB-CLDW-DEMO") (princ))
(defun C:DCNDR () (LAYERSET "MP-CNDR-DEMO") (princ))
(defun C:DCOND () (LAYERSET "MP-COND-DEMO") (princ))
(defun C:DCONT () (LAYERSET "ME-CONT-DEMO") (princ))
(defun C:DCWR () (LAYERSET "MP-CWR-DEMO") (princ))
(defun C:DCWS () (LAYERSET "MP-CWS-DEMO") (princ))
(defun C:DDIFF () (LAYERSET "MH-DIFF-DEMO") (princ))
(defun C:DDUCT () (LAYERSET "MH-DUCT-DEMO") (princ))
(defun C:DEQPM () (LAYERSET "ME-EQPM-DEMO") (princ))
(defun C:DEXHA () (LAYERSET "MH-EXHA-DEMO") (princ))
(defun C:DFIXT () (LAYERSET "PE-FIXT-DEMO") (princ))
(defun C:DGWST () (LAYERSET "PS-GWST-DEMO") (princ))
(defun C:DHEAD () (LAYERSET "FS-HEAD-DEMO") (princ))
(defun C:DHHWR () (LAYERSET "MP-HHWR-DEMO") (princ))
(defun C:DHHWS () (LAYERSET "MP-HHWS-DEMO") (princ))
(defun C:DHOTW () (LAYERSET "PB-HOTW-DEMO") (princ))
(defun C:DHTWR () (LAYERSET "PB-HTWR-DEMO") (princ))
(defun C:DLGAS () (LAYERSET "PF-LGAS-DEMO") (princ))
(defun C:DMAIR () (LAYERSET "PM-MAIR-DEMO") (princ))
(defun C:DMVAC () (LAYERSET "PM-MVAC-DEMO") (princ))
(defun C:DNGAS () (LAYERSET "PF-NGAS-DEMO") (princ))
(defun C:DNIOX () (LAYERSET "PM-NIOX-DEMO") (princ))
(defun C:DNTGN () (LAYERSET "PM-NTGN-DEMO") (princ))
(defun C:DOAIR () (LAYERSET "MH-OAIR-DEMO") (princ))
(defun C:DOIL () (LAYERSET "PF-OIL-DEMO") (princ))
(defun C:DOXGN () (LAYERSET "PM-OXGN-DEMO") (princ))
(defun C:DPIPE () (LAYERSET "FS-PIPE-DEMO") (princ))
(defun C:DPEQPM () (LAYERSET "PE-EQPM-DEMO") (princ))
(defun C:DRETN () (LAYERSET "MH-RETN-DEMO") (princ))
(defun C:DRFDN () (LAYERSET "PS-RFDN-DEMO") (princ))
(defun C:DRFGT () (LAYERSET "MP-RFGT-DEMO") (princ))
(defun C:DSPLY () (LAYERSET "MH-SPLY-DEMO") (princ))
(defun C:DSTAN () (LAYERSET "FS-STAN-DEMO") (princ))
(defun C:DSTEM () (LAYERSET "MP-STEM-DEMO") (princ))
(defun C:DVENT () (LAYERSET "PS-VENT-DEMO") (princ))
(defun C:DWSTE () (LAYERSET "PS-WSTE-DEMO") (princ))

(defun C:DCW () (C:DCLDW))
(defun C:DEQ () (C:DEQPM))
(defun C:DEXH () (C:DEXHA))
(defun C:DGW () (C:DGWST))
(defun C:DHW () (C:DHOTW))
(defun C:DHWR () (C:DHTWR))
(defun C:DPEQ () (C:DPEQPM))
(defun C:DPFIX () (C:DFIXT))
(defun C:DRET () (C:DRETN))
(defun C:DSUP () (C:DSPLY))
(defun C:DVTR () (C:DVENT))
(defun C:DW () (C:DWSTE))




