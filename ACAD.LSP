;;;;;
;;  LSW_ACAD.LSP - CUSTOM LSW GENERAL ACAD LISPS
;;
;;  Copyright © 2008 by LSW Engineers Arizona, Inc.
;;
;;  
;;-------------------------------------------------------------------------
;;  DESCRIPTION
;;	General ACAD lisp functions and commands not specific to
;;	mechanical, plumbing, or electrical.
;;	Broken into sections: startup function, support functions, 
;;	commands that call support functions, commands, and layer changes.
;;
;;-------------------------------------------------------------------------
;;

;;;  /----------------------------------------------------------\
;;;  | General startup function.				|
;;;  \----------------------------------------------------------/

(princ "\nLSW Lisp Routines Loading...")
  
(defun S::STARTUP ()
  (princ "\nInitializing drawing variables...")
  (setvar "acadlspasdoc" 1)
  (setvar "menuecho" 0)
  (setvar "cmdecho" 0)
  (setvar "highlight" 1)
  (setvar "plinetype" 2)
  (setvar "psltscale" 1)
  (setvar "regenmode" 1)
  (setvar "rememberfolders" 1)
  (setvar "sortents" 127)
  (setvar "ucsfollow" 0)
  (setvar "xloadctl" 0)
  (setvar "qpmode" 0)
  (command "imageframe" 2)
  (if (= (getvar "dimscale") 0)
    (setvar "dimscale" 1.0)
  )
  (if (and (/= (getvar "userr3") 0)
	   (/= (getvar "userr3") (getvar "dimscale"))
      )
    (setvar "dimscale" (getvar "userr3"))
  )
  (setq dwgsc (getvar "dimscale"))
  (setq edwgsc dwgsc)
  (setvar "textstyle" "Standard")
  (setvar "textsize" (* (SCALEFACTOR) 0.09375))
  ;(command "-style" "" "simplex.shx" 0.0 "" 0.0 "n" "n" "n")
  (command "-plotstamp" "on" "")
;; **************************************************
;; Temorary -- Remove xl2cad from 64bit machines
;;  (command "cuiunload" "xl2cad")  
;; **************************************************
  (setq last_current_layer nil)
  (load "acad-add")
  (prompt "\nDrawing initialization complete.")
  (setvar "layereval" 0)
  (princ)
)



;;;  /----------------------------------------------------------\
;;;  | General ACAD support functions.                          |
;;;  \----------------------------------------------------------/

 ;ERROR FUNCTION
(defun *ERROR* (msg)
  (princ "\nError: ")
  (princ msg)
  (if (/= nil v:mlst)
    (progn
      (F:RLAY)
      (F:MODR)
      (prompt "\nSettings restored to pre-last-command states.")
    )
  )
  (terpri)
)

 ;Arccosine (in radians) of value
(defun ACOS (x /)
  (- (/ pi 2) (ASIN x))
)

 ;Arcsine (in radians) of value
(defun ASIN (x /)
  (* 2 (atan (/ x (1+ (sqrt (- 1 (expt x 2)))))))
)

 ;Changes block entity to by-layer
(defun BLOCKBYLAYER (manual / ss cnt idx blkname donelist grp update)
  (defun grp (gc el) (cdr (assoc gc el)))
  (defun update	(bname / ename elist)
    (setq ename (tblobjname "BLOCK" bname))
    (if
      (and ename
	   (zerop (logand 52 (grp 70 (entget ename '("*")))))
      )
       (progn
	 (while	ename
	   (setq elist (entget ename '("*"))
		 elist (subst '(8 . "0") (assoc 8 elist) elist)
		 elist (if (assoc 62 elist)
			 (subst '(62 . 256) (assoc 62 elist) elist)
			 (append elist '((62 . 256)))
		       )
	   )
	   (entmake elist)
	   (setq ename (entnext ename))
	 )
	 (if (/= "ENDBLK" (grp 0 elist))
	   (entmake '((0 . "ENDBLK") (8 . "0") (62 . 256)))
	 )
	 'T
       )
    )
  )
  (if (> (logand (grp 70 (tblsearch "layer" "0")) 1) 0)
    (princ "\nLayer 0 must be thawed before running FIXBLOCK!\n"
    )
    (progn
      (if
	(progn
	  (princ "\nSelect a block to fix or press <Enter> to fix all defined blocks:\n")
	  (setq	cnt 0
		ss  (if (= manual "y")
		      (ssget '((0 . "INSERT")))
		      (ssget "_X" '((0 . "INSERT")))
		    )
	  )
	)
	 (progn
	   (setq idx (sslength ss))
	   (while (>= (setq idx (1- idx)) 0)
	     (if
	       (not
		 (member (setq blkname (Grp 2 (entget (ssname ss idx))))
			 donelist
		 )
	       )
		(progn
		  (if (update blkname)
		    (setq cnt (1+ cnt))
		  )
		  (setq donelist (cons blkname donelist))
		)
	     )
	   )
	 )
	 (while	(setq blkname (Grp 2 (tblnext "BLOCK" (not blkname))))
	   (if (update blkname)
	     (setq cnt (1+ cnt))
	   )
	 )
      )
      (princ (strcat "\n"
		     (itoa cnt)
		     " block"
		     (if (= cnt 1)
		       ""
		       "s"
		     )
		     " redefined."
	     )
      )
    )
  )
  (princ)
) ;END OF BLOCKBYLAYER

(defun BLOCKBYBLOCK (manual / ss cnt idx blkname donelist grp update)
  (defun grp (gc el) (cdr (assoc gc el)))
  (defun update	(bname / ename elist)
    (setq ename (tblobjname "BLOCK" bname))
    (if
      (and ename
	   (zerop (logand 52 (grp 70 (entget ename '("*")))))
      )
       (progn
	 (while	ename
	   (setq elist (entget ename '("*"))
		 elist (subst '(8 . "0") (assoc 8 elist) elist)
		 elist (if (assoc 62 elist)
			 (subst '(62 . 0) (assoc 62 elist) elist)
			 (append elist '((62 . 0)))
		       )
	   )
	   (entmake elist)
	   (setq ename (entnext ename))
	 )
	 (if (/= "ENDBLK" (grp 0 elist))
	   (entmake '((0 . "ENDBLK") (8 . "0") (62 . 0)))
	 )
	 'T
       )
    )
  )
  (if (> (logand (grp 70 (tblsearch "layer" "0")) 1) 0)
    (princ "\nLayer 0 must be thawed before running FIXBLOCK!\n"
    )
    (progn
      (if
	(progn
	  (princ "\nSelect a block to fix or press <Enter> to fix all defined blocks:\n")
	  (setq	cnt 0
		ss  (if (= manual "y")
		      (ssget '((0 . "INSERT")))
		      (ssget "_X" '((0 . "INSERT")))
		    )
	  )
	)
	 (progn
	   (setq idx (sslength ss))
	   (while (>= (setq idx (1- idx)) 0)
	     (if
	       (not
		 (member (setq blkname (Grp 2 (entget (ssname ss idx))))
			 donelist
		 )
	       )
		(progn
		  (if (update blkname)
		    (setq cnt (1+ cnt))
		  )
		  (setq donelist (cons blkname donelist))
		)
	     )
	   )
	 )
	 (while	(setq blkname (Grp 2 (tblnext "BLOCK" (not blkname))))
	   (if (update blkname)
	     (setq cnt (1+ cnt))
	   )
	 )
      )
      (princ (strcat "\n"
		     (itoa cnt)
		     " block"
		     (if (= cnt 1)
		       ""
		       "s"
		     )
		     " redefined."
	     )
      )
    )
  )
  (princ)
) ;END OF BLOCKBYBLOCK

 ;changes a given variable in a dynamic block
(defun CHGDYNPROP (block propname newval / obj v vval sal tot i)
  (setq	obj  (if (= (type block) 'vla-object)
	       block
	       (vlax-ename->vla-object block)
	     )
	v    (vla-getdynamicblockproperties obj)
	vval (vlax-variant-value v)
	sal  (vlax-safearray->list vval)
	tot  (length sal)
	i    0
  )
  (while (< i tot)
    (if	(= (vlax-get-property (nth i sal) "PropertyName") propname)
      (progn
	(vlax-put-property (nth i sal) "Value" newval)
	(setq i tot)
      )
      (setq i (1+ i))
    )
  )
)

 ;Copy objects in set ortho direction
(defun COPY_ORTHO (direction / in1 ss)
  (setq ss (ssget))
  (setq distc0 (cond ((> distc0 "") distc0)
		     (t "")
		)
	in1 (getstring (strcat "\nDistance of displacement <" distc0 "> ? "))
	distc0 (cond ((= in1 " ") "")
		     ((> in1 "") in1)
		     (t distc0)
	       )
  )
  (command "copy" ss "" "@" (strcat "@" distc0 direction))
  (princ)
)

 ;CONVERSION OF DEGREES INTO RADIANS
(defun D->R (num_deg)
  (* pi (/ num_deg 180.0))
)

 ;returns the distance between two points for a single given dimension
(DEFUN DIST_1D	(point1 point2 dimension / n)
  (if (setq n
		   (cond
			((= (strcase dimension) "X") 0)
			((= (strcase dimension) "Y") 1)
			((= (strcase dimension) "Z") 2)
			(t nil)
		   )
	 )
    (abs (- (nth n point1) (nth n point2)))
  )
)

 ;
(defun FILEOPEN (filenameandpath /)
  (if (findfile filenameandpath)
    (command "vbastmt" (strcat "AcadApplication.Documents.Open \"" filenameandpath "\""))
    (princ "\nFile not found, open canceled.")
  )
 (princ)
)

 ;Function to set standard variables and drawing scale, nil scl prompts for input
(defun DWGSET (scl / error_obj)
  (setq pfxa (getvar "userr3"))
  (setq pfx (rtos pfxa 2 0))
  (setq	pfx (cond ((> pfx "") pfx)
		  (t "")
	    )
  )
  (if (= scl nil)
    (setq in1 (getstring (strcat "\nWhat is the Scale of Drawing <" pfx "> ? ")))
    (setq in1 (itoa scl))
  )
  (if (= in1 "")
    ()
    (setq dwgsc (atof in1))
  )
  (if (= (eval dwgsc) 48)
    (setq edwgsc 64)
    (setq edwgsc dwgsc)
  )
  (setvar "lunits" 4)
  (setvar "userr3" dwgsc)
  (setvar "cmdecho" 0)
  (setvar "expert" 0)
  (setvar "dragp1" 6)
  (setvar "dragp2" 6)
  (setvar "mirrtext" 0)
  (setvar "blipmode" 0)
  (setvar "useri2" 1)
  (setvar "ucsicon" 0)
  (setvar "attdia" 0)
  (setvar "filedia" 1)
  (setvar "textsize" (* dwgsc 0.09375))
  (if (> (car (getvar "extmax")) (car (getvar "extmin")))
      (command "limits" (getvar "extmin") (getvar "extmax"))
  )
  (setvar "ltscale" (* (scalefactor) 0.5))
  (setvar "dimscale" (scalefactor))
  (graphscr)
  (prompt "\nDone.")
  (princ)
)

 ;Insert predefined blocks
(defun INS_BLOCK (block_name   block_layer  initial_osm	 rotate_osm
		  ort	       rotate	    scale	 /
		  scale_value
		 )
  (if (/= block_layer "current_layer")
    (F:SLAY block_layer)
  )
  (F:MODS (quote ("attreq" "orthomode" "osmode")))
  (setvar "attreq" 0)
  (setvar "orthomode" ort)
  (cond ((= scale "scale")
	 (setq scale_value (getreal "\nInput Size (in inches): "))
	)
	((= scale 0)
	 (setq scale_value (SCALEFACTOR))
	)
	(t (setq scale_value scale)
	)
  )
  (prompt "\nInsertion Point: ")
  (if (= rotate "rotate")
    (progn
      (SET_OSMODE initial_osm)
      (command "insert" block_name "s" scale_value pause)
      (SET_OSMODE rotate_osm)
      (command pause)
    )
    (progn
      (SET_OSMODE initial_osm)
      (command "insert" block_name "s" scale_value "r" 0 pause)
    )
  )
  (SET_OSMODE "non")
  (if (= block_layer "current_layer")
    (MATCH_ENTITY_LAYER nil)
  )
  (F:MODR)
  (F:RLAY)
  (princ)
)

 ;insert double line DUCT BLOCKS
(defun INS_BLOCK_DL (block_name block_layer ort / yscale pt1 pt2 pt3)
  (if (/= block_layer "current_layer")
    (F:SLAY block_layer)
  )
  (F:MODS (quote ("osmode" "orthomode")))
  (setvar "orthomode" ort)
  (SET_OSMODE "nea")
  (setq pt1 (getpoint "\nSelect Point On Duct:"))
  (SET_OSMODE "per")
  (setq	pt2
	 (getpoint pt1
		   "\nSelect Point On Duct Perpendicular To First Point:"
	 )
  )
  (SET_OSMODE "non")
  (setq pt3 (polar pt1 (angle pt1 pt2) (/ (distance pt1 pt2) 2)))
  (setq yscale (/ (* (+ (distance pt1 pt2) 9) 16) 3))
  (command "insert"
	   block_name
	   "x"
	   (SCALEFACTOR)
	   "y"
	   yscale
	   pt3
	   (+ (R->D (angle pt1 pt2)) 90)
  )
  (SET_OSMODE "non")
  (if (= block_layer "current_layer")
    (MATCH_ENTITY_LAYER pt1);nil)
  )
  (F:MODR)
  (F:RLAY)
  (princ)
)

 ;Insert Title block and notation
(defun INS_TB (name)
  (command "insert" name "s" 1 "r" 0 "0,0")
  (command "explode" (entlast))
  (princ)
)

 ;insert BLOCK and TRIM line
(defun INS_BLOCK_TRIM (block_name    block_layer   trim_scale
		       rotate	     /		   trim_entity
		       trimee_ss     insertion_point
		       no	     trimee_ls	   last_ss
		      )
  (command "undo" "group")
  (if (/= block_layer "current_layer")
    (F:SLAY block_layer)
  )
  (F:MODS (quote ("osmode" "orthomode")))
  (setvar "orthomode" 0)
  (SET_OSMODE "end nea")
  (prompt "\nInsertion point: ")
  (if (= rotate "rotate")
    (command "insert" block_name "s" (SCALEFACTOR) pause pause)
    (command "insert" block_name "s" (SCALEFACTOR) "r" 0 pause)
  )
  (SET_OSMODE "non")
  (setq insertion_point (getvar "lastpoint"))
  (if (= block_layer "current_layer")
    (MATCH_ENTITY_LAYER nil)
  )
  (setq last_ss (ssget "p"))
  (command "select" "Box" insertion_point insertion_point "")
  (setq trimee_ss (ssget "p"))
  (if (/= trimee_ss last_ss)
    (progn
      (setq trimee_ls (sslength trimee_ss)
	    no	      -1
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
  )
  (F:RLAY)
  (F:MODR)
  (command "undo" "end")
  (princ)
)


 ;insert SENSOR BLOCK NEXT TO WALL
(defun INS_SENSOR (block_name offset_dist / pt1 pt2 pt3)
  (F:SLAY "ME-CONT-NEW")
  (F:MODS (quote ("orthomode" "osmode")))
  (setvar "orthomode" 1)
  (SET_OSMODE "nea")
  (setq pt1 (getpoint "\nSelect Point on Wall:"))
  (SET_OSMODE "non")
  (setq pt2 (getpoint pt1 "\nSelect Direction:"))
  (setq pt3 (polar pt1 (angle pt1 pt2) (* (SCALEFACTOR) offset_dist)))
  (command "-insert"
	   block_name
	   "r"
	   "0"
	   pt3
	   (SCALEFACTOR)
	   ""
	   pt2
  )
  (F:RLAY)
  (F:MODR)
  (princ)
)

 ;MAKES DEMO HATCHING OR EQUIPMENT HATCHING
(defun LSW_HATCHING (layer hatch_pattern hatch_scale border_erase draw_order / demo_border)
  (command "undo" "group")
  (F:SLAY layer)
  (prompt "Select hatch boundry(s)")
  (pause_for_command "select" 0 )
  (setq demo_border (ssget "p" ))
  (command "-hatch" "p" hatch_pattern (scalefactor) hatch_scale "a" "a" "y" "h" "y" "" "s" demo_border "" "")  
  (cond  ((= draw_order "send_back")
	   (command "draworder" (entlast) "" "b")
	  )
         ((= draw_order "send_front")
	   (command "draworder" (entlast) "" "f")
	  )
  )
    (if (= border_erase "yes_erase")
	   (command "erase" demo_border "")
    )
  (F:RLAY)
  (princ)
  (command "undo" "end")
)

 ;Function that takes the last entity and matches it's layer to another
 ;entity's layer passing through it's insertion point
(defun MATCH_ENTITY_LAYER
			  (point /	     last_entity
			   ss_point	     last_selection_set
			   selection_set     layer
			  )
  (setq last_entity (entlast))
  (if (/= point nil)
    (setq ss_point point)
    (setq ss_point (cdr (assoc (quote 10) (entget last_entity))))
  )
  (setq last_selection_set (ssget "p"))
  (command "select" "box" ss_point ss_point "")
  (setq selection_set (ssget "p"))
  (if (/= selection_set last_selection_set)
    (cond ((= (sslength selection_set) 1)
	   (setq
	     layer
	      (cdr (assoc (quote 8) (entget (ssname selection_set 0))))
	   )
	  )
	  ((> (sslength selection_set) 1)
	   (setq
	     layer
	      (cdr (assoc (quote 8) (entget (ssname selection_set 1))))
	   )
	  )
    )
  )
  (if (/= layer nil)
    (command "change" last_entity "" "p" "la" layer "")
  )
)


 ;Save current drawing system variables
(defun F:MODS (qj)
  (setq V:MLST (quote nil))
  (repeat (length qj)
    (setq
      V:MLST (append V:MLST (list (list (car qj) (getvar (car qj)))))
    )
    (setq qj (cdr qj))
  )
)

 ;Restore pre-command drawing system variables
(defun F:MODR nil
  (repeat (length V:MLST)
    (setvar (caar V:MLST) (cadar V:MLST))
    (setq V:MLST (cdr V:MLST))
  )
)

 ;Move objects in set ortho direction
(defun MOVE_ORTHO (direction / in1 ss)
  (setq ss (ssget))
  (setq distm0 (cond ((> distm0 "") distm0)
		     (t "")
		)
	in1 (getstring (strcat "\nDistance of displacement <" distm0 "> ? "))
	distm0 (cond ((= in1 " ") "")
		     ((> in1 "") in1)
		     (t distm0)
	       )
  )
  (command "move" ss "" "@" (strcat "@" distm0 direction))
  (princ)
)

;;; "Rounds" an angle to the nearest ortho angle (in radians) based on the
;;; rouning style requested ("up", "down", or "") where "" rounds up just
;;; like the ROUND function -- e.g., 45 deg rounds to 90)
(DEFUN ORTHO_ROUND (ang rnd_style / pu_angle pu_angle_rnd angle_rnd)

  (setq pu_angle  (/ ang (/ pi 2))
	   angle_rnd nil
  )

  (if (= pu_angle (fix pu_angle))
    (setq pu_angle_rnd pu_angle)
    (cond
	 ((= (strcase rnd_style) "UP")
	  (setq pu_angle_rnd (1+ (fix pu_angle)))
	 )
	 ((= (strcase rnd_style) "DOWN")
	  (setq pu_angle_rnd (fix pu_angle))
	 )
	 (t (setq pu_angle_rnd (ROUND pu_angle)))
    )
  )

  (cond
    ((= pu_angle_rnd 0) (setq angle_rnd 0))
    ((= pu_angle_rnd 1) (setq angle_rnd (* pi 0.5)))
    ((= pu_angle_rnd 2) (setq angle_rnd pi))
    ((= pu_angle_rnd 3) (setq angle_rnd (* pi 1.5)))
    ((= pu_angle_rnd 4) (setq angle_rnd 0))
    (t nil)
  ) ;_end cond

)

 ;Adds pause for commands that do not wait for input in lisps
(defun PAUSE_FOR_COMMAND (com first_point / n number_of_points)
  (setvar "cmdecho" 1)
  (command com)
  (cond	((= (type first_point) 'LIST)
	 ;(prompt "\nlist")
	 (setq number_of_points (length first_point))
	 (setq n 0)
	 (while	(< n number_of_points)
	   (command (nth n first_point))
	   (setq n (1+ n))
	 )
	)
	((/= first_point nil)
	 ;(prompt "\nnot list")
	 (command first_point)
	)
	(t
	)
  )
  (while (= (getvar "cmdactive") 1)
    (command pause)
  )
  (setvar "cmdecho" 0)
)

 ;CONVERSION OF RADIANS INTO DEGREES
(defun R->D (num_rad)
  (* 180.0 (/ num_rad pi))
)

 ;Restore pre-command drawing layer
(defun F:RLAY nil
  (cond	((not (null last_current_layer))
	 (setvar "menuecho" 3)
	 (command "layer" "s" last_current_layer "")
	 (setq last_current_layer nil)
	)
  )
  (setvar "menuecho" 1)
  (princ)
)

 ;RETURNS NUMBER ROUNDED TO NEAREST INTEGER
(defun ROUND (number / number_cropped difference)
  (setq number_cropped (fix number))
  (setq difference (- number number_cropped))
  (cond	((>= difference 0.5) (+ number_cropped 1))
	((<= difference -0.5) (- number_cropped 1))
	(t number_cropped)
  )
)

 ;RETURNS DRAWING SCALE IN MODEL SPACE, AND 1 IN PAPER SPACE
(defun SCALEFACTOR ()
  (if (= (getvar "cvport") 1)
    (* 1.0 1) ; return 1
    (* dwgsc 1) ;return dwgsc
  )
)

 ;Sets object snap mode using string list inputs.
(defun SET_OSMODE (modes / osm n temp)
  (setq	os_bitcode
	 (list "NON"  0	     "END"  1	   "MID"  2	 "CEN"	4
	       "NOD"  8	     "QUA"  16	   "INT"  32	 "INS"	64
	       "PER"  128    "TAN"  256	   "NEA"  512	 "APP"	2048
	       "EXT"  4096   "PAR"  8192
	      )
	osm 0
	n 0
	modes (read (strcat "(" modes ")"))
  )
  
  (repeat (length modes)
    (setq temp (member (strcase (vl-princ-to-string (nth n modes))) os_bitcode))
    (if (/= temp nil)
      (setq osm (+ osm (nth 1 temp)))
    )
    (setq n (1+ n))
  )
  (setvar "osmode" osm)
  (princ)
)

;;; returns 1 or -1 based on the sign of the given real number.
(DEFUN SIGN (number)
  (/ number (abs number))
)

 ;Save current drawing layer
(defun F:SLAY (new_current_layer /)
  (setvar "menuecho" 3)
  (cond	((null last_current_layer)
	 (setq last_current_layer (getvar "clayer"))
	)
  )
  (layerset new_current_layer)
  (setvar "menuecho" 1)
)

 ;Change snap angle
(defun SNAP_ANGLE (degrees)
  (setvar "snapang" degrees)
  (princ)
)

 ;Switch all objects from model space to paper space, or visa versa
(defun SWITCH_TILEMODE (mode)
  (command "block" "-temp-" "0,0" "all" "")
  (command "tilemode" mode)
  (command "insert" "*-temp-" "0,0" "1" "" "")
  (command "-purge" "b" "-temp-" "n")
  (command "zoom" "e")
  (princ)
)

 ;Tangent of angle (in radians)
(defun TAN (ang /)
  (/ (sin ang) (cos ang))
)

 ;Zoom function
(defun ZOOM_FUNCTION (mode)
  (command "zoom" mode)
  (princ)
)


;;;  /----------------------------------------------------------\
;;;  | General ACAD commands that use support functions.	|
;;;  \----------------------------------------------------------/

 ;Copy Ortho
(defun C:C0 () (COPY_ORTHO "<0"))
(defun C:C1 () (COPY_ORTHO "<90"))
(defun C:C2 () (COPY_ORTHO "<180"))
(defun C:C3 () (COPY_ORTHO "<270"))

 ;File Open
(defun C:ELDETAILS ()
  (FILEOPEN "F:/MASTER/AutoCAD/DETAILS/ELEC/LSW-STD LIGHTING DETAILS.dwg")
)
(defun C:EPDETAILS ()
  (FILEOPEN "F:/MASTER/AutoCAD/DETAILS/ELEC/LSW-STD POWER DETAILS.dwg")
)
(defun C:FADETAILS ()
  (FILEOPEN "F:/MASTER/AutoCAD/DETAILS/ELEC/LSW-STD FIRE ALARM DETAILS.dwg")
)
(defun C:MCONTROLS ()
  (FILEOPEN "F:/MASTER/AutoCAD/DETAILS/MECH/LSW-STD MECH CONTROLS.dwg")
)
(defun C:MDETAILS ()
  (FILEOPEN "F:/MASTER/AutoCAD/DETAILS/MECH/LSW-STD MECH DETAILS.dwg")
)
(defun C:SSDETAILS ()
  (FILEOPEN "F:/MASTER/AutoCAD/DETAILS/ELEC/LSW-STD SPECIAL SYSTEMS DETAILS.dwg")
)
(defun C:MSCHEDULES ()
  (FILEOPEN "F:/MASTER/AutoCAD/Schedules/LSW-STD MECH SCHEDULES.dwg")
)
(defun C:PSCHEDULES ()
  (FILEOPEN "F:/MASTER/AutoCAD/Schedules/LSW-STD PLUM SCHEDULES.dwg")
)


 ;Insert Block
(defun C:CUT ()
  (INS_BLOCK "cut" "current_layer" "end" "nea" 0 "rotate" 0)
)
(defun C:DETTTL ()
  (INS_BLOCK "detttl" "note-new" "non" "non" 0 nil 0)
)
(defun C:DSC ()
  (INS_BLOCK "DWG_Graphic_Scale" "note-new" "non" "non" 0 nil 0)
)
(defun C:LOGO ()
  (INS_BLOCK "logo_az" "0" "non" "non" 0 nil 0)
  (command "updatefield" (entlast) "")
)
(defun C:SHTTTL ()
  (INS_BLOCK "shtttl" "note-new" "non" "non" 0 nil 0)
)

 ;Block by layer deffinition
(defun C:BBYL () (BLOCKBYLAYER "y") (command "REGEN") (princ))

;Block by layer deffinition
(defun C:BBYB () (BLOCKBYBLOCK "y") (command "REGEN") (princ))

 ;Insert Title block/notation
(defun C:ANO () (INS_TB "border_info_a-size"))
(defun C:ASHT () (INS_TB "border_az_a-size"))
(defun C:BNO () (INS_TB "border_info_b-size"))
(defun C:BSHT () (INS_TB "border_az_b-size"))
(defun C:DNO () (INS_TB "border_info_d-size"))
(defun C:DSHT () (INS_TB "border_az_d-size"))
(defun C:E1NO () (INS_TB "border_info_e1-size"))
(defun C:E1SHT () (INS_TB "border_az_e1-size"))
(defun C:E1NO-ALT () (INS_TB "border_info_e1-size-alt"))
(defun C:E1SHT-ALT () (INS_TB "border_az_e1-size-alt"))

 ;Move Ortho
(defun C:M0 () (MOVE_ORTHO "<0"))
(defun C:M1 () (MOVE_ORTHO "<90"))
(defun C:M2 () (MOVE_ORTHO "<180"))
(defun C:M3 () (MOVE_ORTHO "<270"))

 ;Set
(defun C:SET () (DWGSET nil))

 ;Snap Angle
(defun C:0 () (SNAP_ANGLE 0))
(defun C:30 () (SNAP_ANGLE (/ pi 6)))
(defun C:45 () (SNAP_ANGLE (/ pi 4)))
(defun C:60 () (SNAP_ANGLE (/ pi 3)))
(defun C:SNA () (SNAP_ANGLE (D->R (getreal "\nInput angle in degrees: "))))

 ;Switch tilemode
(defun C:M-P () (SWITCH_TILEMODE "0"))
(defun C:P-M () (SWITCH_TILEMODE "1"))

 ;Zoom Function
(defun C:ZA () (ZOOM_FUNCTION "a"))
(defun C:ZD () (ZOOM_FUNCTION "d"))
(defun C:ZE () (ZOOM_FUNCTION "e"))
(defun C:ZP () (ZOOM_FUNCTION "p"))
(defun C:ZW () (ZOOM_FUNCTION "w"))
(defun C:ZX () (ZOOM_FUNCTION "p"))


;;;  /----------------------------------------------------------\
;;;  | General ACAD commands.					|
;;;  \----------------------------------------------------------/

 ;Change snap angle to match line
(defun C:ANG (/ a x y)
  (setq a (entget (ssname (ssget) 0)))
  (setq x (cdr (assoc 10 a)))
  (setq y (cdr (assoc 11 a)))
  (SNAP_ANGLE (angle x y))
)

 ;Adds page setups
(defun C:APS ()
  (command "psetupin" "~" "*")
  (princ "\nPage setups have been added from the master file.")
  (princ)
)

 ;Setup commands of background xref files
(defun C:BGSET (/ switch color)
  (command "undo" "group")
  (initget "Initial Update Title")
  (setq switch (getkword "\nIs this the initial setup or an update? [Initial/Update/Title_Block] <Update>: "))
  (cond ((= switch "Initial")
	 (setq color 8)
	)
	((= switch "Title")
	 (setq color 162)
	)
	(t
	 (setq color 2)
	)
  )
  (command "zoom" "e")
  (command "audit" "y")
  (command "-purge" "a" "" "n")
  (DWGSET 1)
  (command "_.-xref" "D" "*")
  (command "_.-image" "D" "*")
  (command "-layer" "U" "*" "")
  (command "layer" "m" "0_alt_lsw" "T" "0" "s" "0" "f" "*" "")
  (command "change" "all" "" "p" "la" "0_alt_lsw" "")
  (command "layon")
  (command "laythw")
  (setq ssp (ssget "ALL"))
  (command "change" "p" "" "p" "c" "bylayer" "lt" "bylayer" "")
  (BLOCKBYLAYER nil)
  (command "-layer" "c" color "*" "")
  (command "-purge" "a" "" "n")
  (command "_.regenall")
  (command "undo" "end")
  (princ "\nBackground Setup Done.")
  (prin1)
)

 ;Break object given 2 points
(defun C:BR ()
  (setvar "cmdecho" 0)
  (command "break" pause "f")
  (princ)
)

 ;Insert a bracet
(defun C:BRAC ()
  (setvar "cmdecho" 0)
  (command "insert" "bracket" pause (SCALEFACTOR) (SCALEFACTOR) pause)
  (command "explode" (entlast))
  (command "change" "l" "" "p" "la" "note-new" "")
  (princ)
)

 ;Break object given 2 points
(defun C:BRE ()
  (setvar "cmdecho" 1)
  (command "break" pause "f" "qui,end" pause "qui,end" pause)
  (princ)
)

 ;Break object given 1 point
(defun C:BRK ()
  (setvar "cmdecho" 1)
  (command "break" pause "f" "qui,end" pause "@")
  (princ)
)

 ;Double Line break line.
(defun C:BRKL (/	pt1	 pt2	  ang1	   ang2	    dist
	       mid	pt1-1	 pt1-2	  pt2-1	   pt2-2    block_name
	       defined	brkl1	 brkl2	  brkl3
	      )
  (F:SLAY "0")
  (F:MODS (quote ("orthomode" "osmode")))
  (setvar "orthomode" 0)
  (SET_OSMODE "end")
  (setq pt1 (getpoint "Pick first point: "))
  (setq pt2 (getpoint pt1 "Pick second point: "))
  (SET_OSMODE "non")
  (setq ang1 (angle pt2 pt1))
  (setq ang2 (angle pt1 pt2))
  (setq dist (distance pt1 pt2))
  (setq mid (polar pt1 ang2 (/ dist 2)))
  (setq pt1-1 (polar mid ang1 (/ (SCALEFACTOR) 32)))
  (setq pt1-2 (polar pt1 ang1 (/ (SCALEFACTOR) 16)))
  (setq pt2-1 (polar mid ang2 (/ (SCALEFACTOR) 32)))
  (setq pt2-2 (polar pt2 ang2 (/ (SCALEFACTOR) 16)))
  (setq	block_name
	 (strcat "Break_line_"
		 (rtos dist 2 0)
		 "_"
		 (rtos (R->D ang1) 2 0)
	 )
  )
  (setq defined (tblsearch "block" block_name))
  (cond	((= defined nil)
	 (command "line" pt1-1 pt1-2 "")
	 (setq brkl1 (ssget "l"))
	 (command "line" pt2-1 pt2-2 "")
	 (setq brkl2 (ssget "l"))
	 (command "-insert" "brkl" mid (SCALEFACTOR) "" (R->D ang1))
	 (setq brkl3 (ssget "l"))
	 (command "scale"
		  brkl1
		  brkl2
		  brkl3
		  ""
		  mid
		  (/ 1 (SCALEFACTOR))
	 )
	 (command "-block" block_name mid brkl1 brkl2 brkl3 "")
	)
	(t
	 ()
	)
  )
  (F:SLAY "NOTE-NEW")
  (command "insert" block_name "s" (SCALEFACTOR) "r" 0 mid)
  (F:RLAY)
  (F:MODR)
  (princ)
)

 ;Break multiple lines given two intersections
(defun C:BRL (/ pt1 pt2 original trim_1 trim_2)
  (F:SLAY "0")
  (F:MODS (quote ("orthomode" "osmode")))
  (setvar "orthomode" 0)
  (SET_OSMODE "int")
  (setq pt1 (getpoint "\nPick first intersection point of lines: "))
  (setq pt2 (getpoint pt1 "\nPick last intersection point of lines: "))
  (SET_OSMODE "non")
  (command "xline" pt1 pt2 "")
  (setq original (entlast))
  (command "offset"
	   (* 0.055 (SCALEFACTOR))
	   original
	   (polar pt1 (+ (angle pt1 pt2) (/ pi 2)) 10)
	   ""
  )
  (setq trim_1 (entlast))
  (command "offset"
	   (* 0.055 (SCALEFACTOR))
	   original
	   (polar pt1 (- (angle pt1 pt2) (/ pi 2)) 10)
	   ""
  )  
  (setq trim_2 (entlast))
  (command "erase" original "")
  (setq pt1 (polar pt1 (angle pt2 pt1) 0.0625))
  (setq pt2 (polar pt2 (angle pt1 pt2) 0.0625))
  (command "trim" trim_1 trim_2 "" "f" pt1 pt2 "" "")
  (command "erase" trim_1 trim_2 "")
  (F:RLAY)
  (F:MODR)
)

 ;Break multiple lines given intersection and selection fence
(defun C:BRL1 (/ pt1 pt2 original trim_1 trim_2)
  (F:SLAY "0")
  (F:MODS (quote ("orthomode" "osmode")))
  (setvar "orthomode" 0)
  (SET_OSMODE "nea")
  (setq pt1 (getpoint "\nPick first point on line: "))
  (setq pt2 (getpoint pt1 "\nPick second point on line: "))
  (SET_OSMODE "non")
  (command "xline" pt1 pt2 "")
  (setq original (entlast))
  (command "offset"
	   (* 0.055 (SCALEFACTOR))
	   original
	   (polar pt1 (+ (angle pt1 pt2) (/ pi 2)) 10)
	   ""
  )
  (setq trim_1 (entlast))
  (command "offset"
	   (* 0.055 (SCALEFACTOR))
	   original
	   (polar pt1 (- (angle pt1 pt2) (/ pi 2)) 10)
	   ""
  )  
  (setq trim_2 (entlast))
  (command "erase" original "")
  (SNAP_ANGLE (angle pt1 pt2))
  (setvar "orthomode" 1)
  (command "trim" trim_1 trim_2 "" "f" pause pause "" "")
  (command "erase" trim_1 trim_2 "")
  (C:0)
  (F:RLAY)
  (F:MODR)
)

 ;Break crossing lines
(defun C:BRO (/ pt cs ss ls no)
  (setvar "cmdecho" 0)
  (F:MODS (quote ("osmode")))
  (SET_OSMODE "int")
  (command "undo" "group")
  (prompt "\nPick Intersection Point of entities: ")
  (command "circle" pause (* (SCALEFACTOR) 0.055))
  (setq pt (getvar "lastpoint"))
  (setq cs (ssget "l"))
  (prompt "\nSelect Object to trim...")
  (command "select" "single" pause)
  (setq
    ss (ssget "p")
    ls (sslength ss)
    no -1
  )
  (command "trim" cs "")
  (repeat ls
    (setq no (1+ no))
    (command (list (ssname ss no) pt))
  )
  (command "")
  (setvar "cmdecho" 1)
  (command "erase" cs "")
  (command "undo" "end")
  (princ "\nBreakout Entity done.")
  (F:MODR)
  (prin1)
)

 ;Change objects color and linetype to bylayer
(defun C:BY (/ ssp)
  (setq ssp (ssget))
  (command "change" "p" "" "p" "c" "bylayer" "lt" "bylayer" "")
  (princ)
)

 ;Change objects color to bylayer
(defun C:CBY (/ ssp)
  (setq ssp (ssget))
  (command "change" "p" "" "p" "c" "bylayer" "")
  (princ)
)

 ;Change ALL objects color to bylayer, set ALL layer colors to input
(defun c:CCALL (/ color)
  (setq color (getstring "Which Color? "))
  (command "chprop" "all" "" "c" "byl" "")
  (command "layer" "c" color "*" "")
  (princ)
)

; 
(defun c:cleanaec ()
  (command "-xref" "d" "*")
  (command "-exporttoautocad" "S" "_acad" "" "")
  (princ)
)

 ;Defines DIM?!
(defun C:DIM ()
  (command "layer" "m" "G-ANNO-DIMS" "c" "12" "" "lt" "continuous" "" "")
  (command "layer" "s" "G-ANNO-DIMS" "")
  (command ".dim")
  (princ)
)

 ;Defines dimension variables?!
(defun C:DIMSET ()
  (command "script" "dimset")
  (princ)
)

 ;Opens a Windows file explorer
(defun C:EXPLORE ()
  (startapp "explorer" (strcat "/n,/e," (getvar "dwgprefix")))
  (princ)
)

 ;Open a dwg file.  Path is optional.
(defun FILEOPEN (filenameandpath /)
  (if (findfile filenameandpath)
    (command "vbastmt" (strcat "AcadApplication.Documents.Open \"" filenameandpath "\""))
    (princ "\nFile not found, open canceled.")
  )
 (princ)
)

;LSW Hatch commands for equipment and demo hatching
(defun C:DH ()
  (LSW_HATCHING
    "G-ANNO-PATT-DEMO"
    "ansi31"
    "15"
    "yes_erase"
    "send_front"
   )
)
(defun C:EH ()
  (LSW_HATCHING
    "G-EQPM-PATT-NEW"
    "solid"
    "0"
    "no_erase"
    "send_back"
   )
)
(defun C:EEH ()
  (LSW_HATCHING
    "G-EQPM-PATT-EXST"
    "solid"
    "0"
    "no_erase"
    "send_back"
  )
)
(defun C:DEH ()
  (LSW_HATCHING
    "G-EQPM-PATT-DEMO"
    "solid"
    "0"
    "no_erase"
    "send_back"
  )
)

 ;Change object linetype to "hidden"
(defun C:H (/ ss1)
  (setq ss1 (ssget))
  (command "change" ss1 "" "p" "lt" "hidden" "")
  (princ)
)

 ;Change object linetype to "hidden2"
(defun C:H2 (/ ss1)
  (setq ss1 (ssget))
  (command "change" ss1 "" "p" "lt" "hidden2" "")
  (princ)
)

 ;Alias to keep people happy (not needed!)
(defun C:H1 () (C:H2))

 ;General insert command
(defun C:IN (/ bl)
  (setvar "cmdecho" 0)
  (setq bl (getstring "\nWhat is the Name of the Block to Insert? "))
  (prompt "Insertion Point: ")
  (command "insert" bl "s" (SCALEFACTOR) pause)
  (princ)
)

 ;TOGGLE ISOMETRIC CROSSHAIRS
(defun C:IO ()
  (if (= (getvar "snapstyl") 0)
    (setvar "snapstyl" 1)
    (setvar "snapstyl" 0)
  )
  (princ)
)

 ;Load macro
(defun C:LM (/ name)
  (F:MODS (quote ("cmdecho")))
  (setvar "cmdecho" 0)
  (setq name (getstring "\nName Of Macro To Load: "))
  (load name)
  (F:MODR)
  (princ)
)

 ;Menu load - electrical
(defun C:MLDE ()
  (command "cuiunload" "lswelec")
  (command "cuiunload" "lswmech")
  (command "cuiload" "lswelec")
  (princ)
)

 ;Menu load - mechanical
(defun C:MLDM ()
  (command "cuiunload" "lswelec")
  (command "cuiunload" "lswmech")
  (command "cuiload" "lswmech")
  (princ)
)

 ;Menu load - LSW
(defun C:MLDL ()
  (command "cuiunload" "lsw")
  (command "cuiload" "lsw")
  (princ)
)

 ;Menu load - mechanical (titus vav boxes)
(defun C:MLDT ()
  (command "cuiunload" "titusvav")
  (command "cuiload" "titusvav")
  (princ)
)

 ;Create viewport
(defun C:MV ()
  (F:MODS (quote ("cmdecho")))
  (F:SLAY "_viewport")
  (PAUSE_FOR_COMMAND "_.mview" nil)
  (F:MODR)
  (F:RLAY)
)
  
 ;insert POC WITH LEADER
(defun C:POC (/ insertion_point trim_entity end_point ang1 pt3)
  (F:SLAY "note-new")
  (F:MODS
    (quote ("attdia" "attreq" "highlight" "osmode" "orthomode"))
  )
  (setvar "attdia" 0)
  (setvar "attreq" 1)
  (setvar "highlight" 0)
  (SET_OSMODE "non")
  (setvar "orthomode" 0)
  (prompt "\nPick Insertion Point: ")
  (command "insert"
	   "TAG_TRIM_POC"
	   "s"
	   (SCALEFACTOR)
	   "r"
	   0
	   pause
  )
  (setq insertion_point (getvar "lastpoint"))
  (command "explode" (entlast))
  (setq trim_entity (entlast))
  (setq end_point (getpoint "\nPoint at the Subject:  "))

  (while (/= end_point nil)
    (setq ang1 (angle end_point insertion_point))
    (setq pt3 (polar end_point ang1 (* (SCALEFACTOR) 0.09375)))
    (command "pline"
	     insertion_point
	     pt3
	     "w"
	     (* (SCALEFACTOR) 0.046875)
	     0
	     end_point
	     ""
    )
    (command "trim" trim_entity "" insertion_point "")
    (setq end_point (getpoint "\nPick another point or press enter:  "))
  )
  (command "erase" trim_entity "")
  (command "-insert"
	   "POC"
	   "s"
	   (SCALEFACTOR)
	   "r"
	   0
	   insertion_point
  )
  (F:MODR)
  (F:RLAY)
  (princ)
)

 ;insert double line PIPE end
(defun C:PEND (/ yscale pt1 pt2 pt3)
  (F:MODS (quote ("osmode" "orthomode")))
  (setvar "orthomode" 0)
  (SET_OSMODE "end")
  (setq pt1 (getpoint "\nSelect One Side of Pipe End:"))
  (setq pt2 (getpoint pt1 "\nSelect Opposite Point On Pipe End:"))
  (SET_OSMODE "non")
  (setq pt3 (polar pt1 (angle pt1 pt2) (/ (distance pt1 pt2) 2)))
  (setq yscale (distance pt1 pt2))
  (command "insert"
	   "PIPEEND"
	   "X"
	   (distance pt1 pt2)
	   "Y"
	   yscale
	   pt3
	   (+ (R->D (angle pt1 pt2)) 90)
  )
  (F:MODR)
  (princ)
)

 ;Draw rectangle with input dimensions
(defun C:RECT (/ pt1 pt2 pt3 l w)
  (initget 1);3D point can't be null
  (setq pt1 (getpoint (strcat "\nCorner of rectangle or square: ")))
  (initget 7);Length can't be 0, neg, or null
  (setq l (getdist pt1 "\nInput length: "))
  (setq pt2 (list (+ (car pt1) l) (cadr pt1) (caddr pt1)))
  (grdraw pt1 pt2 7)
  (initget 6);Width can't be 0, or neg
  (setq w (getdist pt2 "\nInput width or hit Enter for a square: "))
  (if (= w nil)
    (setq w l)
  )
  (setq pt3 (list (car pt2) (+ (cadr pt2) w) (caddr pt2)))
  (command "rectangle" pt1 pt3)
  (command "'redraw")
  (princ)
)

 ;Mirror and erase previous
(defun C:REV ()
  (F:MODS (quote ("cmdecho" "highlight")))
  (setvar "cmdecho" 0)
  (prompt "\nSelect objects: ")
  (command "select" "auto" pause)
  (prompt "\nFirst point of mirror line: ")
  (setvar "highlight" 0)
  (command "mirror" "p" "" pause pause "y")
  (F:MODR)
  (princ)
)

 ;Resets the scale list (for acad 2008) when bloated
(defun C:RSL ()
  (command "-scalelistedit" "r" "y" "e")
  (princ)
)

 ;Replaces page setups
(defun C:RPS (/ ans ps)
  (vl-load-com)
  ; Get the page setups
  (vlax-for ps (vla-get-plotconfigurations
		 (vla-get-activedocument
		   (vlax-get-acad-object)
		 )
	       )
    (vla-delete ps); Delete the current page setup
  );vlax-for
  (princ "\nOld page setups deleted.")
  (command "psetupin" "~" "*")
  (princ
    "\nPage setups have been replaced from the master file."
  )
  (princ)
)

 ;PURGE, ZOOM, SAVE and CLOSE
(defun C:SCL ()
  (LAYERSET "0")
  (command "-purge" "a" "" "n")
  (command "zoom" "e")
  (command "qsave")
  (command "close")
)

 ;PURGE, ZOOM, SWITCH TO LAYOUT, ZOOM, SAVE and CLOSE
(defun C:SCLL ()
  (LAYERSET "0")
  (command "-purge" "a" "" "n")
  (command "zoom" "e")
  (cond	((= (getvar "tilemode") 1)
	 (setvar "tilemode" 0)
	 (command "zoom" "e")
	)
	(t)
  )
  (command "qsave")
  (command "close")
)

 ;Arc command
(defun C:SED ()
  (F:MODS (quote ("orthomode" "cmdecho")))
  (setvar "orthomode" 0)
  (setvar "cmdecho" 1)
  (command "arc" pause "e" pause "d" pause)
  (F:MODR)
  (princ)
)

 ;?
(defun C:SET85 (/ select_set)
  (setq	select_set
	 (ssget	"X"
		'(
		  (-4 . "<OR")
		  (-4 . "<AND")
		  (0 . "TEXT")
		  (7 . "STANDARD")
		  (-4 . "AND>")
		  (-4 . "<AND")
		  (0 . "TEXT")
		  (7 . "SIMPLEX")
		  (-4 . "AND>")
		  (-4 . "OR>")
		 )
	 )
  )
  (setq counter -1)

  (repeat (sslength select_set)
    (setq ent_list
	   (entget (ssname select_set (setq counter (1+ counter))))
    )
    (setq ent_list (subst (cons 41 0.85) (assoc 41 ent_list) ent_list))
    (entmod ent_list)
  )
  (princ)
)

(defun set_text_width (width_factor / select_set)
  (setvar "cmdecho" 0)
  (prompt "\nSelect objects: ")
  (setq select_set
  	 (ssget
		'(
		  (0 . "TEXT")
		 )
	 )
  )
  (setq counter -1)
  (repeat (sslength select_set)
    (setq ent_list
	   (entget (ssname select_set (setq counter (1+ counter)))
	   )
    )	
	(setq ent_list
	  (subst
		 (cons 41
		       width_factor
		 )
		 (assoc 41 ent_list)
		 ent_list
	  )
	)
	(entmod ent_list)
  )
)

(defun c:sw1  () (set_text_width 1.00) (princ))
(defun c:sw85 () (set_text_width 0.85) (princ))
(defun c:sw65 () (set_text_width 0.65) (princ))

 ;Insert shadow box, explode, and stretch
(defun C:SHAD ()
  (command "-insert" "*shbox" pause (SCALEFACTOR) 0)
  (prompt "Pick points to Stretch: ")
  (command "stretch")
  (princ)
)

 ;Draw shadow box given two points of rectangle
(defun C:SHBOX (/	   pt1	      pt2	 rect	    shad_ll_x
		shad_ll_y  shad_ur_x  shad_ur_y	 shad_ll    shad_mid
		shad_ur	   shad
	       )
  (F:SLAY "NOTE-NEW")
  (F:MODS (quote ("orthomode" "osmode" "plinewid")))
  (setvar "orthomode" 0)
  (SET_OSMODE "NON")
  (setq pt1 (getpoint "\nSpecify first corner point of rectangle: "))
  (prompt "\nSpecify second corner point of rectangle: ")
  (command "rectang" pt1 pause)
  (setq pt2 (getvar "lastpoint"))
  (setq rect (entlast))
  (command "change" rect "" "p" "c" "red" "")
  (setq	shad_ll_x (+ (min (nth 0 pt1) (nth 0 pt2)) (/ (SCALEFACTOR) 16))
	shad_ll_y (- (min (nth 1 pt1) (nth 1 pt2)) (/ (SCALEFACTOR) 64))
	shad_ur_x (+ (max (nth 0 pt1) (nth 0 pt2)) (/ (SCALEFACTOR) 64))
	shad_ur_y (- (max (nth 1 pt1) (nth 1 pt2))
		     (/ (* 3 (SCALEFACTOR)) 64)
		  )
	shad_ll	  (list shad_ll_x shad_ll_y 0.0) ;lower left
	shad_mid  (list shad_ur_x shad_ll_y 0.0) ;middle
	shad_ur	  (list shad_ur_x shad_ur_y 0.0) ;upper right
  )
  (command "pline"
	   shad_ll
	   "w"
	   (/ (SCALEFACTOR) 32)
	   ""
	   shad_mid
	   shad_ur
	   ""
  )
  (setq shad (entlast))
  (command "change" shad "" "p" "c" 8 "")
  (command "draworder" shad "" "u" rect "")
  (F:MODR)
  (F:RLAY)
  (princ)
)


;;; ;Insert shadow box
;;;(defun C:SHBOX ()
;;;  (prompt "\nShadow Box Lower Left Corner: ")
;;;  (command "insert" "*shbox" pause (SCALEFACTOR) "0")
;;;  (princ)
;;;)

; Sheet-set Association Remove
(defun C:SSAREM ()
	(dictremove (namedobjdict) "AcSheetSetData") 
	(princ)
)

 ;Stretch
(defun C:STR ()
  (setvar "cmdecho" 0)
  (command "stretch" "c")
  (princ)
)

; Sheet-set Association Remove
(defun C:SSAREM ()
                (dictremove (namedobjdict) "AcSheetSetData") 
                (princ)
)


 ;Opens the text window
(defun C:TX ()
  (textscr)
  (princ)
)

;;; CADALYST 05/07  Tip 2209: TWB.lsp	Toggle Black and White Background Color 	(c) 2007 Watson Kilbourne 
;;;;;;          *** TWB.LSP *** ;;;;;; Toggle White or Black screen;;; (Does not effect colored crosshairs);;;
;; LSW note: command used in update seal.
(defun c:TWB (/ prf col)
   (setq prf (vla-get-display (vla-get-Preferences (vlax-get-acad-object))))
   (setq col (vlax-variant-value
		(vlax-variant-change-type
		   (vla-get-graphicswinmodelbackgrndcolor prf)
		   vlax-vblong
		)
	     )
   )
   (if (zerop col)
      (vla-put-graphicswinmodelbackgrndcolor
	 prf
	 (vlax-make-variant 16777215 vlax-vblong)
      )
      (vla-put-graphicswinmodelbackgrndcolor
	 prf
	 (vlax-make-variant 0 vlax-vblong)
      )
   )
   (vlax-release-object prf)
   (princ)
)

 ;FOR UPDATING THE SEAL WMF IMAGE FILES FOR INSERTION INTO WORD DOCUMENTS
(defun C:UPDATESEAL (/		     location	     file_name
		     initials	     location_file_name
		     aux	     wmfheight	     wmfwidth
		     blocks_w_att
		     )
  (setq location "F:/MASTER/LSW-MISC/SEALS/SEAL_"); where to save the exported seal
  (setq file_name (getvar "DWGNAME"))
  ;initialize default variables.  Note that the height and width variables are in pixels where
  ; 96 pixels = 1".  For Windows 7 machines we need to add 58 pixels to the height and 20 pixels
  ; to the width to account for the window 'frame'.  A scale value correction factor was added
  ; to hone in on the desired seal dimensions.
  (setq wmfheight 192
	wmfwidth 144
	frameheight 58
	framewidth 20
	scalevalue 1.04
	blocks_w_att nil)
  ; get just Blocks w/ attr
  (setq blocks_w_att (ssget "_X" (list (cons 0 "INSERT") (cons 66 1))))
  (cond ((/= (substr file_name 1 5) "SEAL_")
	 (prompt "\nFile is not a seal.  You cannot run that command.")
	)
	((= blocks_w_att nil)
	 (prompt "\nFile does not contain the SEAL_PARAMETERS block required for that command.  Please insert and try again.")
	)
	((> (sslength blocks_w_att) 1)
	 (prompt "\nFile appears to have multiple blocks with attributes.  Verify that there is only one instance of the SEAL_PARAMETERS block in the file.")
	)
	( t
	 (vl-load-com)
	 (READ_ATT blocks_w_att)
	 (setq wmfheight (* scalevalue (+ wmfheight frameheight)))
	 (setq wmfwidth (* scalevalue (+ wmfwidth framewidth)))
	 (WINDOW_RESIZE wmfwidth wmfheight)
	 (setq aux (getstring "\nPress Enter to complete wmf export: "))
	 (command "zoom" "e")
	 (C:TWB)
	 (setq initials (substr file_name 6 (- (strlen file_name) 9)))
	 (setq location_file_name (strcat location initials))
	 (command "change" "all" "" "p" "la" "SEAL" "")
	 (command "-layer" "c" "7" "*" "")
	 (command "wmfout" location_file_name "all" "")
	 (command "undo" "3" "")
	 (C:TWB)
	 (command "qsave")
	 (command "qsave")
	 (command "close")
	)
  )
  (princ)
)

 ; Read invisible attribute values in Seal_Parameters block
(defun READ_ATT	(ss / blk atts att)
  (setq blk (vlax-ename->vla-object (ssname ss 0)))
  (if (safearray-value
	(setq atts (vlax-variant-value (vla-getattributes blk)))
      )
    (progn
      (setq atts (vlax-safearray->list
		   (vlax-variant-value (vla-getattributes blk))
		 )
      )
      (foreach att atts
	(if (= (vla-get-tagstring att) "WMF-HEIGHT")
	  (setq wmfheight (atoi (vla-get-textstring att)))
	); if
	(if (= (vla-get-tagstring att) "WMF-WIDTH")
	  (setq wmfwidth (atoi (vla-get-textstring att)))
	); if
      ); foreach
    ); progn
  ); if
)


;Changes drawing window size
;(C)2010  XANADU.cz for CADforum.cz
;; LSW note: command used in update seal.
(defun window_resize (w h / actdoc)
  (setq actdoc (vla-get-activedocument (vlax-get-acad-object)))
  (vla-put-width actdoc w)
  (vla-put-height actdoc h)
  (vlax-release-object actdoc)
  (prin1)
)

 ;Write block with all objects in file
(defun C:WBALL (/ dwgfilename thisdrawing ssets newSet)
  (setq	dwgfilename
	 (strcase (strcat (getvar "dwgprefix")
                          "-"
			  (strcase (getvar "dwgname"))
;;;		    	  (vl-string-right-trim
;;;			    ".DWG"
;;;			    (strcase (getvar "dwgname"))
;;;			  )
		  )
	 )
  )
  (princ (strcat "Wblock-Saving drawing to...\n" dwgfilename))
  (vl-load-com)
  (setq	thisdrawing
	 (vla-get-activedocument
	   (vlax-get-acad-object)
	 )
  )
  (setq ssets (vla-get-selectionsets thisdrawing))
  (if (vl-catch-all-error-p
	(vl-catch-all-apply 'vla-item (list ssets "$Set"))
      )
    (setq newSet (vla-add ssets "$Set"))
    (progn
      (vla-delete (vla-item ssets "$Set"))
      (setq newSet (vla-add ssets "$Set"))
    );progn
  );if
  ;select all objects in the drawing
  (vla-Select newSet acSelectionSetAll)
  (vla-WBlock thisdrawing dwgfilename newSet)
  (princ)
)

 ;Xref-attach onto the _XREF layer
(defun C:XA ()
  (F:SLAY "_xref")
  ;(command "-layer" "U" "_xref" "")
  (PAUSE_FOR_COMMAND "xattach" "~")
  (command "-layer" "LO" "_xref" "")
  (F:RLAY)
  (princ)
)

 ;Xref-attach an "Excel2Dwg" file at "1:1" scale and set colors
(defun c:XS ()
  (f:slay "G-ANNO-SCHD")
  (command "-xref" "attach" "~" "Scale" 0.0144 "Rotate" 0.0 pause)
  (command "-layer" "c" "3" "*|TEXT *" "c" "2" "*|LINE *" "c" "1" "*|LINE 255*" "c" "6" "*|LINE 112*" "")
  (f:rlay)
(princ))


 ;Explodes All AEC Objects
(defun C:XAEC (/ selection_set)
  (setq selection_set (ssget "X" '((0 . "aec*"))))
  (prompt (strcat "\nFound... " (rtos (sslength selection_set) 2 0) ".\n"))
  (sssetfirst selection_set selection_set)
  (command "explode")
  (princ)
)

(princ " Done.")

(load "lsw_tags")
(load "lsw_text")
(load "lsw_layers")
(load "lsw_ltscale")
(load "lsw_tp")
(load "aset")

