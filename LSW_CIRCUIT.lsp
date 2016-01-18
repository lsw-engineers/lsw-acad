;;
;;  <file name> - <short description>
;;
;;  Copyright © 2008 by LSW Engineers Arizona, Inc.
;;
;;  
;;-------------------------------------------------------------------------
;;  DESCRIPTION
;;    <long description>
;;
;;-------------------------------------------------------------------------
;;


(princ "\nNow loading electrical test lisps...")

(DEFUN C:CKT () (CKT_COMMAND "EP-CIRC-NEW") )

(DEFUN C:CK () (CKT_COMMAND "") )


(DEFUN CKT_COMMAND ( layer /		 cpnt1	   cpnt2		input1
		    input2	 input_real  input1_done	input2_done
		   )

;;;  Global variables used: min_offset, bend_radius

  (F:MODS
    (quote
	 ("orthomode"
	   "osmode" "filletrad" "aperture"	"snapang"	"CELTYPE"	"CECOLOR"
	   "pickbox" "plinegen")
    )
  )

  ;; Initalize the min_offset value on first use at the default value of
  ;;  0.15625 * (SCALEFACTOR) = 7.5 at 48, 15 at 96

  (if (null min_offset)
    (progn (setq min_offset (* 0.15625 (SCALEFACTOR)))
		 (princ (strcat "\nConduit offset set to "
					 (rtos min_offset)
					 " (Default Value)"
			   )
		 )
    )
  )

  ;; Initalize the bend_radius value on first use at the default value of
  ;;  the larger of 0.0625 * dwg scale factor (to ensure reasonable graphical
  ;;  representation) or 6.0 (a reasonable code bending radius covering up to
  ;;  1" trade size)

  (if (null bend_radius)
    (progn (setq bend_radius (max (* 0.0625 (SCALEFACTOR)) 6.0))
		 (princ (strcat "\nConduit bending radius set to "
					 (rtos bend_radius)
					 " (Default Value)"
			   )
		 )
    )
  )

;;;
;;;  ;; 12" bend radius covers up to 2" CND, 6" for up to 1" CND
;;;  ;; graphically keep min bend radius to 0.125 * (SCALEFACTOR) = 6 at 48, 12 at 96
;;;  ;; with a hard floor of 6" min allowed is 4" for 1/2" cnd.
;;;  (initget)
;;;  (setq input_real (getreal "\nSet conduit bend radius: "))
;;;  (if input_real
;;;    (setq bend_radius input_real)
;;;  )
;;;  (setq bend_radius (max bend_radius (* 0.125 (SCALEFACTOR)) 6.0))

  (setq input2_done nil)

  (while (not input1_done)
    ;; Get first device or change settings

    (SET_OSMODE "qua")
    (setvar "aperture" 20)
    (setvar "pickbox" 3)
    (initget "Offset Radius")

    (setq	input1 (getpoint (strcat
					    "\nPick first device or [Break/Offset("
					    (rtos min_offset)
					    ")/Radius("
					    (rtos bend_radius)
					    ")/Undo] (Enter when done): "
					  )
			  )
    )

    (SET_OSMODE "non")
    (setvar "aperture" 1)
    (setvar "pickbox" 0)

    (cond
	 ((= input1 nil) ;_Enter for done.
	  ;; setting input1_done to a non-null value will prevent the
	  ;; while loop from executing again -- command done.
	  (setq input1_done "")
	 )
	 ((= input1 "Radius")
	  (princ "\nGet bending radius here.")
	 )
	 ((= input1 "Offset")
	  (progn
         ;; default min_offset to 0.15625 * (SCALEFACTOR) = 7.5 at 48, 15 at 96
         ;; offset should be <= radius?
         (initget)
         (setq input_real (getreal "\nSet offset distance: "))
         (if input_real
           (setq min_offset input_real)
         )
	  )  
	 )
	 ((setq cpnt1 (osnap input1 "_cen"))
	  (progn
	    (princ "\nGood data, continue to input2")
	    (setq input2_done nil)
	    (initget)

	    (while ;; Get second device
			 (not input2_done)

		 (SET_OSMODE "qua")
		 (setvar "aperture" 20)
		 (setvar "pickbox" 3)
		 (setq input2
			   (getpoint "\nPick second device (Enter to cancel): "
			   )
		 )

		 (SET_OSMODE "non")
		 (setvar "aperture" 1)
		 (setvar "pickbox" 0)

		 (cond
		   ((setq cpnt2 (osnap input2 "_cen"))
		    (progn
			 ;; setting input2_done to a non-null value will prevent the
			 ;; "get second device" while loop from executing again.
			 (setq input2_done "")
			 (princ "\nGood data, continue to draw circuiting.")

			 (draw_circuiting
			   cpnt1
			   (distance cpnt1 input1)
			   cpnt2
			   (distance cpnt2 input2)
			   layer
			 )

		    ) ;_ end of progn (got valid cpnt2)
		   )
		   (t (princ "\nInvalid input."))

		 ) ;_ end of cond (checking second input)

	    ) ;_ end while loop (looking for second device))

	  ) ;_ end of progn (got valid cpnt1)
	 )
	 (t (princ "\nInvalid input."))

    ) ;_ end of cond (checking first input)

  ) ;_ end while loop (overall command, initial input)

  (f:modr)
  (if (/= layer "") (f:rlay))

  (princ)
)



;;;
;;;
;;;

(DEFUN draw_circuiting (cpnt1		  rad1		cpnt2
				    rad2		  layer		/
				    x_diff	  y_diff		cpnt1_cpnt2_ang
				    draw_ang	  midp		old_snapang
				    offset_ang_abs			offset_ang_rel
				    offset_dist  offset_midp	cir1
				    cir2
				   )

;;;  Global variables used: min_offset, bend_radius

  (command ".undo" "begin")

  (setvar "filletrad" bend_radius)
  (setvar "plinegen" 1)
  (setvar "cecolor" "220")
  (command "LINETYPE" "s" "HIDDEN" "")


  (SET_OSMODE "qua")
  (if (/= layer "") (F:SLAY layer))

  (SET_OSMODE "non")
  (setvar "aperture" 1)
  (setvar "pickbox" 0)

  (setq
    x_diff		(DIST_1D cpnt1 cpnt2 "x")
    y_diff		(DIST_1D cpnt1 cpnt2 "y")
    cpnt1_cpnt2_ang	(angle cpnt1 cpnt2)
    draw_ang		(ORTHO_ROUND cpnt1_cpnt2_ang "")
    midp			(polar cpnt1 cpnt1_cpnt2_ang (* 0.5 (distance cpnt1 cpnt2)))
  )

  (setq old_snapang (getvar "snapang"))

  (setvar "snapang" cpnt1_cpnt2_ang)

  (setvar "orthomode" 0)

  (command "ray" cpnt1 cpnt2 "")
  (command "ray" cpnt2 cpnt1 "")


  (initget 32)
  (setq offset_ang_abs
	    (R->D (getangle midp "Offset in which direction? "))
  )
 ;_offset_midp	   (polar midp (d->r offset_ang_abs) min_offset)

  (command "erase" "l" "") ;_erase 1st ray
  (command "erase" "l" "") ;_erase 2nd ray

  (setq offset_ang_rel (- offset_ang_abs (R->D (angle cpnt1 cpnt2))))

  (if (> offset_ang_rel 180)
    (setq offset_ang_rel (* -1.0 (- 360 offset_ang_rel)))
  )
  (if (< offset_ang_rel -180)
    (setq offset_ang_rel (+ 360 offset_ang_rel))
  )

  (setq offset_dist	(+ min_offset (* 0.5 (min x_diff y_diff)))
	   offset_midp
				(polar
				  midp
				  (+ draw_ang (* (SIGN offset_ang_rel) (/ pi 2)))
				  offset_dist
				)
  )

  (setvar "snapang" old_snapang)

  (princ (strcat "\noffset_ang_abs: " (rtos offset_ang_abs 2))
  )
  (princ (strcat "\noffset_ang_rel: " (rtos offset_ang_rel 2))
  )
  (princ (strcat "\ncpnt1_cpnt2_ang: "
			  (rtos (R->D cpnt1_cpnt2_ang) 2)
	    )
  )
  (princ (strcat "\ndraw_ang: " (rtos (R->D draw_ang) 2))
  )
  (princ "\n")

  (command "LINETYPE" "s" "CONTINUOUS" "")
  (command "circle" cpnt1 input1)
  (setq cir1 (entlast))
  (command "circle" cpnt2 input2)
  (setq cir2 (entlast))

  ;; determine if devices are "close" in either the horz or vertical plane
  (if (<= (min x_diff y_diff) bend_radius)

    (progn ;_devices are close (no turns are needed)

	 ;; determine whether the devices are too close for a filleted pline
	 (if	(< (max x_diff y_diff) (* 4 bend_radius))

	   (progn ;_devices are too close for a filleted pline
		(princ "\nDRAW WITH NO TURNS -- ARC METHOD!!!")
		(command "arc" cpnt1 offset_midp cpnt2)
		(command "layiso" "l" "")
		(command "change" "l" "" "p" "lt" "bylayer" "c" "bylayer" "")

	   ) ;_ end of if->true (draw with arc method)

	   (progn ;_devices are ok for a filleted pline
		(princ "\nDRAW WITH NO TURNS -- PLINE METHOD!!!")
		(command
		  "pline"
		  cpnt1
		  (polar cpnt1
			    (+ draw_ang (* (SIGN offset_ang_rel) (/ pi 4)))
			    min_offset
		  )
		  ""
		)
		(command
		  "pline"
		  cpnt2
		  (polar cpnt2
			    (- draw_ang (* (SIGN offset_ang_rel) (/ pi 4)))
			    (* -1.0 min_offset)
		  )
		  ""
		)

		(command
		  "pline"
		  (polar midp
			    (+ (+ draw_ang (* (SIGN offset_ang_rel) (/ pi 2)))
				  (-	(/ pi 2)
					(atan offset_dist (* 0.5 (max x_diff y_diff)))
				  )
			    )
			    (sqrt	(+ (expt offset_dist 2)
					   (expt (* 0.5 (max x_diff y_diff)) 2)
					)
			    )
		  )
		  (polar midp
			    (- (+ draw_ang (* (SIGN offset_ang_rel) (/ pi 2)))
				  (-	(/ pi 2)
					(atan offset_dist (* 0.5 (max x_diff y_diff)))
				  )
			    )
			    (sqrt	(+ (expt offset_dist 2)
					   (expt (* 0.5 (max x_diff y_diff)) 2)
					)
			    )
		  )
		  ""
		)

		(command "layiso" "l" "")
		(command "fillet" offset_midp cpnt1)
		(command "fillet" offset_midp cpnt2)
		(command "change" "l" "" "p" "lt" "bylayer" "c" "bylayer" "")

	   ) ;_ end of if->false (draw with pline method)
	 ) ;_ end of if (for close choose arc or pline method)


    ) ;_ end of if->true (draw with no turns)

    (progn ;_devices are not close (turns are needed)

	 (princ "\nDRAW WITH TURNS!!!")

	 (if	(> offset_ang_rel 0)
	   (setq draw_ang (r->d (ORTHO_ROUND cpnt1_cpnt2_ang "up")))
	   (setq draw_ang (r->d (ORTHO_ROUND cpnt1_cpnt2_ang "down")))
	 )

	 (princ (strcat "\ndraw_ang: " (rtos draw_ang 2))
	 )
	 (princ "\n")

	 (if	(or (= draw_ang 0) (= draw_ang 180))
	   (setq draw_dist x_diff)
	   (setq draw_dist y_diff)
	 )

	 (command	"pline"
			cpnt1
			(strcat "@"
				   (rtos draw_dist 2 9)
				   "<"
				   (rtos draw_ang 2 9)
			)
			cpnt2
			""
	 )

	 (command "layiso" "l" "")
	 (command "fillet" "p" "l")
	 (command "change" "l" "" "p" "lt" "bylayer" "c" "bylayer" "")

    ) ;_ end of if->false (draw with turns)
  )

  (command "trim" cir1 "" cpnt1 "")
  (command "trim" cir2 "" cpnt2 "")

  (command "erase" cir1 cir2 "")
  (command "redraw")
  (command "layuniso")

  (command ".undo" "end")
)






;;;
;;;
;;;

(defun insert_symbol (block		   description	    layer
				  /			   block_angle	    condition
				  input_point	   insertion_point block_angle
				  entity_pointer
				 )
  (F:SLAY layer)
  (F:MODS
    (quote ("osmode"))
  )
  ;;(setvar "cmdecho" 0)
  (setvar "osmode" 0)

  (setq input_point "")

  (while
    (setq	input_point
		 (getpoint
		   (strcat "\nSpecify insertion point for '"
				 description
				 "' (Enter when done): "
		   )
		 )
    ) ;_ loop until input is nil (Enter)

	(command "insert" block "s" (SCALEFACTOR) "r" 0 input_point)

  ) ;_ end of while (insersion loop)

  (prompt " Done.  Exiting...")

  (F:MODR)
  (F:RLAY)
  (princ)
)



(DEFUN C:J ()
  (insert_symbol "pow36" "duplex receptacle" "EP-CIRC-NEW")
)
