(princ "\nAT&T CAD Specification Lisp Routines Loading...")

;;;  /----------------------------------------------------------\
;;;  | Load supporting lisp files.                              |
;;;  \----------------------------------------------------------/

;(load "new_ddif.lsp")
;(load "attlay.lsp")
;(load "attla.lsp")
(load "att_layers.lsp")
;(load "att_elec.lsp")
;(load "att_eblk.lsp")
;(load "att_fblk.lsp")
;(load "att_bblk.lsp")
;(load "att_mblk.lsp")

 ;Xref-attach an "Excel2Dwg" file at "1:1" scale and set colors
(defun c:XS ()
  (f:slay "G-PDWG-BASE")
  (command "-xref" "attach" "~" "Scale" 0.0144 "Rotate" 0.0 pause)
  (command "-layer" "c" "3" "*|TEXT *" "c" "2" "*|LINE *" "c" "1" "*|LINE 255*" "c" "6" "*|LINE 112*" "")
  (f:rlay)
(princ))

; switch to AT&T CRE layers
(defun c:CRE ()
  (load "ATT_Layers_CRE.lsp")
  (princ)
)

 ;Create viewport
(defun C:MV ()
  (F:MODS (quote ("cmdecho")))
  (F:SLAY "Defpoints")
  (PAUSE_FOR_COMMAND "_.mview" nil)
  (F:MODR)
  (F:RLAY)
)

(DEFUN model_scale ()
  (setvar "ltscale" (* dwgsc 0.25))
  (setvar "dimscale" dwgsc)
  (princ)
)

(DEFUN layout_scale ()
  (setvar "ltscale" 0.25)
  (setvar "dimscale" 1.0)
  (princ)
)

; Redefine Rev Layer for AT&T
(defun REVISION_LAYER (layer_prefix    layer_check     /
		       layer_check     layer_name      available_colors
		       color
		      )
  (setq	defltr (cond ((> defltr "") defltr)
		     (t "")
	       )
  )
  (setq layer_name (strcat layer_prefix defltr))
  (while (/= (strcase layer_check)
	     "Y"
	     (strcase layer_check)
	     "YES"
	     (strcase layer_check)
	     ""
	 )
    (setq in1	 (getstring
		   (strcat "\nEnter Revision Number <" defltr ">: ")
		 )
	  defltr (cond ((= in1 " ") "")
		       ((> in1 "") in1)
		       (t defltr)
		 )
    )
    (setq layer_name (strcat layer_prefix defltr))
    (if	(= (tblsearch "layer" layer_name) nil)
      (setq layer_check
	     (getstring
	       (strcat
		 "\nLayer "
		 layer_name
		 " is not defined.  Would you like to create it? (Y/N) <Y>: "
	       )
	     )
      )
      (setq layer_check "Y")
    )
  )
  (setq available_colors (list 8 9 51 100 141 180 220 240))
  (setq color (read defltr))
  (if (or (= 'SYM (type color)) (= nil color))
    (setq color 6)
    (setq color (nth (rem color 6) available_colors))
  )
  (MAKE_LAYER layer_name color "continuous" "p" "n")
  layer_name
)


; Redefine Rev Tag for AT&T
(defun C:RT ()
  (F:MODS
    (quote ("attdia" "attreq" "highlight" "osmode" "orthomode"))
  )
  (setvar "attdia" 0)
  (setvar "attreq" 1)
  (setvar "highlight" 0)
  (SET_OSMODE "non")
  (setvar "orthomode" 0)
  (F:SLAY (REVISION_LAYER "REV-TAG-" "N"))
  (prompt "\nEnter Tag Insertion Point: ")
  (command "insert"
	   "g-indrnt"
	   "s"
	   (SCALEFACTOR)
	   "r"
	   0
	   pause
	   defltr
  )
  (F:MODR)
  (F:RLAY)
  (princ)
)

 ;REVISION CLOUD AND TAG
(defun C:RV (/ layer_name_cld layer_name_tag color)
  (F:MODS
    (quote ("attdia" "attreq" "highlight" "osmode" "orthomode" "cmdecho"))
  )
  (setvar "attdia" 0)
  (setvar "attreq" 1)
  (setvar "highlight" 0)
  (SET_OSMODE "non")
  (setvar "orthomode" 0)
  (setq layer_name_cld (REVISION_LAYER "REV-CLD-" "N"))
  (setq layer_name_tag (REVISION_LAYER "REV-TAG-" "Y"))
  (F:SLAY layer_name_cld)
  (PAUSE_FOR_COMMAND "_.revcloud" nil)
  (F:SLAY layer_name_tag)
  (prompt "\nEnter Tag Insertion Point: ")
  (command "insert"
	   "g-indrnt"
	   "s"
	   (SCALEFACTOR)
	   "r"
	   0
	   pause
	   defltr
  )
  (F:MODR)
  (F:RLAY)
  (princ)
)

(defun C:POC (/ insertion_point trim_entity end_point ang1 pt3)
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
  (princ)
)

(defun c:vpneww (/ selected_vports)
  (prompt "\nSelect the viewports which will contain \"NEWW\" work: ")
  (setq selected_vports (ssget '((0 . "VIEWPORT"))))
  (command "vplayer" "r" "*" "s" selected_vports "" "f" "*-demo*" "s" selected_vports "" "")
  (princ)
)

(defun c:vpdemo (/ selected_vports)
  (prompt "\nSelect the viewports which will contain \"DEMO\" work: ")
  (setq selected_vports (ssget '((0 . "VIEWPORT"))))
  (command "vplayer" "r" "*" "s" selected_vports "" "f" "*-new*" "s" selected_vports "" "")
  (princ)
)

(defun c:zn ()
  (command "zoom" "-540,240" "-480,280")
  (princ)
)


(if (= (getvar "userr3") 0.0)
  (setvar "userr3" 1)
)


(setq dwgsc (getvar "userr3"))

(if (= (getvar "tilemode") 0)
  (setvar "ltscale" 0.25)
  (setvar "ltscale" (/ (scalefactor) 4.0))
)

(defun c:RN ()
  (command "layer" "m" "revision-note" "c" "cyan" "" "")
  (command "insert" "G-INDRNT" PAUSE (scalefactor) (scalefactor) 0)
  (princ)
)


(defun c:newcad ()
  (load "att_layers_CRE.lsp")
)

(defun c:FONTSET ()
  (command "style" "STANDARD" "romans.shx" "0" "0.85" "" "" "" "")
  (princ)
)

;;;  /----------------------------------------------------------\
;;;  | Overwrites of base LSW functions                         |
;;;  \----------------------------------------------------------/

; Loads a single requested layer from the master layer setup file.
(defun load_a_layer (new_layer	  /	       file_handle
		     last_line	  color	       linetype
		     plot_noplot
		    )
  (setq new_layer (strcase new_layer))
  (setq file_handle (open (findfile "att_layers.txt") "r"))
  (setq last_line 1)
  (while (/= last_line nil)
    (setq last_line (read-line file_handle))
    (if	(/= last_line nil)
      (progn
	(setq last_line (read (strcat "(" last_line ")")))
	(if (= new_layer
	       (strcase (vl-princ-to-string (nth 2 last_line)))
	    )
	  (progn
	    (setq color	      (strcase (vl-princ-to-string (nth 3 last_line)))
		  linetype    (strcase (vl-princ-to-string (nth 4 last_line)))
		  plot_noplot (strcase (vl-princ-to-string (nth 5 last_line))
			      )
	    )
	    (make_layer new_layer color linetype plot_noplot "n")
	    (prompt "\n*** A NEW LAYER WAS AUTOMATICALLY CREATED. ***")
	    (setq last_line nil)
	  )
	)
      )
    )
  )
  (close file_handle)
  (princ)
)

(defun c:2pl (/ pnt1 pnt2 ort osm)
  (setq ort (getvar "orthomode"))
  (setq osm (getvar "osmode"))
  (setvar "cmdecho" 0)
  (setvar "osmode" 0)
  (setvar "orthomode" 1)
  (setq pnt1 (getpoint "\nPoint at the subject:  "))
  (prompt "\nOrigin of 2-point Leader:  ")
  (command "line" pnt1 pause "")
  (setq pnt2 (getvar "lastpoint"))
  (command "insert" "edarr01l" "s" (scalefactor) pnt1 pnt2)
  (setvar "orthomode" ort)
  (setvar "orthomode" osm)
  (princ)
)

(defun c:3pl (/ pnt1 pnt2 pnt3 ort osm)
  (setq ort (getvar "orthomode"))
  (setq osm (getvar "osmode"))
  (setvar "cmdecho" 0)
  (setvar "osmode" 0)
  (setvar "orthomode" 1)
  (setq pnt1 (getpoint "\nInput Start Point at End of Text: "))
  (prompt "\nInput Second Point at End of Straight Section: ")
  (command "line" pnt1 pause "")
  (setq pnt2 (getvar "lastpoint"))
  (command "erase" (entlast) "")
  (setvar "orthomode" 0)
  (prompt "\nInput Third Point near Object: ")
  (command "pline" pnt1 "w" 0 0 pnt2 pause "")
  (command "insert" "edarr01l" "s" (scalefactor) "@" pnt2)
  (setvar "orthomode" ort)
  (setvar "orthomode" osm)
  (princ)
)

(defun c:ar ()
  (command "insert" "edarr01l" "end" pause (scalefactor) (scalefactor) pause)
  (princ)
)

(DEFUN C:CUT ()
  (command "insert"
	   "GDBRK00F"
	   "s"
	   (* 0.5 (scalefactor))
	   "end"
	   pause
	   pause
  )
  (princ)
)

(defun c:HA ( / end_of_homerun ar_angle )
  (F:MODS (QUOTE ("osmode" "cmdecho" "orthomode" "snapang" "aunits")))
  (command "undo" "group")
  (command "osnap" "end")
  (setvar "cmdecho" 0)
  (setvar "orthomode" 1)
  (setvar "aunits" 3)
  (setq end_of_homerun (getpoint "Select the end of the homerun: "))
  (command "snapang" "cen" end_of_homerun end_of_homerun)
  (setq ar_angle (+ (/ pi 2) (getvar "snapang")))
  (princ (getvar "snapang"))
  (princ ar_angle)
  (command "insert"
	   "EPHRUN1F"
	   end_of_homerun
	   (* 0.75 (scalefactor))
	   (* 0.75 (scalefactor))
	   ar_angle
  )
  (princ)
  (command "undo" "end")
  (f:modr)
)

(defun C:LP () (THREE_POINT_LEADER "" "loop"))

(DEFUN C:SET ()
  (setvar "cmdecho" 0)
  (setq pfxa (getvar "userr3"))
  (setq pfx (rtos pfxa 2 0))
  (setq	pfx (cond ((> pfx "") pfx)
		  (t "")
	    )
  )
  (setq	in1 (getstring
	      (strcat "\n(AT&T) What is the Scale of Drawing <" pfx "> ? "
	      )
	    )
  )
  (if (= in1 "")
    ()
    (setq dwgsc (atof in1))
  )
  (setvar "userr3" dwgsc)
  (SETQ Edwgsc PFXA)
  (COMMAND "UNITS" "4" "64" "1" "0" "0" "N")
  (SETVAR "ATTMODE" 1)
  (setvar "attreq" 1)
  (setvar "attdia" 0)
  (setvar "cmddia" 1)
  (setvar "filedia" 1)
  (SETVAR "PSLTSCALE" 1)
  (SETVAR "VISRETAIN" 1)
  (setvar "cmdecho" 0)
  (setvar "expert" 0)
  (setvar "dragp1" 6)
  (setvar "dragp2" 6)
  (setvar "ltscale" (/ (scalefactor) 4.0))
  (setvar "regenmode" 0)
  (setvar "mirrtext" 0)
  (setvar "blipmode" 0)
  (setvar "useri2" 1)
  (setvar "ucsicon" 0)
  (setvar "userr3" dwgsc)
  (setvar "dimscale" dwgsc)
  (command "limits" "off")
  (graphscr)
  (PROMPT "AT&T Drawing Variables Set.")
  (princ)
)

(defun c:TCSNAZMA-RDFIX ()
  (command "tilemode" "1")
  (command "-layer" "on" "*" "th" "*" "")
  (command "move" ss "" "0,0" "-257.69236711,-250.20192098")
  (princ)
)

(defun c:TCSNAZMA-VPFIX ( / i nv)
  (command "tilemode" "0")
  (command "MSPACE")
  
  (setq i 1)   ;initialise counter 
  (setq nv (length (vports)))    ;get number of vports 

  (repeat nv 
    (setq i (1+ i))  ;start counting vports at 2 
    (setvar "CVPORT" i)     ;set vport 
    (command "-pan" "0,0" "-257.69236711,-250.20192098")
  ) 

  (command "PSPACE")
  
  (princ)
)

(princ "Done.")
(C:CRE)