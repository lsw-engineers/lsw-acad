;;
;;  LSW_LAYERS.LSP - LSW Layer Creation and related routines
;;
;;  Copyright © 2008 by LSW Engineers Arizona, Inc.
;;
;;  
;;-------------------------------------------------------------------------
;;  DESCRIPTION
;;    General LSW layer lisp functions and commands for manipulating,
;;    creating, setting, etc. layers.
;;    Broken into sections: support functions, commands that call support
;;    functions, commands, and LSW Layer Creater Dialog Box Function.
;;
;;-------------------------------------------------------------------------
;;

;;;  /----------------------------------------------------------\
;;;  | Define Global Variables                                  |
;;;  \----------------------------------------------------------/

(setq layer_filename "lsw_layers.txt")
(setq worktype_suffix_new "-NEW")
(setq worktype_suffix_existing "-EXST")
(setq worktype_suffix_demolition "-DEMO")
(setq worktype_suffix_future "-FUTR")

;(defun c:test ()
;  (setvar "LAYLOCKFADECTL" (getreal "\nEnter % dimming: "))
;)

;;;  /----------------------------------------------------------\
;;;  | General layer support functions.                         |
;;;  \----------------------------------------------------------/


 ;Change the work type (new, exst, demo, or futr) for selected entities.
 ;(e.g., entities on layer "NOTE-NEW" can be changed to the "NOTE-EXST"
 ;layer with the "exst" command.)
(defun CHANGE_LAYER_WORKTYPE (new_worktype	/
			      select_set	counter
			      ent_list		current_obj_layer
			      proposed_obj_layer
			     )
  (setvar "cmdecho" 0)
  (prompt "\nSelect objects: ")
  (setq select_set (ssget))
  (setq counter -1)
  (repeat (sslength select_set)
    (setq ent_list
	   (entget (ssname select_set (setq counter (1+ counter)))
	   )
    )	
    (setq current_obj_layer (cdr (assoc 8 ent_list)))
    (setq proposed_obj_layer
	   (CHANGE_SUFFIX current_obj_layer new_worktype)
    )
    (if	(null (tblsearch "layer" proposed_obj_layer))
      (LOAD_A_LAYER proposed_obj_layer)
    )
    (if	(tblsearch "layer"
		   proposed_obj_layer
	)
      (progn
	(setq ent_list
	       (subst
		 (cons 8
		       proposed_obj_layer
		 )
		 (assoc 8 ent_list)
		 ent_list
	       )
	)
	(entmod ent_list)
      )
    )
  )
  (princ)
)

 ;called by CHANGE_LAYER_WORKTYPE to change the layer's suffix
(defun CHANGE_SUFFIX
       (old_layer_name new_suffix / base_layer_name new_layer_name)
  (setq old_layer_name (strcase old_layer_name))
  (setq	base_layer_name
	 (vl-string-right-trim
	   (vl-string-left-trim "-" worktype_suffix_new)
	   (strcase old_layer_name)
	 )
  )
  (setq	base_layer_name
	 (vl-string-right-trim
	   (vl-string-left-trim "-" worktype_suffix_existing)
	   (strcase base_layer_name)
	 )
  )
  (setq	base_layer_name
	 (vl-string-right-trim
	   (vl-string-left-trim "-" worktype_suffix_demolition)
	   (strcase base_layer_name)
	 )
  )
  (setq	base_layer_name
	 (vl-string-right-trim
	   (vl-string-left-trim "-" worktype_suffix_future)
	   (strcase base_layer_name)
	 )
  )
  (setq	base_layer_name
	 (vl-string-right-trim
	   "-"
	   (strcase base_layer_name)
	 )
  )
  (setq new_layer_name (strcat base_layer_name (strcase new_suffix)))
)

 ;Change layer to LSW standard layer
(defun LAYERSET	(new_current_layer /)
  (if (null (tblsearch "layer" new_current_layer))
    (LOAD_A_LAYER new_current_layer)
  )
  (setvar "clayer" new_current_layer)
)

 ; Loads a layer's properties from the master layer setup file.
(defun LAYER_PROPERTIES	(layer_name   /		   file_handle
			 last_line    worktype	   color
			 linetype     plot_noplot  lineweight layer_props
			)
  (setq file_handle (open (findfile layer_filename) "r"))
  (setq last_line 1)
  (setq layer_props (list "Non-LSW layer"))
  (while (/= last_line nil)
    (setq last_line (read-line file_handle))
    (if	(/= last_line nil)
      (progn
	(setq last_line (read (strcat "(" last_line ")")))
	(if (= layer_name
	       (strcase (vl-princ-to-string (nth 2 last_line)))
	    )
	  (setq	worktype    (strcase (vl-princ-to-string (nth 1 last_line)))
		color	    (strcase (vl-princ-to-string (nth 3 last_line)))
		linetype    (strcase (vl-princ-to-string (nth 4 last_line)))
		plot_noplot (strcase (vl-princ-to-string (nth 5 last_line)))
		lineweight (strcase (vl-princ-to-string (nth 6 last_line)))
		layer_props (list worktype color linetype plot_noplot lineweight)
		last_line   nil
	  )
	)
      )
    )
  )
  (close file_handle)
  layer_props;returns layer_props list or "Non-LSW layer"
)


 ; Loads a single requested layer from the master layer setup file.
(defun LOAD_A_LAYER (new_layer	  /	       worktype
		     color	  linetype     plot_noplot  lineweight
		     layer_props
		    )
  (setq new_layer (strcase new_layer))
  (setq layer_props (LAYER_PROPERTIES new_layer))
  
  (if (/= (nth 0 layer_props) "Non-LSW layer")
    (progn
      (setq color	(nth 1 layer_props)
	    linetype	(nth 2 layer_props)
	    plot_noplot	(nth 3 layer_props)
		lineweight  (nth 4 layer_props)
      )
      (MAKE_LAYER new_layer color linetype plot_noplot lineweight "n")
      (prompt "\n*** A NEW LAYER WAS AUTOMATICALLY CREATED. ***")
    )
  )
)


 ; Load all layers matching the requested discipline and work type from
 ; a master layer setup file (external ascii text file).
(defun LOAD_LAYERS (disciplines	  work_type	overwrite
		    /		  file_handle	last_line
		    name	  color		linetype
		    plot_noplot	  lineweight	l_work_type	l_discipline
		   )
  (setq file_handle (open (findfile layer_filename) "r"))
  (setq last_line 1)
  (while (/= last_line nil)
    (setq last_line (read-line file_handle))
    (if	(/= last_line nil)
      (progn
	(setq last_line (read (strcat "(" last_line ")")))
	(setq l_discipline (strcase (vl-princ-to-string (nth 0 last_line)))
	      l_work_type  (strcase (vl-princ-to-string (nth 1 last_line)))
	      name	   (strcase (vl-princ-to-string (nth 2 last_line)))
	      color	   (strcase (vl-princ-to-string (nth 3 last_line)))
	      linetype	   (strcase (vl-princ-to-string (nth 4 last_line)))
	      plot_noplot  (strcase (vl-princ-to-string (nth 5 last_line)))
		  lineweight  (strcase (vl-princ-to-string (nth 6 last_line)))
	)
	(if (and (vl-position l_discipline disciplines)
		 (or (= "A" l_work_type)
		     (vl-position l_work_type work_type)
		 )
	    )
	  (MAKE_LAYER name color linetype plot_noplot lineweight overwrite)
	)
      )
    )
  )
  (close file_handle)
  (prompt "Layer Load Complete.")
  (princ)
)

 ; Create a layer when given a name color linetype and P/N for plot option.
(defun MAKE_LAYER (name color linetype plot_noplot lineweight overwrite /)
  (if (or (= nil (tblsearch "layer" name))
	  (= (strcase overwrite) "Y")
      )
    (command "layer" "m"     name    "c"     color   ""	     "l"
	     linetype	     ""	     "p"     plot_noplot     ""
		 "LW"	lineweight	""	"S"     "0"     ""
	    )
  )
)


;;;  /----------------------------------------------------------\
;;;  | General LSW Layer commands that use support functions.	|
;;;  \----------------------------------------------------------/

 ;CHANGE_LAYER_WORKTYPE
(defun C:DEMO () (CHANGE_LAYER_WORKTYPE worktype_suffix_demolition))
(defun C:EXST () (CHANGE_LAYER_WORKTYPE worktype_suffix_existing))
(defun C:NEWW () (CHANGE_LAYER_WORKTYPE worktype_suffix_new))
(defun C:FUTR () (CHANGE_LAYER_WORKTYPE worktype_suffix_future))


(DEFUN C:DFR () (COMMAND "-layer" "fr" "*demo*" "") (princ))
(DEFUN C:EFR () (COMMAND "-layer" "fr" "*exst*" "") (princ))
(DEFUN C:NFR () (COMMAND "-layer" "fr" "*new*" "") (princ))
(DEFUN C:FFR () (COMMAND "-layer" "fr" "*futr*" "") (princ))

(DEFUN C:DTH () (COMMAND "-layer" "th" "*demo*" "") (princ))
(DEFUN C:ETH () (COMMAND "-layer" "th" "*exst*" "") (princ))
(DEFUN C:NTH () (COMMAND "-layer" "th" "*new*" "") (princ))
(DEFUN C:FTH () (COMMAND "-layer" "th" "*futr*" "") (princ))

 ;LAYERSET
(defun C:HAT () (LAYERSET "G-DETL-PATT") (princ))
(defun C:L0 () (LAYERSET "0") (princ))
(defun C:NOTE () (LAYERSET "NOTE-NEW") (princ))
(defun C:NPLT () (LAYERSET "_NPLT") (princ))
(defun C:NP () (C:NPLT))
(defun C:VPORT () (LAYERSET "_VIEWPORT") (princ))
(defun C:XRE () (LAYERSET "_XREF") (princ))

 ;LOAD_LAYERS
(defun C:LLDALL	()
  (LOAD_LAYERS
    (list "G"	 "E"	"EG"   "EL"   "EO"   "EP"   "ES"   "FA"
	  "FS"	 "ME"	"MH"   "MP"   "PB"   "PE"   "PF"   "PM"
	  "PS"	 "SS"
	 )
    (list "N" "E" "D")
    "n"
  )
)
(defun C:LLDALL-FORCE ()
  (LOAD_LAYERS
    (list "G"	 "E"	"EG"   "EL"   "EO"   "EP"   "ES"   "FA"
	  "FS"	 "ME"	"MH"   "MP"   "PB"   "PE"   "PF"   "PM"
	  "PS"	 "SS"
	 )
    (list "N" "E" "D")
    "y"
  )
)

(defun C:LLD ()
  (LOAD_LAYERS (list "G") (list "N" "E" "D") "n")
)

(defun C:LLDE ()
  (LOAD_LAYERS
    (list "G" "E" "EG" "EL" "EO" "EP" "ES" "FA" "SS")
    (list "N" "E" "D")
    "n"
  )
)

(defun C:LLDEO ()
  (LOAD_LAYERS
    (list "G" "EO")
    (list "N" "E" "D" "F")
    "y"
  )
)

(defun C:LLDM ()
  (LOAD_LAYERS
    (list "G" "ME" "MH" "MP")
    (list "N" "E" "D")
    "n"
  )
)
(defun C:LLDP ()
  (LOAD_LAYERS
    (list "G" "FS" "PB" "PE" "PF" "PM" "PS")
    (list "N" "E" "D")
    "n"
  )
)

(defun C:LLDX ()
  (LOAD_LAYERS (list "X") (list "N" "E" "D") "n")
)

;;;  /----------------------------------------------------------\
;;;  | General LSW Layer commands that use support functions.	|
;;;  \----------------------------------------------------------/

(DEFUN C:CBG () 
 (COMMAND "-layer" "C"   "8" "*AF??|*,*AC??|*,*IQ??|*,*IF??|*,*df??|*,*Col_Tags*"
                   "C" "250" "*FLOR-RAIS*,*CLNG-GRID*"
				   "C" "192" "*S-GRID*"
				   "FR" "*A-FLOR-PATT*"
				   "")
 (princ)
)
(DEFUN C:COLORAB () 
 (COMMAND "-layer"

   "C" "TR" "255,0,0" "*_DISTA*"	;COLOR ALL A DISTRIBUTION RED
   "C" "TR" "0,0,255" "*_DISTB*"	;COLOR ALL B DISTRIBUTION BLUE
   "C" "TR" "0,255,0" "*_DISTC*"	;COLOR ALL C DISTRIBUTION GREEN
   "C" "TR" "255,128,0" "*_DISTD*"	;COLOR ALL D DISTRIBUTION ORANGE
   
   "C" "TR" "128,0,0" "*_DISTA-EXST,*_DISTA-FUTR,*_DISTA???-EXST,*_DISTA???-FUTR"		;HALF-TONE EXST+FUTR A DISTRIBUTION
   "C" "TR" "0,0,128" "*_DISTB-EXST,*_DISTB-FUTR,*_DISTB???-EXST,*_DISTB???-FUTR"		;HALF-TONE EXST+FUTR B DISTRIBUTION
   "C" "TR" "0,128,0" "*_DISTC-EXST,*_DISTC-FUTR,*_DISTC???-EXST,*_DISTC???-FUTR"		;HALF-TONE EXST+FUTR C DISTRIBUTION
   "C" "TR" "128,64,0" "*_DISTD-EXST,*_DISTD-FUTR,*_DISTD???-EXST,*_DISTD???-FUTR"		;HALF-TONE EXST+FUTR D DISTRIBUTION
   
   "tr" "0" "*_DIST*"	;TURN OFF TRANSPARENCY
   "")
 (princ)
)

(defun c:tred (/ selection_set)
  (setq selection_set (ssget))
  (command "change" selection_set "" "p" "c" "t" "255,0,0" "")
  (princ)
)

(defun c:tblue (/ selection_set)
  (setq selection_set (ssget))
  (command "change" selection_set "" "p" "c" "t" "0,0,255" "")
  (princ)
)

(defun c:tgreen (/ selection_set)
  (setq selection_set (ssget))
  (command "change" selection_set "" "p" "c" "t" "0,255,0" "")
  (princ)
)

(defun c:torange (/ selection_set)
  (setq selection_set (ssget))
  (command "change" selection_set "" "p" "c" "t" "255,128,0" "")
  (princ)
)

(defun c:tyellow (/ selection_set)
  (setq selection_set (ssget))
  (command "change" selection_set "" "p" "c" "t" "255,255,0" "")
  (princ)
)

(defun c:CS1 (/ selection_set) ; COLOR FOR SYSTEM #1
  (setq selection_set (ssget))
  (command "change" selection_set "" "p" "c" "t" "178,213,109" "")
  (princ)
)

(defun c:CS2 (/ selection_set) ; COLOR FOR SYSTEM #2
  (setq selection_set (ssget))
  (command "change" selection_set "" "p" "c" "t" "166,38,153" "")
  (princ)
)

(defun c:CS3 (/ selection_set) ; COLOR FOR SYSTEM #3
  (setq selection_set (ssget))
  (command "change" selection_set "" "p" "c" "t" "26,87,173" "")
  (princ)
)

(defun c:CS4 (/ selection_set) ; COLOR FOR SYSTEM #4
  (setq selection_set (ssget))
  (command "change" selection_set "" "p" "c" "t" "157,114,114" "")
  (princ)
)

(defun c:CS5 (/ selection_set) ; COLOR FOR SYSTEM #5
  (setq selection_set (ssget))
  (command "change" selection_set "" "p" "c" "t" "86,4,134" "")
  (princ)
)

(defun c:CS6 (/ selection_set) ; COLOR FOR SYSTEM #6
  (setq selection_set (ssget))
  (command "change" selection_set "" "p" "c" "t" "76,144,115" "")
  (princ)
)

(DEFUN C:EO-SETLINE ()
 (COMMAND "-layer"
   ; --==[ MATCHING LINETYPES TO REVIT ]==--

   "LT" "CONTINUOUS"        "*EO-*-NEW,*NOTE*-NEW"				;SET NEW to CONTINUOUS
   "LT" "LSW_One-line_Demo" "*EO-*-DEMO,*NOTE*-DEMO"			;SET DEMO to the special linetype matching Revit "DEMO"
   "LT" "DASHED"	        "*EO-*-EXST,*NOTE*-EXST"			;SET EXST to CONTINUOUS
   "LT" "HIDDEN2"	        "*EO-*-FUTR,*NOTE*-FUTR"			;SET EXST to CONTINUOUS
   
   ; Exceptions
   "LT" "PHANTOM2"			"*EO-ITLK*-NEW"			;SET ITLK-NEW to PHANTOM2
   "LT" "CONTINUOUS"	    "*EO-NODE*-EXST"		;SET NODE-EXST to CONTINUOUS
   
   ; --==[ MATCHING LINEWEIGHTS TO REVIT ]==--

   "LW" "0.53" "*NOTE*-DEMO,*NOTE*-NEW"				;SET DEMO/NEW NOTES TO HEAVY
   "LW" "0.20" "*NOTE*-EXST"						;SET DEMO/NEW NOTES TO LIGHT
   "LW" "0.08" "*NOTE*-FUTR"						;SET DEMO/NEW NOTES TO THIN

   
   "LW" "1.27" "*EO-BUSS*-DEMO,*EO-BUSS*-NEW"		;SET DEMO/NEW DISTRIBUTION TO EXTRA-HEAVY
   "LW" "0.41" "*EO-BUSS*-EXST"						;SET DEMO/NEW INTERLOCKS TO MEDIUM
   "LW" "0.08" "*EO-BUSS*-FUTR"						;SET DEMO/NEW INTERLOCKS TO THIN
   
   "LW" "0.20" "*EO-CTRL*-DEMO,*EO-CTRL*-NEW"		;SET DEMO/NEW INTERLOCKS TO SUPER-THIN
   "LW" "0.08" "*EO-CTRL*-EXST,*EO-CTRL*-FUTR"		;SET DEMO/NEW INTERLOCKS TO LIGHT

   "LW" "0.53" "*EO-DIST*-DEMO,*EO-DIST*-NEW"		;SET DEMO/NEW DISTRIBUTION TO HEAVY
   "LW" "0.20" "*EO-DIST*-EXST"						;SET DEMO/NEW INTERLOCKS TO LIGHT
   "LW" "0.08" "*EO-DIST*-FUTR"						;SET DEMO/NEW INTERLOCKS TO THIN
	
   "LW" "0.08" "*EO-ENCL*"							;SET ALL ENCLOSURES AND NODES TO THIN
   
   "LW" "0.20" "*EO-ITLK*-DEMO,*EO-ITLK*-NEW"		;SET DEMO/NEW INTERLOCKS TO THIN
   "LW" "0.08" "*EO-ITLK*-EXST,*EO-ITLK*-FUTR"		;SET DEMO/NEW INTERLOCKS TO LIGHT
   
   "LW" "0.08" "*EO-NODE*"							;SET ALL ENCLOSURES AND NODES TO THIN
   
   "")
 (princ)
)

(Defun c:FixBus (/ s ss sn i)
  (if (setq s  (ssadd)
        ss (ssget "_x" '((0 . "*POLYLINE") (8 . "EO-BUSS*")))
      )
    (progn
      (repeat (setq i (sslength ss))
    (setq sn (ssname ss (setq i (1- i))))
    (if (not (eq (cdr (assoc 100 (reverse (entget sn))))
             "AcDb3dPolyline"
         )
        )
      (ssadd sn s)
    )
      )
      (if (> (sslength s) 0)
    (command "_.pedit" "_m" s "" "w" 0 "")
      )
      (princ "\n .... ")
      (princ (strcat "Number of Polylines changed : "
             " < "
             (itoa (sslength s))
             " > "
         )
      )
    )
    (princ "\n Couldn't find any 2D polyline !!")
  )
  (princ)
)

(Defun c:FixEOAnno (/)
  (C:LLDEO)
  (command "LAYMRG" "N" "NOTE-NEW" "" "N" "EO-ANNO-NEW" "Y")
  (command "LAYMRG" "N" "NOTE-EXST" "" "N" "EO-ANNO-EXST" "Y")
  (command "LAYMRG" "N" "NOTE-FUTR" "" "N" "EO-ANNO-FUTR" "Y")
  (command "LAYMRG" "N" "NOTE-DEMO" "" "N" "EO-ANNO-DEMO" "Y")
  (princ)
)

 ;Freezes all layers except current
(defun C:FR ()
  (setvar "cmdecho" 0)
  (command "layer" "f" "*" "")
  (princ)
)

 ;Turn all layers off except current
(defun C:OFF ()
  (command "layer" "off" "*" "" "")
  (princ)
)

 ;Set layers to xref_layers file
(defun C:SETX ()
  (vl-vbaload (findfile "xref_layers.dvb"))
  (vl-vbarun "SetupXrefLayers")
  (command "_vbaunload" "xref_layers")
  (princ)
)

 ;Set layers to xref_layers file
(defun C:SETX2 ()
  (vl-vbaload (findfile "xref_layers.dvb"))
  (vl-vbarun "SetupSupplimentalXrefLayers")
  (command "_vbaunload" "xref_layers")
  (princ)
)

 ;Export layers to xref_layers file
(defun C:SETX-EXPORT ()
  (vl-vbaload (findfile "xref_layers.dvb"))
  (vl-vbarun "ExportXrefLayerSettings")
  (command "_vbaunload" "xref_layers")
  (princ)
)

 ;Unlock all layers
(defun C:UNL ()
  (setvar "cmdecho" 0)
  (command "layer" "unlock" "*" "")
  (princ)
)

;Allows user to viewport freeze LSW New or Demo layers
(defun C:VPL (/ work freeze thaw vpselection)
  (command "undo" "group")
  (initget "New Demo")
  (setq work (getkword "\nEnter type of work you wish to show in the viewport (New/Demo): ")
	freeze (if (= work "New") "*-demo" "*-new")
	thaw (if (= work "New") "*-new" "*-demo")
  )
  (prompt (strcat "\nSelect viewport(s) that you want to show " work " and Existing layers in: \n"))
  (command "select" pause)
  (setq vpselection (ssget "p"))
  (command "vplayer" "freeze" freeze "Select" vpselection "" "thaw" thaw "Select" vpselection "" "")
  (command "-vports" "lock" "on" vpselection "")
  (command "undo" "end")
  (princ)
)

 ;Change the layer color or freeze a layer by picking an entity,
 ;including EXTERNAL REFERENCE layers: (layer of an entity nested
 ;within a block, nested block, or external reference).
 ;By - William J. Tury
 ;Last updated 7/26/95
 ;LSW renamed from ML to XL 4/9/98
(defun C:XL (/ alist entity lyr_name key string_key prompt_string)
  ;; keep picking until you pick an entity
  (setq xl1 1)
  (setq alist nil)
  (while (= alist nil)
    (setq alist (nentsel "\nSelect object:"))
  ) ;while
  ;; extract dxf codes from nentsel data
  (setq lyr_name (cdr (assoc 8 (entget (nth 0 alist)))))

  ;; check for special case of layer 0
  (if (and (= "0" lyr_name) (nth 3 alist))
    (progn
      (setq lyr_name (cdr (assoc 8 (entget (nth 0 (nth 3 alist))))))
    ) ;progn
  ) ;if
  ;; initial default for prompt 
  (if (= nil default_key)
    (setq default_key "Freeze")
  ) ;if
  ;; initial default for linetype
  (if (= nil default_line_type)
    (setq default_line_type "Continuous")
  ) ;if
  ;; convert the DEFAULT_KEY to a string for the prompt
  (if (= 'INT (type default_key))
    (setq string_key (itoa default_key))
    (setq string_key default_key)
  ) ;if
  ;; set prompt string to indicate the layer selected and default
  (setq	prompt_string
	 (strcat
	   "\nEnter color/Ltype/Vpfreeze/Freeze/Off  layer "
	   (strcat (strcat lyr_name "  <")
		   (strcat string_key ">: ")
	   )
	 )
  )
  ;; prompt user a color number, color name, Freeze or NIL<default>
  (initget
    6
    "Red Yellow Green Blue Cyan Magenta White Freeze Off Vpfreeze Ltype"
  )
  (if (= nil (setq key (getint prompt_string)))
    (setq key default_key)
    (setq default_key key)
  ) ;if
  ;; use the layer command with command echo off
  (setvar "cmdecho" 0)
  (cond
    ((= "Freeze" key) (command "layer" "freeze" lyr_name ""))

    ((= "Off" key) (command "layer" "Off" lyr_name ""))

    ((= "Vpfreeze" key)
     (if (= 1 (getvar "tilemode"))
       (prompt "\nVplayer Freeze NOT allowed in Tilemode 1")
       (command "vplayer" "freeze" lyr_name "" "")
     )
    )
    ((= "Ltype" key)
     (progn
       (setq line_type
	      (getstring
		(strcat	(strcat	"\nEnter linetype <"
				default_line_type
			)
			">: "
		)
	      ) ;getstring
       ) ;setq 
       (if (= "" line_type)
	 (setq line_type default_line_type)
	 (setq default_line_type line_type)
       ) ;if
       (command "layer" "ltype" line_type lyr_name "")
       (if (= 0 (getvar "regenmode"))
	 (princ "\nRegen requested for display update")
       ) ;if
     ) ;progn
    ) ;
    (t (command "layer" "color" key lyr_name ""))
  ) ;cond
  (setvar "cmdecho" 1)
  (setvar "visretain" 1)
  ;; clean exit
  (princ)
) ;END OF XL

(defun c:XS-LAYERS ()
  (command "-layer" "c" "3" "*|TEXT *" "c" "2" "*|LINE *" "c" "1" "*|LINE 255*" "c" "6" "*|LINE 112*" "")
(princ))

(defun c:vpneww (/ selected_vports)
  (prompt "\nSelect the viewports which will contain \"NEW\" work: ")
  (setq selected_vports (ssget '((0 . "VIEWPORT"))))
  (command "vplayer" "r" "*" "s" selected_vports "" 
					 "f" "*-demo*" "s" selected_vports ""
					 "f" "*_??D??|*" "s" selected_vports ""
					 "")
  (princ)
)

(defun c:vpdemo (/ selected_vports)
  (prompt "\nSelect the viewports which will contain \"DEMO\" work: ")
  (setq selected_vports (ssget '((0 . "VIEWPORT"))))
  (command "vplayer" "r" "*" "s" selected_vports ""
					 "f" "*-new*" "s" selected_vports ""
					 "f" "*_A???|*" "s" selected_vports ""
					 "")
  (princ)
)

(DEFUN C:048 () 
 (COMMAND "-layer" "th" "*048*" "fr" "*096*,*192*" "")
 (princ)
)

(DEFUN C:096 () 
 (COMMAND "-layer" "th" "*096*" "fr" "*048*,*192*" "")
 (princ)
)

(defun c:vp048 (/ selected_vports)
  (prompt "\nSelect the viewports which will contain \"048\" scale elements: ")
  (setq selected_vports (ssget '((0 . "VIEWPORT"))))
  (command "vplayer" "f" "*096*,*192*" "s" selected_vports "" "t" "*048*" "s" selected_vports "" "")
  (princ)
)

(defun c:vp096 (/ selected_vports)
  (prompt "\nSelect the viewports which will contain \"096\" scale elements: ")
  (setq selected_vports (ssget '((0 . "VIEWPORT"))))
  (command "vplayer" "f" "*048*,*192*" "s" selected_vports "" "t" "*096*" "s" selected_vports "" "")
  (princ)
)


;;;  /------------------------------------------------------------------\
;;;  | The following comprise the LSW Layer Creater Dialog Box Function	|
;;;  | and all relevent function definitions.				|
;;;  \------------------------------------------------------------------/

(defun C:LSWLC (/ sel_disc dwg_type new exst demo overwrite)
  (setq id (load_dialog "lswlc.dcl"))
  (if (not (new_dialog "lswlc" id))
    (exit)
  )
  (LOAD_LOGO "Logo_box" "lsw_logo") ; Image load
  (setq	sel_types ""
	sel_list ""
	new "ON"
	exst "OFF"
	demo "OFF"
	overwrite "N"
  )
  (action_tile
    "DISC"
    "(setq sel_list (DISCIPLINE_SELECT $value))"
  )
  (action_tile
    "Dwg_Type_List"
    "(setq sel_types (DWG_TYPE_SELECT $value sel_list))"
  )
  (action_tile "NEW" "(setq new (TOGGLE_VARIABLE new))")
  (action_tile "EXST" "(setq exst (TOGGLE_VARIABLE exst))")
  (action_tile "DEMO" "(setq demo (TOGGLE_VARIABLE demo))")
  (action_tile
    "Overwrite"
    "(setq overwrite (YES_NO_RADIO $value))"
  )
  (action_tile
    "accept"
    "(if (= sel_list \"\")(setq err 1)(setq err 0))(DONE_DIALOG 1)"
  )
  (action_tile "cancel" "(setq err 1)(done_dialog 0)")
  (action_tile "help" "(help \"lswlc\")")
  (setq dr_it (start_dialog))
  (unload_dialog id)
  (if (= err 0)
    (progn
      ;; This is where it executes
      (LOAD_LAYERS
	sel_types
	(SEND_WORK_TYPES new exst demo)
	overwrite
      )
    )
  )
  (princ)
)

(defun DISCIPLINE_SELECT (discipline / return_value)
  (mode_tile "Dwg_Type_List" 0)
  (mode_tile "Dwg_Type_List" 2)
  (start_list "Dwg_Type_List")
  (cond
    ((= discipline "ELEC")
     (progn
       (add_list "EP - POWER PLAN")
       (add_list "EL - LIGHTING PLAN")
       (add_list "EG - GROUNDING PLAN")
       (add_list "ES - SITE PLAN")
       (add_list "EO - ONE-LINE DIAGRAM")
       (add_list "FA - FIRE ALARM PLAN")
       (add_list "SS - SPECIAL SYSTEMS PLAN")
     )
    )
    ((= discipline "MECH")
     (progn
       (add_list "MH - DUCTWORK PLAN")
       (add_list "MP - PIPING PLAN")
     )
    )
    ((= discipline "PLUM")
     (progn
       (add_list "PB - PLUMBING PLAN")
       (add_list "PF - FUEL OIL PLAN")
       (add_list "PM - MEDICAL GAS PLAN")
       (add_list "FS - FIRE SPRINKLER")
     )
    )
  )
  (end_list)
  (cond
    ((= discipline "ELEC")
     (setq return_value
	    (list
	      "EP - POWER PLAN"
	      "EL - LIGHTING PLAN"
	      "EG - GROUNDING PLAN"
	      "ES - SITE PLAN"
	      "EO - ONE-LINE DIAGRAM"
	      "FA - FIRE ALARM PLAN"
	      "SS - SPECIAL SYSTEMS PLAN"
	     )
     )
    )
    ((= discipline "MECH")
     (setq return_value
	    (list
	      "MH - DUCTWORK PLAN"
	      "MP - PIPING PLAN"
	    )
     )
    )
    ((= discipline "PLUM")
     (setq return_value
	    (list
	      "PB - PLUMBING PLAN"
	      "PF - FUEL OIL PLAN"
	      "PM - MEDICAL GAS PLAN"
	      "FS - FIRE SPRINKLER"
	    )
     )
    )
  )
)

(defun DWG_TYPE_SELECT
       (dwg_types selection_list / counter return_value)
  (setq dwg_types (read (strcat "(" dwg_types ")")))
  (setq counter (1- (length dwg_types)))
  (while (>= counter 0)
    (setq return_value
	   (cons
	     (substr (nth (nth counter dwg_types) selection_list)
		     1
		     2
	     )
	     return_value
	   )
    )
    (setq counter (1- counter))
  )
  (cond
    ((= (get_tile "DISC") "ELEC")
     (setq return_value (cons "E" return_value))
    )
    ((= (get_tile "DISC") "MECH")
     (setq return_value (cons "ME" return_value))
    )
    ((= (get_tile "DISC") "PLUM")
     (progn
       (if (vl-position "PB" return_value)
 ; Add the PS if PB is selected
	 (setq return_value (cons "PS" return_value))
       )
       (setq return_value (cons "PE" return_value))
     )
    )
  )
  (setq return_value (cons "G" return_value))
)

(defun LOAD_LOGO (tile_name slide_name)
  (start_image tile_name)
  (fill_image
    0
    0
    (dimx_tile tile_name)
    (dimy_tile tile_name)
    254
  )
  (slide_image
    0
    0
    (dimx_tile tile_name)
    (dimy_tile tile_name)
    slide_name
  )
  (end_image)
)

(defun SEND_WORK_TYPES (new exst demo / return_value)
  (if (= new "ON")
    (setq return_value (cons "N" return_value))
  )
  (if (= exst "ON")
    (setq return_value (cons "E" return_value))
  )
  (if (= demo "ON")
    (setq return_value (cons "D" return_value))
  )
  (acad_strlsort return_value)
)

(defun TOGGLE_VARIABLE (current_state / return_value)
  (if (= current_state "ON")
    (setq return_value "OFF")
    (setq return_value "ON")
  )
)

(defun YES_NO_RADIO (selected_button / return_value)
  (cond
    ((= selected_button "Overwrite_yes")
     (setq return_value "Y")
    )
    ((= selected_button "Overwrite_no") (setq return_value "N"))
  )
)

;;;  /----------------------------------------------------------\
;;;  | Custom layer functions and commands                      |
;;;  \----------------------------------------------------------/

(defun make_custom_layer_from_base (base_layer	new_layer
				  /		file_handle
				  last_line	color
				  linetype	plot_noplot lineweight work_type
				 )
  (setq base_layer (strcase base_layer))
  (setq file_handle (open (findfile layer_filename) "r"))
  (setq last_line 1)
  (while (/= last_line nil)
    (setq last_line (read-line file_handle))
    (if	(/= last_line nil)
      (progn
	(setq last_line (read (strcat "(" last_line ")")))
	(if (= base_layer
	       (strcase (vl-princ-to-string (nth 2 last_line)))
	    )
	  (progn
	    (setq color	      (strcase (vl-princ-to-string (nth 3 last_line)))
		  linetype    (strcase (vl-princ-to-string (nth 4 last_line)))
		  plot_noplot (strcase (vl-princ-to-string (nth 5 last_line)))
		  lineweight (strcase (vl-princ-to-string (nth 6 last_line)))
	    )
	    (make_layer new_layer color linetype plot_noplot lineweight "n")

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

(defun convert_to_custom_layer (custom_desc_substring  /
			      select_set	   counter
			      ent_list		   current_obj_layer
			      proposed_obj_layer   base_layer_name
			      current_layer_suffix
			     )
  (setvar "cmdecho" 0)

  (if (/= "" custom_desc_substring)
    (setq custom_desc_substring
	   (strcat "-_" (strcase custom_desc_substring))
    )
  )

  (prompt "\nSelect objects: ")
  (setq select_set (ssget))
  (setq counter -1)
  (repeat (sslength select_set)
    (setq ent_list
	   (entget (ssname select_set (setq counter (1+ counter)))
	   )
    )
    (setq current_obj_layer (cdr (assoc 8 ent_list)))

					;  Ignore all layers of less than 7 characters 
    (if	(< 7 (strlen current_obj_layer))
      (progn

		(setq current_layer_suffix
				(substr current_obj_layer
					(- (strlen current_obj_layer) 4)
				)
		   
	
		)
		
		(if
			(= "-NEW" (substr current_layer_suffix (- (strlen current_layer_suffix) 3)))
			(setq current_layer_suffix (substr current_layer_suffix (- (strlen current_layer_suffix) 3)))
		)
				
		(setq work_type	(cond
				  ((= current_layer_suffix worktype_suffix_new) worktype_suffix_new)
				  ((= current_layer_suffix worktype_suffix_existing) worktype_suffix_existing)
				  ((= current_layer_suffix worktype_suffix_demolition) worktype_suffix_demolition)
				  ((= current_layer_suffix worktype_suffix_future) worktype_suffix_future)
				  (t nil)
				)
		)
		
						;  Ignore all layers not ending in a proper work type
		(if (/= work_type nil)
		  (progn

						;check to see if the obj is already on a custom specific layer
			(if	(vl-string-search "-_" current_obj_layer)
			  (setq base_layer_name

				 (substr current_obj_layer
					 1
					 (vl-string-search "-_" current_obj_layer)
				 )
			  )


			  (setq base_layer_name
				 (substr current_obj_layer
					 1
					 (- (strlen current_obj_layer) (strlen current_layer_suffix))
				 )
			  )
			)

			(setq proposed_obj_layer
			   (strcat
				 base_layer_name
				 custom_desc_substring
				 work_type
			   )
			)

			(if	(null (tblsearch "layer" proposed_obj_layer))
			  (make_custom_layer_from_base
			(strcat base_layer_name work_type)
			proposed_obj_layer
			  )
			)

			(setq ent_list
			   (subst
				 (cons 8
				   proposed_obj_layer
				 )
				 (assoc 8 ent_list)
				 ent_list
			   )
			)
			(entmod ent_list)
		  )
		  ;(prompt "not a valid worktype\n")
		)
		  )
		  ;(prompt "too short\n")
    )
  )
  (princ)
)


(defun c:cla ( / desc_input )
  (setq cur_custom_desc_substr (cond ((> cur_custom_desc_substr "") cur_custom_desc_substr)
		     (t "")
	       )
  )
    (setq desc_input (strcase (getstring
		   (strcat "\nEnter customect Specific Layer Substring: <" cur_custom_desc_substr "> ")
		 ))
	)

	(if (and (/= desc_input " ") (/= desc_input ""))
	  (progn
	  (prompt "\nNew string given.")
	  (setq cur_custom_desc_substr desc_input)
	  )
	)
	
	(if (> cur_custom_desc_substr "")
	;then
	(convert_to_custom_layer cur_custom_desc_substr)
	;else
	(prompt "\nNo valid substring given -- Aborting.")
	)
	(princ)
)

(defun c:cla-strip ( / )
  (convert_to_custom_layer "")
  (princ)
)

;;;  /----------------------------------------------------------\
;;;  | Client Specific Layer commands                           |
;;;  \----------------------------------------------------------/

; switch to AT&T CRE layers
(defun c:CRE ()
  (load "ATT_Layers_CRE.lsp")
  (princ)
)

; CAWCD layers
(DEFUN C:CAP-D () 
 (COMMAND "-layer"
	"c" "253" "X-???-?-????-*"
	"c" "252" "D-???-?-????-*"
	"c" "252" "D-???-?-????-*"
	"c" "252" "D-???-?-????-*"
	"c" "252" "D-???-?-????-*"
	"c" "240" "X-???-?-????-*"
	"c" "240" "X-???-?-????-*"
	"c" "240" "X-???-?-????-*"
	"c" "240" "X-???-?-????-*"
	"c" "90" "D-???-?-????-*|G-DEMO*"
	"c" "90" "D-???-?-????-*|G-DEMO*"
	"c" "90" "D-???-?-????-*|G-DEMO*"
	"c" "90" "D-???-?-????-*|G-DEMO*"
	"c" "253" "D-???-?-????-*|?-ANNO*"
	"c" "253" "D-???-?-????-*|?-ANNO*"
	"c" "253" "D-???-?-????-*|?-ANNO*"
	"c" "253" "D-???-?-????-*|?-ANNO*"
	"c" "240" "X-???-?-????-*|?-ANNO*"
	"c" "240" "X-???-?-????-*|?-ANNO*"
	"c" "240" "X-???-?-????-*|?-ANNO*"
	"c" "240" "X-???-?-????-*|?-ANNO*"
	"c" "253" "X-???-?-????-FP*"
	"TH" "*DEMO"
	"")
 (princ)
)

(DEFUN C:CAP-N () 
	(C:CAP-D)
	(COMMAND "-layer"
	"FR" "*DEMO"
	"c" "253" "X-???-?-????-FP*"
	"")
	(princ)
)

(DEFUN C:CAP-D-OLD () 
 (COMMAND "-layer"
	"c" "253" "X-???-A-*"
	"c" "252" "D-???-E-*"
	"c" "252" "D-???-M-*"
	"c" "252" "D-???-P-*"
	"c" "252" "D-???-F-*"
	"c" "240" "X-???-E-*"
	"c" "240" "X-???-M-*"
	"c" "240" "X-???-P-*"
	"c" "240" "X-???-F-*"
	"c" "90" "D-???-E-*|G-DEMO"
	"c" "90" "D-???-M-*|G-DEMO"
	"c" "90" "D-???-P-*|G-DEMO"
	"c" "90" "D-???-F-*|G-DEMO"
	"c" "253" "D-???-E-*|?-ANNO*"
	"c" "253" "D-???-M-*|?-ANNO*"
	"c" "253" "D-???-P-*|?-ANNO*"
	"c" "253" "D-???-F-*|?-ANNO*"
	"c" "240" "X-???-E-*|?-ANNO*"
	"c" "240" "X-???-M-*|?-ANNO*"
	"c" "240" "X-???-P-*|?-ANNO*"
	"c" "240" "X-???-F-*|?-ANNO*"
	"TH" "*G-DEMO"
	"")
 (princ)
)