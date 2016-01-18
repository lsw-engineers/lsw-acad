 ; Load all layers matching the requested discipline and work type from
 ; a master layer setup file (external ascii text file).
(defun LOAD_LAYERS (disciplines	  work_type	overwrite
		    /		  file_handle	last_line
		    name	  color		linetype
		    plot_noplot	  l_work_type	l_discipline
		   )
  (setq file_handle (open (findfile "att_layers.txt") "r"))
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
	)
	(if (and (vl-position l_discipline disciplines)
		 (or (= "A" l_work_type)
		     (vl-position l_work_type work_type)
		 )
	    )
	  (MAKE_LAYER name color linetype plot_noplot overwrite)
	)
      )
    )
  )
  (close file_handle)
  (prompt "Layer Load Complete.")
  (princ)
)

(defun make_proj_layer_from_base (base_layer	new_layer
				  /		file_handle
				  last_line	color
				  linetype	plot_noplot work_type
				 )
  (setq base_layer (strcase base_layer))
  (setq file_handle (open (findfile "att_layers.txt") "r"))
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

(defun convert_to_proj_layer (proj_desc_substring  /
			      select_set	   counter
			      ent_list		   current_obj_layer
			      proposed_obj_layer   base_layer_name
			      current_layer_suffix
			     )
  (setvar "cmdecho" 0)

  (if (/= "" proj_desc_substring)
    (setq proj_desc_substring
	   (strcat "-_" (strcase proj_desc_substring))
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

					;  Ignore all layers of less than 11 characters 
    (if	(< 11 (strlen current_obj_layer))
      (progn

	(setq current_layer_suffix
	       (substr current_obj_layer
		       (- (strlen current_obj_layer) 4)
	       )
	)
	(setq work_type	(cond
			  ((= current_layer_suffix "-NEWW") "-NEWW")
			  ((= current_layer_suffix "-EXST") "-EXST")
			  ((= current_layer_suffix "-DEMO") "-DEMO")
			  ((= current_layer_suffix "-FUTR") "-FUTR")
			  (t nil)
			)
	)

					;  Ignore all layers not ending in a proper work type
	(if (/= work_type nil)
	  (progn

					;check to see if the obj is already on a proj specific layer
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
			     (- (strlen current_obj_layer) 5)
		     )
	      )
	    )

	    (setq proposed_obj_layer
		   (strcat
		     base_layer_name
		     proj_desc_substring
		     work_type
		   )
	    )

	    (if	(null (tblsearch "layer" proposed_obj_layer))
	      (make_proj_layer_from_base
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


(defun c:pla ( / desc_input )
  (setq cur_proj_desc_substr (cond ((> cur_proj_desc_substr "") cur_proj_desc_substr)
		     (t "")
	       )
  )
    (setq desc_input (strcase (getstring
		   (strcat "\nEnter Project Specific Layer Substring: <" cur_proj_desc_substr "> ")
		 ))
	)

	(if (and (/= desc_input " ") (/= desc_input ""))
	  (progn
	  (prompt "\nNew string given.")
	  (setq cur_proj_desc_substr desc_input)
	  )
	)
	
	(if (> cur_proj_desc_substr "")
	;then
	(convert_to_proj_layer cur_proj_desc_substr)
	;else
	(prompt "\nNo valid substring given -- Aborting.")
	)
	(princ)
)

(defun c:pla-strip ( / )
  (convert_to_proj_layer "")
  (princ)
)

 ;LOAD_LAYERS
(defun C:LLDALL	()
  (LOAD_LAYERS
    (list 
    "G" 
    "A" "AF" "AR" "AC"
    "B" "BA" "BR" "BS"
    "C" "CS" "CU" "CF" "CG"
    "D" "DF"
    "E" "ED" "EO" "ER" "EG" "EI" "EP"
    "F" "FA" "FR" "FS" 
    "M" "MA" "MC" "MH" "MY" "MQ" "MD" "MP"
    "P" "PP" "PD" "PS" "PG" "PR" "PF"
    "S" "SC" "SP" "SF" "SR" "SB" "SS"
    "T" "TF" "TC" "TX" "TG" "TL" "TO" "TD" "TW" "TM"
	 )
    (list "N" "E" "D" "F")
    "n"
  )
)

(defun C:LLDALL-FORCE	()
  (LOAD_LAYERS
    (list 
    "G" 
    "A" "AF" "AR" "AC"
    "B" "BA" "BR" "BS"
    "C" "CS" "CU" "CF" "CG"
    "D" "DF"
    "E" "ED" "EO" "ER" "EG" "EI" "EP"
    "F" "FA" "FR" "FS" 
    "M" "MA" "MC" "MH" "MY" "MQ" "MD" "MP"
    "P" "PP" "PD" "PS" "PG" "PR" "PF"
    "S" "SC" "SP" "SF" "SR" "SB" "SS"
    "T" "TF" "TC" "TX" "TG" "TL" "TO" "TD" "TW" "TM"
	 )
    (list "N" "E" "D" "F")
    "y"
  )
)

(defun C:LLD ()
  (LOAD_LAYERS (list "G") (list "N" "E" "D" "F") "n")
)

(defun C:LLDA	()
  (LOAD_LAYERS
    (list 
    "A" "AF" "AR" "AC"
	 )
    (list "N" "E" "D" "F")
    "n"
  )
)

(defun C:LLDB	()
  (LOAD_LAYERS
    (list 
    "B" "BA" "BR" "BS"
	 )
    (list "N" "E" "D" "F")
    "n"
  )
)

(defun C:LLDC	()
  (LOAD_LAYERS
    (list 
    "C" "CS" "CU" "CF" "CG"
	 )
    (list "N" "E" "D" "F")
    "n"
  )
)

(defun C:LLDD	()
  (LOAD_LAYERS
    (list 
    "D" "DF"
	 )
    (list "N" "E" "D" "F")
    "n"
  )
)

(defun C:LLDE ()
  (LOAD_LAYERS
  (LOAD_LAYERS
    (list 
    "E" "ED" "EO" "ER" "EG" "EI" "EP"
	 )
    (list "N" "E" "D" "F")
    "n"
  )
)

(defun C:LLDALL	()
  (LOAD_LAYERS
    (list 
    "F" "FA" "FR" "FS" 
	 )
    (list "N" "E" "D" "F")
    "n"
  )
)

(defun C:LLDM ()
  (LOAD_LAYERS
  (LOAD_LAYERS
    (list 
    "M" "MA" "MC" "MH" "MY" "MQ" "MD" "MP"
	 )
    (list "N" "E" "D" "F")
    "n"
  )
)

(defun C:LLDP ()
  (LOAD_LAYERS
  (LOAD_LAYERS
    (list 
    "P" "PP" "PD" "PS" "PG" "PR" "PF"
	 )
    (list "N" "E" "D" "F")
    "n"
  )
)

(defun C:LLDS	()
  (LOAD_LAYERS
    (list 
    "S" "SC" "SP" "SF" "SR" "SB" "SS"
	 )
    (list "N" "E" "D" "F")
    "n"
  )
)

(defun C:LLDT	()
  (LOAD_LAYERS
    (list 
    "T" "TF" "TC" "TX" "TG" "TL" "TO" "TD" "TW" "TM"
	 )
    (list "N" "E" "D" "F")
    "n"
  )
)