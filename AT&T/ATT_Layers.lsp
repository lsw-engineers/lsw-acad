;;;  /----------------------------------------------------------\
;;;  | Define Global Variables                                  |
;;;  \----------------------------------------------------------/

(setq layer_filename "att_layers.txt")
(setq worktype_suffix_new "-NEWW")
(setq worktype_suffix_existing "-EXST")
(setq worktype_suffix_demolition "-DEMO")
(setq worktype_suffix_future "-FUTR")


(DEFUN C:DFR () (COMMAND "-layer" "fr" "*demo*" "") (princ))
(DEFUN C:EFR () (COMMAND "-layer" "fr" "*exst*" "") (princ))
(DEFUN C:NFR () (COMMAND "-layer" "fr" "*neww*" "") (princ))
(DEFUN C:FFR () (COMMAND "-layer" "fr" "*futr*" "") (princ))

(DEFUN C:DTH () (COMMAND "-layer" "th" "*demo*" "") (princ))
(DEFUN C:ETH () (COMMAND "-layer" "th" "*exst*" "") (princ))
(DEFUN C:NTH () (COMMAND "-layer" "th" "*neww*" "") (princ))
(DEFUN C:FTH () (COMMAND "-layer" "th" "*futr*" "") (princ))

(DEFUN C:TFR ()
	(COMMAND "-layer"
		"fr" "*T-TEXT*"
		"fr" "*T-PLAN"
		"fr" "*T-CBHL*"
		"fr" "*T-EQPT-TMSP*"
		"fr" "*d-nism-oth*"
		"fr" "*D-TEXT-HEAT*,*D-TEXT-EQ*,*D-TEXT-MISC*"
		"fr" "defpoints"
		"fr" "*clearance*,*clr1*,*clr2*,*clr3*"
		"c" "250" "*|T-*"
	""
	)
	(princ)
)


(DEFUN C:CBG () 
 (COMMAND "-layer"
	"c" "7" "????A???|*,????DF??|*,*_NAI*"
	"c" "7" "AZ????A???|*,AZ????DF??|*"
	"FR" "*A-EQPM*,*A-EQUIP*,*A-ANNO-NOTE*"
	"fr" "*A-CLNG-NOTE*"
	"FR" "*G-RDWG-IFRM*"
	"FR" "*A-DOOR-IDEN*"
	"FR" "*BUILDING AREAS*,*M-AREA*"
	"P" "NO" "*HIDDEN-NOTES*,*NPLT*"
 "")
 (princ)
)

(DEFUN C:SM () 
 (COMMAND "-layer" "c" "7" "????M???|*,????P???|*" "")
 (princ)
)

(DEFUN C:SE () 
 (COMMAND "-layer" "c" "7" "????E???|*" "")
 (princ)
)

(DEFUN C:RAFFR () (COMMAND "-layer" "fr" "????AF??|*RAIS*" "") (princ))
(DEFUN C:RAFTH () (COMMAND "-layer" "th" "????AF??|*RAIS*" "") (princ))

(DEFUN C:DIMFR () (COMMAND "-layer" "fr" "*DIMS*" "") (princ))
(DEFUN C:DIMTH () (COMMAND "-layer" "th" "*DIMS*" "") (princ))




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

(defun C:LLDA-FORCE	()
  (LOAD_LAYERS
    (list 
    "A" "AF" "AR" "AC"
	 )
    (list "N" "E" "D" "F")
    "y"
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
    (list 
    "E" "ED" "EO" "ER" "EG" "EI" "EP"
	 )
    (list "N" "E" "D" "F")
    "n"
  )
)

(defun C:LLDF ()
  (LOAD_LAYERS
    (list 
    "F" "FA" "FR" "FS"
	 )
    (list "N" "E" "D" "F")
    "n"
  )
)

(defun C:FPDLLD ()
  (LOAD_LAYERS
    (list 
    "F"
	 )
    (list "A")
    "n"
  )
)

(defun C:EPDLLD ()
  (LOAD_LAYERS
    (list 
    "E"
	 )
    (list "A")
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
    (list 
    "M" "MA" "MC" "MH" "MY" "MQ" "MD" "MP"
	 )
    (list "N" "E" "D" "F")
    "n"
  )
)

(defun C:LLDP ()
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

(defun C:LLDMC ()
  (LOAD_LAYERS
    (list 
    "MC"
	 )
    (list "N" "E" "D" "F")
    "n"
  )
)