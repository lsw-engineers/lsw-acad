;;;  /----------------------------------------------------------\
;;;  | Define Global Variables                                  |
;;;  \----------------------------------------------------------/

(PRINC "Setting to AT&T CRE Layer system.")

(setq layer_filename "att_layers_cre.txt")
(setq worktype_suffix_new "")
(setq worktype_suffix_existing "-E")
(setq worktype_suffix_demolition "-D")
(setq worktype_suffix_future "-F")


(DEFUN C:DFR () (COMMAND "-layer" "fr" "*-D" "") (princ))
(DEFUN C:EFR () (COMMAND "-layer" "fr" "*-E" "") (princ))

(DEFUN C:NSET ()
 (COMMAND "-layer" "TH" "??????????|?-*"
				   "TH" "?????????|?-*"
				   "TH" "????????|?-*"
                   "FR" "*-D"
				   "")
 (PRINC)
)

(DEFUN C:DSET ()
 (COMMAND "-layer" "FR" "??????????|?-*"
				   "FR" "?????????|?-*"
				   "FR" "????????|?-*"
                   "TH" "*-?"
				   "TH" "*|S-*"
				   "TH" "*|A-GRID-IDEN*"
				   "")
 (princ)
)

(DEFUN C:FFR () (COMMAND "-layer" "fr" "*-F" "") (princ))

(DEFUN C:DTH () (COMMAND "-layer" "th" "*-D" "") (princ))
(DEFUN C:ETH () (COMMAND "-layer" "th" "*-E" "") (princ))
(DEFUN C:FTH () (COMMAND "-layer" "th" "*-F" "") (princ))

(DEFUN C:CBG () 
 (COMMAND "-layer"
	"c" "9" "??????A???|*,??????A??|*,????A???|*"
	"c" "9" "??????T???|*,??????T??|*,????D???|*"
	"FR" "*A-DIMS*,*A-EQPM*,*|A-PATT*,*A-FLOR-PATT*,*A-FLOR-ACCS*,*A-DOOR-TEXT*"
	"FR" "*BUILDING AREAS*,*M-AREA*"
	"FR" "*D-NISM-OTH_*,*T-TEXT-DIMS*,*T-TEXT-EQPT*,*T-TEXT-MISC*,*T-CBHL-DIMS*,*T-CBHL-FLOR*,*T-TEXT-HEAT*"
	"P" "NO" "*HIDDEN-NOTES*,*NPLT*,*DESIGNER-NOTES*"
 "")
 (princ)
)


(DEFUN C:SM () 
 (COMMAND "-layer" "c" "9" "????M???|*,????P???|*"
                   "c" "9" "AZ????M???|*,AZ????P???|*"
				   "fr" "AZ????M???|*-PATT*,AZ????P???|*-PATT*"
				   "")
 (princ)
)

(DEFUN C:SE () 
 (COMMAND "-layer" "c" "9" "????E???|*" "")
 (princ)
)

(DEFUN C:RAFFR () (COMMAND "-layer" "fr" "????AF??|*RAIS*" "") (princ))
(DEFUN C:RAFTH () (COMMAND "-layer" "th" "????AF??|*RAIS*" "") (princ))

(DEFUN C:DIMFR () (COMMAND "-layer" "fr" "*DIMS*" "") (princ))
(DEFUN C:DIMTH () (COMMAND "-layer" "th" "*DIMS*" "") (princ))


 ;LOAD_LAYERS
(defun C:LLDALL	()
  (LOAD_LAYERS
    (list 
    "A" "C" "D" "E" "F" "G" "L" "M" "N" "P" "R" "S" "T"
	 )
    (list "N" "E" "D" "F")
    "n"
  )
)

(defun C:LLDALL-FORCE	()
  (LOAD_LAYERS
    (list 
    "A" "C" "D" "E" "F" "G" "L" "M" "N" "P" "R" "S" "T"
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
    "A"
	 )
    (list "N" "E" "D" "F")
    "n"
  )
)

(defun C:LLDA-FORCE	()
  (LOAD_LAYERS
    (list 
    "A"
	 )
    (list "N" "E" "D" "F")
    "y"
  )
)


(defun C:LLDE ()
  (LOAD_LAYERS
    (list 
    "E"
	 )
    (list "N" "E" "D" "F")
    "n"
  )
)

(defun C:LLDF ()
  (LOAD_LAYERS
    (list 
    "D"
	 )
    (list "N" "E" "D" "F")
    "n"
  )
)

(defun C:LLDM ()
  (LOAD_LAYERS
    (list 
    "M"
	 )
    (list "N" "E" "D" "F")
    "n"
  )
)

(defun C:LLDN ()
  (LOAD_LAYERS
    (list 
    "N"
	 )
    (list "N" "E" "D" "F")
    "n"
  )
)

(defun C:LLDP ()
  (LOAD_LAYERS
    (list 
    "P" 
	 )
    (list "N" "E" "D" "F")
    "n"
  )
)

(defun C:LLDS	()
  (LOAD_LAYERS
    (list 
    "S" 
	 )
    (list "N" "E" "D" "F")
    "n"
  )
)

(defun C:LLDT	()
  (LOAD_LAYERS
    (list 
    "T"
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
  (setvar "ltscale" (/ (scalefactor) 2.0))
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




;;;  /----------------------------------------------------------\
;;;  | COMMANDS TO INSERT TAGS -- OVERWRTING LSW LAYERS         |
;;;  \----------------------------------------------------------/

 ;ALIAS
(defun C:DBBL () (C:DTP))

 ;ARC_LEADER
(defun C:AL () (ARC_LEADER "G-ANNO-TEXT"))
(defun C:DAL () (ARC_LEADER "G-ANNO-TEXT-D"))
(defun C:EAL () (ARC_LEADER "G-ANNO-TEXT-E"))

 ;INS_BLOCK
(defun C:AR ()
  (INS_BLOCK "arroa" "G-ANNO-TEXT" "end" "non" 0 "rotate" 0)
)
(defun C:DTPA ()
  (INS_BLOCK "TAG_DETAIL_ARROW"	"G-ANNO-TEXT" "cen" "non" 1 "rotate" 0)
)
(defun C:DTT ()
  (INS_BLOCK "TAG_DETAIL_TITLE" "G-ANNO-TEXT" "end" "non" 0 0 0)
)

 ;REVISION CLOUD
(defun C:RC ()
  (F:MODS (quote ("osmode" "orthomode" "cmdecho")))
  (SET_OSMODE "non")
  (setvar "orthomode" 0)
  (F:SLAY (REVISION_LAYER "REV-CLD-" "N"))
  (PAUSE_FOR_COMMAND "_.revcloud" nil)
  (F:MODR)
  (F:RLAY)
  (princ)
)

 ;REVISION TAG
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
	   "Tag_Rev"
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
	   "Tag_Rev"
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

 ;TAG_INSERT
(defun C:CT () (TAG_INSERT "Tag_Circle" "G-ANNO-TEXT"))
(defun C:DCT () (TAG_INSERT "Tag_Circle" "G-ANNO-TEXT-D"))
(defun C:ECT () (TAG_INSERT "Tag_Circle" "G-ANNO-TEXT-E"))
(defun C:DIT () (TAG_INSERT "Tag_Diamond" "G-ANNO-TEXT"))
(defun C:DDIT () (TAG_INSERT "Tag_Diamond" "G-ANNO-TEXT-D"))
(defun C:EDIT () (TAG_INSERT "Tag_Diamond" "G-ANNO-TEXT-E"))
(defun C:ET () (TAG_INSERT "Tag_Ellipse" "G-ANNO-TEXT"))
(defun C:DET () (TAG_INSERT "Tag_Ellipse" "G-ANNO-TEXT-D"))
(defun C:EET () (TAG_INSERT "Tag_Ellipse" "G-ANNO-TEXT-E"))
(defun C:HT () (TAG_INSERT "Tag_Hex" "G-ANNO-TEXT"))
(defun C:DHT () (TAG_INSERT "Tag_Hex" "G-ANNO-TEXT-D"))
(defun C:EHT () (TAG_INSERT "Tag_Hex" "G-ANNO-TEXT-E"))
(defun C:SQ () (TAG_INSERT "Tag_Square" "G-ANNO-TEXT"))
(defun C:DSQ () (TAG_INSERT "Tag_Square" "G-ANNO-TEXT-D"))
(defun C:ESQ () (TAG_INSERT "Tag_Square" "G-ANNO-TEXT-E"))


 ;TAG_INSERT_TWO_LEVEL
(defun C:DTP ()
  (TAG_INSERT_TWO_LEVEL "TAG_DETAIL" "G-ANNO-TEXT")
)
(defun C:HT2 ()
  (TAG_INSERT_TWO_LEVEL "TAG_HEX2" "G-ANNO-TEXT")
)
(defun C:DHT2 ()
  (TAG_INSERT_TWO_LEVEL "TAG_HEX2" "G-ANNO-TEXT-D")
)
(defun C:EHT2 ()
  (TAG_INSERT_TWO_LEVEL "TAG_HEX2" "G-ANNO-TEXT-E")
)
(defun C:EQT ()
  (TAG_INSERT_TWO_LEVEL "TAG_HEX2EQ" "G-ANNO-TEXT")
)
(defun C:HT2A ()
  (TAG_INSERT_TWO_LEVEL "TAG_HEX2A" "G-ANNO-TEXT")
)
(defun C:HT2B ()
  (TAG_INSERT_TWO_LEVEL "TAG_HEX2B" "G-ANNO-TEXT")
)

 ;Command to reload the equipment hex tag from the network.
 ;This is to purge the color and make everything by layer.
(defun C:ETU ()
  (command "undo" "group")
  (F:SLAY "0")
  (command "insert" "TAG_HEX2EQ=" "s" 1 "r" 0 0 "" "")
  (command "erase" (entlast) "")
  (command "attsync" "n" "TAG_HEX2EQ" "")
  (F:RLAY)
  (command "undo" "end")
  (princ)
)

 ;Section bubble with arrow and line
(defun C:SBBL ()
  (TAG_INSERT_TWO_LEVEL "tag_detail" "G-ANNO-TEXT")
  (F:MODS (quote ("cmdecho" "orthomode" "osmode")))
  (F:SLAY "G-ANNO-TEXT")
  (prompt "\nEnter rotation angle: ")
  (command "insert"
	   "tag_detail_arrow"
	   (getvar "lastpoint")
	   (SCALEFACTOR)
	   (SCALEFACTOR)
	   pause
  )
  (prompt "\nDraw a section cut line (Quad of): ")
  (command "line" "qua" pause pause "")
  (command "chprop" "l" "" "c" "6" "")
  (princ)
)

 ;TAG_WITH_LEADER
(defun C:CTA ()
  (TAG_WITH_LEADER
    "Tag_Trim_Circle"
    "Tag_Circle"
    "G-ANNO-TEXT"
    "arc"
  )
)
(defun C:DCTA ()
  (TAG_WITH_LEADER
    "Tag_Trim_Circle"
    "Tag_Circle"
    "G-ANNO-TEXT-D"
    "arc"
  )
)
(defun C:ECTA ()
  (TAG_WITH_LEADER
    "Tag_Trim_Circle"
    "Tag_Circle"
    "G-ANNO-TEXT-E"
    "arc"
  )
)
(defun C:DITA ()
  (TAG_WITH_LEADER
    "Tag_Trim_Diamond"
    "Tag_Diamond"
    "G-ANNO-TEXT"
    "arc"
  )
)
(defun C:DDITA ()
  (TAG_WITH_LEADER
    "Tag_Trim_Diamond"
    "Tag_Diamond"
    "G-ANNO-TEXT-D"
    "arc"
  )
)
(defun C:EDITA ()
  (TAG_WITH_LEADER
    "Tag_Trim_Diamond"
    "Tag_Diamond"
    "G-ANNO-TEXT-E"
    "arc"
  )
)
(defun C:ETA ()
  (TAG_WITH_LEADER
    "Tag_Trim_Ellipse"
    "Tag_Ellipse"
    "G-ANNO-TEXT"
    "arc"
  )
)
(defun C:DETA ()
  (TAG_WITH_LEADER
    "Tag_Trim_Ellipse"
    "Tag_Ellipse"
    "G-ANNO-TEXT-D"
    "arc"
  )
)
(defun C:EETA ()
  (TAG_WITH_LEADER
    "Tag_Trim_Ellipse"
    "Tag_Ellipse"
    "G-ANNO-TEXT-E"
    "arc"
  )
)
(defun C:HTA ()
  (TAG_WITH_LEADER "Tag_Trim_Hex" "Tag_Hex" "G-ANNO-TEXT" "arc")
)
(defun C:DHTA ()
  (TAG_WITH_LEADER "Tag_Trim_Hex" "Tag_Hex" "G-ANNO-TEXT-D" "arc")
)
(defun C:EHTA ()
  (TAG_WITH_LEADER "Tag_Trim_Hex" "Tag_Hex" "G-ANNO-TEXT-E" "arc")
)
(defun C:SQA ()
  (TAG_WITH_LEADER
    "Tag_Trim_Square"
    "Tag_Square"
    "G-ANNO-TEXT"
    "arc"
  )
)
(defun C:DSQA ()
  (TAG_WITH_LEADER
    "Tag_Trim_Square"
    "Tag_Square"
    "G-ANNO-TEXT-D"
    "arc"
  )
)
(defun C:ESQA ()
  (TAG_WITH_LEADER
    "Tag_Trim_Square"
    "Tag_Square"
    "G-ANNO-TEXT-E"
    "arc"
  )
)
(defun C:CTL ()
  (TAG_WITH_LEADER
    "Tag_Trim_Circle"
    "Tag_Circle"
    "G-ANNO-TEXT"
    "loop"
  )
)
(defun C:DCTL ()
  (TAG_WITH_LEADER
    "Tag_Trim_Circle"
    "Tag_Circle"
    "G-ANNO-TEXT-D"
    "loop"
  )
)
(defun C:ECTL ()
  (TAG_WITH_LEADER
    "Tag_Trim_Circle"
    "Tag_Circle"
    "G-ANNO-TEXT-E"
    "loop"
  )
)
(defun C:DITL ()
  (TAG_WITH_LEADER
    "Tag_Trim_Diamond"
    "Tag_Diamond"
    "G-ANNO-TEXT"
    "loop"
  )
)
(defun C:DDITL ()
  (TAG_WITH_LEADER
    "Tag_Trim_Diamond"
    "Tag_Diamond"
    "G-ANNO-TEXT-D"
    "loop"
  )
)
(defun C:EDITL ()
  (TAG_WITH_LEADER
    "Tag_Trim_Diamond"
    "Tag_Diamond"
    "G-ANNO-TEXT-E"
    "loop"
  )
)
(defun C:ETL ()
  (TAG_WITH_LEADER
    "Tag_Trim_Ellipse"
    "Tag_Ellipse"
    "G-ANNO-TEXT"
    "loop"
  )
)
(defun C:DETL ()
  (TAG_WITH_LEADER
    "Tag_Trim_Ellipse"
    "Tag_Ellipse"
    "G-ANNO-TEXT-D"
    "loop"
  )
)
(defun C:EETL ()
  (TAG_WITH_LEADER
    "Tag_Trim_Ellipse"
    "Tag_Ellipse"
    "G-ANNO-TEXT-E"
    "loop"
  )
)
(defun C:HTL ()
  (TAG_WITH_LEADER "Tag_Trim_Hex" "Tag_Hex" "G-ANNO-TEXT" "loop")
)
(defun C:DHTL ()
  (TAG_WITH_LEADER
    "Tag_Trim_Hex"
    "Tag_Hex"
    "G-ANNO-TEXT-D"
    "loop"
  )
)
(defun C:EHTL ()
  (TAG_WITH_LEADER
    "Tag_Trim_Hex"
    "Tag_Hex"
    "G-ANNO-TEXT-E"
    "loop"
  )
)
(defun C:SQL ()
  (TAG_WITH_LEADER
    "Tag_Trim_Square"
    "Tag_Square"
    "G-ANNO-TEXT"
    "loop"
  )
)
(defun C:DSQL ()
  (TAG_WITH_LEADER
    "Tag_Trim_Square"
    "Tag_Square"
    "G-ANNO-TEXT-D"
    "loop"
  )
)
(defun C:ESQL ()
  (TAG_WITH_LEADER
    "Tag_Trim_Square"
    "Tag_Square"
    "G-ANNO-TEXT-E"
    "loop"
  )
)
(defun C:CTS ()
  (TAG_WITH_LEADER
    "Tag_Trim_Circle"
    "Tag_Circle"
    "G-ANNO-TEXT"
    "arrow"
  )
)
(defun C:DCTS ()
  (TAG_WITH_LEADER
    "Tag_Trim_Circle"
    "Tag_Circle"
    "G-ANNO-TEXT-D"
    "arrow"
  )
)
(defun C:ECTS ()
  (TAG_WITH_LEADER
    "Tag_Trim_Circle"
    "Tag_Circle"
    "G-ANNO-TEXT-E"
    "arrow"
  )
)
(defun C:DITS ()
  (TAG_WITH_LEADER
    "Tag_Trim_Diamond"
    "Tag_Diamond"
    "G-ANNO-TEXT"
    "arrow"
  )
)
(defun C:DDITS ()
  (TAG_WITH_LEADER
    "Tag_Trim_Diamond"
    "Tag_Diamond"
    "G-ANNO-TEXT-D"
    "arrow"
  )
)
(defun C:EDITS ()
  (TAG_WITH_LEADER
    "Tag_Trim_Diamond"
    "Tag_Diamond"
    "G-ANNO-TEXT-E"
    "arrow"
  )
)
(defun C:ETS ()
  (TAG_WITH_LEADER
    "Tag_Trim_Ellipse"
    "Tag_Ellipse"
    "G-ANNO-TEXT"
    "arrow"
  )
)
(defun C:DETS ()
  (TAG_WITH_LEADER
    "Tag_Trim_Ellipse"
    "Tag_Ellipse"
    "G-ANNO-TEXT-D"
    "arrow"
  )
)
(defun C:EETS ()
  (TAG_WITH_LEADER
    "Tag_Trim_Ellipse"
    "Tag_Ellipse"
    "G-ANNO-TEXT-E"
    "arrow"
  )
)
(defun C:HTS ()
  (TAG_WITH_LEADER
    "Tag_Trim_Hex"
    "Tag_Hex"
    "G-ANNO-TEXT"
    "arrow"
  )
)
(defun C:DHTS ()
  (TAG_WITH_LEADER
    "Tag_Trim_Hex"
    "Tag_Hex"
    "G-ANNO-TEXT-D"
    "arrow"
  )
)
(defun C:EHTS ()
  (TAG_WITH_LEADER
    "Tag_Trim_Hex"
    "Tag_Hex"
    "G-ANNO-TEXT-E"
    "arrow"
  )
)
(defun C:SQS ()
  (TAG_WITH_LEADER
    "Tag_Trim_Square"
    "Tag_Square"
    "G-ANNO-TEXT"
    "arrow"
  )
)
(defun C:DSQS ()
  (TAG_WITH_LEADER
    "Tag_Trim_Square"
    "Tag_Square"
    "G-ANNO-TEXT-D"
    "arrow"
  )
)
(defun C:ESQS ()
  (TAG_WITH_LEADER
    "Tag_Trim_Square"
    "Tag_Square"
    "G-ANNO-TEXT-E"
    "arrow"
  )
)
(defun C:CTM ()
  (TAG_WITH_LEADER
    "Tag_Trim_Circle"
    "Tag_Circle"
    "G-ANNO-TEXT"
    "multiloop"
  )
)
(defun C:DCTM ()
  (TAG_WITH_LEADER
    "Tag_Trim_Circle"
    "Tag_Circle"
    "G-ANNO-TEXT-D"
    "multiloop"
  )
)
(defun C:ECTM ()
  (TAG_WITH_LEADER
    "Tag_Trim_Circle"
    "Tag_Circle"
    "G-ANNO-TEXT-E"
    "multiloop"
  )
)
(defun C:DITM ()
  (TAG_WITH_LEADER
    "Tag_Trim_Diamond"
    "Tag_Diamond"
    "G-ANNO-TEXT"
    "multiloop"
  )
)
(defun C:DDITM ()
  (TAG_WITH_LEADER
    "Tag_Trim_Diamond"
    "Tag_Diamond"
    "G-ANNO-TEXT-D"
    "multiloop"
  )
)
(defun C:EDITM ()
  (TAG_WITH_LEADER
    "Tag_Trim_Diamond"
    "Tag_Diamond"
    "G-ANNO-TEXT-E"
    "multiloop"
  )
)
(defun C:ETM ()
  (TAG_WITH_LEADER
    "Tag_Trim_Ellipse"
    "Tag_Ellipse"
    "G-ANNO-TEXT"
    "multiloop"
  )
)
(defun C:DETM ()
  (TAG_WITH_LEADER
    "Tag_Trim_Ellipse"
    "Tag_Ellipse"
    "G-ANNO-TEXT-D"
    "multiloop"
  )
)
(defun C:EETM ()
  (TAG_WITH_LEADER
    "Tag_Trim_Ellipse"
    "Tag_Ellipse"
    "G-ANNO-TEXT-E"
    "multiloop"
  )
)
(defun C:HTM ()
  (TAG_WITH_LEADER "Tag_Trim_Hex" "Tag_Hex" "G-ANNO-TEXT" "multiloop")
)
(defun C:DHTM ()
  (TAG_WITH_LEADER
    "Tag_Trim_Hex"
    "Tag_Hex"
    "G-ANNO-TEXT-D"
    "multiloop"
  )
)
(defun C:EHTM ()
  (TAG_WITH_LEADER
    "Tag_Trim_Hex"
    "Tag_Hex"
    "G-ANNO-TEXT-E"
    "multiloop"
  )
)
(defun C:SQM ()
  (TAG_WITH_LEADER
    "Tag_Trim_Square"
    "Tag_Square"
    "G-ANNO-TEXT"
    "multiloop"
  )
)
(defun C:DSQM ()
  (TAG_WITH_LEADER
    "Tag_Trim_Square"
    "Tag_Square"
    "G-ANNO-TEXT-D"
    "multiloop"
  )
)
(defun C:ESQM ()
  (TAG_WITH_LEADER
    "Tag_Trim_Square"
    "Tag_Square"
    "G-ANNO-TEXT-E"
    "multiloop"
  )
)

 ;THREE_POINT_LEADER
(defun C:3PL () (THREE_POINT_LEADER "G-ANNO-TEXT" "arrow"))
(defun C:SL () (THREE_POINT_LEADER "G-ANNO-TEXT" "arrow"))
(defun C:ESL () (THREE_POINT_LEADER "G-ANNO-TEXT-E" "arrow"))
(defun C:DSL () (THREE_POINT_LEADER "G-ANNO-TEXT-D" "arrow"))
(defun C:LP () (THREE_POINT_LEADER "G-ANNO-TEXT" "loop"))
(defun C:ELP () (THREE_POINT_LEADER "G-ANNO-TEXT-E" "loop"))
(defun C:DLP () (THREE_POINT_LEADER "G-ANNO-TEXT-D" "loop"))

 ;TWO_POINT_LEADER
(defun C:2LP () (TWO_POINT_LEADER "G-ANNO-TEXT" "loop"))
(defun C:E2LP () (TWO_POINT_LEADER "G-ANNO-TEXT-E" "loop"))
(defun C:D2LP () (TWO_POINT_LEADER "G-ANNO-TEXT-D" "loop"))
(defun C:2PL () (TWO_POINT_LEADER "G-ANNO-TEXT" "arrow"))
(defun C:E2PL () (TWO_POINT_LEADER "G-ANNO-TEXT-E" "arrow"))
(defun C:D2PL () (TWO_POINT_LEADER "G-ANNO-TEXT-D" "arrow"))
(defun C:2SL () (TWO_POINT_LEADER "G-ANNO-TEXT" "arrow"))
(defun C:E2SL () (TWO_POINT_LEADER "G-ANNO-TEXT-E" "arrow"))
(defun C:D2SL () (TWO_POINT_LEADER "G-ANNO-TEXT-D" "arrow"))