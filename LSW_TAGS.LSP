;;;;;
;;;;;
;;  LSW_TAGS.LSP - LSW TAGS
;;
;;  Copyright © 2008 by LSW Engineers Arizona, Inc.
;;
;;  
;;-------------------------------------------------------------------------
;;  DESCRIPTION
;;    Custom LSW commands related to tags.
;;    Broken into sections for tag support functions, commands that call
;;    support functions, and commands.
;;
;;
;;-------------------------------------------------------------------------
;;


;;;  /----------------------------------------------------------\
;;;  | THIS SECTION CONTAINS ALL THE LSW TAG SUPPORT FUNCTIONS. |
;;;  \----------------------------------------------------------/

 ;COMMAND TO SET MLEADER LANDING LINE FIRST
(defun C:SETLEADER ()
  (F:SLAY "0")
  (command "mleader" "L" "0,0" "1,0" "")
  (command "erase" "L" "")
  (F:RLAY)
  (princ)
)

 ;ARC LEADER FUNCTION
(defun ARC_LEADER (layer / pt1 pt2)
  (F:SLAY layer)
  (F:MODS (quote ("orthomode")))
  (setvar "orthomode" 0)
  (setq pt1 (getpoint "\nPoint at the subject :  "))
  (setq pt2 (getpoint "\nPick end of leader:  "))
  (prompt "\nEnter arrow rotation angle: ")
  (command "arc" pt1 "e" pt2 "d" pause)
  (command "insert" "arroa" "s" (SCALEFACTOR) pt1 pause)
  (F:RLAY)
  (F:MODR)
  (princ)
)

 ;DEFINES REVISION LAYERS AND DEFAULT TAG NUMBER
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
  (setq available_colors (list 55 11 151 84 30 91))
  (setq color (read defltr))
  (if (or (= 'SYM (type color)) (= nil color))
    (setq color 6)
    (setq color (nth (rem color 6) available_colors))
  )
  (MAKE_LAYER layer_name color "continuous" "p" "Default" "n")
  layer_name
)

 ;INSERT TAG
(defun TAG_INSERT (tag_block layer / tag_text_input)
  (F:SLAY layer)
  (F:MODS
    (quote ("attdia" "attreq" "highlight" "osmode" "orthomode"))
  )
  (setvar "attdia" 0)
  (setvar "attreq" 1)
  (setvar "highlight" 0)
  (SET_OSMODE "non")
  (setvar "orthomode" 0)
  (setq	default_tag_text
	 (cond ((> default_tag_text "") default_tag_text)
	       (t "")
	 )
  )
  (setq	tag_text_input	 (getstring
			   (strcat "\nEnter Tag Text <"
				   default_tag_text
				   ">: "
			   )
			 )
	default_tag_text (cond ((= tag_text_input " ") "")
			       ((> tag_text_input "") tag_text_input)
			       (t default_tag_text)
			 )
  )
  (prompt "\nEnter Tag Insertion Point: ")
  (command "insert"
	   tag_block
	   "s"
	   (SCALEFACTOR)
	   "r"
	   0
	   pause
	   default_tag_text
  )
  (F:MODR)
  (F:RLAY)
  (princ)
)

 ;INSERT TWO LEVEL HEXT TAG
(defun TAG_INSERT_TWO_LEVEL (tag_block layer / ip upper lower)
  (F:SLAY layer)
  (F:MODS
    (quote ("attdia" "attreq" "highlight" "osmode" "orthomode"))
  )
  (setvar "attdia" 0)
  (setvar "attreq" 1)
  (setvar "highlight" 0)
  (SET_OSMODE "non")
  (setvar "orthomode" 0)
  (prompt "\nEnter Tag Insertion Point: ")
  (command "-insert"
	   tag_block
	   "r"
	   0
	   "s"
	   (SCALEFACTOR)
	   pause
	   ""
	   ""
  )
  (setq ip (getvar "lastpoint"))
  (setq upper (getstring "\nEnter Top Designator: "))
  (setq lower (getstring "\nEnter Bottom Number: "))
  (command "erase" (entlast) "")
  (command "insert"
	   tag_block
	   "s"
	   (SCALEFACTOR)
	   "r"
	   0
	   ip
	   upper
	   lower
  )
  (F:MODR)
  (F:RLAY)
  (princ)
)

 ;INSERT TAG WITH LEADER
(defun TAG_WITH_LEADER (trim_block    tag_block	    layer
			leader_type   /		    tag_text_input
			insertion_point		    trim_entity
			end_point     ang1	    pt3
			pt4	      pt5	    pt6
			near_point    message
		       )
  (F:SLAY layer)
  (F:MODS
    (quote ("attdia" "attreq" "highlight" "osmode" "orthomode"
		     "plinewid")
    )
  )
  (setvar "attdia" 0)
  (setvar "attreq" 1)
  (setvar "highlight" 0)
  (SET_OSMODE "non")
  (setvar "orthomode" 0)
  (setvar "plinewid" 0)
  (setq	default_tag_text
	 (cond ((> default_tag_text "") default_tag_text)
	       (t "")
	 )
  )
  (setq	tag_text_input	 (getstring
			   (strcat "\nEnter Tag Text <"
				   default_tag_text
				   ">: "
			   )
			 )
	default_tag_text (cond ((= tag_text_input " ") "")
			       ((> tag_text_input "") tag_text_input)
			       (t default_tag_text)
			 )
  )
  (prompt "\nPick Tag Insertion Point: ")
  (command "insert" trim_block "s" (SCALEFACTOR) "r" 0 pause)
  (setq insertion_point (getvar "lastpoint"))
  (command "explode" (entlast))
  (setq trim_entity (entlast))
  (cond ((= leader_type "multiloop")
	 (SET_OSMODE "nea")
	 (setq message "\nSelect a point on the furthest line to be looped:  ")
	)
	((= leader_type "loop")
	 (setq message "\nSelect a point just past the line you wish to loop:  ")
	)
	(t
	 (setq message "\nPoint at the Subject:  ")
	)
  )
  (setq end_point (getpoint message))
  (while (/= end_point nil)
    (setq ang1 (angle end_point insertion_point))
    (cond ((= leader_type "arc")
	   (command "arc" end_point "e" insertion_point "d" pause)
	   (command "trim" trim_entity "" insertion_point "")
	   (command "insert"
		    "arroa"
		    end_point
		    (SCALEFACTOR)
		    (SCALEFACTOR)
		    pause
	   )
	  )
	  ((= leader_type "arrow")
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
	  )
	  ((= leader_type "loop")
	   (setq pt3 (polar end_point (+ ang1 (* (/ pi 30) 13)) 1))
	   (setq pt4 (polar end_point
			    (- ang1 (/ pi 15))
			    (* (SCALEFACTOR) 0.09375)
		     )
	   )
	   (command "pline"    insertion_point	     end_point
		    "a"	       "d"	  pt3	     pt4
		    ""
		   )
	   (command "trim" trim_entity "" insertion_point "")
	  )
	  ((= leader_type "multiloop")
	   (SET_OSMODE "non")
	   (setq
	     pt3 (polar end_point (+ ang1 pi) (* (SCALEFACTOR) 0.03711))
	   )
	   (setq
	     pt4 (polar pt3 (+ ang1 (/ pi 2)) (* (SCALEFACTOR) 0.03711))
	   )
	   (command "line" insertion_point pt3 "")
	   (SET_OSMODE "int")
	   (setq near_point
		  (getpoint
		    "\nSelect point on closest line to be looped:  "
		  )
	   )
	   (command "erase" (entlast) "")
	   (SET_OSMODE "non")
	   (setq pt5 (polar pt4
			    ang1
			    (+ (distance end_point near_point)
			       (* (SCALEFACTOR) 0.07422)
			    )
		     )
	   )
	   (setq
	     pt6 (polar pt5 (- ang1 (/ pi 2)) (* (SCALEFACTOR) 0.055665))
	   )
	   (command "pline" insertion_point pt3	pt4 pt5	pt6 "")
	   (command "trim" trim_entity "" insertion_point "")
	   (SET_OSMODE "nea")
	  )
	  (t
	  )
    )
    (setq end_point (getpoint "\nPick another point or press enter:  "))
  )
  (command "erase" trim_entity "")
  (command "insert"
	   tag_block
	   insertion_point
	   (SCALEFACTOR)
	   (SCALEFACTOR)
	   0
	   default_tag_text
  )
  (F:MODR)
  (F:RLAY)
  (princ)
)




 ;DRAW 3-POINT LEADER
(defun THREE_POINT_LEADER (layer leader_type / pt1 pt2 pt3 pt4 pt5)
  (command "undo" "group")
  (if (/= layer "") (F:SLAY layer))  
  (F:MODS
    (quote
      ("orthomode" "osmode" "plinewid" "snapang" "snapstyl")
    )
  )
  (setvar "orthomode" 1)
  (SET_OSMODE "non")
  (setvar "plinewid" 0)
  (setvar "snapang" 0)
  (setvar "snapstyl" 0)
  (setq pt1 (getpoint "\nInput the Start Point at the End of text: "))
  (setq	pt2
	 (getpoint pt1
		   "\nInput the Second Point at End of Straight Section: "
	 )
  )
  (command "line" pt1 pt2 "")
  (setvar "orthomode" 0)
  (setq pt3 (getpoint pt2 "\nInput Third Point near Object: "))
  (command "erase" "l" "")
  (while (/= pt3 nil)
    (cond
      ((= leader_type "arrow")
       (setq pt4 (polar pt3 (angle pt3 pt2) (* (SCALEFACTOR) 0.09375)))
       (command	"pline"
		pt1
		pt2
		pt4
		"w"
		(* (SCALEFACTOR) 0.046875)
		0
		pt3
		""
       )
      )
      ((= leader_type "loop")
       (setq pt4 (polar pt3 (+ (angle pt3 pt2) (* (/ pi 30) 13)) 1))
       (setq
	 pt5
	  (polar pt3
		 (- (angle pt3 pt2) (/ pi 15))
		 (* (SCALEFACTOR) 0.09375)
	  )
       )
       (command "pline" pt1 pt2 pt3 "a" "d" pt4 pt5 "")
      )
    )
    (setq pt3 (getpoint pt2 "\nPick another point or press enter:  "))
  )
  (if (/= layer "") (F:RLAY))
  (F:MODR)
  (command "undo" "end")
  (princ)
)

 ;DRAW 2-POINT LEADER
(defun TWO_POINT_LEADER	(layer leader_type / pt1 pt2 pt3 pt4)
  (command "undo" "group")
  (F:SLAY layer)
  (F:MODS (quote ("orthomode" "osmode" "plinewid")))
  (setvar "orthomode" 0)
  (SET_OSMODE "non")
  (setvar "plinewid" 0)
  (setq pt1 (getpoint "\nPoint near the subject:  "))
  (setq pt2 (getpoint pt1 "\nPoint near the tag/text:  "))
  (cond
    ((= leader_type "arrow")
     (setq pt3 (polar pt1 (angle pt1 pt2) (* (SCALEFACTOR) 0.09375)))
     (command "pline"
	      pt2
	      pt3
	      "w"
	      (* (SCALEFACTOR) 0.046875)
	      0
	      pt1
	      ""
     )
    )
    ((= leader_type "loop")
     (setq pt3 (polar pt1 (+ (angle pt1 pt2) (* (/ pi 30) 13)) 1))
     (setq
       pt4 (polar pt1
		  (- (angle pt1 pt2) (/ pi 15))
		  (* (SCALEFACTOR) 0.09375)
	   )
     )
     (command "pline" pt2 pt1 "a" "d" pt3 pt4 "")
    )
  )
  (F:RLAY)
  (F:MODR)
  (command "undo" "end")
  (princ)
)


;;;  /----------------------------------------------------------\
;;;  | COMMANDS TO INSERT TAGS.					|
;;;  \----------------------------------------------------------/

 ;ALIAS
(defun C:DBBL () (C:DTP))

 ;ARC_LEADER
(defun C:AL () (ARC_LEADER "NOTE-NEW"))
(defun C:DAL () (ARC_LEADER "NOTE-DEMO"))
(defun C:EAL () (ARC_LEADER "NOTE-EXST"))

 ;INS_BLOCK
(defun C:AR ()
  (INS_BLOCK "arroa" "NOTE-NEW" "end" "non" 0 "rotate" 0)
)
(defun C:DTPA ()
  (INS_BLOCK "TAG_DETAIL_ARROW"	"note-new" "cen" "non" 1 "rotate" 0)
)
(defun C:DTT ()
  (INS_BLOCK "TAG_DETAIL_TITLE" "NOTE-NEW" "end" "non" 0 0 0)
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
(defun C:CT () (TAG_INSERT "Tag_Circle" "NOTE-NEW"))
(defun C:DCT () (TAG_INSERT "Tag_Circle" "NOTE-DEMO"))
(defun C:ECT () (TAG_INSERT "Tag_Circle" "NOTE-EXST"))
(defun C:DIT () (TAG_INSERT "Tag_Diamond" "NOTE-NEW"))
(defun C:DDIT () (TAG_INSERT "Tag_Diamond" "NOTE-DEMO"))
(defun C:EDIT () (TAG_INSERT "Tag_Diamond" "NOTE-EXST"))
(defun C:ET () (TAG_INSERT "Tag_Ellipse" "NOTE-NEW"))
(defun C:DET () (TAG_INSERT "Tag_Ellipse" "NOTE-DEMO"))
(defun C:EET () (TAG_INSERT "Tag_Ellipse" "NOTE-EXST"))
(defun C:HT () (TAG_INSERT "Tag_Hex" "NOTE-NEW"))
(defun C:DHT () (TAG_INSERT "Tag_Hex" "NOTE-DEMO"))
(defun C:EHT () (TAG_INSERT "Tag_Hex" "NOTE-EXST"))
(defun C:SQ () (TAG_INSERT "Tag_Square" "NOTE-NEW"))
(defun C:DSQ () (TAG_INSERT "Tag_Square" "NOTE-DEMO"))
(defun C:ESQ () (TAG_INSERT "Tag_Square" "NOTE-EXST"))


 ;TAG_INSERT_TWO_LEVEL
(defun C:DTP ()
  (TAG_INSERT_TWO_LEVEL "TAG_DETAIL" "note-new")
)
(defun C:HT2 ()
  (TAG_INSERT_TWO_LEVEL "TAG_HEX2" "note-new")
)
(defun C:DHT2 ()
  (TAG_INSERT_TWO_LEVEL "TAG_HEX2" "note-demo")
)
(defun C:EHT2 ()
  (TAG_INSERT_TWO_LEVEL "TAG_HEX2" "note-exst")
)
(defun C:EQT ()
  (TAG_INSERT_TWO_LEVEL "TAG_HEX2EQ" "note-new")
)
(defun C:HT2A ()
  (TAG_INSERT_TWO_LEVEL "TAG_HEX2A" "note-new")
)
(defun C:HT2B ()
  (TAG_INSERT_TWO_LEVEL "TAG_HEX2B" "note-new")
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
  (TAG_INSERT_TWO_LEVEL "tag_detail" "note-new")
  (F:MODS (quote ("cmdecho" "orthomode" "osmode")))
  (F:SLAY "note-new")
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
    "NOTE-NEW"
    "arc"
  )
)
(defun C:DCTA ()
  (TAG_WITH_LEADER
    "Tag_Trim_Circle"
    "Tag_Circle"
    "NOTE-DEMO"
    "arc"
  )
)
(defun C:ECTA ()
  (TAG_WITH_LEADER
    "Tag_Trim_Circle"
    "Tag_Circle"
    "NOTE-EXST"
    "arc"
  )
)
(defun C:DITA ()
  (TAG_WITH_LEADER
    "Tag_Trim_Diamond"
    "Tag_Diamond"
    "NOTE-NEW"
    "arc"
  )
)
(defun C:DDITA ()
  (TAG_WITH_LEADER
    "Tag_Trim_Diamond"
    "Tag_Diamond"
    "NOTE-DEMO"
    "arc"
  )
)
(defun C:EDITA ()
  (TAG_WITH_LEADER
    "Tag_Trim_Diamond"
    "Tag_Diamond"
    "NOTE-EXST"
    "arc"
  )
)
(defun C:ETA ()
  (TAG_WITH_LEADER
    "Tag_Trim_Ellipse"
    "Tag_Ellipse"
    "NOTE-NEW"
    "arc"
  )
)
(defun C:DETA ()
  (TAG_WITH_LEADER
    "Tag_Trim_Ellipse"
    "Tag_Ellipse"
    "NOTE-DEMO"
    "arc"
  )
)
(defun C:EETA ()
  (TAG_WITH_LEADER
    "Tag_Trim_Ellipse"
    "Tag_Ellipse"
    "NOTE-EXST"
    "arc"
  )
)
(defun C:HTA ()
  (TAG_WITH_LEADER "Tag_Trim_Hex" "Tag_Hex" "NOTE-NEW" "arc")
)
(defun C:DHTA ()
  (TAG_WITH_LEADER "Tag_Trim_Hex" "Tag_Hex" "NOTE-DEMO" "arc")
)
(defun C:EHTA ()
  (TAG_WITH_LEADER "Tag_Trim_Hex" "Tag_Hex" "NOTE-EXST" "arc")
)
(defun C:SQA ()
  (TAG_WITH_LEADER
    "Tag_Trim_Square"
    "Tag_Square"
    "NOTE-NEW"
    "arc"
  )
)
(defun C:DSQA ()
  (TAG_WITH_LEADER
    "Tag_Trim_Square"
    "Tag_Square"
    "NOTE-DEMO"
    "arc"
  )
)
(defun C:ESQA ()
  (TAG_WITH_LEADER
    "Tag_Trim_Square"
    "Tag_Square"
    "NOTE-EXST"
    "arc"
  )
)
(defun C:CTL ()
  (TAG_WITH_LEADER
    "Tag_Trim_Circle"
    "Tag_Circle"
    "NOTE-NEW"
    "loop"
  )
)
(defun C:DCTL ()
  (TAG_WITH_LEADER
    "Tag_Trim_Circle"
    "Tag_Circle"
    "NOTE-DEMO"
    "loop"
  )
)
(defun C:ECTL ()
  (TAG_WITH_LEADER
    "Tag_Trim_Circle"
    "Tag_Circle"
    "NOTE-EXST"
    "loop"
  )
)
(defun C:DITL ()
  (TAG_WITH_LEADER
    "Tag_Trim_Diamond"
    "Tag_Diamond"
    "NOTE-NEW"
    "loop"
  )
)
(defun C:DDITL ()
  (TAG_WITH_LEADER
    "Tag_Trim_Diamond"
    "Tag_Diamond"
    "NOTE-DEMO"
    "loop"
  )
)
(defun C:EDITL ()
  (TAG_WITH_LEADER
    "Tag_Trim_Diamond"
    "Tag_Diamond"
    "NOTE-EXST"
    "loop"
  )
)
(defun C:ETL ()
  (TAG_WITH_LEADER
    "Tag_Trim_Ellipse"
    "Tag_Ellipse"
    "NOTE-NEW"
    "loop"
  )
)
(defun C:DETL ()
  (TAG_WITH_LEADER
    "Tag_Trim_Ellipse"
    "Tag_Ellipse"
    "NOTE-DEMO"
    "loop"
  )
)
(defun C:EETL ()
  (TAG_WITH_LEADER
    "Tag_Trim_Ellipse"
    "Tag_Ellipse"
    "NOTE-EXST"
    "loop"
  )
)
(defun C:HTL ()
  (TAG_WITH_LEADER "Tag_Trim_Hex" "Tag_Hex" "NOTE-NEW" "loop")
)
(defun C:DHTL ()
  (TAG_WITH_LEADER
    "Tag_Trim_Hex"
    "Tag_Hex"
    "NOTE-DEMO"
    "loop"
  )
)
(defun C:EHTL ()
  (TAG_WITH_LEADER
    "Tag_Trim_Hex"
    "Tag_Hex"
    "NOTE-EXST"
    "loop"
  )
)
(defun C:SQL ()
  (TAG_WITH_LEADER
    "Tag_Trim_Square"
    "Tag_Square"
    "NOTE-NEW"
    "loop"
  )
)
(defun C:DSQL ()
  (TAG_WITH_LEADER
    "Tag_Trim_Square"
    "Tag_Square"
    "NOTE-DEMO"
    "loop"
  )
)
(defun C:ESQL ()
  (TAG_WITH_LEADER
    "Tag_Trim_Square"
    "Tag_Square"
    "NOTE-EXST"
    "loop"
  )
)
(defun C:CTS ()
  (TAG_WITH_LEADER
    "Tag_Trim_Circle"
    "Tag_Circle"
    "NOTE-NEW"
    "arrow"
  )
)
(defun C:DCTS ()
  (TAG_WITH_LEADER
    "Tag_Trim_Circle"
    "Tag_Circle"
    "NOTE-DEMO"
    "arrow"
  )
)
(defun C:ECTS ()
  (TAG_WITH_LEADER
    "Tag_Trim_Circle"
    "Tag_Circle"
    "NOTE-EXST"
    "arrow"
  )
)
(defun C:DITS ()
  (TAG_WITH_LEADER
    "Tag_Trim_Diamond"
    "Tag_Diamond"
    "NOTE-NEW"
    "arrow"
  )
)
(defun C:DDITS ()
  (TAG_WITH_LEADER
    "Tag_Trim_Diamond"
    "Tag_Diamond"
    "NOTE-DEMO"
    "arrow"
  )
)
(defun C:EDITS ()
  (TAG_WITH_LEADER
    "Tag_Trim_Diamond"
    "Tag_Diamond"
    "NOTE-EXST"
    "arrow"
  )
)
(defun C:ETS ()
  (TAG_WITH_LEADER
    "Tag_Trim_Ellipse"
    "Tag_Ellipse"
    "NOTE-NEW"
    "arrow"
  )
)
(defun C:DETS ()
  (TAG_WITH_LEADER
    "Tag_Trim_Ellipse"
    "Tag_Ellipse"
    "NOTE-DEMO"
    "arrow"
  )
)
(defun C:EETS ()
  (TAG_WITH_LEADER
    "Tag_Trim_Ellipse"
    "Tag_Ellipse"
    "NOTE-EXST"
    "arrow"
  )
)
(defun C:HTS ()
  (TAG_WITH_LEADER
    "Tag_Trim_Hex"
    "Tag_Hex"
    "NOTE-NEW"
    "arrow"
  )
)
(defun C:DHTS ()
  (TAG_WITH_LEADER
    "Tag_Trim_Hex"
    "Tag_Hex"
    "NOTE-DEMO"
    "arrow"
  )
)
(defun C:EHTS ()
  (TAG_WITH_LEADER
    "Tag_Trim_Hex"
    "Tag_Hex"
    "NOTE-EXST"
    "arrow"
  )
)
(defun C:SQS ()
  (TAG_WITH_LEADER
    "Tag_Trim_Square"
    "Tag_Square"
    "NOTE-NEW"
    "arrow"
  )
)
(defun C:DSQS ()
  (TAG_WITH_LEADER
    "Tag_Trim_Square"
    "Tag_Square"
    "NOTE-DEMO"
    "arrow"
  )
)
(defun C:ESQS ()
  (TAG_WITH_LEADER
    "Tag_Trim_Square"
    "Tag_Square"
    "NOTE-EXST"
    "arrow"
  )
)
(defun C:CTM ()
  (TAG_WITH_LEADER
    "Tag_Trim_Circle"
    "Tag_Circle"
    "NOTE-NEW"
    "multiloop"
  )
)
(defun C:DCTM ()
  (TAG_WITH_LEADER
    "Tag_Trim_Circle"
    "Tag_Circle"
    "NOTE-DEMO"
    "multiloop"
  )
)
(defun C:ECTM ()
  (TAG_WITH_LEADER
    "Tag_Trim_Circle"
    "Tag_Circle"
    "NOTE-EXST"
    "multiloop"
  )
)
(defun C:DITM ()
  (TAG_WITH_LEADER
    "Tag_Trim_Diamond"
    "Tag_Diamond"
    "NOTE-NEW"
    "multiloop"
  )
)
(defun C:DDITM ()
  (TAG_WITH_LEADER
    "Tag_Trim_Diamond"
    "Tag_Diamond"
    "NOTE-DEMO"
    "multiloop"
  )
)
(defun C:EDITM ()
  (TAG_WITH_LEADER
    "Tag_Trim_Diamond"
    "Tag_Diamond"
    "NOTE-EXST"
    "multiloop"
  )
)
(defun C:ETM ()
  (TAG_WITH_LEADER
    "Tag_Trim_Ellipse"
    "Tag_Ellipse"
    "NOTE-NEW"
    "multiloop"
  )
)
(defun C:DETM ()
  (TAG_WITH_LEADER
    "Tag_Trim_Ellipse"
    "Tag_Ellipse"
    "NOTE-DEMO"
    "multiloop"
  )
)
(defun C:EETM ()
  (TAG_WITH_LEADER
    "Tag_Trim_Ellipse"
    "Tag_Ellipse"
    "NOTE-EXST"
    "multiloop"
  )
)
(defun C:HTM ()
  (TAG_WITH_LEADER "Tag_Trim_Hex" "Tag_Hex" "NOTE-NEW" "multiloop")
)
(defun C:DHTM ()
  (TAG_WITH_LEADER
    "Tag_Trim_Hex"
    "Tag_Hex"
    "NOTE-DEMO"
    "multiloop"
  )
)
(defun C:EHTM ()
  (TAG_WITH_LEADER
    "Tag_Trim_Hex"
    "Tag_Hex"
    "NOTE-EXST"
    "multiloop"
  )
)
(defun C:SQM ()
  (TAG_WITH_LEADER
    "Tag_Trim_Square"
    "Tag_Square"
    "NOTE-NEW"
    "multiloop"
  )
)
(defun C:DSQM ()
  (TAG_WITH_LEADER
    "Tag_Trim_Square"
    "Tag_Square"
    "NOTE-DEMO"
    "multiloop"
  )
)
(defun C:ESQM ()
  (TAG_WITH_LEADER
    "Tag_Trim_Square"
    "Tag_Square"
    "NOTE-EXST"
    "multiloop"
  )
)

 ;THREE_POINT_LEADER
(defun C:3PL () (THREE_POINT_LEADER "NOTE-NEW" "arrow"))
(defun C:SL () (THREE_POINT_LEADER "NOTE-NEW" "arrow"))
(defun C:ESL () (THREE_POINT_LEADER "NOTE-EXST" "arrow"))
(defun C:DSL () (THREE_POINT_LEADER "NOTE-DEMO" "arrow"))
(defun C:LP () (THREE_POINT_LEADER "NOTE-NEW" "loop"))
(defun C:ELP () (THREE_POINT_LEADER "NOTE-EXST" "loop"))
(defun C:DLP () (THREE_POINT_LEADER "NOTE-DEMO" "loop"))

 ;TWO_POINT_LEADER
(defun C:2LP () (TWO_POINT_LEADER "NOTE-NEW" "loop"))
(defun C:E2LP () (TWO_POINT_LEADER "NOTE-EXST" "loop"))
(defun C:D2LP () (TWO_POINT_LEADER "NOTE-DEMO" "loop"))
(defun C:2PL () (TWO_POINT_LEADER "NOTE-NEW" "arrow"))
(defun C:E2PL () (TWO_POINT_LEADER "NOTE-EXST" "arrow"))
(defun C:D2PL () (TWO_POINT_LEADER "NOTE-DEMO" "arrow"))
(defun C:2SL () (TWO_POINT_LEADER "NOTE-NEW" "arrow"))
(defun C:E2SL () (TWO_POINT_LEADER "NOTE-EXST" "arrow"))
(defun C:D2SL () (TWO_POINT_LEADER "NOTE-DEMO" "arrow"))


