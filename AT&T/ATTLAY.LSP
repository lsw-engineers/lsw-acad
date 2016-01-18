(defun c:qq () (load "attlay"))
;
;
;  AT&T CAD Tool - ATTLAY - "AT&T Layer Setup Tool"
;  
;  Written by John M. Volkar over the weekend of July 27-28 (1996) in response to
;  several complaints by local consultants utilizing the AT&T CAD Specifcation.
;
;  Revised: December 10 1998.  
;
;  John M. Volkar  --  
;  Private Email:  volkar@usaor.net
;
;
; ====================================================================================================
(defun C:attlay ()
   (setvar "CMDECHO" 0)
   (defun c:att() (c:attlay))
   ;
   ; ----------------------------------------
   ; Standard error--Begin
   (if (not ld_sld)       ;Check for the presence of one of the base functions.
      (progn
         (if (findfile "atterr.lsp")
            (load (findfile "atterr.lsp"))
            (progn (alert "\nCannot find ATTERR file!") (exit))
         ) ;if
      ) ;progn
   ) ;if
   ; Standard error--End
   ;
   ; ----------------------------------------
   ; Checks ATT_ERR version
   (if (< att_err_version 0.1)
      (progn (alert "\nNeed ATT_ERR version 0.1 or greater!") (exit))
   ) ;if   
   ;
   ; ----------------------------------------
   ; Check for necessary files
   (file_exists "attlay.dcl")
   (file_exists "attlay.ahp")
   (file_exists "attlay.slb")
   (file_exists "attlay.dat")
   (file_exists "att_acad.lin")
   ;
   ; ----------------------------------------
   ; List Data for the drawing type lists
   (setq attlay_ARCH
      (list
         (list "AC" "AC\tCeiling Plan")
         (list "AF" "AF\tFloor Plan")
         (list "AR" "AR\tRoof Plan")
         (list "AH" "AH\tRegen Hut Plan")
         (list "AV" "AV\tVault (CEV) Plan")
      ) ;list
   ) ;setq
   (setq attlay_BSYS
      (list
         (list "BA" "BA\tAlarm Plan")
         (list "BR" "BR\tSecurity Riser Diagram")
         (list "BS" "BS\tSecurity Plan")
      ) ;list
   ) ;setq
   (setq attlay_CIVL
      (list
         (list "CS" "CS\tSite Plan")
         (list "CU" "CU\tSite Utility Plan")
         (list "CF" "CF\tConstruction Facilities Plan")
         (list "CG" "CG\tGrading Plan")
         (list "CH" "CH\tRegen Hut Plan")
         (list "CV" "CV\tVault (CEV) Plan")
      ) ;list
   ) ;setq
   (setq attlay_CTRL
      (list
         (list "nil" "  \tNone")
      ) ;list
   ) ;setq
   (setq attlay_DVLP
      (list
         (list "DF" "DF\tFloor Plan")
         (list "DH" "DH\tRegen Hut Plan")
         (list "DV" "DV\tVault (CEV) Plan")
      ) ;list
   ) ;setq
   (setq attlay_ELEC
      (list
         (list "ED" "ED\tDistribution One-Line Diagram")
         (list "EO" "EO\tMain One-Line Diagram")
         (list "ER" "ER\tBlock Riser Diagram")
         (list "EG" "EG\tGrounding Plan")
         (list "EI" "EI\tLighting Plan")
         (list "EP" "EP\tPower Plan")
         (list "EH" "EH\tRegen Hut Plan")
         (list "EV" "EV\tVault (CEV) Plan")
      ) ;list
   ) ;setq
   (setq attlay_FIRE
      (list
         (list "FC" "FC\tFire Alarm/Control Diagram ")
         (list "FR" "FR\tFire Suppression Riser Diagram")
         (list "FA" "FA\tFire Alarm Plan")
         (list "FS" "FS\tFire Suppression Plan")
      ) ;list
   ) ;setq
   (setq attlay_MECH
      (list
         (list "MA" "MA\tAir Flow P&ID")
         (list "MC" "MC\tCooling P&ID")
         (list "MH" "MH\tHeating P&ID")
         (list "MY" "MY\tStandby Power P&ID")
         (list "MD" "MD\tDuct Plan")
         (list "MQ" "MQ\tEquipment Plan")
         (list "MP" "MP\tPiping Plan")
         (list "MH" "MH\tRegen Hut Plan")
         (list "MV" "MV\tVault (CEV) Plan")
      ) ;list
   ) ;setq
   (setq attlay_PLUM
      (list
         (list "PP" "pp\tPiping Plan")
         (list "PD" "PD\tDomestic Water Riser Diagram")
         (list "PS" "PS\tSanitary Riser Diagram")
         (list "PG" "PG\tNatural Gas Riser Diagram")
         (list "PR" "PR\tStorm (Roof) Drainage Riser Diagram")
         (list "PF" "PF\tFuel Tank Plan")
      ) ;list
   ) ;setq
   (setq attlay_STRU
      (list
         (list "SC" "SC\tColumn Location Plan")
         (list "SP" "SP\tFoundation Plan")
         (list "SF" "SF\tSteel Floor Framing Plan")
         (list "SR" "SR\tSteel Roof Framing Plan")
         (list "SB" "SB\tSteel Bracing Elevations")
         (list "SS" "SS\tConcrete Slab Plans")
      ) ;list
   ) ;setq
   (setq attlay_TELE
      (list
         (list "TF" "TF\tFloor Plan")
         (list "TC" "TC\tCable Racking Plan")
         (list "TD" "TD\tCable Diversity Plan")
         (list "TL" "TL\tLighting Plan")
         (list "TG" "TG\tGrounding Plan ")
         (list "TP" "TP\tFiber Protection Plan")
         (list "TM" "TM\tDMACS Plan")
         (list "TW" "TW\tWaveguide Plan")
         (list "TH" "TH\tRegen Hut Plan")
         (list "TV" "TV\tVault (CEV) Plan")
      ) ;list
   ) ;setq
   ;
   ;
   ; ----------------------------------------
   ; Load the DCL file.
   (setq dcl_id (load_dialog "attlay.dcl"))
   (if (not (new_dialog "ATTLAY" dcl_id)) (exit))
   ;
   ; ----------------------------------------
   ; Loads logo into preview box
   (ld_sld "ATT_CAD_Tool" "attlay" "logo_att")
   ;
   ; ----------------------------------------
   ;
   ; ----------------------------------------
   ; Initialize some variables
   (setq attlay_typedata nil)
   (setq attlay_go_type nil)
   ;
   ; Set toggles and radios to initial default values
   (setq attlay_go_clr T)     (set_tile "Reset_Color" "1")
   (setq attlay_go_lt T)      (set_tile "Reset_LT" "1")
   (setq attlay_go_xref nil)
   (setq attlay_go_neww T)    (set_tile "For_NEWW" "1")
   (setq attlay_go_exst nil)
   (setq attlay_go_demo nil)
   (setq attlay_go_futr nil)
   (setq attlay_go_nil nil)   
   (setq attlay_go_lays "ALL") (set_tile "For_All_Lays" "1")
   (setq attlay_go_do "CREATE") (set_tile "Do_Create" "1")
   (attlay_toggle_do)
   (attlay_toggle_for)
   
   ; ----------------------------------------
   ; Sets the actions tiles
   (action_tile "Disc_ARCH" "(setq attlay_typedata attlay_ARCH) (attlay_drawlist)")
   (action_tile "Disc_BSYS" "(setq attlay_typedata attlay_BSYS) (attlay_drawlist)")
   (action_tile "Disc_CIVL" "(setq attlay_typedata attlay_CIVL) (attlay_drawlist)")
   (action_tile "Disc_CTRL" "(setq attlay_typedata attlay_CTRL) (attlay_drawlist)")
   (action_tile "Disc_DVLP" "(setq attlay_typedata attlay_DVLP) (attlay_drawlist)")
   (action_tile "Disc_ELEC" "(setq attlay_typedata attlay_ELEC) (attlay_drawlist)")
   (action_tile "Disc_FIRE" "(setq attlay_typedata attlay_FIRE) (attlay_drawlist)")
   (action_tile "Disc_MECH" "(setq attlay_typedata attlay_MECH) (attlay_drawlist)")
   (action_tile "Disc_PLUM" "(setq attlay_typedata attlay_PLUM) (attlay_drawlist)")
   (action_tile "Disc_STRU" "(setq attlay_typedata attlay_STRU) (attlay_drawlist)")
   (action_tile "Disc_TELE" "(setq attlay_typedata attlay_TELE) (attlay_drawlist)")
   (action_tile "Draw_Type" "(setq attlay_go_type (car (nth (atoi $value) attlay_typedata)))")
   (action_tile "Reset_Color" "(setq attlay_go_clr (not attlay_go_clr))")
   (action_tile "Reset_LT" "(setq attlay_go_lt (not attlay_go_lt))")
   (action_tile "XREF_Lays" "(setq attlay_go_xref (not attlay_go_xref))")
   (action_tile "ATT_CAD_Tool" "(set_tile \"error\" \"ATTLAY Version: BETA : 08-25-96\")")
   (action_tile "For_All_Lays" "(setq attlay_go_lays \"ALL\") (attlay_toggle_for)")
   (action_tile "For_Tog_Lays" "(setq attlay_go_lays \"TOG\") (attlay_toggle_for)")
   (action_tile "For_NEWW" "(setq attlay_go_neww (not attlay_go_neww))")
   (action_tile "For_EXST" "(setq attlay_go_exst (not attlay_go_exst))")
   (action_tile "For_DEMO" "(setq attlay_go_demo (not attlay_go_demo))")
   (action_tile "For_FUTR" "(setq attlay_go_futr (not attlay_go_futr))")
   (action_tile "For_NilSuffix" "(setq attlay_go_nil (not attlay_go_nil))")
   (action_tile "Do_Create" "(setq attlay_go_do \"CREATE\") (attlay_toggle_do)")
   (action_tile "Do_Reset" "(setq attlay_go_do \"RESET\") (attlay_toggle_do)")
   ;
   
   (action_tile "accept" "(attlay_check)")
   (action_tile "cancel" "(done_dialog)")
   (action_tile "help" "(help \"attlay\")")
   ;
   (setq dialog_result (start_dialog))
   (unload_dialog dcl_id)
   (princ)
   ;
   (cond 
      ((= dialog_result 0)
         (progn
            (princ "\nATTLAY canceled by user.\n")
            (princ)
         )
      )
      ((= dialog_result 1) (attlay_go))
   ) ;cond
   ;
   (princ)
) ;defun attlay
;
;
; ====================================================================================================
(defun attlay_drawlist ()
   ; Loads the list 
   (start_list "Draw_Type")
   (foreach elem attlay_typedata (add_list (cadr elem)))
   (end_list)
) ;attlay_drawlist
;
;
; ====================================================================================================
(defun attlay_toggle_do ()
   (if (= attlay_go_do "CREATE")
      (progn
         (mode_tile "Reset_Color" 1)
         (mode_tile "Reset_LT" 1)
         (mode_tile "XREF_Lays" 1)
         (mode_tile "Disc_ARCH" 0)
         (mode_tile "Disc_BSYS" 0)
         (mode_tile "Disc_CIVL" 0)
         (mode_tile "Disc_CTRL" 0)
         (mode_tile "Disc_DVLP" 0)
         (mode_tile "Disc_ELEC" 0)
         (mode_tile "Disc_FIRE" 0)
         (mode_tile "Disc_MECH" 0)
         (mode_tile "Disc_PLUM" 0)
         (mode_tile "Disc_STRU" 0)
         (mode_tile "Disc_TELE" 0)
         (mode_tile "Draw_Type" 0)
      )
   )
   (if (= attlay_go_do "RESET")
      (progn
         (mode_tile "Reset_Color" 0)
         (mode_tile "Reset_LT" 0)
         (mode_tile "XREF_Lays" 0)
         (mode_tile "Disc_ARCH" 1)
         (mode_tile "Disc_BSYS" 1)
         (mode_tile "Disc_CIVL" 1)
         (mode_tile "Disc_CTRL" 1)
         (mode_tile "Disc_DVLP" 1)
         (mode_tile "Disc_ELEC" 1)
         (mode_tile "Disc_FIRE" 1)
         (mode_tile "Disc_MECH" 1)
         (mode_tile "Disc_PLUM" 1)
         (mode_tile "Disc_STRU" 1)
         (mode_tile "Disc_TELE" 1)
         (mode_tile "Draw_Type" 1)
      )
   )
   
) ;attlay_toggle_do
;
;
; ====================================================================================================
(defun attlay_toggle_for ()
   (if (= attlay_go_lays "ALL")
      (progn
         (mode_tile "For_NEWW" 1)
         (mode_tile "For_EXST" 1)
         (mode_tile "For_DEMO" 1)
         (mode_tile "For_FUTR" 1)
         (mode_tile "For_NilSuffix" 1)
      )
   )
   (if (= attlay_go_lays "TOG")
      (progn
         (mode_tile "For_NEWW" 0)
         (mode_tile "For_EXST" 0)
         (mode_tile "For_DEMO" 0)
         (mode_tile "For_FUTR" 0)
         (mode_tile "For_NilSuffix" 0)
      )
   )
) ;attlay_toggle_for
;
;
; ====================================================================================================
(defun attlay_check (/ ok_check)
   
   (setq ok_check "OK")
   (if (= attlay_go_do "CREATE")
      (progn
         (if (and (= attlay_go_lays "TOG") (not (or attlay_go_neww attlay_go_exst attlay_go_demo attlay_go_futr  attlay_go_nil )))
            (bad_check "Error--No layers to Create!")
         )
         (if (not attlay_go_type)
            (bad_check "Error--Drawing Type not selected!")
         )
         (if (not attlay_typedata)
            (bad_check "Error--Discipline Type not selected")         
         )
      )
   );if
   
   (if (= attlay_go_do "RESET")
      (progn
         (if (and (= attlay_go_lays "TOG") (not (or attlay_go_neww attlay_go_exst attlay_go_demo attlay_go_futr  attlay_go_nil )))
            (bad_check "Error--No layers to Reset!")
         )
         (if (not (or attlay_go_clr attlay_go_lt))         
            (bad_check "Error--Nothing to Reset!")
         )
      )
   );if
   
   (if (= ok_check "OK")
      (done_dialog 1)
   )
) ;attlay_check
;
;
; ====================================================================================================
(defun attlay_go (/ tmp)
   (princ "\n\nWorking...")
   
   
   ; What layers to create or reset depends on the five toggle values
   ; if all force the toggels to all be on, else just use thier values.
   ;
   (if (= attlay_go_lays "ALL")
      (progn
         (setq attlay_go_neww T)
         (setq attlay_go_exst T)
         (setq attlay_go_demo T)
         (setq attlay_go_futr T)
         (setq attlay_go_nil nil)
      )
   );if
   
   
   ; --------------------------------------------------
   ; "CREATE" Data File Scanner and Layer Creation Code
   ; --------------------------------------------------
   (if (= attlay_go_do "CREATE")
      (progn
         (setq dat_file (open (findfile  "attlay.dat") "r"))
         (setq dat_line (read-line dat_file))
         (setq Short_Circuit_Primed nil)
         (setq Short_Circuit nil)

         (while (and dat_line (not Short_Circuit))
            (if (/= (substr dat_line 1 1) ";")
               (progn
                  (princ ".")
                  
                  (setq data_type (no_spaces (strcase (substr dat_line 1 2))))

                  (if (= attlay_go_type data_type)
                     (progn

                        (princ "!")
                        (setq Short_Circuit_Primed T)
                        (setq basename (no_spaces (strcase (substr dat_line 4 11))))
                        
                        (if attlay_go_neww 
                           (progn                 

                              (setq ltype (no_spaces (strcase (substr dat_line 16 15))))
                              (setq colornum (no_spaces (strcase (substr dat_line 32 3))))
                              (if (/= ltype "") (do_create (strcat basename "-NEWW") ltype colornum))
                           )
                        );if NEWW
                        
                        (if attlay_go_exst
                           (progn                 
                              (setq ltype (no_spaces (strcase (substr dat_line 36 15))))
                              (setq colornum (no_spaces (strcase (substr dat_line 52 3))))
                              (if (/= ltype "") (do_create (strcat basename "-EXST") ltype colornum))
                           )
                        );if EXST
                        
                        (if attlay_go_demo
                           (progn                 
                              (setq ltype (no_spaces (strcase (substr dat_line 56 15))))
                              (setq colornum (no_spaces (strcase (substr dat_line 72 3))))
                              (if (/= ltype "") (do_create (strcat basename "-DEMO") ltype colornum))
                           )
                        );if DEMO
                        
                        (if attlay_go_futr 
                           (progn                 
                              (setq ltype (no_spaces (strcase (substr dat_line 76 15))))
                              (setq colornum (no_spaces (strcase (substr dat_line 92 3))))
                              (if (/= ltype "") (do_create (strcat basename "-FUTR") ltype colornum))
                           )
                        );if FUTR
                        
                        (if attlay_go_nil
                           (progn                 
                              (setq ltype (no_spaces (strcase (substr dat_line 16 15))))
                              (setq colornum (no_spaces (strcase (substr dat_line 32 3))))
                              (if (/= ltype "") (do_create basename ltype colornum))
                           )
                        );if NilSuffix
                        
                     );progn "then match found"
                     
                     ;"else no match was found"
                     ;if we were processing found matches and the data file 
                     ;is assumed to be sorted by data type "XX" we can skip 
                     ;or "short circuit" the remaing data in the file.
                     ;
                     (if Short_Circuit_Primed
                        (setq Short_Circuit T)
                     );if
                     
                  );if go_type = data_type
                  
                  
               ) ;progn "not a comment"
            ) ;if "is it a comment line?"
            
            (setq dat_line (read-line dat_file))
            
         ) ;while
         (close dat_file)
         
      );progn
   );if CREATE
   
   
   
   ; --------------------------------------------------
   ; "RESET" Data File Scanner and Layer Creation Code
   ; --------------------------------------------------
   (if (= attlay_go_do "RESET")
      (progn
         
         ;
         ;SCAN LAYER TABLE to build Basename_List and Layer_List
         ;
         (setq basename_list nil)
         (setq layer_list nil)
         
         (setq layerdata (tblnext "LAYER" T))
         (while layerdata
            
            (setq layername_long (cdr (assoc 2 layerdata)))
            
            
            (if (>= (strlen layername_long) 6)
               (progn
                  
                  (setq trailing5 (substr layername_long (- (strlen layername_long) 4) 5))
                  (if (or (= trailing5 "-NEWW") (= trailing5 "-EXST") (= trailing5 "-DEMO") (= trailing5 "-FUTR") )
                     (setq layername (substr layername_long 1 (- (strlen layername_long) 5)))
                     (setq layername layername_long)
                  );if
                  
                  ;if there is a "|" in the layer name then it is xrefed.
                  (if (wcmatch layername "*`|*")
                     (progn
                        ;Parse layer name to determin the length of the xref filename.
                        (setq counter 1)
                        (setq char (substr layername counter 1))
                        (while (and (/= char "|") (<= counter (strlen layername)))
                           (setq counter (1+ counter))
                           (setq char (substr layername counter 1))
                        )
                        (setq basename (substr layername (1+ counter) (- (strlen layername) counter))) 
                        (setq XREF_layer T)
                     );progn
                     (progn
                        (setq basename layername)
                        (setq XREF_layer nil)
                     );progn
                  );if xref
                  
                  
                  
                  (if (or attlay_go_xref (and (not attlay_go_xref) (not xref_layer)))
                     (progn
                        (setq temp (member basename basename_list))
                        (if temp
                           (progn
                              (setq oldll (nth (- (length basename_list) (length temp)) layer_list))
                              (if (not (member layername oldll))
                                 (progn
                                    (setq newll (append oldll (list layername)))
                                    (subst newll oldll layer_list)
                                 );progn
                              );if
                              
                           );progn
                           (progn
                              (setq basename_list (append basename_list (list basename)))
                              (setq layer_list (append layer_list (list (list basename layername))))
                           );progn
                        );if
                     );progn
                  );if xref
                  
               );progn 
            );if {length test}
            
            
            (setq layerdata (tblnext "LAYER"))
            
         );while still layer table data
         
         
         
         ;
         ; SCAN DATA FILE and do the resets
         ;
         (setq dat_file (open (findfile  "attlay.dat") "r"))
         (setq dat_line (read-line dat_file))
         
         (while dat_line
            (if (/= (substr dat_line 1 1) ";")
               (progn
                  (princ ".")
                  
                  (setq basename (no_spaces (strcase (substr dat_line 4 11))))
                  (setq MatchFound (member basename basename_list))                  
                  
                  (if MatchFound
                     (progn
                        (princ "!")
                        (setq neww_ltype (no_spaces (strcase (substr dat_line 16 15))))
                        (setq neww_colornum (no_spaces (strcase (substr dat_line 32 3))))
                        (setq exst_ltype (no_spaces (strcase (substr dat_line 36 15))))
                        (setq exst_colornum (no_spaces (strcase (substr dat_line 52 3))))
                        (setq demo_ltype (no_spaces (strcase (substr dat_line 56 15))))
                        (setq demo_colornum (no_spaces (strcase (substr dat_line 72 3))))
                        (setq futr_ltype (no_spaces (strcase (substr dat_line 76 15))))
                        (setq futr_colornum (no_spaces (strcase (substr dat_line 92 3))))
                        
                        (setq index (- (length basename_list) (length MatchFound)))
                        (setq Reset_List (cdr (nth index Layer_List)))
                        
                        (foreach layername Reset_list
                           (progn
                              (if attlay_go_neww 
                                 (if (/= neww_ltype "") (do_reset (strcat layername "-NEWW") neww_ltype neww_colornum))
                              );if NEWW
                              
                              (if attlay_go_exst
                                 (if (/= exst_ltype "") (do_reset (strcat layername "-EXST") exst_ltype exst_colornum))
                              );if EXST
                              
                              (if attlay_go_demo
                                 (if (/= demo_ltype "") (do_reset (strcat layername "-DEMO") demo_ltype demo_colornum))
                              );if DEMO
                              
                              (if attlay_go_futr 
                                 (if (/= futr_ltype "") (do_reset (strcat layername "-FUTR") futr_ltype futr_colornum))
                              );if FUTR
                              
                              (if attlay_go_nil
                                 (if (/= neww_ltype "") (do_reset layername neww_ltype neww_colornum))
                              );if NilSuffix
                           );progn
                        );foreach
                        
                        
                     );progn "then match found"
                  );if matchfound
               ) ;progn "not a comment"
            ) ;if "is it a comment line?"
            
            (setq dat_line (read-line dat_file))
            
         ) ;while
         (close dat_file)
         
         
      );progn
   );if RESET


) ;attlay_go



;
; ====================================================================================================
(defun do_create (n lt cn)
   
   (command "_.LAYER" "Make" n "")

   (command "_.LAYER" "Color" cn n "")   
   
   (if (not (tblsearch "LTYPE" lt))
      (progn
         (command "_.LINETYPE" "Load" lt "att_acad.lin" "")
         (if (not (tblsearch "LTYPE" lt))
            (alert (strcat "Linetype " lt " did not load for layer " n "!"))
            (command "_.LAYER" "Ltype" lt n "")
         ) ;if
      );progn
      (command "_.LAYER" "Ltype" lt n "")
   );if
   
);do_create
;
;
;
; ====================================================================================================
(defun do_reset (n lt cn)
   
   (if (and (= attlay_go_do "RESET") (tblsearch "LAYER" n))
      (progn
         
         
         (if attlay_go_clr
            (command "_.LAYER" "Color" cn n "")   
         );if CLR
         
         
         (if attlay_go_lt   
            (progn
               (if (not (tblsearch "LTYPE" lt))
                  (progn
                     (command "_.LINETYPE" "Load" lt "att_acad.lin" "")
                     (if (not (tblsearch "LTYPE" lt))
                        (alert (strcat "Linetype " lt " did not load for layer " n "!"))
                        (command "_.LAYER" "Ltype" lt n "")
                     ) ;if
                  );progn
                  (command "_.LAYER" "Ltype" lt n "")
               );if
            );progn
         );if LT
         
         
      );progn
   );if
   
   
   
);do_create
;
;
; ====================================================================================================
;(princ "Function Name(s) defined: ATTLAY ")
(princ)
; ====================================================================================================



