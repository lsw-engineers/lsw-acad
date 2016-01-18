(defun c:weld (/ OLDERROR ORTH LYR LR1 SCL P1 P2 A1 P3 TP1 TP2 TXTSTY)
    ;
    ;---------------------------------
    ;   Set Environment Information
    ;---------------------------------
    ;
    (setvar "cmdecho" 0)
    (setq olderror *error* *error* welderror)
    (setq ORTH (getvar "orthomode"))
    (setvar "orthomode" 0)
    (setq SCL (getvar "dimscale"))
    (setq LYR (getvar "clayer"))
    (setq MRR (getvar "mirrtext"))
    (setq MIRSET (ssadd))
    (setvar "mirrtext" 0)
;--------* Check for drawing scale factor #dsc, userr1, function
;          resides in Acad.lsp file used by Stone & Webster
(_chkdsc)
(setq scl (getvar "userr1"))
;--------* Check and set annotation layer, function resides in
;          Acad.lsp file used by SWEC
(_asty)
    (setq TXTSTY (getvar "textstyle"))
;--------*  Select Points and Symbol Information
 (prompt "\nSelect weld joint <Return to exit>")
  (while (setq P1 (getpoint))
         (setq P2 (getpoint P1 "\nNext point"))
         (wset)
         (if (= WELDTYP nil)
             (exit)
         )
         (if (or (<= (angle P1 P2) (dtr 90)) (>= (angle P1 P2) (dtr 270)))
             (setq A1 0)
             (setq A1 (dtr 180))
         )
         (setq P3 (polar P2 A1 (* #dsc 0.6)))
;--------*   Sets text placement points
         (if (= 0 A1)
             (progn
                  (setq TP1 (polar P2 A1 (* SCL 0.25)))
                  (setq TP2 (polar P2 A1 (* SCL 0.95)))
              )
              (progn
                   (setq TP1 (polar P2 A1 (* SCL 0.95)))
                   (setq TP2 (polar P2 A1 (* SCL 0.25)))
              )
         )
         (setq TP1 (polar TP1 (dtr 90) (* SCL 0.15)))
         (setq TP2 (polar TP2 (dtr 90) (* SCL 0.15)))
         (command "pline" P1 "w" 0 (* #dsc (/ 1.0 16.0)) (polar P1 (angle P1 P2) (* #dsc (/ 3.0 16.0))) "w" 0 0 P2 (polar P2 A1 (* SCL 1.20)) "")
;--------*  "All Around symbol" information if required
         (if (= ALLR "1")
             (command "circle" P2 (* SCL 0.09))
         )
;--------*  Invokes program to draw selected weld type
         (if (= WELDTYP2 "(baweld)")
             (eval (read WELDTYP2))
         )
         (eval (read WELDTYP))
         (if (= (cdr (assoc 40 (tblsearch "style" TXTSTY))) 0)
             (progn
                  (command "text" "j" "mc" TP1 (* SCL 0.094) 0 WELDSIZE)
                  (mir_set)
                  (command "text" "j" "mc" TP2 (* SCL 0.094) 0 PITCH)
                  (mir_set)
             )
             (progn
                  (command "text" "j" "mc" TP1 0 WELDSIZE)
                  (mir_set)
                  (command "text" "j" "mc" TP2 0 PITCH)
                  (mir_set)
             )
         )
         (if (= WELDSIDE "This")
             (setq QUES "Y")
             (setq QUES "N")
         )
         (if (or (= WELDSIDE "This") (= WELDSIDE "Both"))
             (cond
                 ((/= WELDTYP2 "(baweld)")(command "mirror" MIRSET "" P2 (polar P2 A1 0.5) QUES))
                 ((= WELDTYP2 "(baweld)")(command "mirror" MIRSET "r" RBAK "" P2 (polar P2 A1 0.5) QUES))
             )
         )
;--------*  "Field symbol" information if required
         (if (= FIELDW "1")
             (flag)
         )
         (notes2)
;--------* Loop to restart weld symbol placement
         (setq MIRSET (ssadd))
         (prompt "\nSelect weld joint <Return to exit>")
     )
;--------* Exit Procedure
    (setvar "orthomode" ORTH)
    (setvar "mirrtext" MRR)
    (command "layer" "set" LYR "")
    (setq *error* OLDERROR)
    (princ)    
)
;
;--------------------------------------------
;   Procedure to set defaults
;--------------------------------------------
;
;
;--------------------------------------------
;   Error handler
;--------------------------------------------
;
(defun welderror (msg)
    (setvar "orthomode" ORTH)
    (setvar "mirrtext" MRR)
    (command "layer" "set" LYR "")
    (setq *error* OLDERROR)
    (setq msg1 msg)
    (if (= msg "quit / exit abort")
        (prompt "\nNo Weld Type was Selected\n")
    )
    (princ)
)
;

;***********************************
;   Square Weld Symbol
;***********************************

(defun sqweld ()
    (command "pline" (polar P3 0 (* SCL 0.06)) (polar (getvar "lastpoint") (dtr 90) (* SCL 0.25)) "")
    (mir_set)
    (command "pline" (polar P3 (dtr 180) (* SCL 0.06)) (polar (getvar "lastpoint") (dtr 90) (* SCL 0.25)) "")
    (mir_set)
)
;
;*********************************
;   "U" Weld Symbol
;*********************************
;
(defun uweld (/ UPT)
    (setq UPT (polar P3 (dtr 90) (* SCL 0.13)))
    (command "line" P3 UPT "")
    (mir_set)
    (command "arc" (polar UPT (dtr 45) (* SCL 0.16970563)) UPT (polar UPT (dtr 135) (* SCL 0.16970563)))
    (mir_set)
)
;
;*********************************
;   "J" Weld Symbol OTHER Side
;*********************************
;
(defun jweld (/ JPT) 
    (setq JPT (polar P3 (dtr 90) (* SCL 0.25)))
    (command "line" P3 JPT "")
    (mir_set)
    (command "arc" (polar P3 (dtr 90) (* SCL 0.13)) "c" JPT (polar JPT 0 (* SCL 0.12)))
    (mir_set)
)
;
;*********************************
;   "V" Weld Symbol
;*********************************
;
(defun vweld ()
    (command "line" P3 (polar P3 (dtr 45) (* SCL 0.28284271)) "")
    (mir_set)
    (command "line" P3 (polar P3 (dtr 135) (* SCL 0.28284271)) "")
    (mir_set)
)
;
;*********************************
;   Bevel Weld Symbol
;*********************************
;
(defun bweld ()
    (command "line" P3 (polar P3 (dtr 45) (* SCL 0.28284271)) "")
    (mir_set)
    (command "line" P3 (polar P3 (dtr 90) (* SCL 0.20)) "")
    (mir_set)
)
;
;************************************
;   Flare-V Weld Symbol
;************************************
;
(defun flvweld ()
    (command "arc" "c" (polar P3 0 (* SCL 0.18)) (polar (getvar "lastpoint") (dtr 90) (* SCL 0.12)) (polar P3 0 (* SCL 0.06)))
    (mir_set)
    (command "arc" (polar P3 (dtr 180) (* SCL 0.06)) "c" (polar P3 (dtr 180) (* SCL 0.18)) (polar (getvar "lastpoint") (dtr 90) (* SCL 0.12)))
    (mir_set)
)
;
;****************************************
;   Flare Bevel Weld Symbol
;****************************************
;
(defun flbweld ()
    (command "arc" "c" (polar P3 0 (* SCL 0.18)) (polar (getvar "lastpoint") (dtr 90) (* SCL 0.12)) (polar P3 0 (* SCL 0.06)))
    (mir_set)
    (command "pline" (polar p3 (dtr 180) (* SCL 0.06)) (polar (getvar "lastpoint") (dtr 90) (* SCL 0.12)) "")
    (mir_set)
)
;
;************************************
;   Fillet Weld Symbol
;************************************
;
(defun filweld ()
    (command "pline" (polar P3 (dtr 180) (* SCL 0.10)) (polar (getvar "lastpoint") (dtr 90) (* SCL 0.20)) (polar P3 0 (* SCL 0.10)) "")
    (mir_set)
)
;
;*************************
;   Surface Weld Symbol
;*************************
;
(defun surweld ()
    (command "arc" (polar P3 0 (* SCL 0.12)) "C" (polar P3 0 (* SCL 0.06)) P3)
    (mir_set)
    (command "arc" P3 "c" (polar P3 (dtr 180) (* SCL 0.06)) (polar P3 (dtr 180) (* SCL 0.12)))
    (mir_set)
)
;
;***************************************
;   Flange Edge Weld Symbol
;***************************************
;
(defun fleweld ()
    (command "arc" (polar P3 (dtr 180) (* SCL 0.18)) "c" (polar (getvar "lastpoint") (dtr 90) (* SCL 0.12)) (polar (getvar "lastpoint") 0 (* SCL 0.12)))
    (mir_set)
    (command "pline" (getvar "lastpoint") (polar (getvar "lastpoint") (dtr 90) (* SCL 0.13)) "")
    (mir_set)
    (command "pline" (polar (getvar "lastpoint") 0 (* SCL 0.12)) (polar (getvar "lastpoint") (dtr 270) (* SCL 0.13)) "")
    (mir_set)
    (command "arc" (getvar "lastpoint") "c" (polar (getvar "lastpoint") 0 (* SCL 0.12)) (polar P3 0 (* SCL 0.18)))
    (mir_set)
)
;
;***************************************
;   Flange Corner Weld Symbol
;***************************************
;
(defun flcweld ()
    (command "pline" (polar P3 (dtr 180) (* SCL 0.06)) (polar (getvar "lastpoint") (dtr 90) (* SCL 0.25)) "")
    (mir_set)
    (command "pline" (polar (getvar "lastpoint") 0 (* SCL 0.12)) (polar (getvar "lastpoint") (dtr 270) (* SCL 0.13)) "")
    (mir_set)
    (command "arc" (getvar "lastpoint") "c" (polar (getvar "lastpoint") 0 (* SCL 0.12)) (polar P3 0 (* SCL 0.18)))
    (mir_set)
)
;
;********************************
;   Plug Weld Symbol
;********************************
;
(defun plweld ()
    (command "pline" (polar P3 (dtr 180) (* SCL 0.25)) (polar (getvar "lastpoint") (dtr 90) (* SCL 0.25)) (polar (getvar "lastpoint") 0 (* SCL 0.50)) (polar P3 0 (* SCL 0.25)) "")
    (mir_set)
)
;
;*************************************
;   Backing  Weld Symbol
;*************************************
;
(defun baweld ()
    (if (= WELDSIDE "This")
        (command "arc" "c" P3 (polar P3 0 (* SCL 0.06)) (polar p3 (dtr 180) (* SCL 0.06)))
        (command "arc" "c" P3 (polar P3 (dtr 180) (* SCL 0.06)) (polar P3 0 (* SCL 0.06)))
    )
    (setq RBAK (entlast))
)
;
;********************************************
;   "Field symbol" information if required
;********************************************
;
(defun flag (/ FPT)
    (setq fpt (polar P2 (dtr 90) (* SCL 0.315)))
    (setq fpt (polar FPT A1 (* SCL 0.20)))
    (command "line" P2 (polar P2 (dtr 90) (* SCL 0.375)) "")
    (command "solid" (getvar "lastpoint") FPT (polar P2 (dtr 90) (* SCL 0.255)) "")
    (command)
)
;
;********************************************
;   Converts degrees to radians
;********************************************
;
(defun dtr (ANG)(* PI (/ ANG 180.0)))
;
;
;
;
;
;*************************************
;   Places Notes and Note Tail
;*************************************
;
;
(defun notes2 ()    
    (setq TXTPT (polar P2 A1 (* SCL 1.30)))
    (setq TAILPT (polar P2 A1 (* SCL 1.20)))
    (setq PASS 1)
    (setq RPT 0)
    (repeat 3
        (if (/= (eval (read (strcat "note" (itoa PASS)))) "")
            (setq RPT (1+ RPT))
        )
        (setq PASS (1+ PASS))
    )        
    (if (/= RPT 0)                                             
        (progn
            (cond
                ((= RPT 1)(setq TXTPT1 TXTPT)(cond ((/= NOTE1 "")(setq NTE1 NOTE1))
                        ((/= NOTE2 "")(setq NTE1 NOTE2))
                        ((/= NOTE3 "")(setq NTE1 NOTE3))
                    )
                )                                   
                ((= RPT 2)(setq TXTPT (polar TXTPT A1 (* SCL 0.0625)))(setq TXTPT1 (polar TXTPT (dtr 90) (* SCL 0.0781)))(cond ((= NOTE1 "")(setq NTE1 NOTE2 NTE2 NOTE3))
                        ((= NOTE2 "")(setq NTE1 NOTE1 NTE2 NOTE3))
                        ((= NOTE3 "")(setq NTE1 NOTE1 NTE2 NOTE2))
                    )
                )                                                                                                               
                ((= RPT 3)(setq TXTPT (polar TXTPT A1 (* SCL 0.125)))(setq TXTPT1 (polar TXTPT (dtr 90) (* SCL 0.15625)))(setq NTE1 NOTE1 NTE2 NOTE2 NTE3 NOTE3))         
            )
            (setq PASS 1)
            (if (= A1 0)
                (progn
                    (repeat RPT
                        (if (= (cdr (assoc 40 (tblsearch "style" TXTSTY))) 0)
                            (command "text" "j" "ml" TXTPT1 (* SCL 0.094) 0 (eval (read (strcat "NTE" (itoa PASS)))))
                            (command "text" "j" "ml" TXTPT1 0 (eval (read (strcat "NTE" (itoa PASS)))))
                        )
                        (setq PASS (1+ PASS))
                        (setq TXTPT1 (polar TXTPT1 (dtr 270) (* SCL 0.15625)))
                    )
                )
                (progn
                    (repeat RPT
                        (if (= (cdr (assoc 40 (tblsearch "style" TXTSTY))) 0)
                            (command "text" "j" "mr" TXTPT1 (* SCL 0.094) 0 (eval (read (strcat "NTE" (itoa PASS)))))
                            (command "text" "j" "mr" TXTPT1 0 (eval (read (strcat "NTE" (itoa PASS)))))
                        )
                        (setq PASS (1+ PASS))
                        (setq TXTPT1 (polar TXTPT1 (dtr 270) (* SCL 0.15625)))
                    )    
                )    
            )
            (if (= A1 0)
                (command ".line" TAILPT (polar TAILPT (dtr 45) (* SCL 0.2828)) "" "line" TAILPT (polar TAILPT (dtr -45) (* SCL 0.2828)) "")
                (command ".line" TAILPT (polar TAILPT (dtr 135) (* SCL 0.2828)) "" "line" TAILPT (polar TAILPT (dtr -135) (* SCL 0.2828)) "")
            )
        )
    )
)
;
;
;*********************************************
;   Sets up and Calls Setup Dialog Box
;*********************************************
;
;
;
(defun wset ()
    (setq dcl_id (load_dialog "aweld.DCL"))
    (if (not (new_dialog "aweld" dcl_id))
        (exit)
    )
    (setq x (dimx_tile "sq_weld")
        y (dimy_tile "sq_weld")
    )      
    (start_image "sq_weld")
    (slide_image 0 0 x y "welds(sqweld)")
    (end_image)
    (setq x (dimx_tile "u_weld")
        y (dimy_tile "u_weld")
    )      
    (start_image "u_weld")
    (slide_image 0 0 x y "welds(uweld)")
    (end_image)                                            
    (setq x (dimx_tile "j_weld")                           
        y (dimy_tile "j_weld")                             
    )                                                      
    (start_image "j_weld")                                 
    (slide_image 0 0 x y "welds(jweld)")       
    (end_image)                                            
    (setq x (dimx_tile "v_weld")                           
        y (dimy_tile "v_weld")                             
    )                                                      
    (start_image "v_weld")                                 
    (slide_image 0 0 x y "welds(vweld)")        
    (end_image)
    (setq x (dimx_tile "b_weld")
        y (dimy_tile "b_weld")
    )      
    (start_image "b_weld")
    (slide_image 0 0 x y "welds(bweld)")
    (end_image)
    (setq x (dimx_tile "flv_weld")
        y (dimy_tile "flv_weld")
    )      
    (start_image "flv_weld")
    (slide_image 0 0 x y "welds(flvweld)")
    (end_image)
    (setq x (dimx_tile "flb_weld")
        y (dimy_tile "flb_weld")
    )      
    (start_image "flb_weld")
    (slide_image 0 0 x y "welds(flbweld)")
    (end_image)
    (setq x (dimx_tile "fil_weld")
        y (dimy_tile "fil_weld")
    )      
    (start_image "fil_weld")
    (slide_image 0 0 x y "welds(filweld)")
    (end_image)
    (setq x (dimx_tile "sur_weld")
        y (dimy_tile "sur_weld")
    )      
    (start_image "sur_weld")                                
    (slide_image 0 0 x y "welds(surweld)")    
    (end_image)                                             
    (setq x (dimx_tile "fle_weld")                          
        y (dimy_tile "fle_weld")                            
    )                                                       
    (start_image "fle_weld")                                
    (slide_image 0 0 x y "welds(fleweld)")    
    (end_image)                                             
    (setq x (dimx_tile "flc_weld")                          
        y (dimy_tile "flc_weld")                            
    )                                                       
    (start_image "flc_weld")
    (slide_image 0 0 x y "welds(flcweld)")
    (end_image)
    (setq x (dimx_tile "pl_weld")
        y (dimy_tile "pl_weld")
    )      
    (start_image "pl_weld")
    (slide_image 0 0 x y "welds(plweld)")
    (end_image)
    (aw_deflt)
    (action_tile "ba_weld" "(setq BA_DEF $value)(ba_set)")
    (action_tile "ar_weld" "(setq ALLR $value)")
    (action_tile "fi_weld" "(setq FIELDW $value)")
    (action_tile "sq_weld" "(setq WELDTYP \"(sqweld)\")")
    (action_tile "u_weld" "(setq WELDTYP \"(uweld)\")")
    (action_tile "j_weld" "(setq WELDTYP \"(jweld)\")")
    (action_tile "v_weld" "(setq WELDTYP \"(vweld)\")")
    (action_tile "b_weld" "(setq WELDTYP \"(bweld)\")")
    (action_tile "flv_weld" "(setq WELDTYP \"(flvweld)\")")
    (action_tile "flb_weld" "(setq WELDTYP \"(flbweld)\")")
    (action_tile "fil_weld" "(setq WELDTYP \"(filweld)\")")
    (action_tile "sur_weld" "(setq WELDTYP \"(surweld)\")")
    (action_tile "fle_weld" "(setq WELDTYP \"(fleweld)\")")
    (action_tile "flc_weld" "(setq WELDTYP \"(flcweld)\")")
    (action_tile "pl_weld" "(setq WELDTYP \"(plweld)\")")
    (action_tile "this_side" "(setq WELDSIDE \"This\")")
    (action_tile "other_side" "(setq WELDSIDE \"Other\")")
    (action_tile "both_side" "(setq WELDSIDE \"Both\")")
    (action_tile "fill_size" "(setq WELDSIZE $value)")
    (action_tile "pitch_size" "(setq PITCH $value)")
    (action_tile "note1" "(setq NOTE1 $value)")
    (action_tile "note2" "(setq NOTE2 $value)")
    (action_tile "note3" "(setq NOTE3 $value)")
    (action_tile "accept" "(done_dialog)")
    (action_tile "cancel" "(exit)")
    (start_dialog)
    (unload_dialog dcl_id)
)    
;
;
;***********************************************
;   Determines if Backing Weld is to be Used
;***********************************************
;
;
(defun ba_set ()
    (mode_tile "both_side" (atoi $value))
    (if (and (= "1" $value)(= (get_tile "both_side") "1"))
        (progn
            (set_tile "this_side" "1")
            (setq WELDSIDE "This")
        )
    )
    (if (= "1" $value) 
        (setq WELDTYP2 "(baweld)")
        (setq WELDTYP2 nil)
    )
)
;
;
;***********************************************
;   Sets the Defaults in the Setup Dialog Box
;***********************************************
;
;
(defun aw_deflt ()
    (if (cond ((= WELDTYP "(sqweld)")(setq wt_mode "sq_weld"))
            ((= WELDTYP "(uweld)")(setq WT_MODE "u_weld"))
            ((= WELDTYP "(jweld)")(setq WT_MODE "j_weld"))
            ((= WELDTYP "(vweld)")(setq WT_MODE "v_weld"))
            ((= WELDTYP "(bweld)")(setq WT_MODE "b_weld"))
            ((= WELDTYP "(flvweld)")(setq WT_MODE "flv_weld"))
            ((= WELDTYP "(flbweld)")(setq WT_MODE "flb_weld"))
            ((= WELDTYP "(filweld)")(setq WT_MODE "fil_weld"))
            ((= WELDTYP "(surweld)")(setq WT_MODE "sur_weld"))
            ((= WELDTYP "(fleweld)")(setq WT_MODE "fle_weld"))
            ((= WELDTYP "(flcweld)")(setq WT_MODE "flc_weld"))
            ((= WELDTYP "(plweld)")(setq WT_MODE "pl_weld"))
        )
        (mode_tile WT_MODE 2)
    )
    (if (cond ((= WELDSIDE "This")(setq WS_MODE "this_side"))
            ((= WELDSIDE "Other")(setq WS_MODE "other_side"))
            ((= WELDSIDE "Both")(setq WS_MODE "both_side"))
        )
        (set_tile ws_mode "1")
    )    
    (if (= WELDSIZE nil)
        (setq WELDSIZE "")
    )
    (set_tile "fill_size" WELDSIZE)
    (if (= PITCH nil)
        (setq PITCH "")
    )
    (set_tile "pitch_size" PITCH)
    (if (= NOTE1 nil)
        (setq NOTE1 "")
    )
    (set_tile "note1" NOTE1)
    (if (= NOTE2 nil)
        (setq NOTE2 "")
    )
    (set_tile "note2" NOTE2)
    (if (= NOTE3 nil)
        (setq NOTE3 "")
    )
    (set_tile "note3" NOTE3)
    (if (= BA_DEF "1")
        (progn
            (set_tile "ba_weld" "1")
            (mode_tile "both_side" 1)
        )
    )
    (if (= ALLR "1")
        (set_tile "ar_weld" "1")
    )        
    (if (= fieldw "1")
        (set_tile "fi_weld" "1")
    )
)
;
;
;**********************************************
;   Creates the Selection Set to be Mirrored
;**********************************************
;
;
(defun mir_set ()
    (ssadd (entlast) MIRSET)
)
