;ddcplg
;
;--------* Stone & Webster Engineering Corp
;--------* CAD/CAE
;--------* mechanical Application
;--------* 031196                       jdc
;
;
;--------* this program defines the code required for a dialog
;--------* interface for the creation of couplings
;
;
;
;--------*         image tile highlight function
;
;;;   Copyright (C) 1993 by Autodesk, Inc.----Copied From ddsolprm.lsp
(defun hi_lite (tile)
     (if (/= last_pic tile)
       (progn
         (mode_tile tile 4)
         (if (/= nil last_pic)
            (mode_tile last_pic 4)
         )
       )
     )
    (setq last_pic tile)
)
;
;----------* couplings
;
(defun c:ddcplg (/ sz_ls drv con com rep_1 ctls ls_t1 sz_ls2 max_x max_y
                   ls_t2 id me_ft dr_it tile last_pic f_sz2 me_om me_ft
                   args)
;
;--------* check and/or load data collection functions and
;--------* error control
;
(if (not initerr)
    (load (findfile "errcntl.lsp"))
)
(if (not md_lst)
    (load (findfile "md_list.lsp"))
)
(if (and (not mf_gtee)(not fil_tee))
    (load (findfile "mf_gsz.lsp"))
)
;--------* Error functions
;
(initerr)
;
;--------* create initial args list
;
(setq args '(sz_ls sz_ls2 rep_1 ctls ls_t1 max_x max_y h_osm h_plw h_ucf
             ls_t2 id me_ft dr_it last_pic f_sz2 sw_ft args))
;
;--------* take control of certain variables just in case
;
(setq h_osm (getvar "osmode")
      h_plw (getvar "plinewid")
      h_ucf (getvar "ucsfollow")
)
(setvar "cmdecho" 0)
(setvar "plinewid" 0.0)
(setvar "ucsfollow" 0)
(setvar "osmode" 0)
(adderr '(setvar "osmode" h_osm))
(adderr '(setvar "plinewid" h_plw))
(adderr '(setvar "ucsfollow" h_ucf))
;
;--------* Load dialog
;
 (setq id (load_dialog "att_fit.dcl"))
 (if (not (new_dialog "ddcplg" id))(exit))
;
;--------* define image tiles
;
;
;--------* display slide images
;
 (start_image "dcp")
 (setq max_x (dimx_tile "dcp")
       max_y (dimy_tile "dcp")
 )
 (slide_image 0 0 max_x max_y "m_lib(cpl_01)")
 (end_image)
 (start_image "scp")
 (slide_image 0 0 max_x max_y "m_lib(cpl_02)")
 (end_image)
 (start_image "wcp")
 (slide_image 0 0 max_x max_y "m_lib(cpl_03)")
 (end_image)
;
;--------* Add action to tiles
;
 (action_tile "dcp" "(hi_lite \"dcp\")(setq att_fv $key)(setq sw_ft \"dif_cpl\")(md_list \"dif_cpl\")")
 (action_tile "scp" "(hi_lite \"scp\")(setq att_fv $key)(setq sw_ft \"scf_cpl\")(md_list \"scf_cpl\")")
 (action_tile "wcp" "(hi_lite \"wcp\")(setq att_fv $key)(setq sw_ft \"wdf_cpl\")(md_list \"wdf_cpl\")")
 (action_tile "f_sz" "(setq f_sz2 $value sz_1 (nth (atoi f_sz2) szls1))
                       (mf_csz sz_1 sw_ft)(fil_cpl)")
 (action_tile "r1" "(setq r1 (atof $value))")                      
 (action_tile "pd1" "(setq pd (atof $value))")
 (action_tile "cd1" "(setq cd (atof $value))")
 (action_tile "cw1" "(setq cw (atof $value))")
 (action_tile "accept" "(chk_sec)")
 (action_tile "cancel" "(done_dialog 0)")
;
;--------* enable the dialog
;
 (setq dr_it (start_dialog))
 (unload_dialog id)
;
;--------*clean up if cancel or draw it if accept
;
 (cond
      ((= dr_it 0)(reset nil))
      ((= dr_it 1)(draw_c))
 )
 (reset nil)
 (princ)
)
(princ)
;
;--------* check for all settings to be selected
;
(defun chk_sec ()
      (if (= sw_ft nil)
          (set_tile "error" "Error-- Fitting type not selected")
          (progn
               (if (= f_sz2 nil)
                   (set_tile "error" "Error--Fitting size not selected")
                   (done_dialog 1)
               )
          )
      )
 )
;
;--------* draw_c decides the coupling to draw
;
 (defun draw_c ()
  (if (not mk_wdn)
      (load (findfile "mk_cplg.lsp"))
  )    
  (cond
       ((= att_fv "dcp")(mk_cplf))
       ((= att_fv "scp")(mk_cpls))
       ((= att_fv "wcp")(mk_wdn))
  )
  (princ)
 )
(princ) 
