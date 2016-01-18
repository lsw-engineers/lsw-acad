;gdtee
;
;--------* Stone & Webster engineering Corp
;--------* CAD/CAE
;--------* Mechanical Application
;--------* 030496                       jdc
;
;--------* this program contains the code for an dialog interface
;--------* for the creation of tee fittings it will then run the
;--------* required function to place the tee definition in the
;--------* autocad database as a block and insert it
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
;----------* Grooved ductile tees and crosses
;
(defun c:ddtee (/ sz_ls drv con com rep_1 ctls ls_t1 sz_ls2 max_x max_y
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
;
;--------* Error functions
;
(initerr)
;
;--------* create initial args list
;
(setq args '(sz_ls sz_ls2 drv con com rep_1 ctls ls_t1 max_x max_y
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
 (if (not (new_dialog "ddtee" id))(exit))
;
;--------* display slide images
;
 (start_image "s_cl")
 (setq max_x (dimx_tile "s_cl")
       max_y (dimy_tile "s_cl")
 )
 (slide_image 0 0 max_x max_y "m_lib(tee_01)")
 (end_image)
 (start_image "d_cl")
 (slide_image 0 0 max_x max_y "m_lib(tee_02)")
 (end_image)
 (start_image "u_cl")
 (slide_image 0 0 max_x max_y "m_lib(tee_03)")
 (end_image)
;
;--------* Add action to tiles
;
 (action_tile "dif_tee" "(setq att_ct $key)(md_lst $key)")
 (action_tile "wdf_tee" "(setq att_ct $key)(md_lst $key)")
 (action_tile "scf_tee" "(setq att_ct $key)(md_lst $key)")
 (action_tile "s_cl" "(hi_lite \"s_cl\")(setq sw_ft $key)")
 (action_tile "d_cl" "(hi_lite \"d_cl\")(setq sw_ft $key)")
 (action_tile "u_cl" "(hi_lite \"u_cl\")(setq sw_ft $key)")
 (action_tile "f_sz" "(setq f_sz2 $value sz_1 (nth (atoi f_sz2) szls1))
                       (mf_gtee sz_1 att_ct)(fil_tee)")
 (action_tile "r_1" "(setq l_1 (atof $value))")
 (action_tile "l_2" "(setq br_1 (atof $value))")
 (action_tile "pd1" "(setq pd_1 (atof $value))")
 (action_tile "pd2" "(setq pd_2 (atof $value))")
 (action_tile "cd1" "(setq cd_1 (atof $value))")
 (action_tile "cw1" "(setq cw_1 (atof $value))")
 (action_tile "cd2" "(setq cd_2 (atof $value))")
 (action_tile "cw2" "(setq cw_2 (atof $value))")
 (action_tile "accept" "(chk_set)")
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
      ((= dr_it 1)(draw_t))
 )
 (reset nil)
 (princ)
)
(princ)
;
;--------* check for all settings to be selected
;
(defun chk_set ()
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
;--------* draw_t decides what type of fitting to draw
;
 (defun draw_t ()
  (cond
       ((= att_ct "dif_tee")(load "ditee"))
       ((= att_ct "wdf_tee")(load "wdtee"))
       ((= att_ct "scf_tee")(load "sctee"))
  )
  (cond
       ((= sw_ft "s_cl")(teescl))
       ((= sw_ft "d_cl")(teedcl))
       ((= sw_ft "u_cl")(teeucl))
  )
  (princ)
 )
(princ)
