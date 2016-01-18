;dde9s.lsp
;
;--------* Stone & Webster Engineering Corp
;--------* CAD/CAE
;--------* Mechanical Application
;--------* 012196                       jdc
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
;--------* dde9s uses a dialog interface to define the parameters
;--------* for the creation of Pipe Fitting Elbows
;
(defun c:dde9s ( / last_pic)
;
;--------* check for external functions
;
 (if (not initerr)
     (load (findfile "errcntl.lsp"))
 )
(if (not md_list)
    (load (findfile "md_list.lsp"))
)
(if (not mf_gsz)
    (load (findfile "mf_gsz.lsp"))
)
;
;--------* initialize error control for program and functions
;
(initerr)
;
;--------* take control of certain variables just in case
;
(setq h_osm (getvar "osmode")
      h_plw (getvar "plinewid")
      h_ucf (getvar "ucsfollow")
)
(setvar "cmdecho" 0)
(setvar "plinewid" 0.0)
(setvar "osmode" 0)
(setvar "ucsfollow" 0)
(adderr '(setvar "osmode" h_osm))
(adderr '(setvar "plinewid" h_plw))
(adderr '(setvar "ucsfollow" h_ucf))
;
;--------* load dialog
;
 (setq id (load_dialog "att_fit.dcl"))
 (if (not (new_dialog "dde9s" id))(exit))
;
;--------* display slide images
;
 (start_image "s_cl")
 (setq max_x (dimx_tile "s_cl")
       max_y (dimy_tile "s_cl")
 )
 (slide_image 0 0 max_x max_y "m_lib(dif_scl)")
 (end_image)
 (start_image "s_cp")
 (slide_image 0 0 max_x max_y "m_lib(dif_scp)")
 (end_image)
 (start_image "d_cl")
 (slide_image 0 0 max_x max_y "m_lib(dif_dcl)")
 (end_image)
 (start_image "d_cp")
 (slide_image 0 0 max_x max_y "m_lib(dif_dcp)")
 (end_image)
 (start_image "u_cl")
 (slide_image 0 0 max_x max_y "m_lib(dif_ucl)")
 (end_image)
 (start_image "u_cp")
 (slide_image 0 0 max_x max_y "m_lib(dif_ucp)")
 (end_image)
 (start_image "u_cln")
 (slide_image 0 0 max_x max_y "m_lib(dif_ucln)")
 (end_image)
;
;--------* add action to tiles
;
 (action_tile "el90lr" "(setq att_fit $key)")
 (action_tile "el90sr" "(setq att_fit $key)")
 (action_tile "el45lr" "(setq att_fit $key)")
 (action_tile "el45sr" "(setq att_fit $key)")
 (action_tile "dif_els" "(setq att_ct $key)(md_list $key)")
 (action_tile "wdf_els" "(setq att_ct $key)(md_list $key)")
 (action_tile "scf_els" "(setq att_ct $key)(md_list $key)")
 (action_tile "s_cl" "(hi_lite \"s_cl\")(setq att_fv $key)")
 (action_tile "s_cp" "(hi_lite \"s_cp\")(setq att_fv $key)")
 (action_tile "d_cl" "(hi_lite \"d_cl\")(setq att_fv $key)")
 (action_tile "d_cp" "(hi_lite \"d_cp\")(setq att_fv $key)")
 (action_tile "u_cl" "(hi_lite \"u_cl\")(setq att_fv $key)")
 (action_tile "u_cln" "(hi_lite \"u_cln\")(setq att_fv $key)")
 (action_tile "u_cp" "(hi_lite \"u_cp\")(setq att_fv $key)")
 (action_tile "f_sz" "(setq f_sz2 $value sz_1 (nth (atoi f_sz2) szls1))
                      (mf_gsz sz_1 att_fit att_ct)")
 (action_tile "accept" "(chk_se)")
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
      ((= dr_it 1)(draw_elv))
 )
);end defun
(princ)
;
;--------* check for all settings to be selected
;
(defun chk_se ()
      (if (= att_fv nil)
          (set_tile "error" "Error-- Fitting view not selected")
          (progn
                (if (= att_fit nil)
                    (set_tile "error" "Error - Fitting type not selected")
                    (progn
                          (if (= f_sz2 nil)
                               (set_tile "error" "Error--Fitting size not selected")
                               (progn
                                     (if (= att_ct nil)
                                         (set_tile "error" "Error--Fitting copling not selected")
                                         (done_dialog 1)
                                     )
                               )
                          )
                    )
                 )
           )
      )
      (princ)
)
;
;--------* draw_elv decides what view of fitting to draw
;
 (defun draw_elv ()
  (cond
       ((= att_ct "dif_els")(load "dife9ma"))
       ((= att_ct "wdf_els")(load "wlde9ma"))
       ((= att_ct "scf_els")(load "sce9ma"))
  )
  (cond
       ((= att_fv "s_cl")(elscl))
       ((= att_fv "s_cp")(elscp))
       ((= att_fv "d_cl")(eldcl))
       ((= att_fv "d_cp")(eldcp))
       ((= att_fv "u_cl")(elucl))
       ((= att_fv "u_cln")(elucln))
       ((= att_fv "u_cp")(elucp))
  )
  (princ)
 )
(princ)
