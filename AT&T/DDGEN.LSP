;
;--------* Stone & Webster Engineering Corp
;--------* CAD/CAE
;--------* mechanical Application
;--------* 031396                       jdc
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
;--------* dialog interface for installing general blocks
;
(defun c:ddgen ( / last_pic)
;
;--------* check and/or load data collection functions and
;--------* error control
;
(if (not initerr)
    (load (findfile "errcntl.lsp"))
)
(if (not flagl)
    (load (findfile "sec_sym.lsp"))
)
;--------* Error functions
;
(initerr)
;
;--------* create initial args list
;
;(setq args '(sz_ls sz_ls2 rep_1 ctls ls_t1 max_x max_y h_osm h_plw h_ucf
;             ls_t2 id me_ft dr_it last_pic f_sz2 sw_ft args))
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
 (if (not (new_dialog "ddgen" id))(exit))
;
;
;--------* define image tiles
;
;
;--------* display slide images
;
 (start_image "gen1")
 (setq max_x (dimx_tile "gen1")
       max_y (dimy_tile "gen1")
 )
 (slide_image 0 0 max_x max_y "m_lib(g_007)")
 (end_image)
 (start_image "gen2")
 (slide_image 0 0 max_x max_y "m_lib(g_008)")
 (end_image)
 (start_image "gen3")
 (slide_image 0 0 max_x max_y "m_lib(g_009)")
 (end_image)
 (start_image "gen4")
 (slide_image 0 0 max_x max_y "m_lib(g_010)")
 (end_image)
 (start_image "gen5")
 (slide_image 0 0 max_x max_y "m_lib(g_013)")
 (end_image)
 (start_image "gen6")
 (slide_image 0 0 max_x max_y "m_lib(g_014)")
 (end_image)
 (start_image "gen7")
 (slide_image 0 0 max_x max_y "m_lib(g_015)")
 (end_image)
 (start_image "gen8")
 (slide_image 0 0 max_x max_y "m_lib(g_016)")
 (end_image)
 (start_image "gen9")
 (slide_image 0 0 max_x max_y "m_lib(g_017)")
 (end_image)
 (start_image "gen10")
 (slide_image 0 0 max_x max_y "m_lib(g_018)")
 (end_image)
 (start_image "gen11")
 (slide_image 0 0 max_x max_y "m_lib(g_019)")
 (end_image)
 (start_image "gen12")
 (slide_image 0 0 max_x max_y "m_lib(g_020)")
 (end_image)
 (start_image "gen13")
 (slide_image 0 0 max_x max_y "m_lib(g_021)")
 (end_image)
  (start_image "gen14")
 (slide_image 0 0 max_x max_y "g_022")
 (end_image)
;
;--------* Add action to tiles
;
 (action_tile "gen1" "(hi_lite \"gen1\")(setq dohuh $key)")
 (action_tile "gen2" "(hi_lite \"gen2\")(setq dohuh $key)")
 (action_tile "gen3" "(hi_lite \"gen3\")(setq dohuh $key)")
 (action_tile "gen4" "(hi_lite \"gen4\")(setq dohuh $key)")
 (action_tile "gen5" "(hi_lite \"gen5\")(setq dohuh $key)")
 (action_tile "gen6" "(hi_lite \"gen6\")(setq dohuh $key)")
 (action_tile "gen7" "(hi_lite \"gen7\")(setq dohuh $key)")
 (action_tile "gen8" "(hi_lite \"gen8\")(setq dohuh $key)")
 (action_tile "gen9" "(hi_lite \"gen9\")(setq dohuh $key)")
 (action_tile "gen10" "(hi_lite \"gen10\")(setq dohuh $key)")
 (action_tile "gen11" "(hi_lite \"gen11\")(setq dohuh $key)")
 (action_tile "gen12" "(hi_lite \"gen12\")(setq dohuh $key)")
 (action_tile "gen13" "(hi_lite \"gen13\")(setq dohuh $key)")
 (action_tile "gen14" "(hi_lite \"gen14\")(setq dohuh $key)")
 (action_tile "accept" "(chk_gen)")
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
      ((= dr_it 1)(drgen))
 )
 (reset nil)
 (princ)
)
(princ)
;
;--------* check for all settings to be selected
;
(defun chk_gen ()
      (if (= dohuh nil)
          (set_tile "error" "Error-- Symbol type not selected")
          (done_dialog 1)
      )
 )
;
;--------* drgen decides what to do either draw and insert
;--------* or insert block
;
(defun drgen ()
 (cond
      ((= dohuh "gen1")(scut1l))
      ((= dohuh "gen2")(scut1r))
      ((= dohuh "gen3")(scut2l))
      ((= dohuh "gen4")(scut2r))
      ((= dohuh "gen5")(flagl))
      ((= dohuh "gen6")(flagr))
      ((= dohuh "gen7")(ingpb "gpst2000"))
      ((= dohuh "gen8")(ingpb "gpst1000"))
      ((= dohuh "gen9")(ingpb "gpst3000"))
      ((= dohuh "gen10")(ingpb "gpdt1000"))
      ((= dohuh "gen11")(ingpb "gpdt2000"))
      ((= dohuh "gen12")(ingpb "gpsb1000"))
      ((= dohuh "gen13")(ingpb "gpsb2000"))
      ((= dohuh "gen14")(ingpb "gpna100f"))
 )
)
