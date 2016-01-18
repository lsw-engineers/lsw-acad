;hductrnd
;
;--------* Stone & Webster engineering Corp
;--------* CAD/CAE
;--------* Mechanical Application
;--------* 031296                       jdc
;
;--------*Round Duct Elbows
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
(defun c:ddrnd (/ id max_x max_y last_pic ftype f_t dr_it)
;
;
;--------* check and/or load data collection functions and
;--------* error control
;
(if (not initerr)
    (load (findfile "errcntl.lsp"))
)
(if (not c:rel30s)
    (load (findfile "hdrndma.lsp"))
)
;--------* Error functions
;
(initerr)
;
;--------* create initial args list
;
(setq args '(id max_x max_y last_pic ftype f_t dr_it args))
;
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
;--------* display dialog
;
 (setq id (load_dialog "att_fit.dcl"))
 (if (not (new_dialog "ddrndd" id))(exit))
;display slide images
 (start_image "s9_rs")
 (setq max_x (dimx_tile "s9_rs")
       max_y (dimy_tile "s9_rs")
 )
 (slide_image 0 0 max_x max_y "m_lib(cd_0001)")
 (end_image)
 (start_image "u9_rs")
 (slide_image 0 0 max_x max_y "m_lib(cd_0002)")
 (end_image)
 (start_image "d9_rs")
 (slide_image 0 0 max_x max_y "m_lib(cd_0003)")
 (end_image)
 (start_image "s4_rs")
 (slide_image 0 0 max_x max_y "m_lib(cd_0004)")
 (end_image)
 (start_image "s3_rs")
 (slide_image 0 0 max_x max_y "m_lib(cd_0005)")
 (end_image)
 (start_image "s9_rl")
 (slide_image 0 0 max_x max_y "m_lib(cd_0006)")
 (end_image)
 (start_image "u9_rl")
 (slide_image 0 0 max_x max_y "m_lib(cd_0007)")
 (end_image)
 (start_image "d9_rl")
 (slide_image 0 0 max_x max_y "m_lib(cd_0008)")
 (end_image)
 (start_image "s4_rl")
 (slide_image 0 0 max_x max_y "m_lib(cd_0009)")
 (end_image)
 (start_image "s3_rl")
 (slide_image 0 0 max_x max_y "m_lib(cd_0010)")
 (end_image)
 (start_image "tw_rs")
 (slide_image 0 0 max_x max_y "m_lib(cd_0011)")
 (end_image)
 (start_image "tee_s")
 (slide_image 0 0 max_x max_y "m_lib(cd_0012)")
 (end_image)
 (start_image "tee_a")
 (slide_image 0 0 max_x max_y "m_lib(cd_0013)")
 (end_image)
 (start_image "tee_u")
 (slide_image 0 0 max_x max_y "m_lib(cd_0014)")
 (end_image)
 (start_image "tee_d")
 (slide_image 0 0 max_x max_y "m_lib(cd_0015)")
 (end_image)
 (start_image "lat_l")
 (slide_image 0 0 max_x max_y "m_lib(cd_0016)")
 (end_image)
 (start_image "lat_r")
 (slide_image 0 0 max_x max_y "m_lib(cd_0017)")
 (end_image)
 (start_image "red_c")
 (slide_image 0 0 max_x max_y "m_lib(cd_0018)")
 (end_image)
 (start_image "red_el")
 (slide_image 0 0 max_x max_y "m_lib(cd_0019)")
 (end_image)
 (start_image "red_er")
 (slide_image 0 0 max_x max_y "m_lib(cd_0020)")
 (end_image)
 ;
 ;-------* add action to tiles
 ;
 (action_tile "m_d" "(setq #dia (atof $value))")
 (action_tile "b_d" "(setq #bdia (atof $value))")
 (action_tile "s9_rs" "(hi_lite \"s9_rs\")(setq ftype $key)(chk_dia)(set_tile \"d_des\" \"90 Degree Elbow Short Radius Side\")")
 (action_tile "u9_rs" "(hi_lite \"u9_rs\")(setq ftype $key)(chk_dia)(set_tile \"d_des\" \"90 Degree Elbow Short Radius Up\")")
 (action_tile "d9_rs" "(hi_lite \"d9_rs\")(setq ftype $key)(chk_dia)(set_tile \"d_des\" \"90 Degree Elbow Short Radius Down\")")
 (action_tile "s4_rs" "(hi_lite \"s4_rs\")(setq ftype $key)(chk_dia)(set_tile \"d_des\" \"45 Degree Elbow Short Radius Side\")")
 (action_tile "s3_rs" "(hi_lite \"s3_rs\")(setq ftype $key)(chk_dia)(set_tile \"d_des\" \"30 Degree Elbow Short Radius Side\")")
 (action_tile "s9_rl" "(hi_lite \"s9_rl\")(setq ftype $key)(chk_dia)(set_tile \"d_des\" \"90 Degree Elbow Long Radius Side\")")
 (action_tile "u9_rl" "(hi_lite \"u9_rl\")(setq ftype $key)(chk_dia)(set_tile \"d_des\" \"90 Degree Elbow Long Radius Up\")")
 (action_tile "d9_rl" "(hi_lite \"d9_rl\")(setq ftype $key)(chk_dia)(set_tile \"d_des\" \"90 Degree Elbow Long Radius Down\")")
 (action_tile "s4_rl" "(hi_lite \"s4_rl\")(setq ftype $key)(chk_dia)(set_tile \"d_des\" \"45 Degree Elbow Long Radius Side\")")
 (action_tile "s3_rl" "(hi_lite \"s3_rl\")(setq ftype $key)(chk_dia)(set_tile \"d_des\" \"30 Degree Elbow Long Radius Side\")")
 (action_tile "tw_rs" "(hi_lite \"tw_rs\")(setq ftype $key)(chk_dia)(set_tile \"d_des\" \"Twin Bend Side\")")
 (action_tile "tee_s" "(hi_lite \"tee_s\")(setq ftype $key)(chk_dia)(set_tile \"d_des\" \"Tee Side\")")
 (action_tile "tee_a" "(hi_lite \"tee_a\")(setq ftype $key)(chk_dia)(set_tile \"d_des\" \"Tee Side (Facing Run)\")")
 (action_tile "tee_u" "(hi_lite \"tee_u\")(setq ftype $key)(chk_dia)(set_tile \"d_des\" \"Tee Up\")")
 (action_tile "tee_d" "(hi_lite \"tee_d\")(setq ftype $key)(chk_dia)(set_tile \"d_des\" \"Tee Down\")")
 (action_tile "lat_l" "(hi_lite \"lat_l\")(setq ftype $key)(chk_dia)(set_tile \"d_des\" \"Lateral Left\")")
 (action_tile "lat_r" "(hi_lite \"lat_r\")(setq ftype $key)(chk_dia)(set_tile \"d_des\" \"Lateral Right\")")
 (action_tile "red_c" "(hi_lite \"red_c\")(setq ftype $key)(chk_dia)(set_tile \"d_des\" \"Concentric Reducer\")")
 (action_tile "red_el" "(hi_lite \"red_el\")(setq ftype $key)(chk_dia)(set_tile \"d_des\" \"Eccentric Reducer Left\")")
 (action_tile "red_er" "(hi_lite \"red_er\")(setq ftype $key)(chk_dia)(set_tile \"d_des\" \"Eccentric Reducer Right\")")
 (action_tile "accept" "(chkdr)")
 (action_tile "cancel" "(done_dialog 0)")
;
;--------* enable the dialog
;
 (setq dr_it (start_dialog))
 (unload_dialog id)
; 
;--------* clean up if cancel or draw it if accept
;
 (if (= dr_it 0)
     (reset nil)
     (drawdr)
 )
 (reset nil)
 (princ)
) 
;
;--------* check dialog settings run diameter
;
(defun chk_dia ()
       (setq f_t (substr ftype 4 1))
       (if (= f_t "r")
           (mode_tile "b_d" 1)
           (mode_tile "b_d" 0)
       )
       (if (or (= #dia nil)
               (<= #dia 0.0)
            )
         (progn
            (set_tile "error" "Enter Diameter of Duct")
            (mode_tile "m_d" 2)
         )
         (progn
            (set_tile "error" "")
            (set_tile "m_d" (rtos #dia))
            (if (= f_t "_")
                 (chk_bdia)
            )
         )
       )
)
;
;--------* check dialog settings dranch diameter
;
(defun chk_bdia ()
       (if (or (= #bdia nil)
               (<= #bdia 0.0)
            )
         (progn
            (set_tile "error" "Enter Branch Diameter of Duct")
            (mode_tile "b_d" 2)
         )
         (progn
            (set_tile "error" "")
            (set_tile "b_d" (rtos #bdia))
         )
       )
)
;
;--------* check dialog settings
;
(defun chkdr ()
       (if (= ftype nil)
              (set_tile "error" "Select Fitting Type")
           (progn
                (if (= f_t "r")
                  (progn
                     (if (or (= #dia nil)
                             (<= #dia 0.0)
                         )
                         (chk_dia)
                         (done_dialog 1)
                     )
                  )
                  (progn
                     (if (or (= #bdia nil)
                             (<= #bdia 0.0)
                         )
                         (chk_dia)
                         (done_dialog 1)
                     )
                  )
                )
           )
       )
)
;
;--------* draw dr decides view to draw from dialog settings
;
 (defun drawdr ()
  (cond
       ((= ftype "s9_rs")(c:rel90s))
       ((= ftype "u9_rs")(c:rel90su))
       ((= ftype "d9_rs")(c:rel90sd))
       ((= ftype "s4_rs")(c:rel45s))
       ((= ftype "s3_rs")(c:rel30s))
       ((= ftype "s9_rl")(c:rel90l))
       ((= ftype "u9_rl")(c:rel90lu))
       ((= ftype "d9_rl")(c:rel90ld))
       ((= ftype "s4_rl")(c:rel45l))
       ((= ftype "s3_rl")(c:rel30l))
       ((= ftype "tw_rs")(c:reltwn))
       ((= ftype "tee_s")(c:rtee))
       ((= ftype "tee_a")(c:rtees))
       ((= ftype "tee_u")(c:rteeu))
       ((= ftype "tee_d")(c:rteed))
       ((= ftype "lat_l")(c:rlatl))
       ((= ftype "lat_r")(c:rlatr))
       ((= ftype "red_c")(c:rredc))
       ((= ftype "red_el")(c:rredel))
       ((= ftype "red_er")(c:rreder))
  )
 )
(princ)
