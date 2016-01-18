;ddtrf
;
;--------* Stone & Webster engineering Corp
;--------* CAD/CAE
;--------* Mechanical Application
;--------* 031296                       jdc
;
;
;;; Copyright (C) 1993 by Autodesk, Inc.---Copied From ddsolprm.lsp
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
;---------* Transformation Fittings
;
(defun c:ddtrf (/ id max_x max_y last_pic ftype trns dr_it)
;
;--------* check and/or load data collection functions and
;--------* error control
;
(if (not initerr)
    (load (findfile "errcntl.lsp"))
)
;(if (not rreder)
;    (load (findfile "htrfmma.lsp"))
;)
;--------* Error functions
;
(initerr)
;
;--------* create initial args list
;
(setq args '(id max_x max_y last_pic ftype trns dr_it args))
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
;---------* display dialog
;
 (setq id (load_dialog "att_fit.dcl"))
 (if (not (new_dialog "ddtrf" id))(exit))
; 
;--------* display slide images
;
 (start_image "sq_r_c")
 (setq max_x (dimx_tile "sq_r_c")
       max_y (dimy_tile "sq_r_c")
 )
 (slide_image 0 0 max_x max_y "m_lib(trfm_001)")
 (end_image)
 (start_image "sq_r_el")
 (slide_image 0 0 max_x max_y "m_lib(trfm_002)")
 (end_image)
 (start_image "sq_r_er")
 (slide_image 0 0 max_x max_y "m_lib(trfm_003)")
 (end_image)
 (start_image "r_sq_c")
 (slide_image 0 0 max_x max_y "m_lib(trfm_004)")
 (end_image)
 (start_image "r_sq_el")
 (slide_image 0 0 max_x max_y "m_lib(trfm_005)")
 (end_image)
 (start_image "r_sq_er")
 (slide_image 0 0 max_x max_y "m_lib(trfm_006)")
 (end_image)
;
;--------* add action to tiles
;
 (action_tile "m_d" "(setq #dia (atof $value))")
 (action_tile "b_d" "(setq #bdia (atof $value))")
 (action_tile "sq_r_c" "(hi_lite \"sq_r_c\")(setq ftype $key)(ckdtrf)")
 (action_tile "sq_r_el" "(hi_lite \"sq_r_el\")(setq ftype $key)(ckdtrf)")
 (action_tile "sq_r_er" "(hi_lite \"sq_r_er\")(setq ftype $key)(ckdtrf)")
 (action_tile "r_sq_c" "(hi_lite \"r_sq_c\")(setq ftype $key)(ckdtrf)")
 (action_tile "r_sq_el" "(hi_lite \"r_sq_el\")(setq ftype $key)(ckdtrf)")
 (action_tile "r_sq_er" "(hi_lite \"r_sq_er\")(setq ftype $key)(ckdtrf)")
; (action_tile "accept" "(ckstrf)")
 (action_tile "accept" "(done_dialog 1)")
 (action_tile "cancel" "(done_dialog 0)")
;
;--------* enable the dialog
;
 (setq dr_it (start_dialog))
 (unload_dialog id)
 ;clean up if cancel or draw it if accept
 (if (= dr_it 0)
     (reset nil)
     (drtrfm)
 )
(reset nil)
(princ)
)
;
;--------* drtrfm decides what type of fitting to draw
;
 (defun drtrfm ()
  (cond
       ((= ftype "sq_r_c")(setq trns "sq_r")(rredc))
       ((= ftype "sq_r_el")(setq trns "sq_r")(rredel))
       ((= ftype "sq_r_er")(setq trns "sq_r")(rreder))
       ((= ftype "r_sq_c")(setq trns "r_sq")(rredc))
       ((= ftype "r_sq_el")(setq trns "r_sq")(rredel))
       ((= ftype "r_sq_er")(setq trns "r_sq")(rreder))
  )
 )
;
;--------* check run diameter
;
(defun ckdtrf ()
       (if (or (= #dia nil)
               (<= #dia 0.0)
            )
         (progn
            (set_tile "error" "Enter Main Size of Duct")
            (mode_tile "m_d" 2)
         )
         (progn
            (set_tile "error" "")
            (set_tile "m_d" (rtos #dia))
            (ckbtrf)
         )
       )
)
;
;--------* check branch diameter
;
(defun ckbtrf ()
       (if (or (= #bdia nil)
               (<= #bdia 0.0)
            )
         (progn
            (set_tile "error" "Enter Branch Size of Duct")
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
(defun ckstrf ()
       (if (= ftype nil)
              (set_tile "error" "Select Fitting Type")
                  (progn
                     (if (or (= #dia nil)
                             (<= #dia 0.0)
                             (= #bdia nil)
                             (<= #bdia 0.0)
                         )
                         (ckdtrf)
                         (done_dialog 1)
                     )
                  )
       )
)
(princ)


;--------* Concentric Reducer
;
(defun RREDC ()
(inpnt)
(command "pline" (list (+ ptx 0.00) (+ pty (* #DIA 0.50)))
"W" 0.00 0.00 (setq p (list (+ ptx 0.00) (+ pty (* #DIA -0.50))))
(setq p (polar p 0.26179938 (* 1.9318525563 (- #DIA #BDIA))))
(polar p 1.57079628 #BDIA) "cl" nil)
(setq ent (entlast))
 (rotent)
 (reset nil)
 (princ)
 )
;
;--------* Eccentric Reducer (Left)
;
(defun RREDEL ()
(inpnt)
(command "pline" (list (+ ptx 0.00) (+ pty (* #DIA 0.50)))
"W" 0.00 0.00 (setq p (list (+ ptx 0.00) (+ pty (* #DIA -0.50))))
(setq p (polar p 2.61799389 (* 2.00 (- #DIA #BDIA))))
(polar p 1.57079628 #BDIA) "cl" nil)
(setq ent (entlast))
 (rotent)
 (reset nil)
 (princ)
 )
;
;--------* Eccentric Reducer (Right)
;
(defun RREDER ()
(inpnt)
(command "pline" (list (+ ptx 0.00) (+ pty (* #DIA 0.50)))
"W" 0.00 0.00 (setq p (list (+ ptx 0.00) (+ pty (* #DIA -0.50))))
(setq p (polar p 0.52359876 (* 2.00 (- #DIA #BDIA))))
(polar p 1.57079628 #BDIA) "cl" nil)
(setq ent (entlast))
 ;(trnsfm)
 (rotent)
 (reset nil)
 (princ)
 )
