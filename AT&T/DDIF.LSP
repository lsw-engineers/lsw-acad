;;;ddif
;;;
;;;--------* Stone & Webster engineering Corp
;;;--------* CAD/CAE
;;;--------* Mechanical Application
;;;--------* 031296                       jdc
;;;
;;;
;;;;;;;;; Copyright (C) 1993 by Autodesk, Inc.---Copied From ddsolprm.lsp
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
;;;---------*
(defun c:ddif (/ id max_x max_y last_pic dr_it stmp ttmp)
;;;
;;;
;;;--------* check and/or load data collection functions and
;;;--------* error control
;;;
  (if (not initerr)
    (load (findfile "errcntl.lsp"))
  )
  (if (not sqr_diff)
    (load (findfile "hdifma.lsp"))
  )
;;;--------* Error functions
;;;
  (initerr)
;;;
;;;--------* create initial args list
;;;
  (setq args '(id max_x max_y last_pic ftype dr_it stmp ttmp args))
;;;
;;;--------* take control of certain variables just in case
;;;
  (setq	h_osm (getvar "osmode")
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




;;;
;;;---------* display dialog
;;;
  (setq id (load_dialog "att_fit.dcl"))
  (if (not (new_dialog "ddiff" id))
    (exit)
  )
;;; 
;;;--------* display slide images
;;;
  (start_image "sqr_p")
  (setq	max_x (dimx_tile "sqr_p")
	max_y (dimy_tile "sqr_p")
  )
  (slide_image 0 0 max_x max_y "m_lib(diff_001)")
  (end_image)
  (start_image "sqr_s")
  (slide_image 0 0 max_x max_y "m_lib(diff_002)")
  (end_image)
  (start_image "sqr_e")
  (slide_image 0 0 max_x max_y "m_lib(diff_003)")
  (end_image)
  (start_image "rnd_p")
  (slide_image 0 0 max_x max_y "m_lib(diff_004)")
  (end_image)
  (start_image "rnd_s")
  (slide_image 0 0 max_x max_y "m_lib(diff_005)")
  (end_image)
  (start_image "rnd_e")
  (slide_image 0 0 max_x max_y "m_lib(diff_006)")
  (end_image)
  (start_image "sid_p")
  (slide_image 0 0 max_x max_y "m_lib(diff_007)")
  (end_image)
  (start_image "sid_s")
  (slide_image 0 0 max_x max_y "m_lib(diff_008)")
  (end_image)
  (start_image "sid_e")
  (slide_image 0 0 max_x max_y "m_lib(diff_009)")
  (end_image)
;;;
;;;--------* add action to tiles
;;;
  (action_tile "d_sz" "(setq sz (atof $value))")
  (action_tile
    "sqr_p"
    "(hi_lite \"sqr_p\")(setq ftype $key)(ckdif)"
  )
  (action_tile
    "sqr_s"
    "(hi_lite \"sqr_s\")(setq ftype $key)(ckdif)"
  )
  (action_tile
    "sqr_e"
    "(hi_lite \"sqr_e\")(setq ftype $key)(ckdif)"
  )
  (action_tile
    "rnd_p"
    "(hi_lite \"rnd_p\")(setq ftype $key)(ckdif)"
  )
  (action_tile
    "rnd_s"
    "(hi_lite \"rnd_s\")(setq ftype $key)(ckdif)"
  )
  (action_tile
    "rnd_e"
    "(hi_lite \"rnd_e\")(setq ftype $key)(ckdif)"
  )
  (action_tile
    "sid_p"
    "(hi_lite \"sid_p\")(setq ftype $key)(ckdif)"
  )
  (action_tile
    "sid_s"
    "(hi_lite \"sid_s\")(setq ftype $key)(ckdif)"
  )
  (action_tile
    "sid_e"
    "(hi_lite \"sid_e\")(setq ftype $key)(ckdif)"
  )
  (action_tile "accept" "(ckdifst)")
  ***
  Removed
  09/26
  AJB
  ***
;;; (action_tile "accept" "(done_dialog 1)")
  (action_tile "cancel" "(done_dialog 0)")
;;; 
;;;--------* enable the dialog
;;;
  (setq dr_it (start_dialog))
  (unload_dialog id)
;;; 
;;;--------* clean up if cancel or draw it if accept
;;;
  (if (= dr_it 0)
    (reset nil)
    (drdif)
  )
  (reset nil)
  (princ)
)
;;;
;;;--------* draw_it decides what type of fitting to draw
;;;
(defun drdif ()
  (cond
    ((= ftype "sqr_p") (sqr_diff))
    ((= ftype "sqr_s") (sqr_diff))
    ((= ftype "sqr_e") (sqr_diff))
    ((= ftype "rnd_p") (rnd_diff))
    ((= ftype "rnd_s") (rnd_diff))
    ((= ftype "rnd_e") (rnd_diff))
    ((= ftype "sid_p") (sid_diff))
    ((= ftype "sid_s") (sid_diff))
    ((= ftype "sid_e") (sid_diff))
  )
)
(princ)
;;;
;;;---------*
;;;
(defun inpnt ()
  (setvar "osmode" h_osm)
  (setq inp (getpoint "\nInsertion point: "))
  (setvar "osmode" 0)
  (setq	plw  (* 0.07 #dsc)
	stmp (ssadd)
	ttmp (ssadd)
  )
)
;;; 
;;;---------*
;;;
(defun rotent ()
  (while ent
    (ssadd ent stmp)
    (setq ent (entnext ent))
  )
  (setvar "osmode" h_osm)
  (command "rotate" stmp "" inp pause)
)
;;;---------*
(defun ckdif ()
  (if (or (= sz nil)
	  (<= sz 0.0)
      )
    (progn
      (set_tile "error" "Enter Diffuser Size")
      (mode_tile "d_sz" 2)
    )
    (progn
      (set_tile "error" "")
      (set_tile "d_sz" (rtos sz))
    )
  )
)
;;;---------*

(defun ckdifst () (princ))

(princ)
