;difelma
;
;--------* Stone & Webster Engineering Corp.
;--------* CAD/CAE
;--------* Mechanical Application
;--------* 012196                       jdc
;
;
;--------* the functions in this file define or redfine the funcions
;--------* used by the ddelbows command to draw ductile iron flanged
;--------* elbows, redefinition is done to preserve memory
;
;--------* flanged coupling function syntex:(mk_cplf insertion point angle)
;
(defun mk_cpl (ip a_1 / v1 v2 v3 v4 p_d v1_d v2_d v3_d v4_d
                b_f b_fc p_dm v1_m v2_m v3_m v4_m c_e)
      (setq v1 (polar ip a_1 (/ cd 2.0))
            v2 (polar v1 (+ a_1 (* pi 1.5)) cw)
            v3 (polar v2 (+ a_1 pi) cd)
            v4 (polar v3 (+ a_1 (* pi 0.5)) cw)
 )
;
;----------* associate data with dxf codes
;
 (setq p_d (cons 70 1.0)
          v1_d (cons 10 v1)
          v2_d (cons 10 v2)
          v3_d (cons 10 v3)
          v4_d (cons 10 v4)
 )
;
;----------* create entity data
;
 (setq p_dm (list '(0 . "polyline") p_d)
          v1_m (list '(0 . "vertex") v1_d)
          v2_m (list '(0 . "vertex") v2_d)
          v3_m (list '(0 . "vertex") v3_d)
          v4_m (list '(0 . "vertex") v4_d)
          c_e (list '(0 . "seqend"))
 )
 (entmake p_dm)
 (entmake v4_m)
 (entmake v3_m)
 (entmake v2_m)
 (entmake v1_m)
 (entmake c_e)
 (princ)
)
;
;--------* side elbow coupling insert
;
(defun elscp (/ inp int_p fp_2 as_r a1_r a2_r as_a ae_a as_a
                 as_m a1_m a2_m f1_m f2_m tmp_1 es_1 e_ans inp_2
                 cp sz_ls rep_1 ctls ls_t1 sz_ls2 ls_t2 id att_fv
                 dr_it tile last_pic f_sz2 args)
;
;--------* add locals to args list
;
(setq args '(inp int_p fp_2 as_r a1_r a2_r as_a ae_a as_a
             as_m a1_m a2_m f1_m f2_m tmp_1 es_1 e_ans inp_2
             cp sz_ls  ctls ls_t1 sz_ls2ls_t2 id att_fv dr_it
             tile last_pic f_sz2 args)
)
;
;--------* reset variables to real numbers
;
(setq cw (atof att_cw)
      cd (atof att_cd)
      pd (atof sz_1)
      l1 (atof att_cte)
)
;
;--------* Get Input from user
;
 (command ".ucs" "w")
 (setq inp (getpoint "\nInsertion point :"))
 (if (or (= att_fit "el90lr")(= att_fit "el90sr")) ; 90 degree elbows
     (progn
            (setq inp_2 (polar inp (* pi 0.5) cw) ;locate cpling1 midpoint
                  int_p (polar inp (* pi 0.5) l1) ;locate center line intersection
                  fp_2 (polar int_p 0.0 (- l1 cw)) ;locate cpling2 midpoint
                  cp (polar inp_2  0.0 (- l1 cw)) ;locate pipe arc center position
            )
     )
     (progn
           (if (or (= att_fit "el45lr")(= att_fit "el45sr"))
               (progn
                     (setq inp_2 (polar inp (* pi 0.5) cw)
                           inp_2a (polar inp_2 0.0 2.0)
                           int_p (polar inp (* pi 0.5) l1)
                           fp_2 (polar int_p (* pi 0.25) (- l1 cw))
                           fp_2a (polar fp_2 (* pi 1.75) 2.0)
                           cp (inters inp_2 inp_2a fp_2 fp_2a nil)
                     )
               )
           )
     )
 )
;
;--------* associate points or angles in radians to the appropriate dxf codes
;
 (if (<= pd 6.0)
     (progn
           (setq as_r (cons 40 (distance cp inp))) ;single line arc radius
     )
     (progn
           (setq  a1_r (cons 40 (- (distance cp inp) (/ pd 2.0))) ;arc 1 radius
                  a2_r (cons 40 (+ (distance cp inp) (/ pd 2.0))) ;arc 2 radius
           )
     )
 )
 (if (or (= att_fit "el90lr")(= att_fit "el90sr")) ; 90 degree elbows
     (setq as_a (cons 50 (/ pi 2.0))) ;starting angle for arcs
      (progn
            (if (or (= att_fit "el45lr")(= att_fit "el45sr")) ; 45 degree elbows
                (setq as_a (cons 50 (* pi 0.75))) ;starting angle for arcs
            )
      )
 )
 (setq ae_a (cons 51 pi) ;ending angle for arcs
       cp (cons 10 cp) ;center point for arcs
 )
;
;--------* create entity data
;
;--------* block information first if block exists use it else create
;
(setq att_bname (strcat "m" att_fit "f-sd" (rtos (* 100 pd) 2 0))
      att_bn att_bname
)
(if (/= (tblsearch "block" att_bname) nil)
    (progn
          (command "_.insert" att_bname inp "" "" pause)
    )
    (progn;progn to contain block creation start
          (setq att_bname (cons 2 att_bname)
                b_lyr (cons 8 "0")
                b_inp (cons 10 inp)
                att_fol (cons 70 0)
                bl_data (list '(0 . "BLOCK")
                               att_bname
                               b_inp
                               att_fol
                        )
          )
;
;
;--------* block stuff is done
;
          (if (<= pd 6.0)
                        (progn
                              (setq as_m (list '(0 . "arc") cp as_r as_a ae_a)) ;single arc data
                        )
                        (progn
                              (setq a1_m (list '(0 . "arc") cp a1_r as_a ae_a) ;arc 1 data
                                    a2_m (list '(0 . "arc") cp a2_r as_a ae_a) ;arc 2 data
                              )
                        )
          )
;
;--------* insert entities into autocad database and create selection set
;
          (setq es_1 (ssadd));create selection set es_1
          (entmake bl_data)
          (if (<= pd 6.0)
                        (progn
                              (entmake as_m) ;insert single arc
                              (setq tmp_1 (entlast)) ;place entities data in variable tmp_1
                        )
                        (progn
                              (entmake a1_m) ;insert arc 1 data
                              (setq tmp_1 (entlast)) ;place entities data in variable tmp_1
                              (entmake a2_m) ;insert arc 2 data
                        )
          )
          (mk_cpl inp_2 0.0)
          (if (or (= att_fit "el90lr")(= att_fit "el90sr"))
              (progn
                    (mk_cpl fp_2 (* pi 0.5))
              )
              (progn
                    (if (or (= att_fit "el45lr")(= att_fit "el45sr"))
                        (progn
                              (mk_cpl fp_2 (* pi 0.75))
                        )
                    )
              )
          )
         (entmake '((0 . "ENDBLK")))
         (command "_.insert" att_bn inp "" "" pause)
    ); end progn to decide if create
 )
;
;--------* Run mirror rotate return to end function
;
(reset nil)
(princ)
)

;
;--------* side elbow center line insert
;
(defun elscl (/ inp int_p fp_2 as_r a1_r a2_r as_a ae_a as_a
                 as_m a1_m a2_m f1_m f2_m tmp_1 es_1 e_ans inp_2
                 cp sz_ls rep_1 ctls ls_t1 sz_ls2 ls_t2 id att_fv
                 dr_it tile last_pic f_sz2 args)
;
;--------* add locals to args list
;
(setq args '(inp int_p fp_2 as_r a1_r a2_r as_a ae_a as_a
              as_m a1_m a2_m f1_m f2_m tmp_1 es_1 e_ans inp_2
             cp sz_ls rep_1 ctls ls_t1 sz_ls2 ls_t2 id att_fv
             dr_it tile last_pic f_sz2 args)
)
;
;--------* reset variables to real numbers
;
(setq cw (atof att_cw)
      cd (atof att_cd)
      pd (atof sz_1)
      l1 (atof att_cte)
)
;
;--------* Get Input from user
;
 (command ".ucs" "w")
 (setq inp (getpoint "\nInsertion point :"))
 (if (or (= att_fit "el90lr")(= att_fit "el90sr")) ; 90 degree elbows
     (progn
           (setq inp_2 (polar inp (* pi 1.5) (- l1 cw))
                 fp_2 (polar inp 0.0 (- l1 cw))
                 cp (polar inp_2  0.0 (- l1 cw))
           )
     )
     (progn
           (if (or (= att_fit "el45lr")(= att_fit "el45sr"))
               (progn
                     (setq inp_2 (polar inp (* pi 1.5) (- l1 cw))
                           inp_2a (polar inp_2 0.0 2.0)
                           fp_2 (polar inp (* pi 0.25) (- l1 cw))
                           fp_2a (polar fp_2 (* pi 1.75) 2.0)
                           cp (inters inp_2 inp_2a fp_2 fp_2a nil)
                     )
               )
           )
     )
 )
 ;associate points or angles in radians to the appropriate dxf codes
 (if (<= pd 6.0)
     (progn
           (setq as_r (cons 40 (distance cp inp_2))) ;single line arc radius
     )
     (progn
           (setq  a1_r (cons 40 (- (distance cp inp_2) (/ pd 2.0))) ;arc 1 radius
                  a2_r (cons 40 (+ (distance cp inp_2) (/ pd 2.0))) ;arc 2 radius
           )
     )
 )
 (if (or (= att_fit "el90lr")(= att_fit "el90sr")) ; 90 degree elbows
     (setq as_a (cons 50 (/ pi 2.0))) ;starting angle for arcs
      (progn
            (if (or (= att_fit "el45lr")(= att_fit "el45sr")) ; 45 degree elbows
                (setq as_a (cons 50 (* pi 0.75))) ;starting angle for arcs
            )
      )
 )
 (setq ae_a (cons 51 pi) ;ending angle for arcs
       cp (cons 10 cp) ;center point for arcs
 )
;
;----------* create entity data
;
;
;--------* block information first if block exists use it else create
;
(setq att_bname (strcat "m" att_fit "c-sd" (rtos (* 100 pd) 2 0))
      att_bn att_bname
)
(if (/= (tblsearch "block" att_bname) nil)
    (progn
          (command "_.insert" att_bname inp "" "" pause)
    )
    (progn;progn to contain block creation start
          (setq att_bname (cons 2 att_bname)
                b_lyr (cons 8 "0")
                b_inp (cons 10 inp)
                att_fol (cons 70 0)
                bl_data (list '(0 . "BLOCK")
                               att_bname
                               b_inp
                               att_fol
                        )
          )
;
;
;--------* block stuff is done
;
 (if (<= pd 6.0)
     (progn
           (setq as_m (list '(0 . "arc") cp as_r as_a ae_a)) ;single arc data
     )
     (progn
           (setq a1_m (list '(0 . "arc") cp a1_r as_a ae_a) ;arc 1 data
                 a2_m (list '(0 . "arc") cp a2_r as_a ae_a) ;arc 2 data
           )
     )
  )
 ;insert entities into autocad database and create selection set
 (setq es_1 (ssadd));create selection set es_1
          (entmake bl_data)
 (if (<= pd 6.0)
     (progn
           (entmake as_m) ;insert single arc
           (setq tmp_1 (entlast)) ;place entities data in variable tmp_1
     )
     (progn
           (entmake a1_m) ;insert arc 1 data
           (setq tmp_1 (entlast)) ;place entities data in variable tmp_1
           (entmake a2_m) ;insert arc 2 data
     )
 )
 (mk_cpl inp_2 0.0)
 (if (or (= att_fit "el90lr")(= att_fit "el90sr"))
     (progn
           (mk_cpl fp_2 (* pi 0.5))
     )
     (progn
           (if (or (= att_fit "el45lr")(= att_fit "el45sr"))
               (progn
                     (mk_cpl fp_2 (* pi 0.75))
               )
           )
      )
 )
         (entmake '((0 . "ENDBLK")))
         (command "_.insert" att_bn inp "" "" pause)
    ); end progn to decide if create
 )
 (reset nil)
 (princ)
)
;
;--------* elbow down coupling insert
;
(defun eldcp (/ inp inp_2 inp_3 cp pd_2 pd_3 pf_id pf_ia pf_1 pf_2
                 ps_p pe_p ps_p2 pe_p2 ps_s ps_e p1_s p1_e p2_s p2_e
                 fc_1 fr_1 fa_1 fa_2 pr_1 pa_1 pa_2 ps_m p1_m p2_m f2_m
                 p3_m es_1 tmp_1 e_ans sz_ls drv con com rep_1 ctls ls_t1
                 sz_ls2 ls_t2 id me_ft dr_it tile last_pic f_sz2 args)
;
;--------* add locals to args list
;
(setq args '(inp inp_2 inp_3 cp pd_2 pd_3 pf_id pf_ia pf_1 pf_2
              ps_p pe_p ps_p2 pe_p2 ps_s ps_e p1_s p1_e p2_s p2_e
              fc_1 fr_1 fa_1 fa_2 pr_1 pa_1 pa_2 ps_m p1_m p2_m f2_m
              p3_m es_1 tmp_1 e_ans sz_ls drv con com rep_1 ctls ls_t1
              sz_ls2 ls_t2 id me_ft dr_it tile last_pic f_sz2 args)
)
;
;--------* reset variables to real numbers
;
(setq cw (atof att_cw)
      cd (atof att_cd)
      pd (atof sz_1)
      l1 (atof att_cte)
)
;
;--------* Get Input from user
;
(command ".ucs" "w")
(setq  inp (getpoint "\nInsertion point : ")
       inp_2 (polar inp 0.0 cw)
       inp_3 (polar inp pi (* cw 0.5))
       cp (polar inp 0.0 l1)
       pd_2 (* (/ pd 2.0)(/ pd 2.0))
       pd_3 (* (/ cd 2.0)(/ cd 2.0))
       pf_id (sqrt (- pd_3 pd_2))
       pf_ia (atan (/ (/ pd 2.0) pf_id)) ;angle from cp to pipe flange intercept
       pf_1 (polar cp (- pi pf_ia) (* cd 0.5)) ;locate pipe flange intercept 1
       pf_2 (polar cp (+ pf_ia pi) (* cd 0.5)) ;locate pipe flange intercept 2
       ps_p (polar inp_2 (* pi 0.5) (* pd 0.5))
       pe_p (polar ps_p (* pi 1.5) pd)
       ps_p2 (polar ps_p 0.0 (- l1 (* cw 0.5)))
       pe_p2 (polar pe_p 0.0 (- l1 (* cw 0.5)))
 )
;associate points to the appropriate dxf codes
(if (<= pd 6.0)
    (progn
          (setq ps_s (cons 10 inp_2)
                ps_e (cons 11 cp)
          )
    )
    (progn
          (setq p1_s (cons 10 ps_p) ;pipe line one start
                p1_e (cons 11 ps_p2) ;pipe line one end
                p2_s (cons 10 pe_p) ;pipe line two start
                p2_e (cons 11 pe_p2) ;pipe line two end
          )
    )
)
(setq fc_1 (cons 10 cp) ;center point of circular pipe/flanges
      fr_1 (cons 40 (/ cd 2.0)) ;radius of flange
      fa_1 (cons 50 (+ pi pf_ia)) ;flange arc start angle
      fa_2 (cons 51 (- pi pf_ia)) ;flange arc end angle
      pr_1 (cons 40 (/ pd 2.0)) ;radius of pipe arc
      pa_1 (cons 50 (* pi 1.5)) ;pipe arc start angle
      pa_2 (cons 51 (* pi 0.5)) ;pipe arc end angle
 )
;--------* create entity data
;
;--------* block information first if block exists use it else create
;
(setq att_bname (strcat "m" att_fit "f-dd" (rtos (* 100 pd) 2 0))
      att_bn att_bname
)
(if (/= (tblsearch "block" att_bname) nil)
    (progn
          (command "_.insert" att_bname inp "" "" pause)
    )
    (progn;progn to contain block creation start
          (setq att_bname (cons 2 att_bname)
                b_lyr (cons 8 "0")
                b_inp (cons 10 inp)
                att_fol (cons 70 0)
                bl_data (list '(0 . "BLOCK")
                               att_bname
                               b_inp
                               att_fol
                        )
          )
;
;
;--------* block stuff is done
;
 (if (<= pd 6.0)
     (progn
           (setq ps_m (list '(0 . "line") ps_s ps_e))
     )
     (progn
           (setq p1_m (list '(0 . "line") p1_s p1_e) ;data for pipe line one
                 p2_m (list '(0 . "line") p2_s p2_e) ;data for pipe line two
           )
     )
 )
 (setq f2_m (list '(0 . "arc") fc_1 fr_1 fa_1 fa_2) ;data for flange arc
       p3_m (list '(0 . "arc") fc_1 pr_1 pa_1 pa_2) ;data for pipe arc
 )
 ;insert entities into autocad database
 (setq es_1 (ssadd))
          (entmake bl_data)
 (if (<= pd 6.0)
     (progn
           (entmake ps_m)
           (setq tmp_1 (entlast))
     )
     (progn
           (entmake p1_m) ;insert pipe line one
           (setq tmp_1 (entlast))
           (entmake p2_m) ;insert pipe line two
           (entmake p3_m) ;insert pipe arc
     )
 )
 (entmake f2_m) ;insert flange arc
(mk_cpl inp_2 (* pi 1.5))
         (entmake '((0 . "ENDBLK")))
         (command "_.insert" att_bn inp "" "" pause)
    ); end progn to decide if create
 )
 (reset nil)
 (princ)
)
;
;--------* elbow down center line insert
;
(defun eldcl (/ inp inp_3 cp pd_2 pd_3 pf_id pf_ia pf_1 pf_2
                 ps_p pe_p ps_p2 pe_p2 ps_s ps_e p1_s p1_e p2_s p2_e
                 fc_1 fr_1 fa_1 fa_2 pr_1 pa_1 pa_2 ps_m p1_m p2_m f2_m
                 p3_m es_1 tmp_1 e_ans sz_ls drv con com rep_1 ctls ls_t1
                 sz_ls2 ls_t2 id me_ft dr_it tile last_pic f_sz2 args)
;
;--------* add locals to args list
;
(setq args '(inp inp_3 cp pd_2 pd_3 pf_id pf_ia pf_1 pf_2
              ps_p pe_p ps_p2 pe_p2 ps_s ps_e p1_s p1_e p2_s p2_e
              fc_1 fr_1 fa_1 fa_2 pr_1 pa_1 pa_2 ps_m p1_m p2_m f2_m
              p3_m es_1 tmp_1 e_ans sz_ls drv con com rep_1 ctls ls_t1
              sz_ls2 ls_t2 id me_ft dr_it tile last_pic f_sz2 args)
)
;
;--------* reset variables to real numbers
;
(setq cw (atof att_cw)
      cd (atof att_cd)
      pd (atof sz_1)
      l1 (atof att_cte)
)
;
;--------* Get Input from user
;
(command ".ucs" "w")
(setq  inp (getpoint "\nInsertion point : ")
       inp_3 (polar inp pi l1)
       pd_2 (* (/ pd 2.0)(/ pd 2.0))
       pd_3 (* (/ cd 2.0)(/ cd 2.0))
       pf_id (sqrt (- pd_3 pd_2))
       pf_ia (atan (/ (/ pd 2.0) pf_id)) ;angle from cp to pipe flange intercept
       pf_1 (polar inp (- pi pf_ia) (* cd 0.5)) ;locate pipe flange intercept 1
       pf_2 (polar inp (+ pf_ia pi) (* cd 0.5)) ;locate pipe flange intercept 2
       ps_p (polar inp (* pi 0.5) (* pd 0.5))
       pe_p (polar ps_p (* pi 1.5) pd)
       ps_p2 (polar ps_p pi (- l1 cw))
       pe_p2 (polar pe_p pi (- l1 cw))
 )
;associate points to the appropriate dxf codes
(if (<= pd 6.0)
    (progn
          (setq ps_s (cons 10 inp_3)
                ps_e (cons 11 inp)
          )
    )
    (progn
          (setq p1_s (cons 10 ps_p) ;pipe line one start
                p1_e (cons 11 ps_p2) ;pipe line one end
                p2_s (cons 10 pe_p) ;pipe line two start
                p2_e (cons 11 pe_p2) ;pipe line two end
          )
    )
)
(setq fc_1 (cons 10 inp) ;center point of circular pipe/flanges
      fr_1 (cons 40 (/ cd 2.0)) ;radius of flange
      fa_1 (cons 50 (+ pi pf_ia)) ;flange arc start angle
      fa_2 (cons 51 (- pi pf_ia)) ;flange arc end angle
      pr_1 (cons 40 (/ pd 2.0)) ;radius of pipe arc
      pa_1 (cons 50 (* pi 1.5)) ;pipe arc start angle
      pa_2 (cons 51 (* pi 0.5)) ;pipe arc end angle
 )
;
;--------* create entity creation date
;
;--------* block information first if block exists use it else create
;
(setq att_bname (strcat "m" att_fit "c-dd" (rtos (* 100 pd) 2 0))
      att_bn att_bname
)
(if (/= (tblsearch "block" att_bname) nil)
    (progn
          (command "_.insert" att_bname inp "" "" pause)
    )
    (progn;progn to contain block creation start
          (setq att_bname (cons 2 att_bname)
                b_lyr (cons 8 "0")
                b_inp (cons 10 inp)
                att_fol (cons 70 0)
                bl_data (list '(0 . "BLOCK")
                               att_bname
                               b_inp
                               att_fol
                        )
          )
;
;
;--------* block stuff is done
;
 (if (<= pd 6.0)
     (progn
           (setq ps_m (list '(0 . "line") ps_s ps_e))
     )
     (progn
           (setq p1_m (list '(0 . "line") p1_s p1_e) ;data for pipe line one
                 p2_m (list '(0 . "line") p2_s p2_e) ;data for pipe line two
           )
     )
 )
 (setq f2_m (list '(0 . "arc") fc_1 fr_1 fa_1 fa_2) ;data for flange arc
       p3_m (list '(0 . "arc") fc_1 pr_1 pa_1 pa_2) ;data for pipe arc
 )
 ;insert entities into autocad database
 (setq es_1 (ssadd))
          (entmake bl_data)
 (if (<= pd 6.0)
     (progn
           (entmake ps_m)
           (setq tmp_1 (entlast))
     )
     (progn
           (entmake p1_m) ;insert pipe line one
           (setq tmp_1 (entlast))
           (entmake p2_m) ;insert pipe line two
           (entmake p3_m) ;insert pipe arc
     )
 )
 (entmake f2_m) ;insert flange arc
 (mk_cpl inp_3 (* pi 0.5))
         (entmake '((0 . "ENDBLK")))
         (command "_.insert" att_bn inp "" "" pause)
    ); end progn to decide if create
 )
 (reset nil)
 (princ)
)
;
;--------* elbow up coupling insert
;
(defun elucp (/ inp inp_2 inp_3 cp pd_2 pd_3 pf_id pf_ia pf_1 pf_2
                 ps_p2 pe_p2 ps_s ps_e p1_s p1_e p2_s p2_e fc_1 fr_1
                 pr_1 ps_m p1_m p2_m f2_m p3_m es_1 tmp_1 e_ans
                 sz_ls drv con com rep_1 ctls ls_t1 sz_ls2
                 ls_t2 id me_ft dr_it tile last_pic f_sz2 args)
;
;--------* add locals to args list
;
(setq args '(inp inp_2 inp_3 cp pd_2 pd_3 pf_id pf_ia pf_1 pf_2
              ps_p2 pe_p2 ps_s ps_e p1_s p1_e p2_s p2_e fc_1 fr_1
              pr_1 ps_m p1_m p2_m f2_m p3_m es_1 tmp_1 e_ans
              sz_ls drv con com rep_1 ctls ls_t1 sz_ls2
              ls_t2 id me_ft dr_it tile last_pic f_sz2 args)
)
;
;--------* reset variables to real numbers
;
(setq cw (atof att_cw)
      cd (atof att_cd)
      pd (atof sz_1)
      l1 (atof att_cte)
)
;
;--------* Get Input from user
;
(command ".ucs" "w")
(setq  inp (getpoint "\nInsertion point : ")
       inp_2 (polar inp 0.0 cw)
       inp_3 (polar inp pi cw)
       cp (polar inp 0.0 l1)
       pd_2 (* (/ pd 2.0)(/ pd 2.0))
       pd_3 (* (/ cd 2.0)(/ cd 2.0))
       pf_id (sqrt (- pd_3 pd_2))
       pf_ia (atan (/ (/ pd 2.0) pf_id)) ;angle from cp to pipe flange intercept
       pf_1 (polar cp (- pi pf_ia) (* cd 0.5)) ;locate pipe flange intercept 1
       pf_2 (polar cp (+ pf_ia pi) (* cd 0.5)) ;locate pipe flange intercept 2
       ps_p2 (polar inp_2 (* pi 0.5)(* pd 0.5))
       pe_p2 (polar ps_p2 (* pi 1.5) pd)
 )
;associate points to the appropriate dxf codes
(if (<= pd 6.0)
    (progn
          (setq ps_s (cons 10 inp_2)
                ps_e (cons 11 (polar cp pi (/ cd 2.0)))
          )
    )
    (progn
          (setq p1_s (cons 10 pf_1) ;pipe line one start
                p1_e (cons 11 ps_p2) ;pipe line one end
                p2_s (cons 10 pf_2) ;pipe line two start
                p2_e (cons 11 pe_p2) ;pipe line two end
          )
    )
)
(setq fc_1 (cons 10 cp) ;center point of circular pipe/flanges
      fr_1 (cons 40 (/ cd 2.0)) ;radius of flange
      pr_1 (cons 40 (/ pd 2.0)) ;radius of pipe arc
 )
;
;--------* create entity creation date
;
;--------* block information first if block exists use it else create
;
(setq att_bname (strcat "m" att_fit "f-ud" (rtos (* 100 pd) 2 0))
      att_bn att_bname
)
(if (/= (tblsearch "block" att_bname) nil)
    (progn
          (command "_.insert" att_bname inp "" "" pause)
    )
    (progn;progn to contain block creation start
          (setq att_bname (cons 2 att_bname)
                b_lyr (cons 8 "0")
                b_inp (cons 10 inp)
                att_fol (cons 70 0)
                bl_data (list '(0 . "BLOCK")
                               att_bname
                               b_inp
                               att_fol
                        )
          )
;
;
;--------* block stuff is done
;
 (if (<= pd 6.0)
     (progn
           (setq ps_m (list '(0 . "line") ps_s ps_e))
     )
     (progn
           (setq p1_m (list '(0 . "line") p1_s p1_e) ;data for pipe line one
                 p2_m (list '(0 . "line") p2_s p2_e) ;data for pipe line two
           )
     )
 )
 (setq f2_m (list '(0 . "circle") fc_1 fr_1) ;data for flange arc
       p3_m (list '(0 . "circle") fc_1 pr_1) ;data for pipe arc
 )
 ;insert entities into autocad database
 (setq es_1 (ssadd))
          (entmake bl_data)
 (if (<= pd 6.0)
     (progn
           (entmake ps_m)
           (setq tmp_1 (entlast))
     )
     (progn
           (entmake p1_m) ;insert pipe line one
           (setq tmp_1 (entlast))
           (entmake p2_m) ;insert pipe line two
     )
 )
 (entmake f2_m) ;insert flange arc
 (entmake p3_m) ;insert pipe arc
 (mk_cpl inp_2 (* pi 1.5))
         (entmake '((0 . "ENDBLK")))
         (command "_.insert" att_bn inp "" "" pause)
    ); end progn to decide if create
 )
 (reset nil)
 (princ)
)
;
;--------* elbow up center line insert
;
(defun elucl (/ inp inp_3 pd_2 pd_3 pf_id pf_ia pf_1 pf_2 ps_p pe_p
                 ps_p2 pe_p2 ps_s ps_e p1_s p1_e p2_s p2_e fc_1 fr_1
                 pr_1 ps_m p1_m p2_m f2_m p3_m es_1 tmp_1 e_ans
                 sz_ls drv con com rep_1 ctls ls_t1 sz_ls2
                 ls_t2 id me_ft dr_it tile last_pic f_sz2 args)
;
;--------* add locals to args list
;
(setq args '(inp inp_3 pd_2 pd_3 pf_id pf_ia pf_1 pf_2 ps_p pe_p
              ps_p2 pe_p2 ps_s ps_e p1_s p1_e p2_s p2_e fc_1 fr_1
              pr_1 ps_m p1_m p2_m f2_m p3_m es_1 tmp_1 e_ans
              sz_ls drv con com rep_1 ctls ls_t1 sz_ls2
              ls_t2 id me_ft dr_it tile last_pic f_sz2 args)
)
;
;--------* reset variables to real numbers
;
(setq cw (atof att_cw)
      cd (atof att_cd)
      pd (atof sz_1)
      l1 (atof att_cte)
)
;
;--------* Get Input from user
;
(command ".ucs" "w")
(setq  inp (getpoint "\nInsertion point : ")
       inp_3 (polar inp pi l1)
       pd_2 (* (/ pd 2.0)(/ pd 2.0))
       pd_3 (* (/ cd 2.0)(/ cd 2.0))
       pf_id (sqrt (- pd_3 pd_2))
       pf_ia (atan (/ (/ pd 2.0) pf_id)) ;angle from cp to pipe flange intercept
       pf_1 (polar inp (- pi pf_ia) (* cd 0.5)) ;locate pipe flange intercept 1
       pf_2 (polar inp (+ pf_ia pi) (* cd 0.5)) ;locate pipe flange intercept 2
       ps_p (polar inp (* pi 0.5) (* pd 0.5))
       pe_p (polar ps_p (* pi 1.5) pd)
       ps_p2 (polar ps_p pi (- l1 cw))
       pe_p2 (polar pe_p pi (- l1 cw))
 )
;associate points to the appropriate dxf codes
(if (<= pd 6.0)
    (progn
          (setq ps_s (cons 10 (polar inp_3 0.0 cw))
                ps_e (cons 11 (polar inp pi (/ cd 2.0)))
          )
    )
    (progn
          (setq p1_s (cons 10 pf_1) ;pipe line one start
                p1_e (cons 11 ps_p2) ;pipe line one end
                p2_s (cons 10 pf_2) ;pipe line two start
                p2_e (cons 11 pe_p2) ;pipe line two end
          )
    )
)
(setq fc_1 (cons 10 inp) ;center point of circular pipe/flanges
      fr_1 (cons 40 (/ cd 2.0)) ;radius of flange
      pr_1 (cons 40 (/ pd 2.0)) ;radius of pipe arc
 )
;
;--------* create entity creation date
;
;--------* block information first if block exists use it else create
;
(setq att_bname (strcat "m" att_fit "c-ud" (rtos (* 100 pd) 2 0))
      att_bn att_bname
)
(if (/= (tblsearch "block" att_bname) nil)
    (progn
          (command "_.insert" att_bname inp "" "" pause)
    )
    (progn;progn to contain block creation start
          (setq att_bname (cons 2 att_bname)
                b_lyr (cons 8 "0")
                b_inp (cons 10 inp)
                att_fol (cons 70 0)
                bl_data (list '(0 . "BLOCK")
                               att_bname
                               b_inp
                               att_fol
                        )
          )
;
;
;--------* block stuff is done
;
 (if (<= pd 6.0)
     (progn
           (setq ps_m (list '(0 . "line") ps_s ps_e))
     )
     (progn
           (setq p1_m (list '(0 . "line") p1_s p1_e) ;data for pipe line one
                 p2_m (list '(0 . "line") p2_s p2_e) ;data for pipe line two
           )
     )
 )
 (setq f2_m (list '(0 . "circle") fc_1 fr_1) ;data for flange arc
       p3_m (list '(0 . "circle") fc_1 pr_1) ;data for pipe arc
 )
 ;insert entities into autocad database
 (setq es_1 (ssadd))
          (entmake bl_data)
 (if (<= pd 6.0)
     (progn
           (entmake ps_m)
           (setq tmp_1 (entlast))
     )
     (progn
           (entmake p1_m) ;insert pipe line one
           (setq tmp_1 (entlast))
           (entmake p2_m) ;insert pipe line two
     )
 )
 (entmake f2_m) ;insert flange arc
 (entmake p3_m) ;insert pipe arc
 (mk_cpl inp_3 (* pi 0.5))
         (entmake '((0 . "ENDBLK")))
         (command "_.insert" att_bn inp "" "" pause)
    ); end progn to decide if create
 )
 (reset nil)
 (princ)
)
;
;--------* elbow up no circles center line insert
;
(defun elucln (/ inp inp_3 pd_2 pd_3 pf_id pf_ia pf_1 pf_2 ps_p pe_p
                  ps_p2 pe_p2 ps_s ps_e p1_s p1_e p2_s p2_e fc_1 fr_1
                  pr_1 ps_m p1_m p2_m f2_m p3_m es_1 tmp_1 e_ans
                  sz_ls drv con com rep_1 ctls ls_t1 sz_ls2
                  ls_t2 id me_ft dr_it tile last_pic f_sz2 args)
;
;--------* add locals to args list
;
(setq args '(inp inp_3 pd_2 pd_3 pf_id pf_ia pf_1 pf_2 ps_p pe_p
              ps_p2 pe_p2 ps_s ps_e p1_s p1_e p2_s p2_e fc_1 fr_1
              pr_1 ps_m p1_m p2_m f2_m p3_m es_1 tmp_1 e_ans
              sz_ls drv con com rep_1 ctls ls_t1 sz_ls2
              ls_t2 id me_ft dr_it tile last_pic f_sz2 args)
)
;
;--------* reset variables to real numbers
;
(setq cw (atof att_cw)
      cd (atof att_cd)
      pd (atof sz_1)
      l1 (atof att_cte)
)
;
;--------* Get Input from user
;
(command ".ucs" "w")
(setq  inp (getpoint "\nInsertion point : ")
       inp_3 (polar inp pi l1)
       pd_2 (* (/ pd 2.0)(/ pd 2.0))
       pd_3 (* (/ cd 2.0)(/ cd 2.0))
       pf_id (sqrt (- pd_3 pd_2))
       pf_ia (atan (/ (/ pd 2.0) pf_id)) ;angle from cp to pipe flange intercept
       pf_1 (polar inp (- pi pf_ia) (* cd 0.5)) ;locate pipe flange intercept 1
       pf_2 (polar inp (+ pf_ia pi) (* cd 0.5)) ;locate pipe flange intercept 2
       ps_p (polar inp (* pi 0.5) (* pd 0.5))
       pe_p (polar ps_p (* pi 1.5) pd)
       ps_p2 (polar ps_p pi (- l1 cw))
       pe_p2 (polar pe_p pi (- l1 cw))
 )
;associate points to the appropriate dxf codes
(if (<= pd 6.0)
    (progn
          (setq ps_s (cons 10 (polar inp_3 0.0 cw))
                ps_e (cons 11 (polar inp pi (/ cd 2.0)))
          )
    )
    (progn
          (setq p1_s (cons 10 pf_1) ;pipe line one start
                p1_e (cons 11 ps_p2) ;pipe line one end
                p2_s (cons 10 pf_2) ;pipe line two start
                p2_e (cons 11 pe_p2) ;pipe line two end
          )
    )
)
(setq fc_1 (cons 10 inp) ;center point of circular pipe/flanges
      fr_1 (cons 40 (/ cd 2.0)) ;radius of flange
      pr_1 (cons 40 (/ pd 2.0)) ;radius of pipe arc
 )
;
;--------* create entity creation date
;
;--------* block information first if block exists use it else create
;
(setq att_bname (strcat "m" att_fit "c-und" (rtos (* 100 pd) 2 0))
      att_bn att_bname
)
(if (/= (tblsearch "block" att_bname) nil)
    (progn
          (command "_.insert" att_bname inp "" "" pause)
    )
    (progn;progn to contain block creation start
          (setq att_bname (cons 2 att_bname)
                b_lyr (cons 8 "0")
                b_inp (cons 10 inp)
                att_fol (cons 70 0)
                bl_data (list '(0 . "BLOCK")
                               att_bname
                               b_inp
                               att_fol
                        )
          )
;
;
;--------* block stuff is done
;
 (if (<= pd 6.0)
     (progn
           (setq ps_m (list '(0 . "line") ps_s ps_e))
     )
     (progn
           (setq p1_m (list '(0 . "line") p1_s p1_e) ;data for pipe line one
                 p2_m (list '(0 . "line") p2_s p2_e) ;data for pipe line two
           )
     )
 )
 (setq f2_m (list '(0 . "circle") fc_1 fr_1) ;data for flange arc
       p3_m (list '(0 . "circle") fc_1 pr_1) ;data for pipe arc
 )
 ;insert entities into autocad database
 (setq es_1 (ssadd))
          (entmake bl_data)
 (if (<= pd 6.0)
     (progn
           (entmake ps_m)
           (setq tmp_1 (entlast))
     )
     (progn
           (entmake p1_m) ;insert pipe line one
           (setq tmp_1 (entlast))
           (entmake p2_m) ;insert pipe line two
     )
 )
 (mk_cpl inp_3 (* pi 0.5))
         (entmake '((0 . "ENDBLK")))
         (command "_.insert" att_bn inp "" "" pause)
    ); end progn to decide if create
 )
 (reset nil)
 (princ)
)
(princ)
