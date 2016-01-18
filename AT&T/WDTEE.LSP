;wdtee
;
;--------* Stone & Webster engineering Corp
;--------* CAD/CAE
;--------* Mechanical Application
;--------* 030896                       jdc
;
;
;--------* this file contains the code to draw split coupling tees
;--------* it is required by the ddtee command
;
;--------* coupling function (mk_cpl2 insertion point angle run(1)\branch(2))
;
(defun mk_cpl2 (ip a_1 me_ct / v1 v2 v3 v4 p_d v1_d v2_d v3_d v4_d
                b_f b_fc p_dm v1_m v2_m v3_m v4_m c_e)

;--------* 1 = run coupling else (2) = branch coupling
;
(if (= me_ct 1)
    (progn
          (setq v1 (polar ip a_1 (/ cd_1 2.0))
                v2 (polar v1 (+ a_1 (* pi 1.5)) cw_1)
                v3 (polar v2 (+ a_1 pi) cd_1)
                v4 (polar v3 (+ a_1 (* pi 0.5)) cw_1)
          )
    )
    (progn
          (setq v1 (polar ip a_1 (/ cd_2 2.0))
                v2 (polar v1 (+ a_1 (* pi 1.5)) cw_2)
                v3 (polar v2 (+ a_1 pi) cd_2)
                v4 (polar v3 (+ a_1 (* pi 0.5)) cw_2)
          )
    )
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
;----------* create entity data
 (setq p_dm (list '(0 . "polyline") p_d)
          v1_m (list '(0 . "vertex") v1_d)
          v2_m (list '(0 . "vertex") v2_d)
          v3_m (list '(0 . "vertex") v3_d)
          v4_m (list '(0 . "vertex") v4_d)
          c_e (list '(0 . "seqend"))
 )
 (foreach cr_it '(p_dm v4_m v3_m v2_m v1_m c_e) (entmake (eval cr_it)))
 (princ)
)
;
;--------* Draw side tee
;
(defun teescl ( / inp fp_1 fp_2 fp_3 p_s1 p_s2 p_s3 p_e1 p_e2 p_e3 p_m2 p_m3
                  tmp_1 es_1 e_ans p_t1 p_t2 p_t3 p_mka p_mkz v1 v2 v3 v4 v5 v6
                  v1_m v2_m v3_m v4_m v5_m v6_m args2)
;
;----------* Add locals to args list
;
(setq args2 '(inp fp_1 fp_2 fp_3 p_s1 p_s2 p_s3 p_e1 p_e2 p_e3 p_m2 p_m3
              tmp_1 es_1 e_ans r_1 br_1 cd_1 cw_1 cd_2 cw_2 pd_1 pd_2
              p_t1 p_t2 p_t3 p_mka p_mkz args2)
)
(setq args (append args2 args))
(setq me_d '(r_1 br_1 cw_1 cw_2 pd_1 pd_2 cd_1 cd_2 me_d))
;
;----------* Get input from user
; 
(command ".ucs" "w")
(setvar "osmode" h_osm)
  (setq inp (getpoint "\nInsertion point : "))
  (setvar "osmode" 0)
  (setq p_t1 (polar inp pi (* 0.5 l_1))
        p_t2 (polar p_t1 0.0 l_1)
        p_t3 (polar inp (* 0.5 pi) br_1)
        p_s1 (polar p_t1 (* 1.5 pi)(* 0.5 pd_1))
        p_e1 (polar p_s1 0.0 l_1)
        p_s2 (polar p_t1 (* 0.5 pi)(* 0.5 pd_1))
        p_m2 (polar p_s2 0.0 (- (* 0.5 l_1)(* 0.5 pd_2)))
        p_e2 (polar p_m2 (* 0.5 pi)(- br_1 (* 0.5 pd_1)))
        p_s3 (polar p_s2 0.0 l_1)
        p_m3 (polar p_m2 0.0 pd_2)
        p_e3 (polar p_e2 0.0 pd_2)
  )
  (if (<= pd_1 6.0)
      (progn
            (if (and (> pd_1 3.0)(<= pd_1 6.0))
                (setq d_dia 3.0)
                (setq d_dia 1.5)         
            )     
            (setq v1 (polar p_t1 pi (* d_dia 0.25))
                  v2 (polar p_t1 0.0 (* d_dia 0.25))
                  v3 (polar p_t2 pi (* d_dia 0.25))
                  v4 (polar p_t2 0.0 (* d_dia 0.25))
                  v5 (polar p_t3 pi (* d_dia 0.25))
                  v6 (polar p_t3 0.0 (* d_dia 0.25))
                  v_bf (cons 42 1.0)
                  v_sw (cons 40 (* d_dia 0.5))
                  v_ew (cons 41 (* d_dia 0.5))
                  p_d (cons 70 1.0)
            )
      )
  )    
;
;--------* associate data with dxf codes and create entity data
;
;
;--------* block information first
;
(setq att_bname (strcat "mptws00f-" sz_1)
      att_bn att_bname
)
(if (/= (tblsearch "block" att_bname) nil)
    (progn
          (command "_.insert" att_bname inp "" "" pause)
    )
    (progn
          (setq att_bname (cons 2 att_bname)
                b_lyr (cons 8 "0")
                b_inp (cons 10 inp)
                att_fol (cons 70 0)
                bl_data (list '(0 . "block")
                               att_bname
                               b_lyr
                               b_inp
                               att_fol
                        )
          )              
          (if (<= pd_1 6.0)
              (progn 
                    (setq p_t1 (list '(0 . "vertex")(cons 10 p_t1) b_lyr)
                          p_t2 (list '(0 . "vertex")(cons 10 p_t2) b_lyr)
                          p_t3 (list '(0 . "vertex")(cons 10 p_t3) b_lyr)
                          inp_t (list '(0 . "vertex")(cons 10 inp) b_lyr)
                          v1_m (list '(0 . "vertex") (cons 10 v1) v_sw v_ew v_bf b_lyr)
                          v2_m (list '(0 . "vertex") (cons 10 v2) v_sw v_ew v_bf b_lyr)
                          v3_m (list '(0 . "vertex") (cons 10 v3) v_sw v_ew v_bf b_lyr)
                          v4_m (list '(0 . "vertex") (cons 10 v4) v_sw v_ew v_bf b_lyr)
                          v5_m (list '(0 . "vertex") (cons 10 v5) v_sw v_ew v_bf b_lyr)
                          v6_m (list '(0 . "vertex") (cons 10 v6) v_sw v_ew v_bf b_lyr)
                    )
              )
              (progn
                    (setq p_s1 (list '(0 . "vertex")(cons 10 p_s1) b_lyr)
                          p_e1 (list '(0 . "vertex")(cons 10 p_e1) b_lyr)
                          p_s2 (list '(0 . "vertex")(cons 10 p_s2) b_lyr)
                          p_m2 (list '(0 . "vertex")(cons 10 p_m2) b_lyr)
                          p_e2 (list '(0 . "vertex")(cons 10 p_e2) b_lyr)
                          p_s3 (list '(0 . "vertex")(cons 10 p_s3) b_lyr)
                          p_m3 (list '(0 . "vertex")(cons 10 p_m3) b_lyr)
                          p_e3 (list '(0 . "vertex")(cons 10 p_e3) b_lyr)
                          p_d (cons 70 1.0)
                    )
              )
          )
;
;--------* create entity data
;
          (setq p_mka (list '(0 . "polyline") p_d b_lyr)
                p_mkb (list '(0 . "polyline") b_lyr)
                p_mkz (list '(0 . "seqend"))
          )
          (entmake bl_data)
          (if (<= pd_1 6.0)
              (progn
                   (foreach cr_it '(p_mkb p_t1 p_t2 p_mkz) (entmake (eval cr_it)))
                   (foreach cr_it '(p_mkb p_t3 inp_t p_mkz) (entmake (eval cr_it)))
                   (foreach cr_it '(p_mka v1_m v2_m p_mkz) (entmake (eval cr_it)))
                   (foreach cr_it '( p_mka v3_m v4_m p_mkz) (entmake (eval cr_it)))
                   (foreach cr_it '(p_mka v5_m v6_m p_mkz) (entmake (eval cr_it)))                
              )
              (progn
                    (foreach cr_it '(p_mka p_s1 p_e1 p_s3 p_m3 p_e3 p_e2 p_m2 p_s2 p_mkz)
                                    (entmake (eval cr_it)) 
                    )
              )
          )
          (entmake '((0 . "endblk")))
          (command "_.insert" att_bn inp "" "" pause)
          (foreach me_d2 me_d (set me_d2 nil))
          (setq me_d2 nil)
    )
)
(reset nil)
(princ)
);end teescl sub-routine
;
;--------* Draw up tee
;
(defun teeucl ( / inp fp_1 fp_2 fp_3 p_s1 p_s2 p_s3 p_e1 p_e2 p_e3 sq_pd sq_cd
                 pf_id pf_ia pf_1 pf_2 pf_3 pf_4 me_d me_d2 tmp_1 es_1 e_ans
                 b_c1 b_c2 args2 p_mka p_mkz args)
;
;----------* Add locals to args list
;
(setq args2 '(inp p_t1 p_t2  p_s1 p_s2 p_s3 p_e1 p_e2 p_e3 sq_pd sq_cd
              pf_id pf_ia pf_1 pf_2 pf_3 pf_4 b_c1 b_cd me_d me_d2
              tmp_1 es_1 e_ans r_1 br_1 cd_1 cw_1 cd_2 cw_2 pd_1 pd_2
              p_t1 p_t2 p_t3 args2 p_mka p_mkz cr_it args2)
)
(setq args (append args2 args))
(setq me_d '(r_1 br_1 cw_1 cw_2 pd_1 pd_2 cd_1 cd_2 me_d))
;
;----------* Get input from user
; 
(command ".ucs" "w")
(setvar "osmode" h_osm)
  (setq inp (getpoint "\nInsertion point : "))
  (setvar "osmode" 0)
  (setq p_t1 (polar inp pi (* 0.5 l_1))
        p_t2 (polar p_t1 0.0 l_1)
        p_s1 (polar p_t1 (* 1.5 pi)(* 0.5 pd_1))
        p_e1 (polar p_s1 0.0 l_1)
        p_s2 (polar p_t1 (* 0.5 pi)(* 0.5 pd_1))
        p_e2 (polar p_e1 (* 0.5 pi) pd_1)
        p_s3 (polar inp pi (* 0.5 pd_2))
        p_e3 (polar inp 0.0 (* 0.5 pd_2))
  )
    (if (<= pd_1 6.0)
      (progn
            (if (and (> pd_1 3.0)(<= pd_1 6.0))
                (setq d_dia 3.0)
                (setq d_dia 1.5)         
            )     
            (setq v1 (polar p_t1 pi (* d_dia 0.25))
                  v2 (polar p_t1 0.0 (* d_dia 0.25))
                  v3 (polar p_t2 pi (* d_dia 0.25))
                  v4 (polar p_t2 0.0 (* d_dia 0.25))
                  v_bf (cons 42 1.0)
                  v_sw (cons 40 (* d_dia 0.5))
                  v_ew (cons 41 (* d_dia 0.5))
                  p_d (cons 70 1.0)
            )
      )
  )
;
;--------* associate data with dxf codes and create entity data
;
;
;--------* block information first
;
(setq att_bname (strcat "mptwu00f-" sz_1)
      att_bn att_bname
)
(if (/= (tblsearch "block" att_bname) nil)
    (progn
          (command "_.insert" att_bname inp "" "" pause)
    )
    (progn
          (setq att_bname (cons 2 att_bname)
                b_lyr (cons 8 "0")
                b_inp (cons 10 inp)
                att_fol (cons 70 0)
                bl_data (list '(0 . "block")
                               att_bname
                               b_lyr
                               b_inp
                               att_fol
                        )
          )
          (if (<= pd_1 6.0)
              (progn
                   (setq p_t1 (list '(0 . "vertex")(cons 10 p_t1) b_lyr)
                         p_t2 (list '(0 . "vertex")(cons 10 p_t2) b_lyr)
                         p_s3 (list '(0 . "vertex")(cons 10 p_s3) b_lyr)
                         p_e3 (list '(0 . "vertex")(cons 10 p_e3) b_lyr)
                         b_c1 (list '(0 . "circle")(cons 10 inp)(cons 40 (/ pd_2 2.0)) b_lyr)
                         v1_m (list '(0 . "vertex") (cons 10 v1) v_sw v_ew v_bf b_lyr)
                         v2_m (list '(0 . "vertex") (cons 10 v2) v_sw v_ew v_bf b_lyr)
                         v3_m (list '(0 . "vertex") (cons 10 v3) v_sw v_ew v_bf b_lyr)
                         v4_m (list '(0 . "vertex") (cons 10 v4) v_sw v_ew v_bf b_lyr)
                   )
              )
              (progn
                   (setq p_s1 (list '(0 . "vertex")(cons 10 p_s1) b_lyr)
                         p_e1 (list '(0 . "vertex")(cons 10 p_e1) b_lyr)
                         p_s2 (list '(0 . "vertex")(cons 10 p_s2) b_lyr)
                         p_e2 (list '(0 . "vertex")(cons 10 p_e2) b_lyr)
                         b_c1 (list '(0 . "circle")(cons 10 inp)(cons 40 (/ pd_2 2.0)) b_lyr)
                   )
              )
          )
;
;--------* create entity data
;
          (setq p_mka (list '(0 . "polyline")(cons 70 1.0) b_lyr)
                p_mkb (list '(0 . "polyline") b_lyr)
                p_mkz (list '(0 . "seqend"))
          )
          (entmake bl_data)
          (if (<= pd_1 6.0)
              (progn
                   (foreach cr_it '(p_mkb p_t1 p_s3 p_mkz) (entmake (eval cr_it)))
                   (foreach cr_it '(p_mkb p_e3 p_t2 p_mkz b_c1) (entmake (eval cr_it)))
                   (foreach cr_it '(p_mka v1_m v2_m p_mkz) (entmake (eval cr_it)))
                   (foreach cr_it '( p_mka v3_m v4_m p_mkz) (entmake (eval cr_it)))
              )
              (progn
                   (foreach cr_it '(p_mka p_s1 p_e1 p_e2 p_s2 p_mkz b_c1)(entmake (eval cr_it)))
              )
          )
         ; )
          (entmake '((0 . "endblk")))
          (command "_.insert" att_bn inp "" "" pause)
          (foreach me_d2 me_d (set me_d2 nil))
          (setq me_d2 nil)
    )
)
(reset nil)
(princ)
);end teeucl sub-routine
;
;--------* Draw down tee
;
(defun teedcl ( / inp fp_1 fp_2 fp_3 p_s1 p_s2 p_s3 p_e1 p_e2 p_e3 sq_pd sq_cd
                 pf_id pf_ia pf_1 pf_2 pf_3 pf_4 me_d me_d2 tmp_1 es_1 e_ans
                 b_c1 b_c2 args2 p_mka p_mkz args)
;
;----------* Add locals to args list
;
(setq args2 '(inp p_t1 p_t2  p_s1 p_s2 p_s3 p_e1 p_e2 p_e3 sq_pd sq_cd
              pf_id pf_ia pf_1 pf_2 pf_3 pf_4 b_c1 b_cd me_d me_d2
              tmp_1 es_1 e_ans r_1 br_1 cd_1 cw_1 cd_2 cw_2 pd_1 pd_2
              p_t1 p_t2 p_t3 args2 p_mka p_mkz cr_it args2)
)
(setq args (append args2 args))
(setq me_d '(r_1 br_1 cw_1 cw_2 pd_1 pd_2 cd_1 cd_2 me_d))
;
;----------* Get input from user
; 
(command ".ucs" "w")
(setvar "osmode" h_osm)
  (setq inp (getpoint "\nInsertion point : "))
  (setvar "osmode" 0)
  (setq p_t1 (polar inp pi (* 0.5 l_1))
        p_t2 (polar p_t1 0.0 l_1)
        p_s1 (polar p_t1 (* 1.5 pi)(* 0.5 pd_1))
        p_e1 (polar p_s1 0.0 l_1)
        p_s2 (polar p_t1 (* 0.5 pi)(* 0.5 pd_1))
        p_e2 (polar p_e1 (* 0.5 pi) pd_1)
  )
    (if (<= pd_1 6.0)
      (progn
            (if (and (> pd_1 3.0)(<= pd_1 6.0))
                (progn
                     (setq d_dia 3.0
                           sq_pd (* 1.25 1.25)
                     )
                )
                (progn
                     (setq d_dia 1.5
                           sq_pd (* 0.25 0.25)
                     )
                )     
            )     
            (setq v1 (polar p_t1 pi (* d_dia 0.25))
                  v2 (polar p_t1 0.0 (* d_dia 0.25))
                  v3 (polar p_t2 pi (* d_dia 0.25))
                  v4 (polar p_t2 0.0 (* d_dia 0.25))
                  v_bf (cons 42 1.0)
                  v_sw (cons 40 (* d_dia 0.5))
                  v_ew (cons 41 (* d_dia 0.5))
                  p_d (cons 70 1.0)
                  sq_cd (* (/ pd_2 2.0)(/ pd_2 2.0))
                  pf_id (sqrt (abs (- sq_cd sq_pd)))
                  pf_ia (atan (/ 2.5 pf_id))
                  pf_1 (- pi pf_ia)
                  pf_2 (+ pi pf_ia)
                  pf_3 (- (* 2.0 pi) pf_ia)
                  pf_4 pf_ia                  
            )
      )
  )  
;
;--------* associate data with dxf codes and create entity data
;
;
;--------* block information first
;
(setq att_bname (strcat "mptwd00f-" sz_1)
      att_bn att_bname
)
(if (/= (tblsearch "block" att_bname) nil)
    (progn
          (command "_.insert" att_bname inp "" "" pause)
    )
    (progn
          (setq att_bname (cons 2 att_bname)
                b_lyr (cons 8 "0")
                b_inp (cons 10 inp)
                att_fol (cons 70 0)
                bl_data (list '(0 . "block")
                               att_bname
                               b_lyr
                               b_inp
                               att_fol
                        )
          )
          (if (<= pd_1 6.0)
              (progn
                   (setq p_t1 (list '(0 . "vertex")(cons 10 p_t1) b_lyr)
                         p_t2 (list '(0 . "vertex")(cons 10 p_t2) b_lyr)
                         p_s3 (list '(0 . "vertex")(cons 10 p_s3) b_lyr)
                         p_e3 (list '(0 . "vertex")(cons 10 p_e3) b_lyr)
                         b_c1 (list '(0 . "arc")(cons 10 inp)(cons 40 (/ pd_2 2.0))(cons 50 pf_4)(cons 51 pf_1) b_lyr)
                         b_c2 (list '(0 . "arc")(cons 10 inp)(cons 40 (/ pd_2 2.0))(cons 50 pf_2)(cons 51 pf_3) b_lyr)
                         v1_m (list '(0 . "vertex") (cons 10 v1) v_sw v_ew v_bf b_lyr)
                         v2_m (list '(0 . "vertex") (cons 10 v2) v_sw v_ew v_bf b_lyr)
                         v3_m (list '(0 . "vertex") (cons 10 v3) v_sw v_ew v_bf b_lyr)
                         v4_m (list '(0 . "vertex") (cons 10 v4) v_sw v_ew v_bf b_lyr)
                   )
              )
              (progn
                   (setq p_s1 (list '(0 . "vertex")(cons 10 p_s1) b_lyr)
                         p_e1 (list '(0 . "vertex")(cons 10 p_e1) b_lyr)
                         p_s2 (list '(0 . "vertex")(cons 10 p_s2) b_lyr)
                         p_e2 (list '(0 . "vertex")(cons 10 p_e2) b_lyr)
                         ;b_c1 (list '(0 . "arc")(cons 10 inp)(cons 40 (/ cd_2 2.0))(cons 50 pf_4)(cons 51 pf_1))
                         ;b_c2 (list '(0 . "arc")(cons 10 inp)(cons 40 (/ cd_2 2.0))(cons 50 pf_2)(cons 51 pf_3))
                   )
              )
          )
;
;--------* create entity data
;
          (setq p_mka (list '(0 . "polyline")(cons 70 1.0))
                p_mkb (list '(0 . "polyline"))         
                p_mkz (list '(0 . "seqend"))
          )
          (entmake bl_data)
          (if (<= pd_1 6.0)
              (progn
                   (foreach cr_it '(p_mkb p_t1 p_t2 p_mkz) (entmake (eval cr_it)))
                   (foreach cr_it '(b_c1 b_c2) (entmake (eval cr_it)))
                   (foreach cr_it '(p_mka v1_m v2_m p_mkz) (entmake (eval cr_it)))
                   (foreach cr_it '( p_mka v3_m v4_m p_mkz) (entmake (eval cr_it)))
              )
              (progn
                    (foreach cr_it '(p_mka p_s1 p_e1 p_e2 p_s2 p_mkz) (entmake (eval cr_it)))
              )
          )
          (entmake '((0 . "endblk")))
          (command "_.insert" att_bn inp "" "" pause)
          (foreach me_d2 me_d (set me_d2 nil))
          (setq me_d2 nil)
    )
)
(reset nil)
(princ)
);end teedcl sub-routine
(princ)
