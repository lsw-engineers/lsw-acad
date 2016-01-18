;mk_cplg.lsp
;
;--------* Stone & Webster Engineering Corp.
;--------* CAD/CAE
;--------* Mechanical Application
;--------* 012196                       jdc
;
;
;--------* the functions defined in this file are used to create couplings
;--------* used with piping (ex flange, split coupling etc)
;
;--------* flanged coupling
;
(defun mk_cplf (/ v1 v2 v3 v4 p_d v1_d v2_d v3_d v4_d
                b_f b_fc p_dm v1_m v2_m v3_m v4_m c_e)
;
;--------* get information and process
;
(setq inp (getpoint "\nInsertion point :")
      v1 (polar inp (* pi 0.5)(/ cd 2.0))
      v2 (polar v1 pi cw)
      v3 (polar v2 (* pi 1.5) cd)
      v4 (polar v3 0.0 cw)
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
 (foreach cr_it '(p_dm v1_m v2_m v3_m v4_m c_e)(entmake (eval cr_it)))
 (command "_.rotate" "l" "" inp pause)
 (princ)
)
;
;--------* split coupling creator syntax:(mk_cplf insertion point angle) 
;
(defun mk_cpls (/ v1 v2 v3 v4 p_d v1_d v2_d v3_d v4_d
               b_f b_fc p_dm v1_m v2_m v3_m v4_m c_e)
;
;--------* get information and process
;
(setq inp (getpoint "\nInsertion point :")               
      v1 (polar inp (* pi 0.5)(- (/ cd 2.0)(/ cw 2.0)))
      v2 (polar v1 pi cw)
      v3 (polar v2 (* pi 1.5) (- cd cw))
      v4 (polar v3 0.0 cw)
 )
;
;--------* associate data with dxf codes
;
 (setq p_d (cons 70 1.0)
       v1_d (cons 10 v1)
       v2_d (cons 10 v2)
       v3_d (cons 10 v3)
       v4_d (cons 10 v4)
       b_f (cons 42 1.0)
       b_fc (cons 42 0.0)
 )
;
;--------* create entity data
;

 (setq p_dm (list '(0 . "polyline") p_d)
       v1_m (list '(0 . "vertex") v1_d b_f)
       v2_m (list '(0 . "vertex") v2_d)
       v3_m (list '(0 . "vertex") v3_d b_f)
       v4_m (list '(0 . "vertex") v4_d)
       c_e (list '(0 . "seqend"))
 )
 (foreach cr_it '(p_dm v1_m v2_m v3_m v4_m c_e)(entmake (eval cr_it)))
 (command "_.rotate" "l" "" inp pause)
 (princ)
)
;
;--------* weld neck coupling 
;
(defun mk_wdn (/ inp inp_f v1 v2 vf1 vf2 vf3 vf4 v1_d v2_d v3_d vf1d vf2d
                 vf3d vf4d p_d v4_d v5_d v1d v2d v1dm v2dm v_bf v_sw v_ew
                 vf1m vf2m vf3m vf4m v1_m v2_m v3_m v4_m v5_m p_dm c_e)
 (setq inp (getpoint "\nInsertion point :")
       inp_f (polar inp pi (- r1 cw))
       v1 (polar inp_f (* pi 0.5)(* pd 0.5))
       v2 (polar v1 (* 1.5 pi) pd)
       vf1 (polar inp_f (* pi 0.5)(/ cd 2.0))
       vf2 (polar vf1 pi cw)
       vf3 (polar vf2 (* pi 1.5) cd)
       vf4 (polar vf3 0.0 cw)       
 )
 (setq v1_d (cons 10 inp)
       v2_d (cons 10 v1)
       v3_d (cons 10 v2)
       vf1d (cons 10 vf1)
       vf2d (cons 10 vf2)
       vf3d (cons 10 vf3)
       vf4d (cons 10 vf4)
       p_d (cons 70 1.0)
 )
 (if (> pd 6.0)
     (progn
           (setq v4_d (cons 10 (polar inp (* pi 0.5)(* pd 0.5)))
                 v5_d (cons 10 (polar inp (* 1.5 pi)(* pd 0.5)))
           )
     )
     (progn
           (setq d_dia 3.0  ; diameter of donut
                 v1d (polar inp pi (* d_dia 0.25))
                 v2d (polar inp 0.0 (* d_dia 0.25))
                 v1dm (cons 10 v1d)
                 v2dm (cons 10 v2d)
                 v_bf (cons 42 1.0)
                 v_sw (cons 40 (* d_dia 0.5))
                 v_ew (cons 41 (* d_dia 0.5))
           )
     )
 )    
;
;----------* create entity data
;
 (setq p_dm (list '(0 . "polyline") p_d)
          vf1m (list '(0 . "vertex") vf1d)
          vf2m (list '(0 . "vertex") vf2d)
          vf3m (list '(0 . "vertex") vf3d)
          vf4m (list '(0 . "vertex") vf4d)
          v1_m (list '(0 . "vertex") v1_d)
          v2_m (list '(0 . "vertex") v2_d)
          v3_m (list '(0 . "vertex") v3_d)
          c_e (list '(0 . "seqend"))
 )
 (if (> pd 6.0)
     (progn
          (setq v4_m (list '(0 . "vertex") v4_d)
                v5_m (list '(0 . "vertex") v5_d)
          )
     )
     (progn
           (setq v1dm (list '(0 . "vertex") v1dm v_sw v_ew v_bf)
                 v2dm (list '(0 . "vertex") v2dm v_sw v_ew v_bf)
           )      
     )      
 )
 ( if (<= pd 6.0)
      (progn
            (foreach cr_it '(p_dm vf1m vf2m vf3m vf4m c_e p_dm v1_m v2_m v3_m c_e)
                            (entmake (eval cr_it))
            )
            (foreach cr_it '(p_dm v1dm v2dm c_e)(entmake (eval cr_it)))
      )
      (progn
            (foreach cr_it '(p_dm vf1m vf2m vf3m vf4m c_e p_dm v4_m v2_m v3_m v5_m c_e)
                            (entmake (eval cr_it))
            )
      )
 )     
)
(princ)
