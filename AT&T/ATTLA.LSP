(defun c:attla ()
 (setq id (load_dialog "attla.dcl"))
 (if (not (new_dialog "attla" id))(exit))
 (action_tile "dt_rd" "(setq dt $key)")
 (action_tile "dt_pd" "(setq dt $key)")
 (action_tile "tw_d" "(setq tw $key)")
 (action_tile "tw_n" "(setq tw $key)")
 (action_tile "es_m" "(setq es_m 1)")
 (action_tile "es_e" "(setq es_e 1)")
 (action_tile "es_t" "(setq es_t 1)")
 (action_tile "accept" "(setq err 0)(done_dialog 1)")
 (action_tile "cancel" "(setq err 1)(done_dialog 0)")
 (setq dr_it (start_dialog))
 (unload_dialog id)
 (if (= err 0) (do_attla))
 (princ))

(defun do_attla ()
(C:LAYTHW)
(command "layer" "s" "0" "")
(if (and (= dt "dt_rd") (= tw "tw_d"))
	(command "layer" "c" "7" "3y14a*"
			     "c" "7" "3y14d*"
;			     "c" "7" "3y14f*"
                       "f" "3y14df*-t"
			"f" "3y14df*a-bound"
			"f" "3y14df*a-furn-p-acsy"
			"f" "3y14df*a-furn-p-peds"
			"f" "3y14df*a-furn-p-stor"
                       "f" "*neww"
                       "f" "*a-flor-rais*"
                       "f" "*a-clng-note*"
                       "f" "*a-clng-dm*"
                       "f" "*s-slab*"
                       "f" "3y14a*|e-*"
                       "f" "3y14a*|m-*"
                       "f" "3y14a*|p-*"
                       "f" "*door-iden*"
                       "f" "*a-eqpm*" "")
)
(if (and (= dt "dt_rd") (= tw "tw_n"))
	(Command "layer" "c" "7" "3y14a*"
			     "c" "7" "3y14d*"
;			     "c" "7" "3y14f*"
                       "f" "3y14df*-t"
			"f" "3y14df*a-bound"
			"f" "3y14df*a-furn-p-acsy"
			"f" "3y14df*a-furn-p-peds"
			"f" "3y14df*a-furn-p-stor"
                       "f" "*demo"
                       "f" "*a-flor-rais*"
                       "f" "*a-clng-note*"
                       "f" "*a-clng-dm*"
                       "f" "*s-slab*"
                       "f" "3y14a*|e-*"
                       "f" "3y14a*|m-*"
                       "f" "3y14a*|p-*"
                       "f" "*door-iden*"
                       "f" "*a-eqpm*" "")
)
(if (and (= dt "dt_pd") (= tw "tw_d"))
	(Command "layer" "c" "7" "3y14a*"
			     "c" "7" "3y14d*"
;			     "c" "7" "3y14f*"
                       "f" "3y14df*-t"
			"f" "3y14df*a-bound"
			"f" "3y14df*a-furn-p-acsy"
			"f" "3y14df*a-furn-p-peds"
			"f" "3y14df*a-furn-p-stor"
                       "f" "3y14*neww"
                       "f" "*a-flor-rais*"
                       "f" "*a-clng-note*"
                       "f" "*a-clng-dm*"
                       "f" "*s-slab*"
                       "f" "3y14a*|e-*"
                       "f" "3y14a*|m-*"
                       "f" "3y14a*|p-*"
                       "f" "*door-iden*"
                       "f" "*a-eqpm*" "")
)
(if (and (= dt "dt_pd") (= tw "tw_n"))
	(Command "layer" "c" "7" "3y14a*"
			     "c" "7" "3y14d*"
;			     "c" "7" "3y14f*"
                       "f" "3y14df*-t"
			"f" "3y14df*a-bound"
			"f" "3y14df*a-furn-p-acsy"
			"f" "3y14df*a-furn-p-peds"
			"f" "3y14df*a-furn-p-stor"
                       "f" "3y14*demo"
                       "f" "*a-flor-rais*"
                       "f" "*a-clng-note*"
                       "f" "*a-clng-dm*"
                       "f" "*s-slab*"
                       "f" "3y14a*|e-*"
                       "f" "3y14a*|m-*"
                       "f" "3y14a*|p-*"
                       "f" "*door-iden*"
                       "f" "*a-eqpm*" "")
)
(if (= em_m 1)
	(Command "layer" "c" "7" "3y14mq*")
)
(if (= em_e 1)
	(Command "layer" "c" "7" "3y14eq*")
)
(if (= em_t 1)
	(Command "layer" "c" "7" "3y14tq*")
)
(command "regenall")
(princ))