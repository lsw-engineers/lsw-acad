;mf_gsz.lsp
;--------*
;--------* Stone & Webster Engineering Corp
;--------* CAD/CAE 
;--------* Mechanical application
;--------* 012096             jdc
;--------*
;--------* mf_gsz.lsp contains functions to extract mechanical
;--------* fitting data from the mechanical dim files
;
;
;--------* mf_gsz function to get elbow straight dimensions using
;--------* the att_ftype variable set in the dialog box
;
;--------* syntax: (mf_gsz "diameter" "fitting type" "dimensional data file")
;--------* ex.: (mf_gsz "54.00" "el90lr" "dif_el")
;
;--------* Returns the following variables:
;
;--------* att_cd  - coupling diameter [string]
;--------* att_cw  - coupling width [string]
;--------* att_fd  - fitting diameter [string]
;--------* att_cte - center to end dimension for elbows [string]
;
;
(defun mf_gsz (att_dia att_fit att_ftype / att_dimf att_chk)
;
;--------* close file attachment on error
;
 (adderr '(close att_dimf))
;
;--------* Open communications with correct dim file
;
(setq att_dimf
             (open
                   (findfile (strcat att_ftype ".dim")) "r"
	     )
)
;
;----------* get the dimensional information from the dim
;----------* file for the fitting type
;
(setq att_rle (read-line att_dimf))
(while att_rle
	       (setq att_chk (substr att_rle 1 5))
	       (if (/= att_chk att_dia)
	           (progn
	                (setq att_rle (read-line att_dimf))
	           )
	           (progn
	                 (setq att_cd (substr att_rle 7 5)
	                       att_cw (substr att_rle 13 5)
                               att_fd att_dia
	                 )
;
;----------* check type of fitting and retrieve require dimensions
;
                         (cond
                             ((= att_fit "el90sr")
                                               (setq att_cte (substr att_rle 19 5))
                             )
                             (( = att_fit "el90lr")
                                               (setq att_cte (substr att_rle 25 5))
                             )
                             ((= att_fit "el45sr")
                                               (setq att_cte (substr att_rle 31 5))
                             )
                             (( = att_fit "el45lr")
                                               (setq att_cte (substr att_rle 37 5))
                             )
                         ) ;end cond
                         (setq att_dimf (close att_dimf)
                               att_rle nil
                         )
                         (delerr '(close att_dimf))
                   ) ; end progn
	       ) ;end if
) ;end while
(princ)
)
;
;--------* this function is used to retrive tee dimensions
;
;--------* syntax (mf_gtee "diameter" "dimensional data file")
;--------* ex.: (mf_gtee "12x10" "sce_tee")
;
;
;--------* Returns the following variables:
;
;--------* att_cd1  - run coupling diameter [string]
;--------* att_cd2  - branch coupling diameter [string]
;--------* att_cw1  - run coupling width [string]
;--------* att_cw2  - branch coupling width [string]
;--------* att_fd1  - run diameter [string]
;--------* att_fd2  - branch diameter [string]
;--------* att_cte  -  1/2 center to end dimension for tee run [string]
;--------* att_cte2 - center to end dimension for tee branch [string]
;
;
(defun mf_gtee (att_dia att_ftype / att_dimf att_chk)
;
;--------* close file attachment on error
;
 (adderr '(close att_dimf))
;
;--------* Open communications with correct dim file
;
(setq att_dimf
             (open
                   (findfile (strcat att_ftype ".dim")) "r"
	     )
)
;
;----------* get the dimensional information from the dim
;----------* file for the fitting type
;
(setq att_rle (read-line att_dimf))
(while att_rle
           (setq count 1 chrct 0)
           (while
                 (if (/= " " (substr att_rle count 1))(setq chrct (1+ chrct)))
                 (setq count (1+ count))
           )
           (setq att_chk (substr att_rle 1 chrct))
           (if (/= att_chk att_dia)
	                          (progn
	                                (setq att_rle (read-line att_dimf))
	                          )
	                          (progn
	                               (setq att_fd1 (substr att_rle 9 5)
	                                     att_fd2 (substr att_rle 15 5)
                                             att_cd1 (substr att_rle 21 5)
                                             att_cw1 (substr att_rle 27 5)
                                             att_cd2 (substr att_rle 33 5)
                                             att_cw2 (substr att_rle 39 5)
                                             att_cte (substr att_rle 45 5)
                                             att_cte2 (substr att_rle 51 5)
	                               )
	                               (setq att_dimf (close att_dimf)
                                             att_rle nil
                                       )      
                                       (delerr '(close att_dimf))
                                  ) ; end progn
	   ) ;end if
           ) ;end while
(princ)
)
;
;
;--------* fil_tee function used to fill dimension edit boxes with
;--------* correct data for tees
;
(defun fil_tee ()
 (set_tile "r_1" (rtos (* (atof att_cte) 2.0)))
 (set_tile "l_2" att_cte2)
 (set_tile "pd1" att_fd1)
 (set_tile "pd2" att_fd2)
 (set_tile "cd1" att_cd1)
 (set_tile "cd2" att_cd2)
 (set_tile "cw1" att_cw1)
 (set_tile "cw2" att_cw2)
;
;--------* set values as reals to variables
;
 (setq l_1 (* (atof att_cte) 2.0)
       br_1 (atof att_cte2)
       pd_1 (atof att_fd1)
       pd_2 (atof att_fd2)
       cd_1 (atof att_cd1)
       cd_2 (atof att_cd2)
       cw_1 (atof att_cw1)
       cw_2 (atof att_cw2)
 )      
(princ)
)

;
;-------* Coupling size reader for ddcplg command
;
(defun mf_csz (att_dia att_ftype / att_dimf att_chk)
;
;--------* close file attachment on error
;
 (adderr '(close att_dimf))
;
;--------* Open communications with correct dim file
;
(setq att_dimf
             (open
                   (findfile (strcat att_ftype ".dim")) "r"
	     )
)
;
;----------* get the dimensional information from the dim
;----------* file for the fitting type
;
(setq att_rle (read-line att_dimf))
(while att_rle
	       (setq att_chk (substr att_rle 1 5))
	       (if (/= att_chk att_dia)
	           (progn
	                (setq att_rle (read-line att_dimf))
	           )
	           (progn
	                 (setq att_cd (substr att_rle 7 5)
	                       att_cw (substr att_rle 13 5)
	                       att_r1 (substr att_rle 19 5)
                            att_fd att_dia
                            att_dimf (close att_dimf)
                            att_rle nil
                      )
                      (delerr '(close att_dimf))
                )
	       )
) ;end while
(princ)
)
;
;--------* fil_cpl function used to fill dimension edit boxes with
;--------* correct data for couiplings
;
(defun fil_cpl ()
 (set_tile "r1" att_r1)
 (set_tile "pd1" att_fd)
 (set_tile "cd1" att_cd)
 (set_tile "cw1" att_cw)
;
;--------* set values as reals to variables
;
 (setq r1 (atof att_r1)
       pd (atof att_fd)
       cd (atof att_cd)
       cw (atof att_cw)
 )      
(princ)
)
(princ)
