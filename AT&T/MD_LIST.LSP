;md_list.lsp
;
;--------* Stone & Webster Engineering Corp
;--------* CAD/CAE 
;--------* Mechanical application
;--------* 012096             jdc
;--------*
;
;--------* md_list creates a dialog usable list of the different
;--------* fitting sizes available in the selected dim file
;
;
;
;--------* Returns the following variables:
;
;--------* szls1  - list of available sizes in the dim file [string]
;
;
;--------* error control initiated by program that uses this function
;
(defun md_list (att_ftype / att_rl dim_list sz_data)
;
;--------* close file attachment on error
;
 (adderr '(close dim_list))
;
;--------* Open communications with correct dim file
;
 (setq dim_list (findfile (strcat att_ftype ".dim")))
;
;--------* check to see tha the file was found, if not warn and exit
;
 (if (= dim_list nil)
     (progn
           (alert "Dimension file not found")
           (reset nil)
           (exit)
     )
     (setq dim_list (open dim_list "r"))
 )    
;
;--------* read each line and store the available diameters to
;--------* list named szls1 - size list one
;
 (setq att_rl (read-line dim_list)
       szls1 nil
 )
 (while att_rl
              (setq sz_data (substr att_rl 1 5 )
                    szls1 (append szls1 (list sz_data))
                    att_rl (read-line dim_list)
              )
 )
;
;--------* close the dimensional data file attachment
;
 (setq dim_list (close dim_list))
 (delerr '(close dim_list))
;
;--------* fill in list box on dialog
;
 (start_list "f_sz")
 (mapcar 'add_list szls1)
 (end_list)
;
;--------* thats it!
;
 (princ)
) ;end defun
;
;
;--------* md_lst creates a dialog usable list of the different
;--------* tee/reduce sizes available in the selected dim file
;
;
(defun md_lst (att_ftype / att_rl dim_list sz_data)
;
;--------* close file attachment on error
;
 (adderr '(close dim_list))
;
;--------* Open communications with correct dim file
;
 (setq dim_list (findfile (strcat att_ftype ".dim")))
;
;--------* check to see tha the file was found, if not warn and exit
;
 (if (= dim_list nil)
     (progn
           (alert "Dimension file not found")
           (reset nil)
           (exit)
     )
     (setq dim_list (open dim_list "r"))
 )    
;
;--------* read each line and store the available diameters to
;--------* list named szls1 - size list one
;
 (setq att_rl (read-line dim_list)
       szls1 nil
 )
 (while att_rl
            (setq count 1 chrct 0)
            (while
                 (if (/= " " (substr att_rl count 1))(setq chrct (1+ chrct)))
                 (setq count (1+ count))
            )
            (setq sz_data (substr att_rl 1 chrct)
                    szls1 (append szls1 (list sz_data))
                    att_rl (read-line dim_list)
            )
 )
;
;--------* close the dimensional data file attachment
;
 (setq dim_list (close dim_list))
 (delerr '(close dim_list))
;
;--------* fill in list box on dialog
;
 (start_list "f_sz")
 (mapcar 'add_list szls1)
 (end_list)
;
;--------* thats it!
;
 (princ)
) ;end defun
(princ)
