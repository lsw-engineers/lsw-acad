;
;
;
;
;
;
; Set this file's version number.
(setq att_err_version 0.1)
;
;========================================================================
;
(defun ld_sld (ld_sld_tile ld_sld_lib ld_sld_img)
   (start_image ld_sld_tile)
   (setq ld_sld_max_X (dimx_tile ld_sld_tile)
      ld_sld_max_Y (dimy_tile ld_sld_tile)
      ld_sld_libimg (strcat ld_sld_lib "(" ld_sld_img ")")
   );setq
   (fill_image 0 0 ld_sld_max_x ld_sld_max_y 0)
   (slide_image 0 0 ld_sld_max_x ld_sld_max_y ld_sld_libimg)
   (end_image)
);defun ld_sld
;
;
;========================================================================
;
(defun file_exists (file_exists_name)
   (if (not (findfile file_exists_name))
      (progn
         (alert (strcat "\nCannot find " (strcase file_exists_name) " file!"))
         (exit)
      )
   )
);defun file_exists
;
;
;========================================================================
;NEW VERSION of this function...
(defun no_spaces (string)
   (setq string_pos 1)
   (setq clean_string "")

   (while (<= string_pos (strlen string))

      (setq string_elem (substr string string_pos 1))
      (if (/= string_elem " ")
	(setq clean_string (strcat clean_string string_elem))
      ) ;end if
      (setq string_pos (1+ string_pos))
   )
   (setq string clean_string)
);defun no_spaces
;
;
;THIS WAS THE ORIGINAL 1996 version of the NO_SPACES function...
;  It doesn't work...use the new version above...
;(defun no_spaces (string)
;   (setq string_pos 1)
;   (setq string_elem (substr string string_pos 1))
;   (while (and (/= string_elem " ") (<= string_pos (strlen string)))
;      (setq string_pos (1+ string_pos))
;      (setq string_elem (substr string string_pos 1))
;   )
;   (setq string (substr string 1 (1- string_pos)))
;);defun no_spaces
;
;
;
;========================================================================
;
(defun bad_check (bad_check_msg)
  (set_tile "error" bad_check_msg)
  (setq OK_check "BAD")
);defun bad_check
;
;
;========================================================================
;



;
;
;========================================================================
;


;
;
;========================================================================
;


