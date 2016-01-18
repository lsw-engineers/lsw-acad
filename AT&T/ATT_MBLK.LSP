
 ;INS_MECH_DIFF
(defun C:EDF () (INS_MECH_DIFF "M-DUCT-CDFF-NEWW" "ATT_EXHDIF" "non" 24 24 nil))
(defun C:EDFG () (INS_MECH_DIFF "M-DUCT-CDFF-NEWW" "ATT_EXHDIF" "int" 24 24 "grid"))
(defun C:EDF1 () (INS_MECH_DIFF "M-DUCT-CDFF-NEWW" "ATT_EXHDIF" "non" 1 nil nil))
(defun C:EDF2 () (INS_MECH_DIFF "M-DUCT-CDFF-NEWW" "ATT_EXHDIF" "non" 0 nil nil))
(defun C:RDF () (INS_MECH_DIFF "M-DUCT-CDFF-NEWW" "ATT_RETDIF" "non" 24 24 nil))
(defun C:RDFG () (INS_MECH_DIFF "M-DUCT-CDFF-NEWW" "ATT_RETDIF" "int" 24 24 "grid"))
(defun C:RDF1 () (INS_MECH_DIFF "M-DUCT-CDFF-NEWW" "ATT_RETDIF" "non" 1 nil nil))
(defun C:RDF2 () (INS_MECH_DIFF "M-DUCT-CDFF-NEWW" "ATT_RETDIF" "non" 0 nil nil))
(defun C:SDF () (INS_MECH_DIFF "M-DUCT-CDFF-NEWW" "ATT_SUPDIF" "non" 24 24 nil))
(defun C:SDFG () (INS_MECH_DIFF "M-DUCT-CDFF-NEWW" "ATT_SUPDIF" "int" 24 24 "grid"))
(defun C:SDF1 () (INS_MECH_DIFF "M-DUCT-CDFF-NEWW" "ATT_SUPDIF" "non" 1 nil nil))
(defun C:SDF2 () (INS_MECH_DIFF "M-DUCT-CDFF-NEWW" "ATT_SUPDIF" "non" 0 nil nil))