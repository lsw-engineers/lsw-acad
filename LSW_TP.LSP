;;
;;  <file name> - <short description>
;;
;;  Copyright © 2003 by LSW Engineers Arizona, Inc.
;;
;;  
;;-------------------------------------------------------------------------
;;  DESCRIPTION
;;    <long description>
;;
;;-------------------------------------------------------------------------
;;

(defun c:tploc ()
	(set_tp "c:\\acad_personal\\toolpalettes")
	(princ)
)

(defun C:tpnet ()
	(set_tp "F:\\MASTER\\AutoCAD\\BLOCK\\ToolPalettes")
	(princ)
)


(defun C:tpoldatt ()
	(set_tp "F:\\MASTER\\AutoCAD\\BLOCK\\AT&T\\ToolPalettes")
	(princ)
)

(defun C:tpatt ()
	(set_tp "F:\\MASTER\\AutoCAD\\BLOCK\\ToolPalettes\\TP-ATT")
	(princ)
)

(defun C:tpall ()
	(set_tp "c:\\acad_personal\\toolpalettes;F:\\MASTER\\AutoCAD\\BLOCK\\ToolPalettes")
	(princ)
)

(defun C:tpfa ()
	(set_tp "F:\\MASTER\\AutoCAD\\BLOCK\\ToolPalettes\\TP-FA")
	(princ)
)

(defun C:tpeo ()
	(set_tp "F:\\MASTER\\AutoCAD\\BLOCK\\ToolPalettes\\TP-EO")
	(princ)
)

(defun C:tpeq ()
	(set_tp "F:\\MASTER\\AutoCAD\\BLOCK\\ToolPalettes\\TP-EQ")
	(princ)
)

(defun C:tpgen ()
	(set_tp "F:\\MASTER\\AutoCAD\\BLOCK\\ToolPalettes\\TP-GEN")
	(princ)
)

(defun C:tpip ()
	(set_tp "F:\\MASTER\\AutoCAD\\BLOCK\\ToolPalettes\\TP-IP")
	(princ)
)

(defun C:tpit ()
	(set_tp "F:\\MASTER\\AutoCAD\\BLOCK\\ToolPalettes\\TP-IT")
	(princ)
)

(defun C:tpm ()
	(set_tp "F:\\MASTER\\AutoCAD\\BLOCK\\ToolPalettes\\TP-M")
	(princ)
)
(defun C:tpmq ()
	(set_tp "F:\\MASTER\\AutoCAD\\BLOCK\\ToolPalettes\\TP-MQ")
	(princ)
)

(defun C:tpp ()
	(set_tp "F:\\MASTER\\AutoCAD\\BLOCK\\ToolPalettes\\TP-P")
	(princ)
)
(defun C:tpss ()
	(set_tp "F:\\MASTER\\AutoCAD\\BLOCK\\ToolPalettes\\TP-SS")
	(princ)
)

(defun C:tpaps ()
	(set_tp "F:\\MASTER\\AutoCAD\\BLOCK\\ToolPalettes\\TP-APS")
	(princ)
)

(defun C:tpsh ()
	(set_tp "F:\\MASTER\\AutoCAD\\BLOCK\\ToolPalettes\\TP-SH")
	(princ)
)

(defun set_tp ( tp_path )
	(command "TOOLPALETTESCLOSE")
	(setvar "*_toolpalettepath" tp_path)
	(command "TOOLPALETTES")
)

;The Sandbox
(defun C:tpsb ()
	(set_tp "F:\\MASTER\\AutoCAD\\BLOCK\\ToolPalettes\\TP-SANDBOX")
	(princ)
)