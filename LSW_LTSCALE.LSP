(vl-load-com)
(VLR-SysVar-Reactor nil '((:VLR-sysVarChanged . lsw::SysVarChanged)) )

(defun lsw::SysVarChanged (pReactor paramsList / pCurDoc newTileMode)
	(setq pCurDoc (vla-get-activedocument (vlax-get-acad-object)))
	; check if the sysvar changed was TILEMODE
	(if (= (car paramsList) "TILEMODE")
		; check if the change was successful
		(if (cadr paramsList)
			(progn
				(setq newTileMode (vlax-variant-value (vla-GetVariable pCurDoc "TILEMODE")))
				(if (= newTileMode 1)
					(model_scale)
					(layout_scale)
				);if
			);progn
			(princ "\nTILEMODE change unsuccessful.")
		);if
	);if
	(vlax-release-object pCurDoc)
);defun lsw::SysVarChanged

(defun c:TM (/ pCurDoc oldTilemode)
	(setq pCurDoc (vla-get-activedocument (vlax-get-acad-object)))
	(setq oldTilemode (vlax-variant-value (vla-GetVariable pCurDoc "TILEMODE")))
	(vla-SetVariable pCurDoc "TILEMODE" (abs (- 1 oldTilemode)))
	(vlax-release-object pCurDoc)
	(princ)
)

(DEFUN model_scale ()
  (setvar "ltscale" (* dwgsc 0.5))
  (setvar "dimscale" dwgsc)
  (princ)
)

(DEFUN layout_scale ()
  (setvar "ltscale" 0.5)
  (setvar "dimscale" 1.0)
  (princ)
)