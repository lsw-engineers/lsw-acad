;--------errcntl
;
;--------* This file is designed to be used with AutoCAD Versions
;--------* R12-R13, It initializes functions used within the programs
;
;   AutoLISP sub-routines and functions
;
;--------*         General Utilities
;
;--------* Layer create/set function for use within menus or AutoLISP
;
(defun mlayer (name color ltype)
  (if (tblsearch "LAYER" name)
    (command "layer" "s" name "")
    (progn
      (setq regen (getvar "REGENMODE")) (setvar "REGENMODE" 0)
      (command "layer" "m" name)
      (if color (command "c"  color name))
      (if ltype (command "lt" ltype name))
      (command "")
      (setvar "REGENMODE" regen)
    )
  )
  (princ)
)
;
;--------* ANGTOC
;
(defun angtoc (ang)
  (setq ang
    (rtos (atof (angtos ang 0 8)) 2 6)
  )
  (strcat "<<" ang)
)
;
;--------* DXF
;
(defun dxf(code elist)
  (cdr (assoc code elist))
)
;--------* Full function error control
;--------* INITERR sets default #ERROR for resetting environment and/or
;--------* error recovery
(defun initerr ()
  (defun #error ()
    (setvar "BLIPMODE" 0)
    (setvar "MENUECHO" 0)
    (setvar "PLINEWID" 0.0)
  );defun
);defun
;--------* ADDERR adds one item to the #ERROR list. ITEM must be quoted
;--------* expression, or in form: (list 'FUNCTION arg arg arg...) where
;--------* arguments are quoted or not depending on whether you pass the
;--------* current value or use them literally.
(defun adderr (item)
  (if (not (member item #error))
    (setq #error (append '(nil) (list item) (cdr #error)))
  );if
);defun
;--------* DELERR deletes one item from the #ERROR list.
;--------* ITEM is same format as ADDERR.
(defun delerr (item)
   (setq #error (subst '() item #error))
);defun
;--------* #ERR stores the original standard error function, before it
;--------* gets redefined.
(if #err nil (setq #err *error*))
;-------* *ERROR* redefines the standard error function.
(defun *error* (msg / errno)
  (setvar "CMDECHO" 0)
;--------* Variables declared local to a program can become global when an
;--------* error occurs
;--------* Include (setq args '(v1 v2 v3 . . . args) in programs, where
;--------* v1, v2, v3 etc. are its locals
;
  (foreach arg args (set arg nil))
  (command)
  (grtext)
  ; If user did not cancel program, print message, but
  ; compare messages to suppress
  (if (not (member msg '("console break" "Function cancelled")))
    (progn
      (prompt
        (strcat "Program ERROR: "
                (if (> 11 (read (getvar "ACADVER")))
                  ""
                  (progn
                    (setq errno (itoa (getvar "ERRNO")))
                    (setvar "ERRNO" 0)  errno
                ) );if&progn
                " " msg
        );strcat
  ) ) );if,progn,prompt
  (reset msg)
);defun *ERROR*
;--------* RESET function resets the standard environment. You can call
;--------* it at the end of a function to put the settings back in order,
;--------* including any tasks you ADDERR but do not DELERR. If you call
;--------* it from a function for normal termination, supply a nil argument.
(defun reset (msg)
  (command "_.UNDO" "_E")
  (if msg (prompt "\nResetting environment..."))
  (#error)
  (initerr)
  (setvar "CMDECHO" 0)
  (if msg (prompt " Done."))
  (princ)
);defun RESET
;--------* C:*ERROR* restores the standard AutoLISP *ERROR* function for testing,
;--------* using the #ERR variable which stored *ERROR*.
(defun C:*ERROR* () (setq *error* #err))
(princ)
;--------* End error functions
;
;--------* Drawing Scale factor and annotation functions
(defun _chkdsc ()
 (if (= (getvar "userr1") 0.0)
     (progn
           (setq #dsc (atof (getstring "\nEnter Drawing Scale Factor :")))
           (setvar "userr1" #dsc)
     )
     (setq #dsc (getvar "userr1"))
 )
)
;
;--------* Standard text creatiojn functions
;
(defun _asty ()
 (command "style" "annot" "simplex" (* (/ 1.0 8.0) #dsc) ".9" "" "" "" "")
)
