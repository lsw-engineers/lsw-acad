(defun c:fa001 ()
(setq dir
	(getstring
		(strcat "\nInsertion point from which side of block? ( <T>op <B>ottom <R>ight <L>eft> ): ")
	)
)
(prompt "\nPick Insertion point: ")
(if (or (= dir "t") (= dir "T")) (command "insert" "fpcpes0t" PAUSE dwgsc dwgsc 0))
(if (or (= dir "b") (= dir "B")) (command "insert" "fpcpes0b" PAUSE dwgsc dwgsc 0))
(if (or (= dir "r") (= dir "R")) (command "insert" "fpcpes0r" PAUSE dwgsc dwgsc 0))
(if (or (= dir "l") (= dir "L")) (command "insert" "fpcpes0l" PAUSE dwgsc dwgsc 0))
(princ))

(defun c:fa002 ()
(setq dir
	(getstring
		(strcat "\nInsertion point from which side of block? ( <T>op <B>ottom <R>ight <L>eft> ): ")
	)
)
(prompt "\nPick Insertion point: ")
(if (or (= dir "t") (= dir "T")) (command "insert" "fpcpa00t" PAUSE dwgsc dwgsc 0))
(if (or (= dir "b") (= dir "B")) (command "insert" "fpcpa00b" PAUSE dwgsc dwgsc 0))
(if (or (= dir "r") (= dir "R")) (command "insert" "fpcpa00r" PAUSE dwgsc dwgsc 0))
(if (or (= dir "l") (= dir "L")) (command "insert" "fpcpa00l" PAUSE dwgsc dwgsc 0))
(princ))

(defun c:fa003 ()
(prompt "\nPick Insertion point: ")
(command "insert" "fpdfi000" PAUSE (* dwgsc 0.75) (* dwgsc 0.75) 0)
(princ))

(defun c:fa004 ()
(prompt "\nPick Insertion point: ")
(command "insert" "fpdfu000" PAUSE (* dwgsc 0.75) (* dwgsc 0.75) 0)
(princ))

(defun c:fa005 ()
(prompt "\nPick Insertion point: ")
(command "insert" "fpdfr000" PAUSE (* dwgsc 0.75) (* dwgsc 0.75) 0)
(princ))

(defun c:fa006 ()
(prompt "\nPick Insertion point: ")
(command "insert" "fpdg0000" PAUSE (* dwgsc 0.75) (* dwgsc 0.75) 0)
(princ))

(defun c:fa007 ()
(prompt "\nPick Insertion point: ")
(command "insert" "fpdsz000" PAUSE (* dwgsc 0.75) (* dwgsc 0.75) 0)
(princ))

(defun c:fa008 ()
(prompt "\nPick Insertion point: ")
(command "insert" "fpdsp000" PAUSE (* dwgsc 0.75) (* dwgsc 0.75) 0)
(princ))

(defun c:fa009 ()
(prompt "\nPick Insertion point: ")
(command "insert" "fpdtrf00" PAUSE (* dwgsc 0.75) (* dwgsc 0.75) 0)
(princ))

(defun c:fa010 ()
(prompt "\nPick Insertion point: ")
(command "insert" "fpdtf000" PAUSE (* dwgsc 0.75) (* dwgsc 0.75) 0)
(princ))

(defun c:fa011 ()
(prompt "\nPick Insertion point: ")
(command "insert" "fpdtc000" PAUSE (* dwgsc 0.75) (* dwgsc 0.75) 0)
(princ))

(defun c:fa012 ()
(prompt "\nPick Insertion point: ")
(command "insert" "fpdtr000" PAUSE (* dwgsc 0.75) (* dwgsc 0.75) 0)
(princ))

(defun c:fa013 ()
(setq dir
	(getstring
		(strcat "\nInsertion point from which side of block? ( <T>op <B>ottom <R>ight <L>eft> ): ")
	)
)
(prompt "\nPick Insertion point: ")
(if (or (= dir "t") (= dir "T")) (command "insert" "fpdh000t" PAUSE dwgsc dwgsc 0))
(if (or (= dir "b") (= dir "B")) (command "insert" "fpdh000b" PAUSE dwgsc dwgsc 0))
(if (or (= dir "r") (= dir "R")) (command "insert" "fpdh000r" PAUSE dwgsc dwgsc 0))
(if (or (= dir "l") (= dir "L")) (command "insert" "fpdh000l" PAUSE dwgsc dwgsc 0))
(princ))

(defun c:fa014 ()
(setq dir
	(getstring
		(strcat "\nInsertion point from which side of block? ( <T>op <B>ottom <R>ight <L>eft> ): ")
	)
)
(prompt "\nPick Insertion point: ")
(if (or (= dir "t") (= dir "T")) (command "insert" "fpmsps0t" PAUSE (* dwgsc 0.75) (* dwgsc 0.75) 0))
(if (or (= dir "b") (= dir "B")) (command "insert" "fpmsps0b" PAUSE (* dwgsc 0.75) (* dwgsc 0.75) 0))
(if (or (= dir "r") (= dir "R")) (command "insert" "fpmsps0r" PAUSE (* dwgsc 0.75) (* dwgsc 0.75) 0))
(if (or (= dir "l") (= dir "L")) (command "insert" "fpmsps0l" PAUSE (* dwgsc 0.75) (* dwgsc 0.75) 0))
(princ))

(defun c:fa015 ()
(setq dir
	(getstring
		(strcat "\nInsertion point from which side of block? ( <T>op <B>ottom <R>ight <L>eft> ): ")
	)
)
(prompt "\nPick Insertion point: ")
(if (or (= dir "t") (= dir "T")) (command "insert" "fpcpsa0t" PAUSE dwgsc dwgsc 0))
(if (or (= dir "b") (= dir "B")) (command "insert" "fpcpsa0b" PAUSE dwgsc dwgsc 0))
(if (or (= dir "r") (= dir "R")) (command "insert" "fpcpsa0r" PAUSE dwgsc dwgsc 0))
(if (or (= dir "l") (= dir "L")) (command "insert" "fpcpsa0l" PAUSE dwgsc dwgsc 0))
(princ))


(defun c:corrsd	()

  (f:slay "FA-CLNG-NEW")
  (setq pt1 (getpoint "\nEnter the first point:"))
  (setq pt2 (getpoint "\nEnter the second point:"))
  (setq corrlen (distance pt1 pt2))
  (setq nodets (+ (fix (/ corrlen 240)) 1))
  (setq seglen (/ corrlen (* nodets 2)))
  (command "line" pt1 pt2 "")
  (setq ln (entlast))
  (command "divide" ln "b" "fa_sd" "n" (* nodets 2))
  (setq select_set (ssget "P"))

  (setq counter -1)
  (repeat (sslength select_set)
    (setq ent_list
	   (entget (ssname select_set (setq counter (1+ counter)))
	   )
    )
    (if	(= (/ (float counter) 2.0) (fix (/ (float counter) 2.0)))
      (progn
	(setq ent_list
	       (subst
		 (cons 41 (scalefactor))
		 (assoc 41 ent_list)
		 ent_list
	       )
	)
	(setq ent_list
	       (subst
		 (cons 42 (scalefactor))
		 (assoc 42 ent_list)
		 ent_list
	       )
	)
	(setq ent_list
	       (subst
		 (cons 43 (scalefactor))
		 (assoc 43 ent_list)
		 ent_list
	       )
	)
	(entmod ent_list)
      )

      (entdel (cdr (assoc -1 ent_list)))
    )
  )
  (command "erase" ln "")
  (f:rlay)
  (princ)
)