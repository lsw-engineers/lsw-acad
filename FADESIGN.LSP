;----- STROBE ONLY DEVICES

(defun c:s0 (/ pt1)
(setq dir
	(getstring
		(strcat "\nInsertion point from which side of block? ( <T>op <B>ottom <R>ight <L>eft> ): ")
	)
)
(F:MODS (QUOTE ("attreq" "osmode")))
(setvar "attreq" 1)(setvar "osmode" 0)(setvar "osmode" 0)
(setq pt1 (getpoint "\nPick Insertion point: "))
(f:slay"FA-WALL-EXST")
;Insert FA block
(if (or (= dir "t") (= dir "T")) (command "insert" "s-top"    pt1 (scalefactor) (scalefactor) 0 ""))
(if (or (= dir "b") (= dir "B")) (command "insert" "s-bottom" pt1 (scalefactor) (scalefactor) 0 ""))
(if (or (= dir "r") (= dir "R")) (command "insert" "s-right"  pt1 (scalefactor) (scalefactor) 0 ""))
(if (or (= dir "l") (= dir "L")) (command "insert" "s-left"   pt1 (scalefactor) (scalefactor) 0 ""))
(f:modr)
(f:rlay)
(princ))

(defun c:s15 (/ pt1)
(setq dir
	(getstring
		(strcat "\nInsertion point from which side of block? ( <T>op <B>ottom <R>ight <L>eft> ): ")
	)
)
(F:MODS (QUOTE ("attreq" "osmode")))
(setvar "attreq" 1)(setvar "osmode" 0)(setvar "osmode" 0)
(setq pt1 (getpoint "\nPick Insertion point: "))
(f:slay"FA-WALL-NEW")
;Insert FA block
(if (or (= dir "t") (= dir "T")) (command "insert" "s-top"    pt1 (scalefactor) (scalefactor) 0 "15"))
(if (or (= dir "b") (= dir "B")) (command "insert" "s-bottom" pt1 (scalefactor) (scalefactor) 0 "15"))
(if (or (= dir "r") (= dir "R")) (command "insert" "s-right"  pt1 (scalefactor) (scalefactor) 0 "15"))
(if (or (= dir "l") (= dir "L")) (command "insert" "s-left"   pt1 (scalefactor) (scalefactor) 0 "15"))
;Insert coverage block
(if (or (= dir "t") (= dir "T")) (command "insert" "15cd=" pt1 1 1 270))
(if (or (= dir "b") (= dir "B")) (command "insert" "15cd=" pt1 1 1 90))
(if (or (= dir "r") (= dir "R")) (command "insert" "15cd=" pt1 1 1 180))
(if (or (= dir "l") (= dir "L")) (command "insert" "15cd=" pt1 1 1 0))
(f:modr)
(f:rlay)
(princ))

(defun c:s30 (/ pt1)
(setq dir
	(getstring
		(strcat "\nInsertion point from which side of block? ( <T>op <B>ottom <R>ight <L>eft> ): ")
	)
)
(F:MODS (QUOTE ("attreq" "osmode")))
(setvar "attreq" 1)(setvar "osmode" 0)
(setq pt1 (getpoint "\nPick Insertion point: "))
(f:slay"FA-WALL-NEW")
;Insert FA block
(if (or (= dir "t") (= dir "T")) (command "insert" "s-top"    pt1 (scalefactor) (scalefactor) 0 "30"))
(if (or (= dir "b") (= dir "B")) (command "insert" "s-bottom" pt1 (scalefactor) (scalefactor) 0 "30"))
(if (or (= dir "r") (= dir "R")) (command "insert" "s-right"  pt1 (scalefactor) (scalefactor) 0 "30"))
(if (or (= dir "l") (= dir "L")) (command "insert" "s-left"   pt1 (scalefactor) (scalefactor) 0 "30"))
;Insert coverage block
(if (or (= dir "t") (= dir "T")) (command "insert" "30cd=" pt1 1 1 270))
(if (or (= dir "b") (= dir "B")) (command "insert" "30cd=" pt1 1 1 90))
(if (or (= dir "r") (= dir "R")) (command "insert" "30cd=" pt1 1 1 180))
(if (or (= dir "l") (= dir "L")) (command "insert" "30cd=" pt1 1 1 0))
(f:modr)
(f:rlay)
(princ))

(defun c:s75 (/ pt1)
(setq dir
	(getstring
		(strcat "\nInsertion point from which side of block? ( <T>op <B>ottom <R>ight <L>eft> ): ")
	)
)
(F:MODS (QUOTE ("attreq" "osmode")))
(setvar "attreq" 1)(setvar "osmode" 0)
(setq pt1 (getpoint "\nPick Insertion point: "))
(f:slay"FA-WALL-NEW")
;Insert FA block
(if (or (= dir "t") (= dir "T")) (command "insert" "s-top"    pt1 (scalefactor) (scalefactor) 0 "60"))
(if (or (= dir "b") (= dir "B")) (command "insert" "s-bottom" pt1 (scalefactor) (scalefactor) 0 "60"))
(if (or (= dir "r") (= dir "R")) (command "insert" "s-right"  pt1 (scalefactor) (scalefactor) 0 "60"))
(if (or (= dir "l") (= dir "L")) (command "insert" "s-left"   pt1 (scalefactor) (scalefactor) 0 "60"))
;Insert coverage block
(if (or (= dir "t") (= dir "T")) (command "insert" "60cd=" pt1 1 1 270))
(if (or (= dir "b") (= dir "B")) (command "insert" "60cd=" pt1 1 1 90))
(if (or (= dir "r") (= dir "R")) (command "insert" "60cd=" pt1 1 1 180))
(if (or (= dir "l") (= dir "L")) (command "insert" "60cd=" pt1 1 1 0))
(f:modr)
(f:rlay)
(princ))

(defun c:s110 (/ pt1)
(setq dir
	(getstring
		(strcat "\nInsertion point from which side of block? ( <T>op <B>ottom <R>ight <L>eft> ): ")
	)
)
(F:MODS (QUOTE ("attreq" "osmode")))
(setvar "attreq" 1)(setvar "osmode" 0)
(setq pt1 (getpoint "\nPick Insertion point: "))
(f:slay"FA-WALL-NEW")
;Insert FA block
(if (or (= dir "t") (= dir "T")) (command "insert" "s-top"    pt1 (scalefactor) (scalefactor) 0 "110"))
(if (or (= dir "b") (= dir "B")) (command "insert" "s-bottom" pt1 (scalefactor) (scalefactor) 0 "110"))
(if (or (= dir "r") (= dir "R")) (command "insert" "s-right"  pt1 (scalefactor) (scalefactor) 0 "110"))
(if (or (= dir "l") (= dir "L")) (command "insert" "s-left"   pt1 (scalefactor) (scalefactor) 0 "110"))
;Insert coverage block
(if (or (= dir "t") (= dir "T")) (command "insert" "110cd=" pt1 1 1 270))
(if (or (= dir "b") (= dir "B")) (command "insert" "110cd=" pt1 1 1 90))
(if (or (= dir "r") (= dir "R")) (command "insert" "110cd=" pt1 1 1 180))
(if (or (= dir "l") (= dir "L")) (command "insert" "110cd=" pt1 1 1 0))
(f:modr)
(f:rlay)
(princ))

;----- HORN/STROBE DEVICES

(defun c:hs0 (/ pt1)
(setq dir
	(getstring
		(strcat "\nInsertion point from which side of block? ( <T>op <B>ottom <R>ight <L>eft> ): ")
	)
)
(F:MODS (QUOTE ("attreq" "osmode")))
(setvar "attreq" 1)(setvar "osmode" 0)
(setq pt1 (getpoint "\nPick Insertion point: "))
(f:slay"FA-WALL-EXST")
;Insert FA block
(if (or (= dir "t") (= dir "T")) (command "insert" "hs-top"    pt1 (scalefactor) (scalefactor) 0 ""))
(if (or (= dir "b") (= dir "B")) (command "insert" "hs-bottom" pt1 (scalefactor) (scalefactor) 0 ""))
(if (or (= dir "r") (= dir "R")) (command "insert" "hs-right"  pt1 (scalefactor) (scalefactor) 0 ""))
(if (or (= dir "l") (= dir "L")) (command "insert" "hs-left"   pt1 (scalefactor) (scalefactor) 0 ""))
(f:modr)
(f:rlay)
(princ))

(defun c:hs15 (/ pt1)
(setq dir
	(getstring
		(strcat "\nInsertion point from which side of block? ( <T>op <B>ottom <R>ight <L>eft> ): ")
	)
)
(F:MODS (QUOTE ("attreq" "osmode")))
(setvar "attreq" 1)(setvar "osmode" 0)
(setq pt1 (getpoint "\nPick Insertion point: "))
(f:slay"FA-WALL-NEW")
;Insert FA block
(if (or (= dir "t") (= dir "T")) (command "insert" "hs-top"    pt1 (scalefactor) (scalefactor) 0 "15"))
(if (or (= dir "b") (= dir "B")) (command "insert" "hs-bottom" pt1 (scalefactor) (scalefactor) 0 "15"))
(if (or (= dir "r") (= dir "R")) (command "insert" "hs-right"  pt1 (scalefactor) (scalefactor) 0 "15"))
(if (or (= dir "l") (= dir "L")) (command "insert" "hs-left"   pt1 (scalefactor) (scalefactor) 0 "15"))
;Insert coverage block
(if (or (= dir "t") (= dir "T")) (command "insert" "15cd=" pt1 1 1 270))
(if (or (= dir "b") (= dir "B")) (command "insert" "15cd=" pt1 1 1 90))
(if (or (= dir "r") (= dir "R")) (command "insert" "15cd=" pt1 1 1 180))
(if (or (= dir "l") (= dir "L")) (command "insert" "15cd=" pt1 1 1 0))
(f:modr)
(f:rlay)
(princ))

(defun c:hs30 (/ pt1)
(setq dir
	(getstring
		(strcat "\nInsertion point from which side of block? ( <T>op <B>ottom <R>ight <L>eft> ): ")
	)
)
(F:MODS (QUOTE ("attreq" "osmode")))
(setvar "attreq" 1)(setvar "osmode" 0)
(setq pt1 (getpoint "\nPick Insertion point: "))
(f:slay"FA-WALL-NEW")
;Insert FA block
(if (or (= dir "t") (= dir "T")) (command "insert" "hs-top"    pt1 (scalefactor) (scalefactor) 0 "30"))
(if (or (= dir "b") (= dir "B")) (command "insert" "hs-bottom" pt1 (scalefactor) (scalefactor) 0 "30"))
(if (or (= dir "r") (= dir "R")) (command "insert" "hs-right"  pt1 (scalefactor) (scalefactor) 0 "30"))
(if (or (= dir "l") (= dir "L")) (command "insert" "hs-left"   pt1 (scalefactor) (scalefactor) 0 "30"))
;Insert coverage block
(if (or (= dir "t") (= dir "T")) (command "insert" "30cd=" pt1 1 1 270))
(if (or (= dir "b") (= dir "B")) (command "insert" "30cd=" pt1 1 1 90))
(if (or (= dir "r") (= dir "R")) (command "insert" "30cd=" pt1 1 1 180))
(if (or (= dir "l") (= dir "L")) (command "insert" "30cd=" pt1 1 1 0))
(f:modr)
(f:rlay)
(princ))

(defun c:hs75 (/ pt1)
(setq dir
	(getstring
		(strcat "\nInsertion point from which side of block? ( <T>op <B>ottom <R>ight <L>eft> ): ")
	)
)
(F:MODS (QUOTE ("attreq" "osmode")))
(setvar "attreq" 1)(setvar "osmode" 0)
(setq pt1 (getpoint "\nPick Insertion point: "))
(f:slay"FA-WALL-NEW")
;Insert FA block
(if (or (= dir "t") (= dir "T")) (command "insert" "hs-top"    pt1 (scalefactor) (scalefactor) 0 "60"))
(if (or (= dir "b") (= dir "B")) (command "insert" "hs-bottom" pt1 (scalefactor) (scalefactor) 0 "60"))
(if (or (= dir "r") (= dir "R")) (command "insert" "hs-right"  pt1 (scalefactor) (scalefactor) 0 "60"))
(if (or (= dir "l") (= dir "L")) (command "insert" "hs-left"   pt1 (scalefactor) (scalefactor) 0 "60"))
;Insert coverage block
(if (or (= dir "t") (= dir "T")) (command "insert" "60cd=" pt1 1 1 270))
(if (or (= dir "b") (= dir "B")) (command "insert" "60cd=" pt1 1 1 90))
(if (or (= dir "r") (= dir "R")) (command "insert" "60cd=" pt1 1 1 180))
(if (or (= dir "l") (= dir "L")) (command "insert" "60cd=" pt1 1 1 0))
(f:modr)
(f:rlay)
(princ))

(defun c:hs110 (/ pt1)
(setq dir
	(getstring
		(strcat "\nInsertion point from which side of block? ( <T>op <B>ottom <R>ight <L>eft> ): ")
	)
)
(F:MODS (QUOTE ("attreq" "osmode")))
(setvar "attreq" 1)(setvar "osmode" 0)
(setq pt1 (getpoint "\nPick Insertion point: "))
(f:slay"FA-WALL-NEW")
;Insert FA block
(if (or (= dir "t") (= dir "T")) (command "insert" "hs-top"    pt1 (scalefactor) (scalefactor) 0 "110"))
(if (or (= dir "b") (= dir "B")) (command "insert" "hs-bottom" pt1 (scalefactor) (scalefactor) 0 "110"))
(if (or (= dir "r") (= dir "R")) (command "insert" "hs-right"  pt1 (scalefactor) (scalefactor) 0 "110"))
(if (or (= dir "l") (= dir "L")) (command "insert" "hs-left"   pt1 (scalefactor) (scalefactor) 0 "110"))
;Insert coverage block
(if (or (= dir "t") (= dir "T")) (command "insert" "110cd=" pt1 1 1 270))
(if (or (= dir "b") (= dir "B")) (command "insert" "110cd=" pt1 1 1 90))
(if (or (= dir "r") (= dir "R")) (command "insert" "110cd=" pt1 1 1 180))
(if (or (= dir "l") (= dir "L")) (command "insert" "110cd=" pt1 1 1 0))
(f:modr)
(f:rlay)
(princ))


;;;  /------------------------------------------------------------------\
;;;  | The following is a function for corridor smoke detector spacing. |
;;;  | (Trial implementation (09/21/2004)                               |
;;;  \------------------------------------------------------------------/

(defun c:corrsd	()

  (f:slay "FA-CLNG-NEW")
  (setq pt1 (getpoint "\nEnter the first point:"))
  (setq pt2 (getpoint "\nEnter the second point:"))
  (setq corrlen (distance pt1 pt2))
  (setq nodets (+ (fix (/ corrlen 360)) 1))
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