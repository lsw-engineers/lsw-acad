(defun c:e001 ()
(setq dir
	(getstring
		(strcat "\nInsertion point from which side of block? ( <T>op <B>ottom <R>ight <L>eft> ): ")
	)
)
(prompt "\nPick Insertion point: ")
(if (or (= dir "t") (= dir "T")) (command "insert" "eplfe00t" PAUSE dwgsc dwgsc 0))
(if (or (= dir "b") (= dir "B")) (command "insert" "eplfe00b" PAUSE dwgsc dwgsc 0))
(if (or (= dir "r") (= dir "R")) (command "insert" "eplfe00r" PAUSE dwgsc dwgsc 0))
(if (or (= dir "l") (= dir "L")) (command "insert" "eplfe00l" PAUSE dwgsc dwgsc 0))
(princ))

(defun c:e002 ()
(setq dir
	(getstring
		(strcat "\nInsertion point from which side of block? ( <T>op <B>ottom <R>ight <L>eft> ): ")
	)
)
(prompt "\nPick Insertion point: ")
(if (or (= dir "t") (= dir "T")) (command "insert" "eplfe01t" PAUSE dwgsc dwgsc 0))
(if (or (= dir "b") (= dir "B")) (command "insert" "eplfe01b" PAUSE dwgsc dwgsc 0))
(if (or (= dir "r") (= dir "R")) (command "insert" "eplfe01r" PAUSE dwgsc dwgsc 0))
(if (or (= dir "l") (= dir "L")) (command "insert" "eplfe01l" PAUSE dwgsc dwgsc 0))
(princ))

(defun c:e003 ()
(setq dir
	(getstring
		(strcat "\nInsertion point from which side of block? ( <T>op <B>ottom <R>ight <L>eft> ): ")
	)
)
(prompt "\nPick Insertion point: ")
(if (or (= dir "t") (= dir "T")) (command "insert" "eplfe02t" PAUSE dwgsc dwgsc 0))
(if (or (= dir "b") (= dir "B")) (command "insert" "eplfe02b" PAUSE dwgsc dwgsc 0))
(if (or (= dir "r") (= dir "R")) (command "insert" "eplfe02r" PAUSE dwgsc dwgsc 0))
(if (or (= dir "l") (= dir "L")) (command "insert" "eplfe02l" PAUSE dwgsc dwgsc 0))
(princ))

(defun c:e004 ()
(setq dir
	(getstring
		(strcat "\nInsertion point from which side of block? ( <T>op <B>ottom <R>ight <L>eft> ): ")
	)
)
(prompt "\nPick Insertion point: ")
(if (or (= dir "t") (= dir "T")) (command "insert" "eplfe04t" PAUSE 96 96 0))
(if (or (= dir "b") (= dir "B")) (command "insert" "eplfe04b" PAUSE 96 96 0))
(if (or (= dir "r") (= dir "R")) (command "insert" "eplfe04r" PAUSE 96 96 0))
(if (or (= dir "l") (= dir "L")) (command "insert" "eplfe04l" PAUSE 96 96 0))
(princ))

(defun c:e005 ()
(setq dir
	(getstring
		(strcat "\nInsertion point from which side of block? ( <T>op <B>ottom <R>ight <L>eft> ): ")
	)
)
(prompt "\nPick Insertion point: ")
(if (or (= dir "t") (= dir "T")) (command "insert" "eplfx00t" PAUSE (* dwgsc 0.75) (* dwgsc 0.75) 0))
(if (or (= dir "b") (= dir "B")) (command "insert" "eplfx00b" PAUSE (* dwgsc 0.75) (* dwgsc 0.75) 0))
(if (or (= dir "r") (= dir "R")) (command "insert" "eplfx00r" PAUSE (* dwgsc 0.75) (* dwgsc 0.75) 0))
(if (or (= dir "l") (= dir "L")) (command "insert" "eplfx00l" PAUSE (* dwgsc 0.75) (* dwgsc 0.75) 0))
(princ))

(defun c:e006 ()
(setq dir
	(getstring
		(strcat "\nInsertion point from which side of block? ( <T>op <B>ottom <R>ight <L>eft> ): ")
	)
)
(prompt "\nPick Insertion point: ")
(if (or (= dir "t") (= dir "T")) (command "insert" "eplfx01t" PAUSE (* dwgsc 0.75) (* dwgsc 0.75) 0))
(if (or (= dir "b") (= dir "B")) (command "insert" "eplfx01b" PAUSE (* dwgsc 0.75) (* dwgsc 0.75) 0))
(if (or (= dir "r") (= dir "R")) (command "insert" "eplfx01r" PAUSE (* dwgsc 0.75) (* dwgsc 0.75) 0))
(if (or (= dir "l") (= dir "L")) (command "insert" "eplfx01l" PAUSE (* dwgsc 0.75) (* dwgsc 0.75) 0))
(princ))

(defun c:e007 ()
(setq dir
	(getstring
		(strcat "\nInsertion point from which side of block? ( <T>op <B>ottom <R>ight <L>eft> ): ")
	)
)
(prompt "\nPick Insertion point: ")
(if (or (= dir "t") (= dir "T")) (command "insert" "eplff13t" PAUSE 96 96 0))
(if (or (= dir "b") (= dir "B")) (command "insert" "eplff13b" PAUSE 96 96 0))
(if (or (= dir "r") (= dir "R")) (command "insert" "eplff13r" PAUSE 96 96 0))
(if (or (= dir "l") (= dir "L")) (command "insert" "eplff13l" PAUSE 96 96 0))
(princ))

(defun c:e008 ()
(setq dir
	(getstring
		(strcat "\nInsertion point from which side of block? ( <T>op <B>ottom <R>ight <L>eft> ): ")
	)
)
(prompt "\nPick Insertion point: ")
(if (or (= dir "t") (= dir "T")) (command "insert" "eplff14t" PAUSE 96 96 0))
(if (or (= dir "b") (= dir "B")) (command "insert" "eplff14b" PAUSE 96 96 0))
(if (or (= dir "r") (= dir "R")) (command "insert" "eplff14r" PAUSE 96 96 0))
(if (or (= dir "l") (= dir "L")) (command "insert" "eplff14l" PAUSE 96 96 0))
(princ))

(defun c:e009 ()
(setq dir
	(getstring
		(strcat "\nInsertion point from which side of block? ( <T>op <B>ottom <R>ight <L>eft> ): ")
	)
)
(prompt "\nPick Insertion point: ")
(if (or (= dir "t") (= dir "T")) (command "insert" "eplff15t" PAUSE 96 96 0))
(if (or (= dir "b") (= dir "B")) (command "insert" "eplff15b" PAUSE 96 96 0))
(if (or (= dir "r") (= dir "R")) (command "insert" "eplff15r" PAUSE 96 96 0))
(if (or (= dir "l") (= dir "L")) (command "insert" "eplff15l" PAUSE 96 96 0))
(princ))

(defun c:e010 ()
(setq dir
	(getstring
		(strcat "\nInsertion point from which side of block? ( <T>op <B>ottom <R>ight <L>eft> ): ")
	)
)
(prompt "\nPick Insertion point: ")
(if (or (= dir "t") (= dir "T")) (command "insert" "eplff16t" PAUSE 96 96 0))
(if (or (= dir "b") (= dir "B")) (command "insert" "eplff16b" PAUSE 96 96 0))
(if (or (= dir "r") (= dir "R")) (command "insert" "eplff16r" PAUSE 96 96 0))
(if (or (= dir "l") (= dir "L")) (command "insert" "eplff16l" PAUSE 96 96 0))
(princ))

(defun c:e011 ()
(setq dir
	(getstring
		(strcat "\nInsertion point from which side of block? ( <T>op <B>ottom <R>ight <L>eft> ): ")
	)
)
(prompt "\nPick Insertion point: ")
(if (or (= dir "t") (= dir "T")) (command "insert" "eplff17t" PAUSE 96 96 0))
(if (or (= dir "b") (= dir "B")) (command "insert" "eplff17b" PAUSE 96 96 0))
(if (or (= dir "r") (= dir "R")) (command "insert" "eplff17r" PAUSE 96 96 0))
(if (or (= dir "l") (= dir "L")) (command "insert" "eplff17l" PAUSE 96 96 0))
(princ))

(defun c:e012 ()
(setq dir
	(getstring
		(strcat "\nInsertion point from which side of block? ( <T>op <B>ottom <R>ight <L>eft> ): ")
	)
)
(prompt "\nPick Insertion point: ")
(if (or (= dir "t") (= dir "T")) (command "insert" "eplff18t" PAUSE 96 96 0))
(if (or (= dir "b") (= dir "B")) (command "insert" "eplff18b" PAUSE 96 96 0))
(if (or (= dir "r") (= dir "R")) (command "insert" "eplff18r" PAUSE 96 96 0))
(if (or (= dir "l") (= dir "L")) (command "insert" "eplff18l" PAUSE 96 96 0))
(princ))

(defun c:e013 ()
(setq dir
	(getstring
		(strcat "\nInsertion point from which side of block? ( <T>op <B>ottom <R>ight <L>eft> ): ")
	)
)
(prompt "\nPick Insertion point: ")
(if (or (= dir "t") (= dir "T")) (command "insert" "eplff19t" PAUSE 96 96 0))
(if (or (= dir "b") (= dir "B")) (command "insert" "eplff19b" PAUSE 96 96 0))
(if (or (= dir "r") (= dir "R")) (command "insert" "eplff19r" PAUSE 96 96 0))
(if (or (= dir "l") (= dir "L")) (command "insert" "eplff19l" PAUSE 96 96 0))
(princ))

(defun c:e014 ()
(setq dir
	(getstring
		(strcat "\nInsertion point from which side of block? ( <T>op <B>ottom <R>ight <L>eft> ): ")
	)
)
(prompt "\nPick Insertion point: ")
(if (or (= dir "t") (= dir "T")) (command "insert" "eplff20t" PAUSE 96 96 0))
(if (or (= dir "b") (= dir "B")) (command "insert" "eplff20b" PAUSE 96 96 0))
(if (or (= dir "r") (= dir "R")) (command "insert" "eplff20r" PAUSE 96 96 0))
(if (or (= dir "l") (= dir "L")) (command "insert" "eplff20l" PAUSE 96 96 0))
(princ))

(defun c:e015 ()
(setq dir
	(getstring
		(strcat "\nInsertion point from which side of block? ( <T>op <B>ottom <R>ight <L>eft> ): ")
	)
)
(prompt "\nPick Insertion point: ")
(if (or (= dir "t") (= dir "T")) (command "insert" "eplfi11t" PAUSE (* dwgsc 0.75) (* dwgsc 0.75) 0))
(if (or (= dir "b") (= dir "B")) (command "insert" "eplfi11b" PAUSE (* dwgsc 0.75) (* dwgsc 0.75) 0))
(if (or (= dir "r") (= dir "R")) (command "insert" "eplfi11r" PAUSE (* dwgsc 0.75) (* dwgsc 0.75) 0))
(if (or (= dir "l") (= dir "L")) (command "insert" "eplfi11l" PAUSE (* dwgsc 0.75) (* dwgsc 0.75) 0))
(princ))

(defun c:e016 ()
(setq dir
	(getstring
		(strcat "\nInsertion point from which side of block? ( <T>op <B>ottom <R>ight <L>eft> ): ")
	)
)
(prompt "\nPick Insertion point: ")
(if (or (= dir "t") (= dir "T")) (command "insert" "eplfi10t" PAUSE (* dwgsc 0.75) (* dwgsc 0.75) 0))
(if (or (= dir "b") (= dir "B")) (command "insert" "eplfi10b" PAUSE (* dwgsc 0.75) (* dwgsc 0.75) 0))
(if (or (= dir "r") (= dir "R")) (command "insert" "eplfi10r" PAUSE (* dwgsc 0.75) (* dwgsc 0.75) 0))
(if (or (= dir "l") (= dir "L")) (command "insert" "eplfi10l" PAUSE (* dwgsc 0.75) (* dwgsc 0.75) 0))
(princ))

(defun c:e017 ()
(prompt "\nPick Insertion point: ")
(command "insert" "eplfe03a" pause 1 1 pause)
(princ))

(defun c:e018 ()
(prompt "\nPick Insertion point: ")
(command "insert" "eplff00a" pause 1 1 pause)
(princ))

(defun c:e019 ()
(prompt "\nPick Insertion point: ")
(command "insert" "eplff01a" pause 1 1 pause)
(princ))

(defun c:e020 ()
(prompt "\nPick Insertion point: ")
(command "insert" "eplff02a" pause 1 1 pause)
(princ))

(defun c:e021 ()
(prompt "\nPick Insertion point: ")
(command "insert" "eplff03a" pause 1 1 pause)
(princ))

(defun c:e022 ()
(prompt "\nPick Insertion point: ")
(command "insert" "eplff04a" pause 1 1 pause)
(princ))

(defun c:e023 ()
(prompt "\nPick Insertion point: ")
(command "insert" "eplff05a" pause 1 1 pause)
(princ))

(defun c:e024 ()
(prompt "\nPick Insertion point: ")
(command "insert" "eplff06a" pause 1 1 0)
(princ))

(defun c:e025 ()
(prompt "\nPick Insertion point: ")
(command "insert" "eplff07a" pause 1 1.0204 pause)
(princ))

(defun c:e026 ()
(prompt "\nPick Insertion point: ")
(command "insert" "eplff08a" pause 1 1 pause)
(princ))

(defun c:e027 ()
(prompt "\nPick Insertion point: ")
(command "insert" "eplff09a" pause 1 1 pause)
(princ))

(defun c:e028 ()
(prompt "\nPick Insertion point: ")
(command "insert" "eplff10a" pause 1 1 pause)
(princ))

(defun c:e029 ()
(prompt "\nPick Insertion point: ")
(command "insert" "eplff11a" pause 1 1 0)
(princ))

(defun c:e030 ()
(prompt "\nPick Insertion point: ")
(command "insert" "eplff12a" pause 1 1 0)
(princ))

(defun c:e031 ()
(prompt "\nPick Insertion point: ")
(command "insert" "eplff21a" pause 1 1 pause)
(princ))

(defun c:e032 ()
(prompt "\nPick Insertion point: ")
(command "insert" "eplfi000" pause 48 48 0)
(princ))

(defun c:e033 ()
(prompt "\nPick Insertion point: ")
(command "insert" "eplfi010" pause 48 48 0)
(princ))

(defun c:e034 ()
(prompt "\nPick Insertion point: ")
(command "insert" "eplfi020" pause 48 48 0)
(princ))

(defun c:e035 ()
(prompt "\nPick Insertion point: ")
(command "insert" "epjbox00" pause dwgsc dwgsc 0)
(princ))

(defun c:e036 ()
(prompt "\nPick Insertion point: ")
(command "insert" "epjbox01" pause dwgsc dwgsc 0)
(princ))

(defun c:e037 ()
(setq dir
	(getstring
		(strcat "\nInsertion point from which side of block? ( <T>op <B>ottom <R>ight <L>eft> ): ")
	)
)
(prompt "\nPick Insertion point: ")
(if (or (= dir "t") (= dir "T")) (command "insert" "epjbox2t" PAUSE dwgsc dwgsc 0))
(if (or (= dir "b") (= dir "B")) (command "insert" "epjbox2b" PAUSE dwgsc dwgsc 0))
(if (or (= dir "r") (= dir "R")) (command "insert" "epjbox2r" PAUSE dwgsc dwgsc 0))
(if (or (= dir "l") (= dir "L")) (command "insert" "epjbox2l" PAUSE dwgsc dwgsc 0))
(princ))

(defun c:e038 ()
(setq dir
	(getstring
		(strcat "\nInsertion point from which side of block? ( <T>op <B>ottom <R>ight <L>eft> ): ")
	)
)
(prompt "\nPick Insertion point: ")
(if (or (= dir "t") (= dir "T")) (command "insert" "epenpn0t" PAUSE 64 100 0))
(if (or (= dir "b") (= dir "B")) (command "insert" "epenpn0b" PAUSE 64 100 0))
(if (or (= dir "r") (= dir "R")) (command "insert" "epenpn0r" PAUSE 100 64 0))
(if (or (= dir "l") (= dir "L")) (command "insert" "epenpn0l" PAUSE 100 64 0))
(princ))

(defun c:pnlf ()
(c:e038)
(princ))

(defun c:e039 ()
(setq dir
	(getstring
		(strcat "\nInsertion point from which side of block? ( <T>op <B>ottom <R>ight <L>eft> ): ")
	)
)
(prompt "\nPick Insertion point: ")
(if (or (= dir "t") (= dir "T")) (command "insert" "epenpn1t" PAUSE 64 100 0))
(if (or (= dir "b") (= dir "B")) (command "insert" "epenpn1b" PAUSE 64 100 0))
(if (or (= dir "r") (= dir "R")) (command "insert" "epenpn1r" PAUSE 100 64 0))
(if (or (= dir "l") (= dir "L")) (command "insert" "epenpn1l" PAUSE 100 64 0))
(princ))

(defun c:pnl ()
(c:e039)
(princ))

(defun c:e040 ()
(prompt "\nPick Insertion point: ")
(command "insert" "epenpb00" pause dwgsc dwgsc 0)
(princ))

(defun c:e041 ()
(prompt "\nPick Insertion point: ")
(command "insert" "epfan000" pause (* dwgsc 0.75) (* dwgsc 0.75) 0)
(princ))

(defun c:e042 ()
(prompt "\nPick Insertion point: ")
(command "insert" "epmtr000" pause (* dwgsc 0.75) (* dwgsc 0.75) 0)
(princ))

(defun c:e043 ()
(setq dir
	(getstring
		(strcat "\nInsertion point from which side of block? ( <T>op <B>ottom <R>ight <L>eft> ): ")
	)
)
(prompt "\nPick Insertion point: ")
(if (or (= dir "t") (= dir "T")) (command "insert" "epmtrc0t" PAUSE (* dwgsc 0.75) (* dwgsc 0.75) 0))
(if (or (= dir "b") (= dir "B")) (command "insert" "epmtrc0b" PAUSE (* dwgsc 0.75) (* dwgsc 0.75) 0))
(if (or (= dir "r") (= dir "R")) (command "insert" "epmtrc0r" PAUSE (* dwgsc 0.75) (* dwgsc 0.75) 0))
(if (or (= dir "l") (= dir "L")) (command "insert" "epmtrc0l" PAUSE (* dwgsc 0.75) (* dwgsc 0.75) 0))
(princ))

(defun c:e044 ()
(setq dir
	(getstring
		(strcat "\nInsertion point from which side of block? ( <T>op <B>ottom <R>ight <L>eft> ): ")
	)
)
(prompt "\nPick Insertion point: ")
(if (or (= dir "t") (= dir "T")) (command "insert" "epmtrc1t" PAUSE (* dwgsc 0.75) (* dwgsc 0.75) 0))
(if (or (= dir "b") (= dir "B")) (command "insert" "epmtrc1b" PAUSE (* dwgsc 0.75) (* dwgsc 0.75) 0))
(if (or (= dir "r") (= dir "R")) (command "insert" "epmtrc1r" PAUSE (* dwgsc 0.75) (* dwgsc 0.75) 0))
(if (or (= dir "l") (= dir "L")) (command "insert" "epmtrc1l" PAUSE (* dwgsc 0.75) (* dwgsc 0.75) 0))
(princ))

(defun c:e045 ()
(setq dir
	(getstring
		(strcat "\nInsertion point from which side of block? ( <T>op <B>ottom <R>ight <L>eft> ): ")
	)
)
(prompt "\nPick Insertion point: ")
(if (or (= dir "t") (= dir "T")) (command "insert" "epmtrc2t" PAUSE (* dwgsc 0.75) (* dwgsc 0.75) 0))
(if (or (= dir "b") (= dir "B")) (command "insert" "epmtrc2b" PAUSE (* dwgsc 0.75) (* dwgsc 0.75) 0))
(if (or (= dir "r") (= dir "R")) (command "insert" "epmtrc2r" PAUSE (* dwgsc 0.75) (* dwgsc 0.75) 0))
(if (or (= dir "l") (= dir "L")) (command "insert" "epmtrc2l" PAUSE (* dwgsc 0.75) (* dwgsc 0.75) 0))
(princ))

(defun c:e046 ()
(setq dir
	(getstring
		(strcat "\nInsertion point from which side of block? ( <T>op <B>ottom <R>ight <L>eft> ): ")
	)
)
(prompt "\nPick Insertion point: ")
(if (or (= dir "t") (= dir "T")) (command "insert" "epmtrc4t" PAUSE (* dwgsc 0.75) (* dwgsc 0.75) 0))
(if (or (= dir "b") (= dir "B")) (command "insert" "epmtrc4b" PAUSE (* dwgsc 0.75) (* dwgsc 0.75) 0))
(if (or (= dir "r") (= dir "R")) (command "insert" "epmtrc4r" PAUSE (* dwgsc 0.75) (* dwgsc 0.75) 0))
(if (or (= dir "l") (= dir "L")) (command "insert" "epmtrc4l" PAUSE (* dwgsc 0.75) (* dwgsc 0.75) 0))
(princ))

(defun c:e047 ()
(setq dir
	(getstring
		(strcat "\nInsertion point from which side of block? ( <T>op <B>ottom <R>ight <L>eft> ): ")
	)
)
(prompt "\nPick Insertion point: ")
(if (or (= dir "t") (= dir "T")) (command "insert" "epmtrc5t" PAUSE (* dwgsc 0.75) (* dwgsc 0.75) 0))
(if (or (= dir "b") (= dir "B")) (command "insert" "epmtrc5b" PAUSE (* dwgsc 0.75) (* dwgsc 0.75) 0))
(if (or (= dir "r") (= dir "R")) (command "insert" "epmtrc5r" PAUSE (* dwgsc 0.75) (* dwgsc 0.75) 0))
(if (or (= dir "l") (= dir "L")) (command "insert" "epmtrc5l" PAUSE (* dwgsc 0.75) (* dwgsc 0.75) 0))
(princ))

(defun c:e048 ()
(setq dir
	(getstring
		(strcat "\nInsertion point from which side of block? ( <T>op <B>ottom <R>ight <L>eft> ): ")
	)
)
(prompt "\nPick Insertion point: ")
(if (or (= dir "t") (= dir "T")) (command "insert" "epmtrc6t" PAUSE (* dwgsc 0.75) (* dwgsc 0.75) 0))
(if (or (= dir "b") (= dir "B")) (command "insert" "epmtrc6b" PAUSE (* dwgsc 0.75) (* dwgsc 0.75) 0))
(if (or (= dir "r") (= dir "R")) (command "insert" "epmtrc6r" PAUSE (* dwgsc 0.75) (* dwgsc 0.75) 0))
(if (or (= dir "l") (= dir "L")) (command "insert" "epmtrc6l" PAUSE (* dwgsc 0.75) (* dwgsc 0.75) 0))
(princ))

(defun c:e049 ()
(setq dir
	(getstring
		(strcat "\nInsertion point from which side of block? ( <T>op <B>ottom <R>ight <L>eft> ): ")
	)
)
(prompt "\nPick Insertion point: ")
(if (or (= dir "t") (= dir "T")) (command "insert" "epmtrc3t" PAUSE (* dwgsc 0.75) (* dwgsc 0.75) 0))
(if (or (= dir "b") (= dir "B")) (command "insert" "epmtrc3b" PAUSE (* dwgsc 0.75) (* dwgsc 0.75) 0))
(if (or (= dir "r") (= dir "R")) (command "insert" "epmtrc3r" PAUSE (* dwgsc 0.75) (* dwgsc 0.75) 0))
(if (or (= dir "l") (= dir "L")) (command "insert" "epmtrc3l" PAUSE (* dwgsc 0.75) (* dwgsc 0.75) 0))
(princ))

(defun c:e050 ()
(setq dir
	(getstring
		(strcat "\nInsertion point from which side of block? ( <T>op <B>ottom <R>ight <L>eft> ): ")
	)
)
(prompt "\nPick Insertion point: ")
(if (or (= dir "t") (= dir "T")) (command "insert" "epswt04t" PAUSE (* dwgsc 0.75) (* dwgsc 0.75) 0))
(if (or (= dir "b") (= dir "B")) (command "insert" "epswt04b" PAUSE (* dwgsc 0.75) (* dwgsc 0.75) 0))
(if (or (= dir "r") (= dir "R")) (command "insert" "epswt04r" PAUSE (* dwgsc 0.75) (* dwgsc 0.75) 0))
(if (or (= dir "l") (= dir "L")) (command "insert" "epswt04l" PAUSE (* dwgsc 0.75) (* dwgsc 0.75) 0))
(princ))

(defun c:e051 ()
(setq dir
	(getstring
		(strcat "\nInsertion point from which side of block? ( <T>op <B>ottom <R>ight <L>eft> ): ")
	)
)
(prompt "\nPick Insertion point: ")
(if (or (= dir "t") (= dir "T")) (command "insert" "epswt05t" PAUSE (* dwgsc 0.75) (* dwgsc 0.75) 0))
(if (or (= dir "b") (= dir "B")) (command "insert" "epswt05b" PAUSE (* dwgsc 0.75) (* dwgsc 0.75) 0))
(if (or (= dir "r") (= dir "R")) (command "insert" "epswt05r" PAUSE (* dwgsc 0.75) (* dwgsc 0.75) 0))
(if (or (= dir "l") (= dir "L")) (command "insert" "epswt05l" PAUSE (* dwgsc 0.75) (* dwgsc 0.75) 0))
(princ))

(defun c:e052 ()
(setq dir
	(getstring
		(strcat "\nInsertion point from which side of block? ( <T>op <B>ottom <R>ight <L>eft> ): ")
	)
)
(prompt "\nPick Insertion point: ")
(if (or (= dir "t") (= dir "T")) (command "insert" "epswt06t" PAUSE (* dwgsc 0.75) (* dwgsc 0.75) 0))
(if (or (= dir "b") (= dir "B")) (command "insert" "epswt06b" PAUSE (* dwgsc 0.75) (* dwgsc 0.75) 0))
(if (or (= dir "r") (= dir "R")) (command "insert" "epswt06r" PAUSE (* dwgsc 0.75) (* dwgsc 0.75) 0))
(if (or (= dir "l") (= dir "L")) (command "insert" "epswt06l" PAUSE (* dwgsc 0.75) (* dwgsc 0.75) 0))
(princ))

(defun c:e053 ()
(setq dir
	(getstring
		(strcat "\nInsertion point from which side of block? ( <T>op <B>ottom <R>ight <L>eft> ): ")
	)
)
(prompt "\nPick Insertion point: ")
(if (or (= dir "t") (= dir "T")) (command "insert" "epswt07t" PAUSE (* dwgsc 0.75) (* dwgsc 0.75) 0))
(if (or (= dir "b") (= dir "B")) (command "insert" "epswt07b" PAUSE (* dwgsc 0.75) (* dwgsc 0.75) 0))
(if (or (= dir "r") (= dir "R")) (command "insert" "epswt07r" PAUSE (* dwgsc 0.75) (* dwgsc 0.75) 0))
(if (or (= dir "l") (= dir "L")) (command "insert" "epswt07l" PAUSE (* dwgsc 0.75) (* dwgsc 0.75) 0))
(princ))

(defun c:e054 ()
(prompt "\nPick Insertion point: ")
(command "insert" "epro0000" pause (* dwgsc 0.75) (* dwgsc 0.75) 0)
(princ))

(defun c:e055 ()
(prompt "\nPick Insertion point: ")
(command "insert" "epro0030" pause (* dwgsc 0.75) (* dwgsc 0.75) 0)
(princ))

(defun c:e056 ()
(prompt "\nPick Insertion point: ")
(command "insert" "eprod000" pause (* dwgsc 0.75) (* dwgsc 0.75) pause)
(princ))

(defun c:e057 ()
(prompt "\nPick Insertion point: ")
(command "insert" "epros000" pause (* dwgsc 0.75) (* dwgsc 0.75) 0)
(princ))

(defun c:e058 ()
(prompt "\nPick Insertion point: ")
(command "insert" "epro0050" pause (* dwgsc 0.75) (* dwgsc 0.75) 0)
(princ))

(defun c:e059 ()
(setq dir
	(getstring
		(strcat "\nInsertion point from which side of block? ( <T>op <B>ottom <R>ight <L>eft> ): ")
	)
)
(prompt "\nPick Insertion point: ")
(if (or (= dir "t") (= dir "T")) (command "insert" "epro000t" PAUSE (* dwgsc 0.75) (* dwgsc 0.75) 0))
(if (or (= dir "b") (= dir "B")) (command "insert" "epro000b" PAUSE (* dwgsc 0.75) (* dwgsc 0.75) 0))
(if (or (= dir "r") (= dir "R")) (command "insert" "epro000r" PAUSE (* dwgsc 0.75) (* dwgsc 0.75) 0))
(if (or (= dir "l") (= dir "L")) (command "insert" "epro000l" PAUSE (* dwgsc 0.75) (* dwgsc 0.75) 0))
(princ))

(defun c:e060 ()
(setq dir
	(getstring
		(strcat "\nInsertion point from which side of block? ( <T>op <B>ottom <R>ight <L>eft> ): ")
	)
)
(prompt "\nPick Insertion point: ")
(if (or (= dir "t") (= dir "T")) (command "insert" "epro001t" PAUSE (* dwgsc 0.75) (* dwgsc 0.75) 0))
(if (or (= dir "b") (= dir "B")) (command "insert" "epro001b" PAUSE (* dwgsc 0.75) (* dwgsc 0.75) 0))
(if (or (= dir "r") (= dir "R")) (command "insert" "epro001r" PAUSE (* dwgsc 0.75) (* dwgsc 0.75) 0))
(if (or (= dir "l") (= dir "L")) (command "insert" "epro001l" PAUSE (* dwgsc 0.75) (* dwgsc 0.75) 0))
(princ))

(defun c:e061 ()
(setq dir
	(getstring
		(strcat "\nInsertion point from which side of block? ( <T>op <B>ottom <R>ight <L>eft> ): ")
	)
)
(prompt "\nPick Insertion point: ")
(if (or (= dir "t") (= dir "T")) (command "insert" "epro002t" PAUSE (* dwgsc 0.75) (* dwgsc 0.75) 0))
(if (or (= dir "b") (= dir "B")) (command "insert" "epro002b" PAUSE (* dwgsc 0.75) (* dwgsc 0.75) 0))
(if (or (= dir "r") (= dir "R")) (command "insert" "epro002r" PAUSE (* dwgsc 0.75) (* dwgsc 0.75) 0))
(if (or (= dir "l") (= dir "L")) (command "insert" "epro002l" PAUSE (* dwgsc 0.75) (* dwgsc 0.75) 0))
(princ))

(defun c:e062 ()
(setq dir
	(getstring
		(strcat "\nInsertion point from which side of block? ( <T>op <B>ottom <R>ight <L>eft> ): ")
	)
)
(prompt "\nPick Insertion point: ")
(if (or (= dir "t") (= dir "T")) (command "insert" "epro003t" PAUSE (* dwgsc 0.75) (* dwgsc 0.75) 0))
(if (or (= dir "b") (= dir "B")) (command "insert" "epro003b" PAUSE (* dwgsc 0.75) (* dwgsc 0.75) 0))
(if (or (= dir "r") (= dir "R")) (command "insert" "epro003r" PAUSE (* dwgsc 0.75) (* dwgsc 0.75) 0))
(if (or (= dir "l") (= dir "L")) (command "insert" "epro003l" PAUSE (* dwgsc 0.75) (* dwgsc 0.75) 0))
(princ))

(defun c:e063 ()
(setq dir
	(getstring
		(strcat "\nInsertion point from which side of block? ( <T>op <B>ottom <R>ight <L>eft> ): ")
	)
)
(prompt "\nPick Insertion point: ")
(if (or (= dir "t") (= dir "T")) (command "insert" "epro004t" PAUSE (* dwgsc 0.75) (* dwgsc 0.75) 0))
(if (or (= dir "b") (= dir "B")) (command "insert" "epro004b" PAUSE (* dwgsc 0.75) (* dwgsc 0.75) 0))
(if (or (= dir "r") (= dir "R")) (command "insert" "epro004r" PAUSE (* dwgsc 0.75) (* dwgsc 0.75) 0))
(if (or (= dir "l") (= dir "L")) (command "insert" "epro004l" PAUSE (* dwgsc 0.75) (* dwgsc 0.75) 0))
(princ))

(defun c:e064 ()
(setq dir
	(getstring
		(strcat "\nInsertion point from which side of block? ( <T>op <B>ottom <R>ight <L>eft> ): ")
	)
)
(prompt "\nPick Insertion point: ")
(if (or (= dir "t") (= dir "T")) (command "insert" "epro006t" PAUSE (* dwgsc 0.75) (* dwgsc 0.75) 0))
(if (or (= dir "b") (= dir "B")) (command "insert" "epro006b" PAUSE (* dwgsc 0.75) (* dwgsc 0.75) 0))
(if (or (= dir "r") (= dir "R")) (command "insert" "epro006r" PAUSE (* dwgsc 0.75) (* dwgsc 0.75) 0))
(if (or (= dir "l") (= dir "L")) (command "insert" "epro006l" PAUSE (* dwgsc 0.75) (* dwgsc 0.75) 0))
(princ))

(defun c:e065 ()
(setq dir
	(getstring
		(strcat "\nInsertion point from which side of block? ( <T>op <B>ottom <R>ight <L>eft> ): ")
	)
)
(prompt "\nPick Insertion point: ")
(if (or (= dir "t") (= dir "T")) (command "insert" "eprod00t" PAUSE (* dwgsc 0.5) (* dwgsc 0.5) 0))
(if (or (= dir "b") (= dir "B")) (command "insert" "eprod00b" PAUSE (* dwgsc 0.5) (* dwgsc 0.5) 0))
(if (or (= dir "r") (= dir "R")) (command "insert" "eprod00r" PAUSE (* dwgsc 0.5) (* dwgsc 0.5) 0))
(if (or (= dir "l") (= dir "L")) (command "insert" "eprod00l" PAUSE (* dwgsc 0.5) (* dwgsc 0.5) 0))
(princ))

(defun c:e066 ()
(setq dir
	(getstring
		(strcat "\nInsertion point from which side of block? ( <T>op <B>ottom <R>ight <L>eft> ): ")
	)
)
(prompt "\nPick Insertion point: ")
(if (or (= dir "t") (= dir "T")) (command "insert" "eprod01t" PAUSE (* dwgsc 0.75) (* dwgsc 0.75) 0))
(if (or (= dir "b") (= dir "B")) (command "insert" "eprod01b" PAUSE (* dwgsc 0.75) (* dwgsc 0.75) 0))
(if (or (= dir "r") (= dir "R")) (command "insert" "eprod01r" PAUSE (* dwgsc 0.75) (* dwgsc 0.75) 0))
(if (or (= dir "l") (= dir "L")) (command "insert" "eprod01l" PAUSE (* dwgsc 0.75) (* dwgsc 0.75) 0))
(princ))

(defun c:e067 ()
(setq dir
	(getstring
		(strcat "\nInsertion point from which side of block? ( <T>op <B>ottom <R>ight <L>eft> ): ")
	)
)
(prompt "\nPick Insertion point: ")
(if (or (= dir "t") (= dir "T")) (command "insert" "eprod02t" PAUSE (* dwgsc 0.75) (* dwgsc 0.75) 0))
(if (or (= dir "b") (= dir "B")) (command "insert" "eprod02b" PAUSE (* dwgsc 0.75) (* dwgsc 0.75) 0))
(if (or (= dir "r") (= dir "R")) (command "insert" "eprod02r" PAUSE (* dwgsc 0.75) (* dwgsc 0.75) 0))
(if (or (= dir "l") (= dir "L")) (command "insert" "eprod02l" PAUSE (* dwgsc 0.75) (* dwgsc 0.75) 0))
(princ))

(defun c:e068 ()
(setq dir
	(getstring
		(strcat "\nInsertion point from which side of block? ( <T>op <B>ottom <R>ight <L>eft> ): ")
	)
)
(prompt "\nPick Insertion point: ")
(if (or (= dir "t") (= dir "T")) (command "insert" "eprod03t" PAUSE (* dwgsc 0.75) (* dwgsc 0.75) 0))
(if (or (= dir "b") (= dir "B")) (command "insert" "eprod03b" PAUSE (* dwgsc 0.75) (* dwgsc 0.75) 0))
(if (or (= dir "r") (= dir "R")) (command "insert" "eprod03r" PAUSE (* dwgsc 0.75) (* dwgsc 0.75) 0))
(if (or (= dir "l") (= dir "L")) (command "insert" "eprod03l" PAUSE (* dwgsc 0.75) (* dwgsc 0.75) 0))
(princ))

(defun c:e069 ()
(setq dir
	(getstring
		(strcat "\nInsertion point from which side of block? ( <T>op <B>ottom <R>ight <L>eft> ): ")
	)
)
(prompt "\nPick Insertion point: ")
(if (or (= dir "t") (= dir "T")) (command "insert" "eprod04t" PAUSE (* dwgsc 0.75) (* dwgsc 0.75) 0))
(if (or (= dir "b") (= dir "B")) (command "insert" "eprod04b" PAUSE (* dwgsc 0.75) (* dwgsc 0.75) 0))
(if (or (= dir "r") (= dir "R")) (command "insert" "eprod04r" PAUSE (* dwgsc 0.75) (* dwgsc 0.75) 0))
(if (or (= dir "l") (= dir "L")) (command "insert" "eprod04l" PAUSE (* dwgsc 0.75) (* dwgsc 0.75) 0))
(princ))

(defun c:e070 ()
(setq dir
	(getstring
		(strcat "\nInsertion point from which side of block? ( <T>op <B>ottom <R>ight <L>eft> ): ")
	)
)
(prompt "\nPick Insertion point: ")
(if (or (= dir "t") (= dir "T")) (command "insert" "eprod05t" PAUSE (* dwgsc 0.75) (* dwgsc 0.75) 0))
(if (or (= dir "b") (= dir "B")) (command "insert" "eprod05b" PAUSE (* dwgsc 0.75) (* dwgsc 0.75) 0))
(if (or (= dir "r") (= dir "R")) (command "insert" "eprod05r" PAUSE (* dwgsc 0.75) (* dwgsc 0.75) 0))
(if (or (= dir "l") (= dir "L")) (command "insert" "eprod05l" PAUSE (* dwgsc 0.75) (* dwgsc 0.75) 0))
(princ))

(defun c:e071 ()
(setq dir
	(getstring
		(strcat "\nInsertion point from which side of block? ( <T>op <B>ottom <R>ight <L>eft> ): ")
	)
)
(prompt "\nPick Insertion point: ")
(if (or (= dir "t") (= dir "T")) (command "insert" "eprsd00t" PAUSE (* dwgsc 0.75) (* dwgsc 0.75) 0))
(if (or (= dir "b") (= dir "B")) (command "insert" "eprsd00b" PAUSE (* dwgsc 0.75) (* dwgsc 0.75) 0))
(if (or (= dir "r") (= dir "R")) (command "insert" "eprsd00r" PAUSE (* dwgsc 0.75) (* dwgsc 0.75) 0))
(if (or (= dir "l") (= dir "L")) (command "insert" "eprsd00l" PAUSE (* dwgsc 0.75) (* dwgsc 0.75) 0))
(princ))

(defun c:e072 ()
(setq dir
	(getstring
		(strcat "\nInsertion point from which side of block? ( <T>op <B>ottom <R>ight <L>eft> ): ")
	)
)
(prompt "\nPick Insertion point: ")
(if (or (= dir "t") (= dir "T")) (command "insert" "eproq00t" PAUSE (* dwgsc 0.5) (* dwgsc 0.5) 0))
(if (or (= dir "b") (= dir "B")) (command "insert" "eproq00b" PAUSE (* dwgsc 0.5) (* dwgsc 0.5) 0))
(if (or (= dir "r") (= dir "R")) (command "insert" "eproq00r" PAUSE (* dwgsc 0.5) (* dwgsc 0.5) 0))
(if (or (= dir "l") (= dir "L")) (command "insert" "eproq00l" PAUSE (* dwgsc 0.5) (* dwgsc 0.5) 0))
(princ))

(defun c:e073 ()
(setq dir
	(getstring
		(strcat "\nInsertion point from which side of block? ( <T>op <B>ottom <R>ight <L>eft> ): ")
	)
)
(prompt "\nPick Insertion point: ")
(if (or (= dir "t") (= dir "T")) (command "insert" "epros00t" PAUSE (* dwgsc 0.75) (* dwgsc 0.75) 0))
(if (or (= dir "b") (= dir "B")) (command "insert" "epros00b" PAUSE (* dwgsc 0.75) (* dwgsc 0.75) 0))
(if (or (= dir "r") (= dir "R")) (command "insert" "epros00r" PAUSE (* dwgsc 0.75) (* dwgsc 0.75) 0))
(if (or (= dir "l") (= dir "L")) (command "insert" "epros00l" PAUSE (* dwgsc 0.75) (* dwgsc 0.75) 0))
(princ))

(defun c:e074 ()
(setq dir
	(getstring
		(strcat "\nInsertion point from which side of block? ( <T>op <B>ottom <R>ight <L>eft> ): ")
	)
)
(prompt "\nPick Insertion point: ")
(if (or (= dir "t") (= dir "T")) (command "insert" "epro005t" PAUSE (* dwgsc 0.75) (* dwgsc 0.75) 0))
(if (or (= dir "b") (= dir "B")) (command "insert" "epro005b" PAUSE (* dwgsc 0.75) (* dwgsc 0.75) 0))
(if (or (= dir "r") (= dir "R")) (command "insert" "epro005r" PAUSE (* dwgsc 0.75) (* dwgsc 0.75) 0))
(if (or (= dir "l") (= dir "L")) (command "insert" "epro005l" PAUSE (* dwgsc 0.75) (* dwgsc 0.75) 0))
(princ))

(defun c:e075 ()
(setq dir
	(getstring
		(strcat "\nInsertion point from which side of block? ( <T>op <B>ottom <R>ight <L>eft> ): ")
	)
)
(prompt "\nPick Insertion point: ")
(if (or (= dir "t") (= dir "T")) (command "insert" "eprot00t" PAUSE (* dwgsc 0.75) (* dwgsc 0.75) 0))
(if (or (= dir "b") (= dir "B")) (command "insert" "eprot00b" PAUSE (* dwgsc 0.75) (* dwgsc 0.75) 0))
(if (or (= dir "r") (= dir "R")) (command "insert" "eprot00r" PAUSE (* dwgsc 0.75) (* dwgsc 0.75) 0))
(if (or (= dir "l") (= dir "L")) (command "insert" "eprot00l" PAUSE (* dwgsc 0.75) (* dwgsc 0.75) 0))
(princ))

(defun c:e076 ()
(setq dir
	(getstring
		(strcat "\nInsertion point from which side of block? ( <T>op <B>ottom <R>ight <L>eft> ): ")
	)
)
(prompt "\nPick Insertion point: ")
(if (or (= dir "t") (= dir "T")) (command "insert" "eprot01t" PAUSE (* dwgsc 0.75) (* dwgsc 0.75) 0))
(if (or (= dir "b") (= dir "B")) (command "insert" "eprot01b" PAUSE (* dwgsc 0.75) (* dwgsc 0.75) 0))
(if (or (= dir "r") (= dir "R")) (command "insert" "eprot01r" PAUSE (* dwgsc 0.75) (* dwgsc 0.75) 0))
(if (or (= dir "l") (= dir "L")) (command "insert" "eprot01l" PAUSE (* dwgsc 0.75) (* dwgsc 0.75) 0))
(princ))

(defun c:e077 ()
(setq dir
	(getstring
		(strcat "\nInsertion point from which side of block? ( <T>op <B>ottom <R>ight <L>eft> ): ")
	)
)
(prompt "\nPick Insertion point: ")
(if (or (= dir "t") (= dir "T")) (command "insert" "epswt01t" PAUSE (* 0.75 dwgsc) (* 0.75 dwgsc) 0))
(if (or (= dir "b") (= dir "B")) (command "insert" "epswt01b" PAUSE (* 0.75 dwgsc) (* 0.75 dwgsc) 0))
(if (or (= dir "r") (= dir "R")) (command "insert" "epswt01r" PAUSE (* 0.75 dwgsc) (* 0.75 dwgsc) 0))
(if (or (= dir "l") (= dir "L")) (command "insert" "epswt01l" PAUSE (* 0.75 dwgsc) (* 0.75 dwgsc) 0))
(princ))

(defun c:e078 ()
(setq dir
	(getstring
		(strcat "\nInsertion point from which side of block? ( <T>op <B>ottom <R>ight <L>eft> ): ")
	)
)
(prompt "\nPick Insertion point: ")
(if (or (= dir "t") (= dir "T")) (command "insert" "epswt02t" PAUSE dwgsc dwgsc 0))
(if (or (= dir "b") (= dir "B")) (command "insert" "epswt02b" PAUSE dwgsc dwgsc 0))
(if (or (= dir "r") (= dir "R")) (command "insert" "epswt02r" PAUSE dwgsc dwgsc 0))
(if (or (= dir "l") (= dir "L")) (command "insert" "epswt02l" PAUSE dwgsc dwgsc 0))
(princ))

(defun c:e079 ()
(setq dir
	(getstring
		(strcat "\nInsertion point from which side of block? ( <T>op <B>ottom <R>ight <L>eft> ): ")
	)
)
(prompt "\nPick Insertion point: ")
(if (or (= dir "t") (= dir "T")) (command "insert" "epswt03t" PAUSE dwgsc dwgsc 0))
(if (or (= dir "b") (= dir "B")) (command "insert" "epswt03b" PAUSE dwgsc dwgsc 0))
(if (or (= dir "r") (= dir "R")) (command "insert" "epswt03r" PAUSE dwgsc dwgsc 0))
(if (or (= dir "l") (= dir "L")) (command "insert" "epswt03l" PAUSE dwgsc dwgsc 0))
(princ))

(defun c:e080 ()
(setq dir
	(getstring
		(strcat "\nInsertion point from which side of block? ( <T>op <B>ottom <R>ight <L>eft> ): ")
	)
)
(prompt "\nPick Insertion point: ")
(if (or (= dir "t") (= dir "T")) (command "insert" "epswt08t" PAUSE dwgsc dwgsc 0))
(if (or (= dir "b") (= dir "B")) (command "insert" "epswt08b" PAUSE dwgsc dwgsc 0))
(if (or (= dir "r") (= dir "R")) (command "insert" "epswt08r" PAUSE dwgsc dwgsc 0))
(if (or (= dir "l") (= dir "L")) (command "insert" "epswt08l" PAUSE dwgsc dwgsc 0))
(princ))

(defun c:e081 ()
(setq dir
	(getstring
		(strcat "\nInsertion point from which side of block? ( <T>op <B>ottom <R>ight <L>eft> ): ")
	)
)
(prompt "\nPick Insertion point: ")
(if (or (= dir "t") (= dir "T")) (command "insert" "epswt09t" PAUSE dwgsc dwgsc 0))
(if (or (= dir "b") (= dir "B")) (command "insert" "epswt09b" PAUSE dwgsc dwgsc 0))
(if (or (= dir "r") (= dir "R")) (command "insert" "epswt09r" PAUSE dwgsc dwgsc 0))
(if (or (= dir "l") (= dir "L")) (command "insert" "epswt09l" PAUSE dwgsc dwgsc 0))
(princ))

(defun c:e082 ()
(setq dir
	(getstring
		(strcat "\nInsertion point from which side of block? ( <T>op <B>ottom <R>ight <L>eft> ): ")
	)
)
(prompt "\nPick Insertion point: ")
(if (or (= dir "t") (= dir "T")) (command "insert" "epswt10t" PAUSE dwgsc dwgsc 0))
(if (or (= dir "b") (= dir "B")) (command "insert" "epswt10b" PAUSE dwgsc dwgsc 0))
(if (or (= dir "r") (= dir "R")) (command "insert" "epswt10r" PAUSE dwgsc dwgsc 0))
(if (or (= dir "l") (= dir "L")) (command "insert" "epswt10l" PAUSE dwgsc dwgsc 0))
(princ))

(defun c:e083 ()
(setq dir
	(getstring
		(strcat "\nInsertion point from which side of block? ( <T>op <B>ottom <R>ight <L>eft> ): ")
	)
)
(prompt "\nPick Insertion point: ")
(if (or (= dir "t") (= dir "T")) (command "insert" "epswt11t" PAUSE dwgsc dwgsc 0))
(if (or (= dir "b") (= dir "B")) (command "insert" "epswt11b" PAUSE dwgsc dwgsc 0))
(if (or (= dir "r") (= dir "R")) (command "insert" "epswt11r" PAUSE dwgsc dwgsc 0))
(if (or (= dir "l") (= dir "L")) (command "insert" "epswt11l" PAUSE dwgsc dwgsc 0))
(princ))

(defun c:e084 ()
(setq dir
	(getstring
		(strcat "\nInsertion point from which side of block? ( <T>op <B>ottom <R>ight <L>eft> ): ")
	)
)
(prompt "\nPick Insertion point: ")
(if (or (= dir "t") (= dir "T")) (command "insert" "epswt15t" PAUSE (* 0.75 dwgsc) (* 0.75 dwgsc) 0))
(if (or (= dir "b") (= dir "B")) (command "insert" "epswt15b" PAUSE (* 0.75 dwgsc) (* 0.75 dwgsc) 0))
(if (or (= dir "r") (= dir "R")) (command "insert" "epswt15r" PAUSE (* 0.75 dwgsc) (* 0.75 dwgsc) 0))
(if (or (= dir "l") (= dir "L")) (command "insert" "epswt15l" PAUSE (* 0.75 dwgsc) (* 0.75 dwgsc) 0))
(princ))

(defun c:e085 ()
(setq dir
	(getstring
		(strcat "\nInsertion point from which side of block? ( <T>op <B>ottom <R>ight <L>eft> ): ")
	)
)
(prompt "\nPick Insertion point: ")
(if (or (= dir "t") (= dir "T")) (command "insert" "epswt16t" PAUSE dwgsc dwgsc 0))
(if (or (= dir "b") (= dir "B")) (command "insert" "epswt16b" PAUSE dwgsc dwgsc 0))
(if (or (= dir "r") (= dir "R")) (command "insert" "epswt16r" PAUSE dwgsc dwgsc 0))
(if (or (= dir "l") (= dir "L")) (command "insert" "epswt16l" PAUSE dwgsc dwgsc 0))
(princ))

(defun c:e086 ()
(setq dir
	(getstring
		(strcat "\nInsertion point from which side of block? ( <T>op <B>ottom <R>ight <L>eft> ): ")
	)
)
(prompt "\nPick Insertion point: ")
(if (or (= dir "t") (= dir "T")) (command "insert" "epswt17t" PAUSE dwgsc dwgsc 0))
(if (or (= dir "b") (= dir "B")) (command "insert" "epswt17b" PAUSE dwgsc dwgsc 0))
(if (or (= dir "r") (= dir "R")) (command "insert" "epswt17r" PAUSE dwgsc dwgsc 0))
(if (or (= dir "l") (= dir "L")) (command "insert" "epswt17l" PAUSE dwgsc dwgsc 0))
(princ))

(defun c:e087 ()
(setq dir
	(getstring
		(strcat "\nInsertion point from which side of block? ( <T>op <B>ottom <R>ight <L>eft> ): ")
	)
)
(prompt "\nPick Insertion point: ")
(if (or (= dir "t") (= dir "T")) (command "insert" "epswt12t" PAUSE dwgsc dwgsc 0))
(if (or (= dir "b") (= dir "B")) (command "insert" "epswt12b" PAUSE dwgsc dwgsc 0))
(if (or (= dir "r") (= dir "R")) (command "insert" "epswt12r" PAUSE dwgsc dwgsc 0))
(if (or (= dir "l") (= dir "L")) (command "insert" "epswt12l" PAUSE dwgsc dwgsc 0))
(princ))

(defun c:e088 ()
(setq dir
	(getstring
		(strcat "\nInsertion point from which side of block? ( <T>op <B>ottom <R>ight <L>eft> ): ")
	)
)
(prompt "\nPick Insertion point: ")
(if (or (= dir "t") (= dir "T")) (command "insert" "epswt13t" PAUSE dwgsc dwgsc 0))
(if (or (= dir "b") (= dir "B")) (command "insert" "epswt13b" PAUSE dwgsc dwgsc 0))
(if (or (= dir "r") (= dir "R")) (command "insert" "epswt13r" PAUSE dwgsc dwgsc 0))
(if (or (= dir "l") (= dir "L")) (command "insert" "epswt13l" PAUSE dwgsc dwgsc 0))
(princ))

(defun c:e089 ()
(setq dir
	(getstring
		(strcat "\nInsertion point from which side of block? ( <T>op <B>ottom <R>ight <L>eft> ): ")
	)
)
(prompt "\nPick Insertion point: ")
(if (or (= dir "t") (= dir "T")) (command "insert" "epswt14t" PAUSE dwgsc dwgsc 0))
(if (or (= dir "b") (= dir "B")) (command "insert" "epswt14b" PAUSE dwgsc dwgsc 0))
(if (or (= dir "r") (= dir "R")) (command "insert" "epswt14r" PAUSE dwgsc dwgsc 0))
(if (or (= dir "l") (= dir "L")) (command "insert" "epswt14l" PAUSE dwgsc dwgsc 0))
(princ))

(defun c:e090 ()
(prompt "\nPick Insertion point: ")
(command "insert" "epgrndbf" pause (* dwgsc 0.75) (* dwgsc 0.75) pause)
(princ))

(defun c:e091 ()
(prompt "\nPick Insertion point: ")
(command "insert" "epgrndcf" pause (* dwgsc 0.75) (* dwgsc 0.75) pause)
(princ))

(defun c:e092 ()
(prompt "\nPick Insertion point: ")
(command "insert" "epgrndwf" pause (* dwgsc 0.75) (* dwgsc 0.75) pause)
(princ))

(defun c:e093 ()
(prompt "\nPick Insertion point: ")
(command "insert" "epctdn0f" pause dwgsc dwgsc pause)
(princ))

(defun c:e094 ()
(prompt "\nPick Insertion point: ")
(command "insert" "epctup0f" pause dwgsc dwgsc pause)
(princ))

(defun c:e095 ()
(prompt "\nPick Insertion point: ")
(command "insert" "epfcnx0a" pause dwgsc dwgsc pause)
(princ))

(defun c:e096 ()
(prompt "\nPick Insertion point: ")
(command "insert" "ephrun1f" pause (* dwgsc 0.75) (* dwgsc 0.75) pause)
(princ))

(defun c:e097 ()
(setq dir
	(getstring
		(strcat "\nDirection of block? ( <H>orizontal <V>ertical ): ")
	)
)
(prompt "\nPick Insertion point: ")
(if (or (= dir "h") (= dir "H")) (command "insert" "edidbush" PAUSE 1 1 0))
(if (or (= dir "v") (= dir "V")) (command "insert" "edidbusv" PAUSE 1 1 0))
(princ))

(defun c:e098 ()
(setq dir
	(getstring
		(strcat "\nDirection of block? ( <H>orizontal <V>ertical ): ")
	)
)
(prompt "\nPick Insertion point: ")
(if (or (= dir "h") (= dir "H")) (command "insert" "edidbudh" PAUSE 1 1 0))
(if (or (= dir "v") (= dir "V")) (command "insert" "edidbudv" PAUSE 1 1 0))
(princ))

(defun c:e099 ()
(setq dir
	(getstring
		(strcat "\nDirection of block? ( <H>orizontal <V>ertical ): ")
	)
)
(prompt "\nPick Insertion point: ")
(if (or (= dir "h") (= dir "H")) (command "insert" "edidcblh" PAUSE 1 1 0))
(if (or (= dir "v") (= dir "V")) (command "insert" "edidcblv" PAUSE 1 1 0))
(princ))

(defun c:e100 ()
(setq dir
	(getstring
		(strcat "\nDirection of block? ( <H>orizontal <V>ertical ): ")
	)
)
(prompt "\nPick Insertion point: ")
(if (or (= dir "h") (= dir "H")) (command "insert" "edideqph" PAUSE 1 1 0))
(if (or (= dir "v") (= dir "V")) (command "insert" "edideqpv" PAUSE 1 1 0))
(princ))

(defun c:e101 ()
(setq dir
	(getstring
		(strcat "\nDirection of block? ( <H>orizontal <V>ertical ): ")
	)
)
(prompt "\nPick Insertion point: ")
(if (or (= dir "h") (= dir "H")) (command "insert" "edidmsch" PAUSE 1 1 0))
(if (or (= dir "v") (= dir "V")) (command "insert" "edidmscv" PAUSE 1 1 0))
(princ))

(defun c:e102 ()
(setq dir
	(getstring
		(strcat "\nDirection of block? ( <H>orizontal <V>ertical ): ")
	)
)
(prompt "\nPick Insertion point: ")
(if (or (= dir "h") (= dir "H")) (command "insert" "edidmcch" PAUSE 1 1 0))
(if (or (= dir "v") (= dir "V")) (command "insert" "edidmccv" PAUSE 1 1 0))
(princ))

(defun c:e103 ()
(prompt "\nPick Insertion point: ")
(command "insert" "edidnd0a" pause 1 1 0)
(princ))

(defun c:e104 ()
(prompt "\nPick Insertion point: ")
(command "insert" "edidpln0" pause 1 1 0)
(princ))

(defun c:e105 ()
(setq dir
	(getstring
		(strcat "\nDirection of block? ( <H>orizontal <V>ertical ): ")
	)
)
(prompt "\nPick Insertion point: ")
(if (or (= dir "h") (= dir "H")) (command "insert" "edidswgh" PAUSE 1 1 0))
(if (or (= dir "v") (= dir "V")) (command "insert" "edidswgv" PAUSE 1 1 0))
(princ))

(defun c:e106 ()
(setq dir
	(getstring
		(strcat "\nDirection of block? ( <H>orizontal <V>ertical ): ")
	)
)
(prompt "\nPick Insertion point: ")
(if (or (= dir "h") (= dir "H")) (command "insert" "edidutch" PAUSE 1 1 0))
(if (or (= dir "v") (= dir "V")) (command "insert" "edidutcv" PAUSE 1 1 0))
(princ))

(defun c:e107 ()
(prompt "\nPick Insertion point: ")
(command "insert" "edarr00l" pause 1 1 pause)
(princ))

(defun c:e108 ()
(prompt "\nPick Insertion point: ")
(command "insert" "edarr01l" pause 1 1 pause)
(princ))

(defun c:e109 ()
(prompt "\nPick Insertion point: ")
(command "insert" "eddotl00" pause 1 1 0)
(princ))

(defun c:e110 ()
(prompt "\nPick Insertion point: ")
(command "insert" "eddotm00" pause 1 1 0)
(princ))

(defun c:e111 ()
(prompt "\nPick Insertion point: ")
(command "insert" "eddots00" pause 1 1 0)
(princ))

(defun c:e112 ()
(prompt "\nPick Insertion point: ")
(command "insert" "edstb20f" pause 1 1 pause)
(princ))

(defun c:e113 ()
(prompt "\nPick Insertion point: ")
(command "insert" "edstb30f" pause 1 1 pause)
(princ))

(defun c:e114 ()
(prompt "\nPick Insertion point: ")
(command "insert" "edstb10f" pause 1 1 pause)
(princ))

(defun c:e115 ()
(prompt "\nPick Insertion point: ")
(command "insert" "edbat00a" pause 1 1 pause)
(princ))

(defun c:e116 ()
(setq dir
	(getstring
		(strcat "\nDirection of block? ( <H>orizontal <V>ertical ): ")
	)
)
(prompt "\nPick Insertion point: ")
(if (or (= dir "h") (= dir "H")) (command "insert" "edbatc0h" PAUSE 1 1 0))
(if (or (= dir "v") (= dir "V")) (command "insert" "edbatc0v" PAUSE 1 1 0))
(princ))

(defun c:e117 ()
(setq dir
	(getstring
		(strcat "\nDirection of block? ( <H>orizontal <V>ertical ): ")
	)
)
(prompt "\nPick Insertion point: ")
(if (or (= dir "h") (= dir "H")) (command "insert" "edcap00h" PAUSE 1 1 0))
(if (or (= dir "v") (= dir "V")) (command "insert" "edcap00v" PAUSE 1 1 0))
(princ))

(defun c:e118 ()
(setq dir
	(getstring
		(strcat "\nDirection of block? ( <H>orizontal <V>ertical ): ")
	)
)
(prompt "\nPick Insertion point: ")
(if (or (= dir "h") (= dir "H")) (command "insert" "edcnt10h" PAUSE 1 1 0))
(if (or (= dir "v") (= dir "V")) (command "insert" "edcnt10v" PAUSE 1 1 0))
(princ))

(defun c:e119 ()
(setq dir
	(getstring
		(strcat "\nDirection of block? ( <H>orizontal <V>ertical ): ")
	)
)
(prompt "\nPick Insertion point: ")
(if (or (= dir "h") (= dir "H")) (command "insert" "edcnt00h" PAUSE 1 1 0))
(if (or (= dir "v") (= dir "V")) (command "insert" "edcnt00v" PAUSE 1 1 0))
(princ))

(defun c:e120 ()
(prompt "\nPick Insertion point: ")
(command "insert" "edcntr00" pause 1 1 0)
(princ))

(defun c:e121 ()
(prompt "\nPick Insertion point: ")
(command "insert" "edcnvt00" pause 1 1 0)
(princ))

(defun c:e122 ()
(prompt "\nPick Insertion point: ")
(command "insert" "edclim00" pause 1 1 0)
(princ))

(defun c:e123 ()
(prompt "\nPick Insertion point: ")
(command "insert" "edinlt00" pause 1 1 0)
(princ))

(defun c:e124 ()
(prompt "\nPick Insertion point: ")
(command "insert" "edmtr000" pause 1 1 0)
(princ))

(defun c:e125 ()
(prompt "\nPick Insertion point: ")
(command "insert" "edmetr00" pause 1 1 0)
(princ))

(defun c:e126 ()
(prompt "\nPick Insertion point: ")
(command "insert" "edmiscl0" pause 1 1 0)
(princ))

(defun c:e127 ()
(prompt "\nPick Insertion point: ")
(command "insert" "edmiscm0" pause 1 1 0)
(princ))

(defun c:e128 ()
(prompt "\nPick Insertion point: ")
(command "insert" "edmiscs0" pause 1 1 0)
(princ))

(defun c:e129 ()
(setq dir
	(getstring
		(strcat "\nDirection of block? ( <H>orizontal <V>ertical ): ")
	)
)
(prompt "\nPick Insertion point: ")
(if (or (= dir "h") (= dir "H")) (command "insert" "ednetp0h" PAUSE 1 1 0))
(if (or (= dir "v") (= dir "V")) (command "insert" "ednetp0v" PAUSE 1 1 0))
(princ))

(defun c:e130 ()
(setq dir
	(getstring
		(strcat "\nType of Pushbutton Station? ( Type: <1> <2> <3> ): ")
	)
)
(prompt "\nPick Insertion point: ")
(if (= dir "1") (command "insert" "edpbs000" PAUSE 1 1 0))
(if (= dir "2") (command "insert" "edpbs100" PAUSE 1 1 0))
(if (= dir "3") (command "insert" "edpbs200" PAUSE 1 1 0))
(princ))

(defun c:e131 ()
(prompt "\nPick Insertion point: ")
(command "insert" "edpbss00" pause 1 1 0)
(princ))

(defun c:e132 ()
(prompt "\nPick Insertion point: ")
(command "insert" "edrwld00" pause 1 1 0)
(princ))

(defun c:e133 ()
(setq dir
	(getstring
		(strcat "\nDirection of block? ( <H>orizontal <V>ertical ): ")
	)
)
(prompt "\nPick Insertion point: ")
(if (or (= dir "h") (= dir "H")) (command "insert" "edrctf0h" PAUSE 1 1 0))
(if (or (= dir "v") (= dir "V")) (command "insert" "edrctf0v" PAUSE 1 1 0))
(princ))

(defun c:e134 ()
(prompt "\nPick Insertion point: ")
(command "insert" "edrly100" pause 1 1 0)
(princ))

(defun c:e135 ()
(prompt "\nPick Insertion point: ")
(command "insert" "edrly000" pause 1 1 0)
(princ))

(defun c:e136 ()
(prompt "\nPick Insertion point: ")
(command "insert" "edrly010" pause 1 1 0)
(princ))

(defun c:e137 ()
(setq dir
	(getstring
		(strcat "\nDirection of block? ( <H>orizontal <V>ertical ): ")
	)
)
(prompt "\nPick Insertion point: ")
(if (or (= dir "h") (= dir "H")) (command "insert" "edsarr0h" PAUSE 1 1 0))
(if (or (= dir "v") (= dir "V")) (command "insert" "edsarr0v" PAUSE 1 1 0))
(princ))

(defun c:e138 ()
(setq dir
	(getstring
		(strcat "\nDirection of block? ( <H>orizontal <V>ertical ): ")
	)
)
(prompt "\nPick Insertion point: ")
(if (or (= dir "h") (= dir "H")) (command "insert" "edsarr1h" PAUSE 1 1 0))
(if (or (= dir "v") (= dir "V")) (command "insert" "edsarr1v" PAUSE 1 1 0))
(princ))

(defun c:e139 ()
(prompt "\nPick Insertion point: ")
(command "insert" "edswt100" pause 1 1 0)
(princ))

(defun c:e140 ()
(setq dir
	(getstring
		(strcat "\nType of Control Switch? ( Type: <1> <2> <3> ): ")
	)
)
(prompt "\nPick Insertion point: ")
(if (= dir "1") (command "insert" "edswtc00" PAUSE 1 1 0))
(if (= dir "2") (command "insert" "edswtc10" PAUSE 1 1 0))
(if (= dir "3") (command "insert" "edswtc20" PAUSE 1 1 0))
(princ))

(defun c:e141 ()
(prompt "\nPick Insertion point: ")
(command "insert" "edswt000" pause 1 1 0)
(princ))

(defun c:e142 ()
(prompt "\nPick Insertion point: ")
(command "insert" "edswtt00" pause 1 1 0)
(princ))

(defun c:e143 ()
(prompt "\nPick Insertion point: ")
(command "insert" "edswtt10" pause 1 1 0)
(princ))

(defun c:e144 ()
(prompt "\nPick Insertion point: ")
(command "insert" "edtbpo00" pause 1 1 0)
(princ))

(defun c:e145 ()
(setq dir
	(getstring
		(strcat "\nDirection of block? ( <H>orizontal <V>ertical ): ")
	)
)
(prompt "\nPick Insertion point: ")
(if (or (= dir "h") (= dir "H")) (command "insert" "edxfmi0h" PAUSE 1 1 0))
(if (or (= dir "v") (= dir "V")) (command "insert" "edxfmi0v" PAUSE 1 1 0))
(princ))

(defun c:e146 ()
(setq dir
	(getstring
		(strcat "\nDirection of block? ( <H>orizontal <V>ertical ): ")
	)
)
(prompt "\nPick Insertion point: ")
(if (or (= dir "h") (= dir "H")) (command "insert" "edxfmv0h" PAUSE 1 1 0))
(if (or (= dir "v") (= dir "V")) (command "insert" "edxfmv0v" PAUSE 1 1 0))
(princ))

(defun c:e147 ()
(prompt "\nPick Insertion point: ")
(command "insert" "edxfmv10" pause 1 1 0)
(princ))

(defun c:e148 ()
(setq dir
	(getstring
		(strcat "\nDirection of block? ( <H>orizontal <V>ertical ): ")
	)
)
(prompt "\nPick Insertion point: ")
(if (or (= dir "h") (= dir "H")) (command "insert" "edxfmp0h" PAUSE 1 1 0))
(if (or (= dir "v") (= dir "V")) (command "insert" "edxfmp0v" PAUSE 1 1 0))
(princ))

(defun c:e149 ()
(prompt "\nPick Insertion point: ")
(command "insert" "edxfms00" pause 1 1 0)
(princ))

(defun c:e150 ()
(prompt "\nPick Insertion point: ")
(command "insert" "edavr000" pause 1 1 0)
(princ))

(defun c:e151 ()
(prompt "\nPick Insertion point: ")
(command "insert" "edstr000" pause 1 1 0)
(princ))

(defun c:e152 ()
(prompt "\nPick Insertion point: ")
(command "insert" "edstr010" pause 1 1 0)
(princ))

(defun c:e153 ()
(prompt "\nPick Insertion point: ")
(command "insert" "edstr020" pause 1 1 0)
(princ))

(defun c:e154 ()
(prompt "\nPick Insertion point: ")
(command "insert" "edstr030" pause 1 1 0)
(princ))

(defun c:e155 ()
(prompt "\nPick Insertion point: ")
(command "insert" "edstrc00" pause 1 1 0)
(princ))

(defun c:e156 ()
(prompt "\nPick Insertion point: ")
(command "insert" "edstrc10" pause 1 1 0)
(princ))

(defun c:e157 ()
(prompt "\nPick Insertion point: ")
(command "insert" "edstrc30" pause 1 1 0)
(princ))

(defun c:e158 ()
(prompt "\nPick Insertion point: ")
(command "insert" "edstrc40" pause 1 1 0)
(princ))

(defun c:e159 ()
(prompt "\nPick Insertion point: ")
(command "insert" "edstrm00" pause 1 1 0)
(princ))

(defun c:e160 ()
(setq dir
	(getstring
		(strcat "\nDirection of block? ( <H>orizontal <V>ertical ): ")
	)
)
(prompt "\nPick Insertion point: ")
(if (or (= dir "h") (= dir "H")) (command "insert" "edcbk00h" PAUSE 1 1 0))
(if (or (= dir "v") (= dir "V")) (command "insert" "edcbk00v" PAUSE 1 1 0))
(princ))

(defun c:e161 ()
(setq dir
	(getstring
		(strcat "\nDirection of block? ( <H>orizontal <V>ertical ): ")
	)
)
(prompt "\nPick Insertion point: ")
(if (or (= dir "h") (= dir "H")) (command "insert" "edcbk01h" PAUSE 1 1 0))
(if (or (= dir "v") (= dir "V")) (command "insert" "edcbk01v" PAUSE 1 1 0))
(princ))

(defun c:e162 ()
(setq dir
	(getstring
		(strcat "\nDirection of block? ( <H>orizontal <V>ertical ): ")
	)
)
(prompt "\nPick Insertion point: ")
(if (or (= dir "h") (= dir "H")) (command "insert" "edfus00h" PAUSE 1 1 0))
(if (or (= dir "v") (= dir "V")) (command "insert" "edfus00v" PAUSE 1 1 0))
(princ))

(defun c:e163 ()
(setq dir
	(getstring
		(strcat "\nDirection of block? ( <H>orizontal <V>ertical ): ")
	)
)
(prompt "\nPick Insertion point: ")
(if (or (= dir "h") (= dir "H")) (command "insert" "edfus10h" PAUSE 1 1 0))
(if (or (= dir "v") (= dir "V")) (command "insert" "edfus10v" PAUSE 1 1 0))
(princ))

(defun c:e164 ()
(prompt "\nPick Insertion point: ")
(command "insert" "editlk00" pause 1 1 0)
(princ))

(defun c:e165 ()
(setq dir
	(getstring
		(strcat "\nDirection of block? ( <H>orizontal <V>ertical ): ")
	)
)
(prompt "\nPick Insertion point: ")
(if (or (= dir "h") (= dir "H")) (command "insert" "edswtd0h" PAUSE 1 1 0))
(if (or (= dir "v") (= dir "V")) (command "insert" "edswtd0v" PAUSE 1 1 0))
(princ))

(defun c:e166 ()
(setq dir
	(getstring
		(strcat "\nDirection of block? ( <H>orizontal <V>ertical ): ")
	)
)
(prompt "\nPick Insertion point: ")
(if (or (= dir "h") (= dir "H")) (command "insert" "edswtd2h" PAUSE 1 1 0))
(if (or (= dir "v") (= dir "V")) (command "insert" "edswtd2v" PAUSE 1 1 0))
(princ))

(defun c:e167 ()
(setq dir
	(getstring
		(strcat "\nDirection of block? ( <H>orizontal <V>ertical ): ")
	)
)
(prompt "\nPick Insertion point: ")
(if (or (= dir "h") (= dir "H")) (command "insert" "edswtd1h" PAUSE 1 1 0))
(if (or (= dir "v") (= dir "V")) (command "insert" "edswtd1v" PAUSE 1 1 0))
(princ))

(defun c:e168 ()
(setq dir
	(getstring
		(strcat "\nDirection of block? ( <H>orizontal <V>ertical ): ")
	)
)
(prompt "\nPick Insertion point: ")
(if (or (= dir "h") (= dir "H")) (command "insert" "edswtd3h" PAUSE 1 1 0))
(if (or (= dir "v") (= dir "V")) (command "insert" "edswtd3v" PAUSE 1 1 0))
(princ))

(defun c:e169 ()
(prompt "\nPick Insertion point: ")
(command "insert" "eddlt10t" pause 1 1 0)
(princ))

(defun c:e170 ()
(prompt "\nPick Insertion point: ")
(command "insert" "eddlt3a0" pause 1 1 0)
(princ))

(defun c:e171 ()
(prompt "\nPick Insertion point: ")
(command "insert" "eddlt2a0" pause 1 1 0)
(princ))

(defun c:e172 ()
(prompt "\nPick Insertion point: ")
(command "insert" "edgnd45a" pause 1 1 0)
(princ))

(defun c:e173 ()
(prompt "\nPick Insertion point: ")
(command "insert" "edgnd90a" pause 1 1 0)
(princ))

(defun c:e174 ()
(prompt "\nPick Insertion point: ")
(command "insert" "edgnd00a" pause 1 1 0)
(princ))

(defun c:e175 ()
(prompt "\nPick Insertion point: ")
(command "insert" "edwye00t" pause 1 1 0)
(princ))

(defun c:e176 ()
(prompt "\nPick Insertion point: ")
(command "insert" "edgend00" pause 1 1 0)
(princ))

(defun c:e177 ()
(prompt "\nPick Insertion point: ")
(command "insert" "edgenp00" pause 1 1 0)
(princ))

(defun c:e178 ()
(prompt "\nPick Insertion point: ")
(command "insert" "edgent00" pause 1 1 0)
(princ))