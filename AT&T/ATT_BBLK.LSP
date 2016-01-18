(defun c:bs001 ()
(setq dir
	(getstring
		(strcat "\nInsertion point from which side of block? ( <T>op <B>ottom <R>ight <L>eft> ): ")
	)
)
(prompt "\nPick Insertion point: ")
(if (or (= dir "t") (= dir "T")) (command "insert" "bpbac00t" PAUSE dwgsc dwgsc 0))
(if (or (= dir "b") (= dir "B")) (command "insert" "bpbac00b" PAUSE dwgsc dwgsc 0))
(if (or (= dir "r") (= dir "R")) (command "insert" "bpbac00r" PAUSE dwgsc dwgsc 0))
(if (or (= dir "l") (= dir "L")) (command "insert" "bpbac00l" PAUSE dwgsc dwgsc 0))
(princ))

(defun c:bs002 ()
(prompt "\nPick Insertion point: ")
(command "insert" "bpcam00l" pause (* dwgsc 0.5) (* dwgsc 0.5) pause)
(princ))

(defun c:bs003 ()
(prompt "\nPick Insertion point: ")
(command "insert" "bpcrg000" pause (* dwgsc 0.75) (* dwgsc 0.75) 0)
(princ))

(defun c:bs004 ()
(prompt "\nPick Insertion point: ")
(command "insert" "bpcrtp00" pause (* dwgsc 0.75) (* dwgsc 0.75) 0)
(princ))

(defun c:bs005 ()
(prompt "\nPick Insertion point: ")
(command "insert" "bpcvcr00" pause (* dwgsc 0.75) (* dwgsc 0.75) 0)
(princ))

(defun c:bs006 ()
(prompt "\nPick Insertion point: ")
(command "insert" "bpcvi000" pause (* dwgsc 0.75) (* dwgsc 0.75) 0)
(princ))

(defun c:bs007 ()
(prompt "\nPick Insertion point: ")
(command "insert" "bpcpu000" pause (* dwgsc 0.75) (* dwgsc 0.75) 0)
(princ))

(defun c:bs008 ()
(prompt "\nPick Insertion point: ")
(command "insert" "bpcond00" pause (* dwgsc 0.75) (* dwgsc 0.75) 0)
(princ))

(defun c:bs009 ()
(prompt "\nPick Insertion point: ")
(command "insert" "bpconc00" pause (* dwgsc 0.75) (* dwgsc 0.75) 0)
(princ))

(defun c:bs010 ()
(prompt "\nPick Insertion point: ")
(command "insert" "bpdscr00" pause (* dwgsc 0.75) (* dwgsc 0.75) 0)
(princ))

(defun c:bs011 ()
(prompt "\nPick Insertion point: ")
(command "insert" "bpdssm00" pause (* dwgsc 0.75) (* dwgsc 0.75) 0)
(princ))

(defun c:bs012 ()
(prompt "\nPick Insertion point: ")
(command "insert" "bpihfm00" pause dwgsc dwgsc 0)
(princ))

(defun c:bs013 ()
(prompt "\nPick Insertion point: ")
(command "insert" "bpihfs00" pause dwgsc dwgsc 0)
(princ))

(defun c:bs014 ()
(prompt "\nPick Insertion point: ")
(command "insert" "bpipt000" pause dwgsc dwgsc 0)
(princ))

(defun c:bs015 ()
(prompt "\nPick Insertion point: ")
(command "insert" "bpky0000" pause (* dwgsc 0.75) (* dwgsc 0.75) 0)
(princ))

(defun c:bs016 ()
(prompt "\nPick Insertion point: ")
(command "insert" "bpkyvs00" pause (* dwgsc 0.75) (* dwgsc 0.75) 0)
(princ))

(defun c:bs017 ()
(prompt "\nPick Insertion point: ")
(command "insert" "bpltp000" pause (* dwgsc 0.75) (* dwgsc 0.75) 0)
(princ))

(defun c:bs018 ()
(prompt "\nPick Insertion point: ")
(command "insert" "bpmdcd00" pause dwgsc dwgsc 0)
(princ))

(defun c:bs019 ()
(prompt "\nPick Insertion point: ")
(command "insert" "bpmon000" pause (* dwgsc 0.75) (* dwgsc 0.75) 0)
(princ))

(defun c:bs020 ()
(prompt "\nPick Insertion point: ")
(command "insert" "bppsg000" pause (* dwgsc 0.75) (* dwgsc 0.75) 0)
(princ))

(defun c:bs021 ()
(prompt "\nPick Insertion point: ")
(command "insert" "bpprt000" pause (* dwgsc 0.75) (* dwgsc 0.75) 0)
(princ))

(defun c:bs022 ()
(prompt "\nPick Insertion point: ")
(command "insert" "bpprts00" pause (* dwgsc 0.75) (* dwgsc 0.75) 0)
(princ))

(defun c:bs023 ()
(prompt "\nPick Insertion point: ")
(command "insert" "bppbm000" pause dwgsc dwgsc 0)
(princ))

(defun c:bs024 ()
(prompt "\nPick Insertion point: ")
(command "insert" "bprvd000" pause dwgsc dwgsc 0)
(princ))

(defun c:bs025 ()
(prompt "\nPick Insertion point: ")
(command "insert" "bprt0000" pause (* dwgsc 0.75) (* dwgsc 0.75) 0)
(princ))

(defun c:bs026 ()
(prompt "\nPick Insertion point: ")
(command "insert" "bprb0000" pause dwgsc dwgsc 0)
(princ))

(defun c:bs027 ()
(prompt "\nPick Insertion point: ")
(command "insert" "bpssp000" pause (* dwgsc 0.75) (* dwgsc 0.75) 0)
(princ))

(defun c:bs028 ()
(prompt "\nPick Insertion point: ")
(command "insert" "bphrg000" pause (* dwgsc 0.75) (* dwgsc 0.75) 0)
(princ))

(defun c:bs029 ()
(prompt "\nPick Insertion point: ")
(command "insert" "bpspg000" pause (* dwgsc 0.5) (* dwgsc 0.5) 0)
(princ))

(defun c:bs030 ()
(prompt "\nPick Insertion point: ")
(command "insert" "bpprtg00" pause dwgsc dwgsc 0)
(princ))

(defun c:bs031 ()
(prompt "\nPick Insertion point: ")
(command "insert" "bpputg00" pause dwgsc dwgsc 0)
(princ))

(defun c:bs032 ()
(prompt "\nPick Insertion point: ")
(command "insert" "bpswgd00" pause (* dwgsc 0.75) (* dwgsc 0.75) 0)
(princ))

(defun c:bs033 ()
(prompt "\nPick Insertion point: ")
(command "insert" "bpswgf00" pause (* dwgsc 0.75) (* dwgsc 0.75) 0)
(princ))

(defun c:bs034 ()
(prompt "\nPick Insertion point: ")
(command "insert" "bpswgg00" pause (* dwgsc 0.75) (* dwgsc 0.75) 0)
(princ))

(defun c:bs035 ()
(prompt "\nPick Insertion point: ")
(command "insert" "bpswgh00" pause (* dwgsc 0.75) (* dwgsc 0.75) 0)
(princ))

(defun c:bs036 ()
(prompt "\nPick Insertion point: ")
(command "insert" "bpswgr00" pause (* dwgsc 0.75) (* dwgsc 0.75) 0)
(princ))

(defun c:bs037 ()
(prompt "\nPick Insertion point: ")
(command "insert" "bpswgt00" pause (* dwgsc 0.75) (* dwgsc 0.75) 0)
(princ))

(defun c:bs038 ()
(prompt "\nPick Insertion point: ")
(command "insert" "bpswgw00" pause (* dwgsc 0.75) (* dwgsc 0.75) 0)
(princ))

(defun c:bs039 ()
(prompt "\nPick Insertion point: ")
(command "insert" "bpswkd00" pause (* dwgsc 0.75) (* dwgsc 0.75) 0)
(princ))

(defun c:bs040 ()
(prompt "\nPick Insertion point: ")
(command "insert" "bpswkr00" pause (* dwgsc 0.75) (* dwgsc 0.75) 0)
(princ))

(defun c:bs041 ()
(prompt "\nPick Insertion point: ")
(command "insert" "bpswks00" pause (* dwgsc 0.75) (* dwgsc 0.75) 0)
(princ))

(defun c:bs042 ()
(prompt "\nPick Insertion point: ")
(command "insert" "bpswkp00" pause (* dwgsc 0.75) (* dwgsc 0.75) 0)
(princ))

(defun c:bs043 ()
(prompt "\nPick Insertion point: ")
(command "insert" "bpsgga00" pause (* dwgsc 0.75) (* dwgsc 0.75) 0)
(princ))

(defun c:bs044 ()
(prompt "\nPick Insertion point: ")
(command "insert" "bpsggs00" pause (* dwgsc 0.75) (* dwgsc 0.75) 0)
(princ))

(defun c:bs045 ()
(setq dir
	(getstring
		(strcat "\nType of Beam Sensor? ( <T>ransmitter <R>eceiver ): ")
	)
)
(prompt "\nPick Insertion point: ")
(if (or (= dir "t") (= dir "T")) (command "insert" "bpsgbi10" PAUSE (* dwgsc 0.75) (* dwgsc 0.75) pause))
(if (or (= dir "r") (= dir "R")) (command "insert" "bpsgbi20" PAUSE (* dwgsc 0.75) (* dwgsc 0.75) pause))
(princ))

(defun c:bs046 ()
(setq dir
	(getstring
		(strcat "\nType of Beam Sensor? ( <T>ransmitter <R>eceiver ): ")
	)
)
(prompt "\nPick Insertion point: ")
(if (or (= dir "t") (= dir "T")) (command "insert" "bpsgbm10" PAUSE (* dwgsc 0.75) (* dwgsc 0.75) pause))
(if (or (= dir "r") (= dir "R")) (command "insert" "bpsgbm20" PAUSE (* dwgsc 0.75) (* dwgsc 0.75) pause))
(princ))

(defun c:bs047 ()
(setq dir
	(getstring
		(strcat "\nType of Beam Sensor? ( <T>ransmitter <R>eceiver ): ")
	)
)
(prompt "\nPick Insertion point: ")
(if (or (= dir "t") (= dir "T")) (command "insert" "bpsgbp10" PAUSE (* dwgsc 0.75) (* dwgsc 0.75) pause))
(if (or (= dir "r") (= dir "R")) (command "insert" "bpsgbp20" PAUSE (* dwgsc 0.75) (* dwgsc 0.75) pause))
(princ))

(defun c:bs048 ()
(prompt "\nPick Insertion point: ")
(command "insert" "bpsgmd00" pause (* dwgsc 0.75) (* dwgsc 0.75) 0)
(princ))

(defun c:bs049 ()
(prompt "\nPick Insertion point: ")
(command "insert" "bpsgmm00" pause (* dwgsc 0.75) (* dwgsc 0.75) 0)
(princ))

(defun c:bs050 ()
(prompt "\nPick Insertion point: ")
(command "insert" "bpsgmi00" pause (* dwgsc 0.75) (* dwgsc 0.75) 0)
(princ))

(defun c:bs051 ()
(prompt "\nPick Insertion point: ")
(command "insert" "bpsgmu00" pause (* dwgsc 0.75) (* dwgsc 0.75) 0)
(princ))

(defun c:bs052 ()
(prompt "\nPick Insertion point: ")
(command "insert" "bpst0000" pause (* dwgsc 0.75) (* dwgsc 0.75) 0)
(princ))
