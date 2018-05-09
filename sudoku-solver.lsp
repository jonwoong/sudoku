(defun reload ()
	(load "sudoku-solver.lsp"))

(defun load-sat()
  (load "sat-solver.lsp"))

(defun reload-all()
  (reload)
  (load-sat)
  )

(defun validRow ()
	(list 1 2 3 4 5 6 7 8 9))

(defun validQuadrants()
	(list 0 1 2 3 4 5 6 7 8))

(defun isUnitClause (l)
	(cond
		((null l) NIL)
		((atom l) T)
		((listp l) (= (length (flatten l)) 1))
		(T NIL)))

(defun getUnitClause (l)
	(cond
		((null l) NIL)
		((atom l) l)
		((and (listp l) (isUnitClause l)) (first l))
		(T NIL)))

(defun isMember (n l)
	(cond
		((null l) NIL)
		((numberp l) (= n l))
		((listp l) 
			(or (isMember n (car l))
				(isMember n (cdr l))))
		(T NIL)))

(defun sameLists (l1 l2)
	(cond
		((null l1) T)
		((atom l1) (isMember l1 l2))
		((listp l1) (and (sameLists (car l1) l2) (sameLists (cdr l1) l2)))
		(T NIL)))

(defun getNth (n r)
	(cond
		((> n 0) (getNth (- n 1) (cdr r)))
		(T (if (atom r)
			r
			(car r)))))

(defun getMFromN (n m r)
	(cond 
		((> n 0) (getMFromN (- n 1) m (cdr r)))
		((= n 0)
			(cond
				((> m 0)
					(cond
						((null r) r)
						((atom r) r)
						((listp r) (append (list (car r)) (getMFromN 0 (- m 1) (cdr r))))
						(T NIL)))
				(T NIL)))
		(T NIL)))

(defun getCol (n s)
	(cond 
		((null s) NIL)
		((listp s)
			(append (list (getNth n (car s)))
				(getCol n (cdr s))))
		(T NIL)))

(defun getRC (r c s)
	(cond 
		((or (null r) (null c)) NIL)
		(T (let* ((clause (getNth c (getNth r s))))
			(cond
				((and (isUnitClause clause) (listp clause)) (car clause))
				(T clause))))))

(defun getFirstEmpty (r c s)
	(cond
		((and (< r 9) (< c 9))
			(if (= 0 (getRC r c s))
				(list r c)
				(getFirstEmpty r (+ c 1) s)))
		((and (< r 9) (= c 9))
			(getFirstEmpty (+ r 1) 0 s))
		(T NIL)))

(defun getFirstEmptyClause (r c clauseGrid)
	(cond
		((and (< r 9) (< c 9))
			(if (not (isUnitClause (getRC r c clauseGrid)))
				(list r c)
				(getFirstEmptyClause r (+ c 1) clauseGrid)))
		((and (< r 9) (= c 9))
			(getFirstEmptyClause (+ r 1) 0 clauseGrid))
		(T NIL)))

(defun rowLength (s)
	(length (car s)))

(defun identifyQuadrant (r c)
	(cond
		((and (>= r 0) (< r 3))
			(cond
				((and (>= c 0) (< c 3)) 0)
				((and (> c 2) (< c 6)) 1)
				((and (> c 5) (< c 9)) 2)
				(T NIL)))
		((and (> r 2) (< r 6))
			(cond 
				((and (>= c 0) (< c 3)) 3)
				((and (> c 2) (< c 6)) 4)
				((and (> c 5) (< c 9)) 5)
				(T NIL)))
		((and (> r 5) (< r 9))
			(cond
				((and (>= c 0) (< c 3)) 6)
				((and (> c 2) (< c 6)) 7)
				((and (> c 5) (< c 9)) 8)
				(T NIL)))
		(T NIL)))

(defun getquadrant (n s a)
	(cond
		((and (< a 3) (= n 0))
			(append (getMFromN 0 3 (car s)) (getquadrant n (cdr s) (+ 1 a))))
		((and (< a 3) (= n 1))
			(append (getMFromN 3 3 (car s)) (getquadrant n (cdr s) (+ 1 a))))
		((and (< a 3) (= n 2))
			(append (getMFromN 6 3 (car s)) (getquadrant n (cdr s) (+ 1 a))))
		((and (< a 3) (= n 3))
			(append (getMFromN 0 3 (car (nthcdr 3 s))) (getquadrant n (cdr s) (+ 1 a))))
		((and (< a 3) (= n 4))
			(append (getMFromN 3 3 (car (nthcdr 3 s))) (getquadrant n (cdr s) (+ 1 a))))
		((and (< a 3) (= n 5))
			(append (getMFromN 6 3 (car (nthcdr 3 s))) (getquadrant n (cdr s) (+ 1 a))))
		((and (< a 3) (= n 6))
			(append (getMFromN 0 3 (car (nthcdr 6 s))) (getquadrant n (cdr s) (+ 1 a))))
		((and (< a 3) (= n 7))
			(append (getMFromN 3 3 (car (nthcdr 6 s))) (getquadrant n (cdr s) (+ 1 a))))
		((and (< a 3) (= n 8))
			(append (getMFromN 6 3 (car (nthcdr 6 s))) (getquadrant n (cdr s) (+ 1 a))))
		(T NIL)))

(defun flatten (l)
	(cond
		((null l) NIL)
		((atom l) (list l))
		((listp l)
			(append (flatten (car l)) (flatten (cdr l))))
		(T NIL)))

(defun getflatquadrant (n s)
	(flatten (getquadrant n s 0)))

(defun removeN (n l)
	(cond
		((null l) NIL)
		((atom l)
			(if (equal n l)
				NIL
				l))
		(T (append (cleanUpList (list (removeN n (car l)))) (removeN n (cdr l))))))

(defun removePair (p l)
	(cond
		((null l) NIL)
		(T (if (equal (car l) p)
				(removePair p (cdr l))
				(append (list (car l)) (removePair p (cdr l)))))))

(defun negateList (l)
	(cond
		((null l) NIL)
		((atom l) (- 0 l))
		((listp l)
			(append (list (negateList (car l))) (negateList (cdr l))))
		(T NIL)))

(defun isMemberPair (p l)
	(cond
		((null l) NIL)
		((listp l) 
			(or (equal p (car l))
				(isMemberPair p (cdr l))))
		(T NIL)))

(defun removeDuplicate (old new)
	(cond
		((null old) new)
		((atom old) 
			(if (isMember old new)
				new
				(append (list old) new)))
		((listp old) 
			(if (isMember (car old) new)
				(removeDuplicate (cdr old) new)
				(removeDuplicate (cdr old) (append new (list (car old))))))
		(T NIL)))

(defun listDifference (l1 l2)
	(cond
		((null l1) l2)
		((listp l1)
			(if (isMember (car l1) l2)
				(listDifference (cdr l1) (removeN (car l1) l2))
				(listDifference (cdr l1) l2)))
		(T NIL)))

(defun pairListDifference (l1 l2)
	(cond
		((null l1) l2)
		((listp l1)
			(if (isMemberPair (car l1) l2)
				(pairListDifference (cdr l1) (removePair (car l1) l2))
				(pairListDifference (cdr l1) l2)))
		(T NIL)))

(defun assigned (r c s)
	(and (not (= 0 (getRC r c s))) (isUnitClause (getRC r c s))))

(defun getAssignedFromRow (r)
	(cond
		((null r) NIL)
		(T (if (not (null (getUnitClause (car r))))
				(append (list (getUnitClause (car r))) (getAssignedFromRow (cdr r)))
				(getAssignedFromRow (cdr r))))))

(defun getUnassignedFromRow (r)
	(cond
		((null r) NIL)
		(T (if (null (getUnitClause (car r)))
			(append (list (car r)) (getUnassignedFromRow (cdr r)))
			(getUnassignedFromRow (cdr r))))))

(defun getBoxConstraints (r c s)
	(cond
		((null r) NIL)
		((null c) NIL)
		(T (let* ((row (flatten (getNth r s)))
			(col (flatten (getCol c s)))
			(quadrant (identifyQuadrant r c))
			(quadrantValues (flatten (getflatquadrant quadrant s))))
		(flatten (append row col quadrantValues))))))

(defun generateClause (r c s)
	(cond
		((> r 8) NIL)
		((> c 8) NIL)
		((assigned r c s) (getRC r c s))
		(T (let* ((constraints (getBoxConstraints r c s))
					(usedVariables (removeDuplicate constraints NIL))
					(clause (cleanUpList (listDifference usedVariables (validRow)))))
			(if (isUnitClause clause)
				(getUnitClause clause)
				clause)))))

(defun generateAllClauses (r c s)
	(cond
		((< c 9)
			(if (< r 9)
				(append (list (generateClause r c s)) (generateAllClauses r (+ c 1) s))
				NIL))
		((= c 9)
			(if (< r 9)
				(generateAllClauses (+ r 1) 0 s)
				NIL))
		(T NIL)))

(defun mapClausesToRow (clauses n)
	(cond
		((null clauses) NIL)
		((> n 0) (append (list (car clauses)) (mapClausesToRow (cdr clauses) (- n 1))))
		(T NIL)))

(defun mapClausesToGrid (clauses)
	(cond
		((null clauses) NIL)
		(T (append (list (mapClausesToRow clauses 9)) (mapClausesToGrid (nthcdr 9 clauses))))))

(defun setNthInRow (n r v)
	(cond
		((> n 0) (append (list (car r)) (setNthInRow (- n 1) (cdr r) v)))
		(T (append (list v) (cdr r)))))

(defun deleteMFromNth (m n r)
	(cond
		((> n 0) (append (list (car r)) (deleteMFromNth m (- n 1) (cdr r))))
		;((isUnitClause (car r)) (append (removeN m (car r)) (cdr r)))
		(T (append (list (removeN m (car r))) (cdr r)))))

(defun setBoxtoNumber (r c number s)
	(cond
		((null number) NIL)
		((> r 0) (append (list (car s)) (setBoxtoNumber (- r 1) c number (cdr s))))
		(T (append (list (setNthInRow c (car s) number)) (cdr s)))))

(defun deleteNumberFromRow (number row)
	(cond
		((null row) NIL)
		;((isUnitClause (car row)) (append (removeN number (car row)) (deleteNumberFromRow number (cdr row))))
		(T (append (list (removeN number (car row))) (deleteNumberFromRow number (cdr row))))))

(defun deleteNumberFromClauseRow (number row clauseGrid)
	(cond
		((> row 0) (append (list (car clauseGrid)) (deleteNumberFromClauseRow number (- row 1) (cdr clauseGrid))))
		(T (append (list (deleteNumberFromRow number (car clauseGrid))) (cdr clauseGrid)))))

(defun deleteNumberFromClauseColumn (c number clauseGrid)
	(cond
		((null clauseGrid) NIL)
		(T (append (list (deleteMFromNth number c (car clauseGrid))) (deleteNumberFromClauseColumn c number (cdr clauseGrid))))))

(defun deleteNumberFromClauseBox (r c number clauseGrid)
	(cond
		((null clauseGrid) NIL)
		((> r 0) (append (list (car clauseGrid)) (deleteNumberFromClauseBox (- r 1) c number (cdr clauseGrid))))
		(T (append (list (deleteMFromNth number c (car clauseGrid))) (cdr clauseGrid)))))

(defun getQuadrantRCs (quadrant)
	(cond
		((= quadrant 0) 
			(list (list 0 0) (list 0 1) (list 0 2) 
				(list 1 0) (list 1 1) (list 1 2)
				(list 2 0) (list 2 1) (list 2 2)))
		((= quadrant 1)
			(list (list 0 3) (list 0 4) (list 0 5)
				(list 1 3) (list 1 4) (list 1 5)
				(list 2 3) (list 2 4) (list 2 5)))
		((= quadrant 2)
			(list (list 0 6) (list 0 7) (list 0 8) 
				(list 1 6) (list 1 7) (list 1 8)
				(list 2 6) (list 2 7) (list 2 8)))
		((= quadrant 3)
			(list (list 3 0) (list 3 1) (list 3 2) 
				(list 4 0) (list 4 1) (list 4 2)
				(list 5 0) (list 5 1) (list 5 2)))
		((= quadrant 4)
			(list (list 3 3) (list 3 4) (list 3 5) 
				(list 4 3) (list 4 4) (list 4 5)
				(list 5 3) (list 5 4) (list 5 5)))
		((= quadrant 5)
			(list (list 3 6) (list 3 7) (list 3 8) 
				(list 4 6) (list 4 7) (list 4 8)
				(list 5 6) (list 5 7) (list 5 8)))
		((= quadrant 6)
			(list (list 6 0) (list 6 1) (list 6 2) 
				(list 7 0) (list 7 1) (list 7 2)
				(list 8 0) (list 8 1) (list 8 2)))
		((= quadrant 7)
			(list (list 6 3) (list 6 4) (list 6 5) 
				(list 7 3) (list 7 4) (list 7 5)
				(list 8 3) (list 8 4) (list 8 5)))
		((= quadrant 8)
			(list (list 6 6) (list 6 7) (list 6 8) 
				(list 7 6) (list 7 7) (list 7 8)
				(list 8 6) (list 8 7) (list 8 8)))
		(T NIL)))

(defun deleteNumberFromMultiple (n pos clauseGrid)
	(cond
		((null pos) clauseGrid)
		(T (let* ((afterDelete (deleteNumberFromClauseBox (first (car pos)) (second (car pos)) n clauseGrid)))
				(deleteNumberFromMultiple n (cdr pos) afterDelete)))))

(defun deleteNumberFromQuadrant (n r c clauseGrid)
	(let* ((quadrant (identifyQuadrant r c))
			(neighbors (pairListDifference (list (list r c)) (getQuadrantRCs quadrant))))
	(deleteNumberFromMultiple n neighbors clauseGrid)))

;(defun cleanUpUnitClauses (r c grid)
;	(cond
;		((and (< r 9) (< c 9))
;			(if (isUnitClause (getRC r c grid))
;				(cleanUpUnitClauses r (+ c 1) (setBoxtoNumber r c (getUnitClause (getRC r c grid)) grid))
;				(cleanUpUnitClauses r (+ c 1) grid)))
;		((and (< r 9) (= c 9))
;			(cleanUpUnitClauses (+ r 1) 0 grid))
;		(T grid)))

(defun cleanUpRowUnitClauses (c row)
	(cond
		((< c 9) (if (isUnitClause (car row))
			(append (list (getUnitClause (car row))) (cleanUpRowUnitClauses (+ c 1) (cdr row)))
			(append (list (car row)) (cleanUpRowUnitClauses (+ c 1) (cdr row)))))
		(T NIL)))

(defun cleanUpUnitClauses (r grid)
	(cond
		((< r 9) (append (list (cleanUpRowUnitClauses 0 (car grid))) (cleanUpUnitClauses (+ r 1) (cdr grid))))
		(T NIL)))

(defun assignNumberToBox (n r c clauseGrid)
	(let* ((assignment (setBoxtoNumber r c n clauseGrid)))
		(cond
			((validAssignment r c assignment)
				(let* ((clearRow (deleteNumberFromClauseRow n r assignment))
						(clearCol (deleteNumberFromClauseColumn c n clearRow))
						(clearQuad (deleteNumberFromQuadrant n r c clearCol))
						(reassign (setBoxtoNumber r c n clearQuad))
						;(cleaned (cleanUpUnitClauses r c reassign)))
						(cleaned (cleanUpUnitClauses 0 reassign)))
				cleaned))
			(T NIL))))

(defun assignNumbersToBox (numbers r c clauseGrid)
	(cond
		((null numbers) NIL)
		((isUnitClause numbers) (assignNumberToBox numbers r c clauseGrid))
		(T (append (list (assignNumberToBox (car numbers) r c clauseGrid))
				(assignNumbersToBox (cdr numbers) r c clauseGrid)))))

(defun hasDuplicate (l)
	(cond
		((null l) NIL)
		(T (if (isMember (car l) (cdr l))
				T
				(hasDuplicate (cdr l))))))

(defun validAssignment (r c clauseGrid)
	(let* ((row (flatten (getAssignedFromRow (getNth r clauseGrid))))
			(col (flatten (getAssignedFromRow (getCol c clauseGrid))))
			(quadrant (flatten (getAssignedFromRow (getquadrant (identifyQuadrant r c) clauseGrid 0)))))
			;(assignedNumbers (append quadrant (listdifference quadrant row) (listdifference quadrant col))))
		(cond
			((hasDuplicate row) NIL)
			((hasDuplicate col) NIL)
			((hasDuplicate quadrant) NIL)
			(T T))))

(defun validRowLength (row)
	(if (= (length (flatten row)) 9)
		T
		NIL))

(defun validGridLength (r grid)
	(cond
		((> r 0) 
			(and (= (length (car grid)) 9) (validGridLength (- r 1) (cdr grid))))
		(T T)))

(defun flattenGrid (grid)
	(cond
		((null grid) NIL)
		(T (append (list (flatten (car grid))) (flattenGrid (cdr grid))))))

(defun validGrid (r c grid)
	(cond
		((and (< r 9) (< c 9))
			(if (validAssignment r c grid)
				(validGrid (+ r 1) (+ c 1) grid)
				NIL))
		(T T)))

(defun isPair (pos)
	(cond
		((null pos) NIL)
		((atom pos) NIL)
		((and (listp pos) (atom (first pos)) (atom (second pos)) (= (length pos) 2)) T)
		(T NIL)))

(defun generateBoxList (r c clauseGrid)
	(cond
		((and (< r 9) (< c 9))
			(if (listp (getNth c (car clauseGrid)))
				(append (list (list r c)) (generateBoxList r (+ c 1) clauseGrid))
				(generateBoxList r (+ c 1) clauseGrid)))
		((and (< r 9) (= c 9))
			(generateBoxList (+ r 1) 0 (cdr clauseGrid)))
		(T NIL)))

(defun testGridAssigment (clauseGrid box possibleValues)
	(cond
		((null possibleValues) NIL)
		((atom possibleValues)
			(let* ((result (assignNumberToBox possibleValues (first box) (second box) clauseGrid)))
				(if (and (validGridLength 9 clauseGrid) (validGrid 0 0 clauseGrid))
					clauseGrid
					NIL)))
		(T (or (testGridAssigment clauseGrid box (first possibleValues))
				(testGridAssigment clauseGrid box (rest possibleValues))))))

(defun testMultiGridAssignment (grids box possibleValues)
	(cond
		((null grids) NIL)
		(T (or (testGridAssigment (car grids) box possibleValues)
			(testMultiGridAssignment (cdr grids) box possibleValues)))))

(defun isGrid (grid)
	(and (= (rowLength grid) 9) (= (length (getCol 0 grid)) 9)))

(defun isClauseGrid (grid)
	(cond
		((null grid) T)
		(T (and (>= (length (flatten (car grid))) 9)
				(isClauseGrid (cdr grid)))))) 

(defun getMostConstrained (boxList grid)
	(cond
		((null boxList) NIL)
		((and (> (length boxList) 2) (isPair (car boxList)))
			(let* ((bestChoice (getRC (first (car boxList)) (second (car boxList)) grid))
				(nextChoice (getRC (first (second boxList)) (second (second boxList)) grid)))
			(if (< (length bestChoice) (length nextChoice))
				(getMostConstrained (append (list (first boxList)) (nthcdr 2 boxList)) grid)
				(getMostConstrained (cdr boxList) grid))))
		(T (car boxList))))

(defun isMonoList (l)
	(cond
		((null l) T)
		((isUnitClause l) T)
		((> (length l) 2) (and (isMember (car l) (cdr l))
			(isMonoList (cdr l))))
		(T (= (first l) (second l)))))

(defun isUniqueList (l)
	(cond
		((null l) T)
		((isUnitClause l) T)
		((> (length l) 2) (and (not (isMember (car l) (cdr l)))
			(not (isMonoList (cdr l)))))
		(T (not (= (first l) (second l))))))

(defun countRestraints (boxConstraints constraintsList)
	(cond
		((null boxConstraints) NIL)
		((atom boxConstraints) (list boxConstraints (count boxConstraints constraintsList)))
		((listp boxConstraints)
			(append (list (countRestraints (car boxConstraints) constraintsList)) (countRestraints (cdr boxConstraints) constraintsList)))
		(T NIL)))

(defun getMinFrequency (l)
	(cond
		((null l) NIL)
		((and (> (length l) 1) (isPair (car l)))
			(let* ((currentMin (second (first l)))
				(nextMin (second (second l))))
			(if (< currentMin nextMin)
				(getMinFrequency (append (list (first l)) (nthcdr 3 l)))
				(getMinFrequency (cdr l)))))
		(T (car l))))

(defun getOrderedAssignments (l)
	(cond 
		((null l) NIL)
		(T (append (list (first (getMinFrequency l))) (getOrderedAssignments (pairListDifference (list (getMinFrequency l)) l))))))

(defun getBoxOrderAssignment (r c grid)
	(getOrderedAssignments (countRestraints (getRC r c grid) (getBoxConstraints r c grid))))


(defun getLeastConstraining (pos clauseGrid)
	(getMinFrequency (getBoxConstraints (first pos) (second pos) clauseGrid)))

(defun getNextBox (boxList grid)
	(cond
		((null boxList) NIL)
		(T (let* ((choice (getMostConstrained boxList grid)))
			(append (list choice) (getNextBox (pairListDifference (list choice) boxList) grid))))))

(defun getNextBoxes (grids)
	(cond
		((null grids) NIL)
		((isClauseGrid (car grids)) 
			(append (list (getNextBox (generateBoxList 0 0 (car grids)) (car grids))) (getNextBoxes (cdr grids))))
		(T NIL)))

(defun getMultiBoxOrderAssignment (pos grid)
	(cond
		((null pos) NIL)
		((isPair (car pos)) 
			(append (list (getBoxOrderAssignment (first (car pos)) (second (car pos)) (car grid))) (getMultiBoxOrderAssignment (cdr pos) (cdr grid))))
		(T NIL)))

(defun solveSudoku (clauseGrid box remainingBoxes)
	(cond
		((null box) 
			(if (validGrid 0 0 clauseGrid)
				clauseGrid
				NIL))
		((isClauseGrid clauseGrid)
			(let* ((possibleValues (getBoxOrderAssignment (first box) (second box) clauseGrid))
					(results (cleanUpList (assignNumbersToBox possibleValues (first box) (second box) clauseGrid)))
					(nextUnassignedBoxes (rest remainingBoxes))
					(nextBoxPos (first remainingBoxes)))
				(or (solveSudoku (car results) nextBoxPos nextUnassignedBoxes)
					(solveSudoku (cdr results) nextBoxPos nextUnassignedBoxes))))
		(T NIL)))

(defun solve-sudoku (s)
	(let* ((clauseGrid (mapClausesToGrid (generateAllClauses 0 0 s)))
		(boxList (generateBoxList 0 0 clauseGrid))
		(orderedBoxList (getNextBox boxList clauseGrid))
		(firstBoxPos (first orderedBoxList))
		(remainingBoxes (rest orderedBoxList)))
	(if (and (validGrid 0 0 clauseGrid) (null firstBoxPos))
		clauseGrid
		(solveSudoku clauseGrid firstBoxPos remainingBoxes))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun cleanUpList (L)
  (cond ((null L) nil)
	(t (let ((cur (car L))
		 (res (cleanUpList (cdr L)))
		 )
	     (if cur 
		 (cons cur res)
		  res)))))

(defun printSquare (s)
  (cond ((= s 0) (format t "0 "))
	((= s 1) (format t "1 "))
	((= s 2) (format t "2 "))
	((= s 3) (format t "3 "))
	((= s 4) (format t "4 "))
	((= s 5) (format t "5 "))
	((= s 6) (format t "6 "))
	((= s 7) (format t "7 "))
	((= s 8) (format t "8 "))
	((= s 9) (format t "9 "))
	(t (format t "|"))))

(defun printRow (r)
  (dolist (cur r)
    (printSquare cur)));

(defun printState (s)
  (progn    
    (dolist (cur s)
      (printRow cur)
      (format t "~%"))))

(defun printStates (sl delay)
  (dolist (cur sl)
    (printState cur)
    (sleep delay)));end defun

(setq s0 '( (1 4 5 3 2 7 6 9 8)
			(8 3 9 6 5 4 1 2 7)
			(6 7 2 9 1 8 5 4 3)
			(4 9 6 1 8 5 3 7 2)
			(2 1 8 4 7 3 9 5 6)
			(7 5 3 2 9 6 4 8 1)
			(3 6 7 5 4 2 8 1 9)
			(9 8 4 7 6 1 2 0 5)
			(5 2 1 8 3 9 7 0 4)))

(setq s1 '( (1 0 5 3 0 0 0 0 0)
			(8 0 0 0 0 0 0 2 0)
			(0 7 0 0 1 0 5 0 0)
			(4 0 0 0 0 5 3 0 0)
			(0 1 0 0 7 0 0 0 6)
			(0 0 3 2 0 0 0 8 0)
			(0 6 0 5 0 0 0 0 9)
			(0 0 4 0 0 0 0 3 0)
			(0 0 0 0 0 9 7 0 0)))