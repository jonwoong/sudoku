;(defun reload ()
;	(load "sat-solver.lsp")
;	(load "parse_cnf.lsp"))

(defun generateVariableList (n)
	(cond
		((null n) NIL)
		((numberp n)
			(if (> n 0)
				(append (generateVariableList (- n 1)) (list n))
				NIL))
		(T NIL)))

(defun appendToEveryElement (l value)
	(cond
		((null l) NIL)
		((atom l) (list l value))
		((listp l) (append (list (appendToEveryElement (car l) value)) (appendToEveryElement (cdr l) value)))
		(T NIL)))

(defun generateAssignmentList (n)
	(appendToEveryElement (generateVariableList n) NIL))

(defun containsOnlyBools (clause)
	(cond
		((null clause) T)
		((atom clause)
			(if (not (numberp clause))
				T
				NIL))
		((listp clause)
			(and (containsOnlyBools (car clause)) (containsOnlyBools (cdr clause))))
		(T NIL)))

(defun fullyAssigned (clause) ;n)
	(if ;(and (containsOnlyBools clause) (= n (length clause)))
		(containsOnlyBools clause)
		T
		NIL))

(defun evaluateClause (clause)
	(cond
		((null clause) NIL)
		((listp clause) (or (first clause) (evaluateClause (cdr clause))))
		(T NIL)))

(defun evaluateDelta (delta)
	(cond
		((null delta) T)
		((listp delta) (and (evaluateClause (car delta)) (evaluateDelta (cdr delta))))
		(T NIL)))

(defun absolute (n)
	(cond
		((null n) NIL)
		((numberp n)
			(if (> n 0)
				n
				(- 0 n)))
		((listp n)
			(append (list (absolute (car n))) (absolute (cdr n))))
		(T NIL)))

(defun clauseContainsVariable (clause variable)
	(let* ((clause (absolute clause)))
		(cond
			((null clause) NIL)
			((atom clause)
				(if (= clause variable)
					T
					NIL))
			((listp clause)
				(or (clauseContainsN (car clause) variable)
					(clauseContainsN (cdr clause) variable)))
			(T NIL))))

(defun assignValueToVariable (assignmentList value variable)
	(cond
		((null assignmentList) NIL)
		((listp assignmentList)
			(let* ((key (car (first assignmentList))))
				(if (= key variable)
					(append (list (list variable value)) (rest assignmentList))
					(append (list (first assignmentList)) (assignValueToVariable (cdr assignmentList) value variable)))))
		(T NIL)))

(defun isNegative (n)
	(cond
		((null n) NIL)
		((numberp n)
			(if (< n 0)
				T
				NIL))
		(T NIL)))

(defun negative (n)
	(cond
		((null n) NIL)
		((numberp n)
			(if (< n 0)
				n
				(- 0 n)))
		(T NIL)))

(defun assignVariableInClause (clause variable value)
	(cond
		((null clause) NIL)
		((numberp clause)
			(cond
				((isNegative clause) 
					(if (= (absolute clause) variable)
						(not value)
						clause))
				((not (isNegative clause))
					(if (= (absolute clause) variable)
						value
						clause))
				(T NIL)))
		((atom clause) clause)
		((listp clause)
			(cond
				((fullyAssigned clause)
					(if (not (null (evaluateClause clause)))
						(list T)
						NIL))
				(T (append (list (assignVariableInClause (car clause) variable value))
					(assignVariableInClause (cdr clause) variable value)))))
		(T NIL)))

(defun assignVariableInDelta (delta variable value)
	(cond
		((null delta) NIL)
		((numberp delta) (assignVariableInClause delta variable value))
		((atom delta) (assignVariableInClause delta variable value))
		((listp delta)
			(let* ((newClause (assignVariableInClause (car delta) variable value)))
				(append (list newClause)
					(assignVariableInDelta (cdr delta) variable value))))
		(T NIL)))

(defun flattenList (l)
	(cond
		((null l) NIL)
		((atom l) (list l))
		((listp l)
			(append (flattenList (car l)) (flattenList (cdr l))))
		(T NIL)))

(defun removeBools (l n)
	(cond
		((> n 0)
			(if (numberp (car l))
				(append (list (car l)) (removeBools (cdr l) (- n 1)))
				(removeBools (cdr l) (- n 1))))
		(T NIL)))

(defun listContainsNum (l n)
	(cond
		((null l) NIL)
		((atom l)
			(if (= l n)
				T
				NIL))
		((listp l)
			(or (listContainsNum (car l) n) (listContainsNum (cdr l) n)))
		(T NIL)))

(defun removeDuplicates (oldList newList)
	(cond
		((null oldList) NIL)
		((atom oldList)
			(if (listContainsNum newList oldList)
				NIL
				(append newList (list oldList))))
		((listp oldList)
			(append (removeDuplicates (car oldList) newList) (removeDuplicates (cdr oldList) newList)))
		(T NIL)))

(defun cleanUpDelta (delta)
	(let* ((flatList (flattenList delta))
		(boolsRemoved (removeBools flatList (length flatList)))
		(duplicatesRemoved (removeDuplicates (absolute boolsRemoved) NIL)))
	duplicatesRemoved))

(defun getNextVariable (delta)
	(let* ((flatList (flattenList delta))
		(boolsRemoved (removeBools flatList (length flatList)))
		(duplicatesRemoved (removeDuplicates (absolute boolsRemoved) NIL)))
	(cond
		((null duplicatesRemoved) NIL)
		((atom duplicatesRemoved) duplicatesRemoved)
		((listp duplicatesRemoved) (first duplicatesRemoved))
		(T NIL))))

(defun countDependencies (delta n)
	(cond
		((null delta) 0)
		((numberp delta)
			(if (= n delta)
				1
				0))
		((listp delta)
			(+ (countDependencies (car delta) n ) (countDependencies (cdr delta) n)))
		(T 0)))

(defun countAllDependencies (delta variables)
	(cond
		((null variables) NIL)
		((numberp variables) (append (list variables) (countDependencies delta variables)))
		((listp variables)
			(append (list (list (first variables) (countDependencies delta (first variables)))) (countAllDependencies delta (rest variables))))
		(T NIL)))

(defun getMaxFrequency (l n)
	(cond
		((null l) NIL)
		((listp l)
			(cond
				((= (length l) 1) (second (first l)))
				((> (length l) 1)
					(if (> (second (first l)) (second (second l)))
						(getMaxFrequency (cdr l) (second (first l)))
						(getMaxFrequency (cdr l) n)))
				(T NIL)))
		(T NIL)))

(defun getMaxFrequencyVariables (frequencyList maxFrequency)
	(cond
		((null frequencyList) NIL)
		((listp frequencyList)
			(if (= (second (first frequencyList)) maxFrequency)
				(append (list (first (first frequencyList))) (getMaxFrequencyVariables (rest frequencyList) maxFrequency))
				(getMaxFrequencyVariables (rest frequencyList) maxFrequency)))
		(T NIL)))

(defun removeNumberFromList (n l)
	(cond
		((null l) NIL)
		((atom l)
			(if (= n l)
				NIL
				(list l)))
		((listp l)
			(append (removeNumberFromList n (car l))
				(removeNumberFromList n (cdr l))))
		(T NIL)))

(defun listDiff (l1 l2) ; all things in l1 but not in l2
	(cond
		((null l1)
			(cond
				((null l2) NIL)
				((atom l2) NIL)
				((listp l2) NIL)
				(T NIL)))
		((atom l1)
			(cond
				((null l2) (list l1))
				((atom l2)
					(if (= l2 l1)
						NIL)
						(list l1))
				((listp l2) 
					(if (listContainsNum l2 l1)
						NIL
						(list l1)))
				(T NIL)))
		((listp l1)
			(cond
				((null l2) l1)
				((atom l2)
					(if (listContainsNum l1 l2)
						(removeNumberFromList l2 l1)
						l1))
				((listp l2) 
					(append (listDiff (car l1) l2)
						(listDiff (cdr l1) l2)))
				(T NIL)))
		(T NIL)))

(defun orderVariablesBasedOnFrequency (delta variables)
	(cond
		((null delta) NIL)
		((null variables) NIL)
		((listp delta) 
			(let* ((frequencyList (countAllDependencies (cleanUpDelta delta) variables))
				(currentMaxFrequency (getMaxFrequency frequencyList 0))
				(maxFrequencyVariables (getMaxFrequencyVariables frequencyList currentMaxFrequency))
				(remainingVariables (listDiff variables maxFrequencyVariables)))
			(append maxFrequencyVariables (orderVariablesBasedOnFrequency delta remainingVariables))))
		(T NIL)))

(defun solve-sat (delta variable assignmentList orderedVariableList)
	(cond
		((and (null variable) (fullyAssigned delta))
			(if (evaluateDelta delta) 
				assignmentList
				NIL))
		((numberp variable)
			(let* ((nilAssignmentResult (assignVariableInDelta delta variable NIL))
				(tAssignmentResult (assignVariableInDelta delta variable T))
				;(nextVariable (getNextVariable nilAssignmentResult)))
				(nextVariable (first orderedVariableList)))
				(or (solve-sat tAssignmentResult nextVariable (append assignmentList (list (list variable T))) (rest orderedVariableList))
					(solve-sat nilAssignmentResult nextVariable (append assignmentList (list (list variable NIL))) (rest orderedVariableList)))))
		(T NIL)))

(defun convertAssignmentsToAnswer (assignments)
	(cond
		((null assignments) NIL)
		((listp assignments)
			(let* ((key (first (first assignments)))
				(value (second (first assignments))))
				(if (null value)
					(append (list (- 0 key)) (convertAssignmentsToAnswer (cdr assignments)))
					(append (list key) (convertAssignmentsToAnswer (cdr assignments))))))
		(T NIL)))

(defun sat? (n delta)
	(let* ((orderedVariableList (orderVariablesBasedOnFrequency delta (generateVariableList n)))
		(nextVariable (first orderedVariableList))
		(assignments (solve-sat delta nextVariable NIL (rest orderedVariableList))))
		(convertAssignmentsToAnswer assignments)))

(defun getPureValues (delta variableList)
	(cond
		((null delta) NIL)
		((null variableList) NIL)
		((atom delta) delta)
		((listp delta)
			(let* ((flatDelta (flattenList delta)))
				(cond
					((and (listContainsNum flatDelta (first variableList))
						(not (listContainsNum flatDelta (negative (first variableList)))))
							(append (list (first variableList)) (getPureValues delta (rest variableList))))
					((and (listContainsNum flatDelta (negative (first variableList)))
						(not (listContainsNum flatDelta (first variableList))))
							(append (list (negative (first variableList))) (getPureValues delta (rest variableList))))
					(T (getPureValues delta (rest variableList))))))
		(T NIL)))
