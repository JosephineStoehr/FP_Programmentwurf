module Aufgabe1 where
    import Control.Monad ( forM_ )

    data School = School
        {
            nameSchool :: String,
            spots :: Int
        } deriving (Eq)

    instance Show School where
        show s = "\nSchule: " ++ nameSchool s ++ "\nPlaetze: " ++ show (spots s)

    data Student = Student
        {
            nameStudent :: String,
            mark :: Float,
            preferences:: [String]
        }

    instance Show Student where
        show s = "\nStudent: " ++ nameStudent s ++ "\nNote: " ++ show (mark s) ++ "\nPraeferenzen: " ++ showPreferencList (preferences s) where
            showPreferencList list = case list of
                [] -> ""
                (x:xs) -> x ++ "; " ++ showPreferencList xs

    data Assignment = Assignment
        {
            assignmentSchoolName :: String,
            assignedStudents :: [String]
        }
        
    instance Show Assignment where
        show a = "\nSchule:  " ++ assignmentSchoolName a ++ "\nZugewiesene Schueler: " ++ showStudentList (assignedStudents a) where
            showStudentList list = case list of
                [] -> ""
                (x:xs) -> x ++ "; " ++ showStudentList xs

    {-
    Argumente: Liste von Schülern
    Rückgabe:  Sortierte Liste von Schülern
    Diese Funktion sortiert die Liste nach Notendurchschnitt
    -}
    sortStudentsByMarks :: [Student] -> [Student]
    sortStudentsByMarks [] = []
    sortStudentsByMarks (x:xs) = sortStudentsByMarks smaller  ++ [x] ++ sortStudentsByMarks larger where
        smaller = [s| s <- xs , mark s <= mark x]
        larger = [l | l<- xs , mark l > mark x]

    {-
    Argumente: Liste aller Schulen, Liste von Schülern, alle möglichen Präferenzen, alle Zuweisungen (am Anfang leer)
    Rückgabe:  Vollständige Liste aller Zuweisungen
    Diese Funktion generiert eine Liste von Zuweisungen zu allen Schulen unter beachtung aller Präferenz-Level
        -die Liste an Schülern ist nach Note sortiert
    -}
    generateCompleteAssignmentTable :: [School] -> [Student] -> [Int] -> [Assignment]-> [Assignment]
    generateCompleteAssignmentTable _ _ [] currentAssignment = currentAssignment
    generateCompleteAssignmentTable schools students (x:xs) currentAssignment = generateCompleteAssignmentTable schools students xs (generateAssignmentsForAllSchoolsOnPreference schools students x currentAssignment)

    {-
    Argumente: Liste aller Schulen, Liste von Schülern, zu betrachtende Präferenz (0, 1 oder 2), aktuelle Zuweisung aller Schulen
    Rückgabe: neue Zuweisung aller Schulen
    Diese Funktion generiert eine Liste von Zuweisungen zu allen Schulen unter beachtung einer bestimmten Präferenz.
        -die Liste an Schülern ist nach Note sortiert
        -es werden alle bereits zugewiesenen Schüler herausgefiltert
        -wenn zu einer Schule noch keine Zuweisung existiert, wird eine neue erstellt
    -}
    generateAssignmentsForAllSchoolsOnPreference :: [School] -> [Student] -> Int -> [Assignment] -> [Assignment]
    generateAssignmentsForAllSchoolsOnPreference schools students currentPreference currentAssignment = [generateAssignmentForSpecificSchoolAndPreference x (determineUnassignedStudents students currentAssignment) currentPreference (findAssignmentBelongingToSchool x currentAssignment)| x <- schools]

    {-
    Argumente: Schule, Liste von Schülern, zu betrachtende Präferenz (0, 1 oder 2), aktuelle Zuweisung zu dieser Schule
    Rückgabe: neue Zuweisung zu dieser Schule
    Diese Funktion weißt aller Schüler einer Schule zu, wenn sie diese als betrachtete Präferenz gewählt haben.
        -die Liste an Schülern ist nach noten sortiert
        -es wird nur zugewiesen, wenn eine Präferenz für diese Schule auf dem betrachteten Level besteht
        -es wird nur zugewiesen, wenn die Schule nicht voll ist
    -}
    generateAssignmentForSpecificSchoolAndPreference :: School -> [Student] -> Int -> Assignment -> Assignment
    generateAssignmentForSpecificSchoolAndPreference _ [] _ assignemnt = assignemnt
    generateAssignmentForSpecificSchoolAndPreference school (x:xs) preference assignemnt =
        if doesStudentHavePreference school x preference && not (isSchoolFull school assignemnt)
            then generateAssignmentForSpecificSchoolAndPreference school xs preference assignemnt {assignedStudents = assignedStudents assignemnt ++ [nameStudent x]}
            else generateAssignmentForSpecificSchoolAndPreference school xs preference assignemnt

     {-
    Argumente: Schule, Liste aller aktuellen Zuweisungen
    Rückgabe: Zuweisungen
    Findet die zu einer Schule zugehörige Zuweisung aus einer Liste von Zuweisungen. Falls keine existiert, wird eine neue erstellt.
    -}
    findAssignmentBelongingToSchool :: School -> [Assignment] -> Assignment
    findAssignmentBelongingToSchool school [] = Assignment {assignmentSchoolName = nameSchool school, assignedStudents = []}
    findAssignmentBelongingToSchool school (x:xs) =
        if nameSchool school == assignmentSchoolName x
            then x
            else findAssignmentBelongingToSchool school xs

    {-
    Argumente: Liste aller Schüler, Aktuelle Zuweisungen zu allen Schulen
    Rückgabe: List von Schülern
    Diese Funktion generiert eine Liste aller Schüler, die noch keiner Schule zugewiesen sind
    -}
    determineUnassignedStudents :: [Student] -> [Assignment] -> [Student]
    determineUnassignedStudents students assignment = [x | x <- students, nameStudent x `notElem` generateListOfAlreadyAssignedStudents assignment]

    {-
    Argumente: Aktuelle Zuweisungen zu allen Schulen
    Rückgabe: String Liste
    Diese Funktion generiert eine Liste mit den Namen von allen Schülern, die bereits zugewiesen wurden
    -}
    generateListOfAlreadyAssignedStudents :: [Assignment] -> [String]
    generateListOfAlreadyAssignedStudents = concatMap assignedStudents

    {-
    Argumente: Schule, Aktuelle Zuweisung zu einer Schule
    Rückgabe: Boolean
    Diese Funktion soll überprüfen, ob eine Schule unter der aktuellen Zuweisung voll ist (bzw. ob sie mit mit einem weiteren Schüler voll wäre) 
    -}
    isSchoolFull :: School -> Assignment -> Bool
    isSchoolFull school assignment = length (assignedStudents assignment) > spots school - 1

    {-
    Argumente: Schule, Schüler, Präferenz
    Rückgabe: Boolean
    Diese Funktion soll überprüfen, ob ein Schüler eine Schule als bestimmte Präferenz hat    
    -}
    doesStudentHavePreference :: School -> Student -> Int ->  Bool
    doesStudentHavePreference school students currentPreference = nameSchool school == preferences students!!currentPreference