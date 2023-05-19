module Aufgabe1 where
    import Control.Monad ( forM_ )

    data School = School
        {
            nameSchool :: String,
            spots :: Int
        } deriving (Eq)

    instance Show School where
        show s = "Schule: " ++ nameSchool s ++ "\nPlaetze: " ++ show (spots s) ++ "\n"

    data Student = Student
        {
            nameStudent :: String,
            mark :: Float,
            preferences:: [String]
        }

    instance Show Student where
        show s = "Student: " ++ nameStudent s ++ "\nNote: " ++ show (mark s) ++ "\nPraeferenzen: " ++ showPreferencList (preferences s) ++ "\n" where
            showPreferencList list = case list of
                [] -> ""
                (x:xs) -> x ++ "; " ++ showPreferencList xs

    data Assignment = Assignment
        {
            assignmentSchoolName :: String,
            assignedStudents :: [String]
        }
        
    instance Show Assignment where
        show a = "Schule:  " ++ assignmentSchoolName a ++ "\nZugewiesene Schueler: " ++ showStudentList (assignedStudents a) ++ "\n" where
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
    generateCompleteAssignmentTable schools students (x:xs) currentAssignment = generateCompleteAssignmentTable schools students xs (assignAll schools (sortStudentsByMarks students) x currentAssignment)

    {-
    Argumente: Liste aller Schulen, Liste von Schülern, zu betrachtende Präferenz (0, 1 oder 2), aktuelle Zuweisung aller Schulen
    Rückgabe: neue Zuweisung aller Schulen
    Diese Funktion generiert eine Liste von Zuweisungen zu allen Schulen unter beachtung einer bestimmten Präferenz.
        -die Liste an Schülern ist nach Note sortiert
        -es werden alle bereits zugewiesenen Schüler herausgefiltert
        -wenn zu einer Schule noch keine Zuweisung existiert, wird eine neue erstellt
    -}
    assignAll :: [School] -> [Student] -> Int -> [Assignment] -> [Assignment]
    assignAll schools students currentPreference currentAssignment = [generateAssignment x (determineUnassignedStudents students currentAssignment) currentPreference (findAssignmentsPerSchool x currentAssignment)| x <- schools]

    {-
    Argumente: Schule, Liste von Schülern, zu betrachtende Präferenz (0, 1 oder 2), aktuelle Zuweisung zu dieser Schule
    Rückgabe: neue Zuweisung zu dieser Schule
    Diese Funktion weißt aller Schüler einer Schule zu, wenn sie diese als betrachtete Präferenz gewählt haben.
        -die Liste an Schülern ist nach noten sortiert
        -es wird nur zugewiesen, wenn eine Präferenz für diese Schule auf dem betrachteten Level besteht
        -es wird nur zugewiesen, wenn die Schule nicht voll ist
    -}
    generateAssignment :: School -> [Student] -> Int -> Assignment -> Assignment
    generateAssignment _ [] _ assignment = assignment
    generateAssignment school (x:xs) preference assignment =
        if doesStudentHavePreference school x preference && not (isSchoolFull school assignment)
            then generateAssignment school xs preference assignment {assignedStudents = assignedStudents assignment ++ [nameStudent x]}
            else generateAssignment school xs preference assignment

     {-
    Argumente: Schule, Liste aller aktuellen Zuweisungen
    Rückgabe: Zuweisungen
    Findet die zu einer Schule zugehörige Zuweisung aus einer Liste von Zuweisungen. Falls keine existiert, wird eine neue erstellt.
    -}
    findAssignmentsPerSchool :: School -> [Assignment] -> Assignment
    findAssignmentsPerSchool school [] = Assignment {assignmentSchoolName = nameSchool school, assignedStudents = []}
    findAssignmentsPerSchool school (x:xs) =
        if nameSchool school == assignmentSchoolName x
            then x
            else findAssignmentsPerSchool school xs

    {-
    Argumente: Liste aller Schüler, Aktuelle Zuweisungen zu allen Schulen
    Rückgabe: Liste von Schülern
    Diese Funktion generiert eine Liste aller Schüler, die noch keiner Schule zugewiesen sind
    -}
    determineUnassignedStudents :: [Student] -> [Assignment] -> [Student]
    determineUnassignedStudents students assignment = [x | x <- students, nameStudent x `notElem` listAssignedStudents assignment]

    {-
    Argumente: Aktuelle Zuweisungen zu allen Schulen
    Rückgabe: String Liste
    Diese Funktion generiert eine Liste mit den Namen von allen Schülern, die bereits zugewiesen wurden
    -}
    listAssignedStudents :: [Assignment] -> [String]
    listAssignedStudents = concatMap assignedStudents

    {-
    Argumente: Schule, aktuelle Zuweisung zu einer Schule
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
    doesStudentHavePreference school student currentPreference = (length (preferences student) - 1 >= currentPreference) && (nameSchool school == preferences student!!currentPreference)
