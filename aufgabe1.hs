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

    sortStudentsByMarks :: [Student] -> [Student]
    sortStudentsByMarks [] = []
    sortStudentsByMarks (x:xs) = sortStudentsByMarks smaller  ++ [x] ++ sortStudentsByMarks larger where
        smaller = [s| s <- xs , mark s <= mark x]
        larger = [l | l<- xs , mark l > mark x]

    generateCompleteAssignmentTable :: [School] -> [Student] -> [Int] -> [Assignment]-> [Assignment]
    generateCompleteAssignmentTable _ _ [] currentAssignment = currentAssignment
    generateCompleteAssignmentTable schools students (x:xs) currentAssignment = generateCompleteAssignmentTable schools students xs (generateAssignmentsForAllSchoolsOnPreference schools students x currentAssignment)

    generateAssignmentsForAllSchoolsOnPreference :: [School] -> [Student] -> Int -> [Assignment] -> [Assignment]
    generateAssignmentsForAllSchoolsOnPreference schools students currentPreference currentAssignment = [generateAssignmentForSpecificSchoolAndPreference x (determineUnassignedStudents students currentAssignment) currentPreference (findAssignmentBelongingToSchool x currentAssignment)| x <- schools]

    generateAssignmentForSpecificSchoolAndPreference :: School -> [Student] -> Int -> Assignment -> Assignment
    generateAssignmentForSpecificSchoolAndPreference _ [] _ assignemnt = assignemnt
    generateAssignmentForSpecificSchoolAndPreference school (x:xs) preference assignemnt =
        if doesStudentHavePreference school x preference && not (isSchoolFull school assignemnt)
            then generateAssignmentForSpecificSchoolAndPreference school xs preference assignemnt {assignedStudents = assignedStudents assignemnt ++ [nameStudent x]}
            else generateAssignmentForSpecificSchoolAndPreference school xs preference assignemnt

    findAssignmentBelongingToSchool :: School -> [Assignment] -> Assignment
    findAssignmentBelongingToSchool school [] = Assignment {assignmentSchoolName = nameSchool school, assignedStudents = []}
    findAssignmentBelongingToSchool school (x:xs) =
        if nameSchool school == assignmentSchoolName x
            then x
            else findAssignmentBelongingToSchool school xs

    determineUnassignedStudents :: [Student] -> [Assignment] -> [Student]
    determineUnassignedStudents students assignment = [x | x <- students, nameStudent x `notElem` generateListOfAlreadyAssignedStudents assignment]

    generateListOfAlreadyAssignedStudents :: [Assignment] -> [String]
    generateListOfAlreadyAssignedStudents = concatMap assignedStudents

    isSchoolFull :: School -> Assignment -> Bool
    isSchoolFull school assignment = length (assignedStudents assignment) > spots school - 1

    doesStudentHavePreference :: School -> Student -> Int ->  Bool
    doesStudentHavePreference school students currentPreference = nameSchool school == preferences students!!currentPreference
