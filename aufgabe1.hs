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


main :: IO ()
main = do
    let schulListe = [(School {nameSchool = "HWR", spots = 2}),(School {nameSchool = "HTW", spots = 3 }),(School {nameSchool = "TH Wildau", spots = 10 })]
    let schuelerListe = [(Student {nameStudent="Emilie", mark=1.0, preferences = ["HWR", "HTW", "TH Wildau"]}),(Student {nameStudent="Clemens", mark=1.0, preferences = ["HTW", "HWR", "TH Wildau"]}),(Student {nameStudent="Josephine", mark=1.0, preferences = ["TH Wildau", "HWR", "HTW"]}),(Student {nameStudent="Tom Vorlost", mark=1.7, preferences = ["HWR", "HTW", "TH Wildau"]}),(Student {nameStudent="Hermine", mark=1.0, preferences =["TH Wildau", "HTW", "HWR"]}),(Student {nameStudent="Ron", mark=3.7, preferences =["HTW", "HWR", "TH Wildau"]}),(Student {nameStudent="Harry", mark=2.3, preferences =["HWR", "TH Wildau", "HTW"]}),(Student {nameStudent="Malfoy", mark=2.3, preferences =["HTW", "HWR", "TH Wildau"]}),(Student {nameStudent="Hirka", mark=2.7, preferences =["TH Wildau", "HWR", "HTW"]}),(Student {nameStudent="Rime", mark=1.3, preferences =["HTW", "HWR", "TH Wildau"]}),(Student {nameStudent="Eragon", mark=4.0, preferences =["TH Wildau", "HWR", "HTW"]}),(Student {nameStudent="Ezio Auditore", mark=3.0, preferences =["HWR", "HTW", "TH Wildau"]})]

    --let besetzung1 = generateAssignmentsForAllSchoolsOnPreference schulListe schuelerListe 0 []
    --let besetzung2 = generateAssignmentsForAllSchoolsOnPreference schulListe schuelerListe 1 besetzung1
    --let besetzung3 = generateAssignmentsForAllSchoolsOnPreference schulListe schuelerListe 2 besetzung2
    --print besetzung1
    --print besetzung2
    --print besetzung3
    putStrLn "\n---------------Liste der Schulen---------------"
    forM_ schulListe print
    putStrLn "\n---------------Liste der Schueler---------------"
    forM_ (sortStudentsByMarks schuelerListe) print
    putStrLn "\n------------------Ergebnisse-------------------"
    forM_ (generateCompleteAssignmentTable schulListe schuelerListe [0,1,2] []) print