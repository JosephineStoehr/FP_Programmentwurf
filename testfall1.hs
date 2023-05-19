import Aufgabe1
import Control.Monad ( forM_ )

main :: IO ()
main = do
    -- Verhalten bei optimalen Bedingungen
    putStrLn "\n------------Verhalten bei optimalen Bedingungen-----------\n"
    let schulListe1 = [(School {nameSchool = "HWR", spots = 2}),(School {nameSchool = "HTW", spots = 3 }),(School {nameSchool = "TH Wildau", spots = 10 })]
    let schuelerListe1 = [(Student {nameStudent="Emilie", mark=1.0, preferences = ["HWR", "HTW", "TH Wildau"]}),(Student {nameStudent="Clemens", mark=1.0, preferences = ["HTW", "HWR", "TH Wildau"]}),(Student {nameStudent="Josephine", mark=1.0, preferences = ["TH Wildau", "HWR", "HTW"]}),(Student {nameStudent="Tom Vorlost", mark=1.7, preferences = ["HWR", "HTW", "TH Wildau"]}),(Student {nameStudent="Hermine", mark=1.0, preferences =["TH Wildau", "HTW", "HWR"]}),(Student {nameStudent="Ron", mark=3.7, preferences =["HTW", "HWR", "TH Wildau"]}),(Student {nameStudent="Harry", mark=2.3, preferences =["HWR", "TH Wildau", "HTW"]}),(Student {nameStudent="Malfoy", mark=2.3, preferences =["HTW", "HWR", "TH Wildau"]}),(Student {nameStudent="Hirka", mark=2.7, preferences =["TH Wildau", "HWR", "HTW"]}),(Student {nameStudent="Rime", mark=1.3, preferences =["HTW", "HWR", "TH Wildau"]}),(Student {nameStudent="Eragon", mark=4.0, preferences =["TH Wildau", "HWR", "HTW"]}),(Student {nameStudent="Ezio Auditore", mark=3.0, preferences =["HWR", "HTW", "TH Wildau"]})]
    putStrLn "---------------Liste der Schulen---------------\n"
    forM_ schulListe1 print
    putStrLn "---------------Liste der Schueler---------------\n"
    forM_ (sortStudentsByMarks schuelerListe1) print
    putStrLn "------------------Ergebnisse-------------------\n"
    forM_ (generateCompleteAssignmentTable schulListe1 schuelerListe1 [0,1,2] []) print

    --------------------------------------------------------
    putStrLn "\n------------Verhalten bei optimalen Bedingungen - 2. Beispiel -----------\n"
    let schuelerListe2 = [(Student {nameStudent="Emilie", mark=1.0, preferences = ["HWR", "HTW", "TH Wildau"]}),(Student {nameStudent="Clemens", mark=1.0, preferences = ["HTW", "HWR", "TH Wildau"]}),(Student {nameStudent="Josephine", mark=1.0, preferences = ["TH Wildau", "HWR", "HTW"]})]
    putStrLn "---------------Liste der Schulen---------------\n"
    forM_ schulListe1 print
    putStrLn "---------------Liste der Schueler---------------\n"
    forM_ (sortStudentsByMarks schuelerListe2) print
    putStrLn "------------------Ergebnisse-------------------\n"
    forM_ (generateCompleteAssignmentTable schulListe1 schuelerListe2 [0,1,2] []) print
    putStrLn "\n*Bei der Zuweisung wird die Reihenfolge der Praeferenzen priorisiert."

        --------------------------------------------------------
    putStrLn "\n------------Verhalten bei optimalen Bedingungen - 3. Beispiel -----------\n"
    let schuelerListe3 = [(Student {nameStudent="Emilie", mark=1.0, preferences = ["HWR", "HTW", "TH Wildau"]}),(Student {nameStudent="Clemens", mark=1.3, preferences = ["HWR", "HTW", "TH Wildau"]}),(Student {nameStudent="Josephine", mark=1.7, preferences = ["HWR", "HTW", "TH Wildau"]})]
    let schulListe3 = [(School {nameSchool = "HWR", spots = 1}),(School {nameSchool = "HTW", spots = 1 }),(School {nameSchool = "TH Wildau", spots = 1 })]
    putStrLn "---------------Liste der Schulen---------------\n"
    forM_ schulListe3 print
    putStrLn "---------------Liste der Schueler---------------\n"
    forM_ (sortStudentsByMarks schuelerListe3) print
    putStrLn "------------------Ergebnisse-------------------\n"
    forM_ (generateCompleteAssignmentTable schulListe3 schuelerListe3 [0,1,2] []) print
    putStrLn "\n*Bei der Zuweisung wird die Reihenfolge der Praeferenzen priorisiert, anschließend werden Schueler nach Note priorisiert."
