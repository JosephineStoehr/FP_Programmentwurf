import Aufgabe1
import Control.Monad ( forM_ )

main :: IO ()
main = do
    let schulListe1 = [(School {nameSchool = "HWR", spots = 2}),(School {nameSchool = "HTW", spots = 3 }),(School {nameSchool = "TH Wildau", spots = 10 })]
    let schuelerListe1 = [(Student {nameStudent="Emilie", mark=1.0, preferences = ["HWR", "HTW", "TH Wildau"]}),(Student {nameStudent="Clemens", mark=1.0, preferences = ["HTW", "HWR", "TH Wildau"]}),(Student {nameStudent="Josephine", mark=1.0, preferences = ["TH Wildau", "HWR", "HTW"]}),(Student {nameStudent="Tom Vorlost", mark=1.7, preferences = ["HWR", "HTW", "TH Wildau"]}),(Student {nameStudent="Hermine", mark=1.0, preferences =["TH Wildau", "HTW", "HWR"]}),(Student {nameStudent="Ron", mark=3.7, preferences =["HTW", "HWR", "TH Wildau"]}),(Student {nameStudent="Harry", mark=2.3, preferences =["HWR", "TH Wildau", "HTW"]}),(Student {nameStudent="Malfoy", mark=2.3, preferences =["HTW", "HWR", "TH Wildau"]}),(Student {nameStudent="Hirka", mark=2.7, preferences =["TH Wildau", "HWR", "HTW"]}),(Student {nameStudent="Rime", mark=1.3, preferences =["HTW", "HWR", "TH Wildau"]}),(Student {nameStudent="Eragon", mark=4.0, preferences =["TH Wildau", "HWR", "HTW"]}),(Student {nameStudent="Ezio Auditore", mark=3.0, preferences =["HWR", "HTW", "TH Wildau"]})]


    -- Randfaelle:
    putStrLn "-----------------------------------------------------\n----------------------Randfaelle---------------------\n-----------------------------------------------------\n"

    -- Schueler hat weniger als drei Praeferenzen, aber die erste passt
    putStrLn "\n\n-------Schueler hat weniger als drei Praeferenzen, aber die erste passt-------"
    let schuelerListe2 = [(Student {nameStudent="Josephine", mark=1.0, preferences = ["TH Wildau", "HWR"]})]
    putStrLn "\n---------------Liste der Schulen---------------"
    forM_ schulListe1 print
    putStrLn "\n---------------Liste der Schueler---------------"
    forM_ (sortStudentsByMarks schuelerListe2) print
    putStrLn "\n------------------Ergebnisse-------------------"
    forM_ (generateCompleteAssignmentTable schulListe1 schuelerListe2 [0,1,2] []) print
    putStrLn "\n*Die Zuweisung erfolgt wie gewohnt."

    --Schueler hat zu wenige Praeferenzen und keine passt
    putStrLn "\n\n-------Schueler hat zu wenige Praeferenzen und die angegebenen koennen nicht erfuellt werden-------"
    let schuelerListe2 = [(Student {nameStudent="Josephine", mark=1.0, preferences = []})]
    putStrLn "\n---------------Liste der Schulen---------------"
    forM_ schulListe1 print
    putStrLn "\n---------------Liste der Schueler---------------"
    forM_ (sortStudentsByMarks schuelerListe2) print
    putStrLn "\n------------------Ergebnisse-------------------"
    putStrLn "\n*Programm stuerzt ab :)"
    forM_ (generateCompleteAssignmentTable schulListe1 schuelerListe2 [0,1,2] []) print
