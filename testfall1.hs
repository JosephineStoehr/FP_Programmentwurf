import Aufgabe1
import Control.Monad ( forM_ )

main :: IO ()
main = do
    -- Verhalten bei optimalen Bedingungen
    putStrLn "\n------------Verhalten bei optimalen Bedingungen-----------\n"
    let schulListe1 = [(School {nameSchool = "HWR", spots = 2}),(School {nameSchool = "HTW", spots = 3 }),(School {nameSchool = "TH Wildau", spots = 10 })]
    let schuelerListe1 = [(Student {nameStudent="Emilie", mark=1.0, preferences = ["HWR", "HTW", "TH Wildau"]}),(Student {nameStudent="Clemens", mark=1.0, preferences = ["HTW", "HWR", "TH Wildau"]}),(Student {nameStudent="Josephine", mark=1.0, preferences = ["TH Wildau", "HWR", "HTW"]}),(Student {nameStudent="Tom Vorlost", mark=1.7, preferences = ["HWR", "HTW", "TH Wildau"]}),(Student {nameStudent="Hermine", mark=1.0, preferences =["TH Wildau", "HTW", "HWR"]}),(Student {nameStudent="Ron", mark=3.7, preferences =["HTW", "HWR", "TH Wildau"]}),(Student {nameStudent="Harry", mark=2.3, preferences =["HWR", "TH Wildau", "HTW"]}),(Student {nameStudent="Malfoy", mark=2.3, preferences =["HTW", "HWR", "TH Wildau"]}),(Student {nameStudent="Hirka", mark=2.7, preferences =["TH Wildau", "HWR", "HTW"]}),(Student {nameStudent="Rime", mark=1.3, preferences =["HTW", "HWR", "TH Wildau"]}),(Student {nameStudent="Eragon", mark=4.0, preferences =["TH Wildau", "HWR", "HTW"]}),(Student {nameStudent="Ezio Auditore", mark=3.0, preferences =["HWR", "HTW", "TH Wildau"]})]
    putStrLn "\n---------------Liste der Schulen---------------"
    forM_ schulListe1 print
    putStrLn "\n---------------Liste der Schueler---------------"
    forM_ (sortStudentsByMarks schuelerListe1) print
    putStrLn "\n------------------Ergebnisse-------------------"
    forM_ (generateCompleteAssignmentTable schulListe1 schuelerListe1 [0,1,2] []) print

    
    -- Randfaelle:
    putStrLn "\n\n-----------------------------------------------------\n-----------------------Randfaelle---------------------\n-----------------------------------------------------"
    
    -- Schueler hat zu viele Praeferenzen (Praeferenz 1-3 frei)
    putStrLn "\n\n-Schueler hat zu viele Praeferenzen (Praeferenz 1-3 frei)-"
    let schulListe2 = [(School {nameSchool = "HWR", spots = 2}),(School {nameSchool = "HTW", spots = 3 }),(School {nameSchool = "TH Wildau", spots = 10 }), (School {nameSchool = "TU Berlin", spots = 2})]
    let schuelerListe2 = [(Student {nameStudent="MehrIstMehr", mark=1.0, preferences = ["HWR", "HTW", "TH Wildau", "TU Berlin"]})]
    putStrLn "\n---------------Liste der Schulen---------------"
    forM_ schulListe2 print
    putStrLn "\n---------------Liste der Schueler---------------"
    forM_ (sortStudentsByMarks schuelerListe2) print
    putStrLn "\n------------------Ergebnisse-------------------"
    forM_ (generateCompleteAssignmentTable schulListe1 schuelerListe2 [0,1,2] []) print
    putStrLn ("\n*Die Zuweisung erfolgt wie gewohnt.")

    -- Schueler hat zu viele Praeferenzen (und Praeferenz 1-3 sind voll)
    putStrLn "\n\n-Schueler hat zu viele Praeferenzen (und Praeferenz 1-3 voll)-"
    let schulListe3 = [(School {nameSchool = "HWR", spots = 0}),(School {nameSchool = "HTW", spots = 0 }),(School {nameSchool = "TH Wildau", spots = 0 }), (School {nameSchool = "TU Berlin", spots = 2})]
    let schuelerListe3 = [(Student {nameStudent="MehrIstMehr", mark=1.0, preferences = ["HWR", "HTW", "TH Wildau", "TU Berlin"]})]
    putStrLn "\n---------------Liste der Schulen---------------"
    forM_ schulListe3 print
    putStrLn "\n---------------Liste der Schueler---------------"
    forM_ (sortStudentsByMarks schuelerListe3) print
    putStrLn "\n------------------Ergebnisse-------------------"
    forM_ (generateCompleteAssignmentTable schulListe3 schuelerListe3 [0,1,2] []) print
    putStrLn ("\n*Die vierte Praeferenz wird ignoriert.")


    -- Schueler hat zu wenige Praeferenzen
    putStrLn "\n\n-------Schueler hat zu wenige Praeferenzen-------"
    --let schulListe4 = 
    --let schuelerListe4 = 
    putStrLn "\n---------------Liste der Schulen---------------"
    --forM_ schulListe4 print
    putStrLn "\n---------------Liste der Schueler---------------"
    --forM_ (sortStudentsByMarks schuelerListe4) print
    putStrLn "\n------------------Ergebnisse-------------------"
    --forM_ (generateCompleteAssignmentTable schulListe4 schuelerListe4 [0,1,2] []) print
    putStrLn ("\n*Die Zuweisung erfolgt wie gewohnt.")
    
    --Schueler hat zu wenige Praeferenzen und keine passt
    putStrLn "\n\n-------Schueler hat zu wenige Praeferenzen und keine passt-------"
    --let schulListe7 = 
    --let schuelerListe7 = 
    putStrLn "\n---------------Liste der Schulen---------------"
    --forM_ schulListe7 print
    putStrLn "\n---------------Liste der Schueler---------------"
    --forM_ (sortStudentsByMarks schuelerListe7) print
    putStrLn "\n------------------Ergebnisse-------------------"
    --forM_ (generateCompleteAssignmentTable schulListe7 schuelerListe7 [0,1,2] []) print
    putStrLn ("\n*Wenn der Schueler nicht zu seinen Praeferenzen zugeordnet wurde, erfolgt keine Zuweisung.")

    
    -- Schule hat zu wenige Plaetze
    putStrLn "\n\n------------Die Schule hat zu wenige Plaetze-----------"
    let schulListe5 = [(School {nameSchool = "HWR", spots = 2})]
    let schuelerListe5 = [(Student {nameStudent="Emilie", mark=1.0, preferences = ["HWR", "HTW", "TH Wildau"]}),(Student {nameStudent="Clemens", mark=1.0, preferences = ["HTW", "HWR", "TH Wildau"]}),(Student {nameStudent="Josephine", mark=1.0, preferences = ["TH Wildau", "HWR", "HTW"]})]
    putStrLn "\n---------------Liste der Schulen---------------"
    forM_ schulListe5 print
    putStrLn "\n---------------Liste der Schueler---------------"
    forM_ (sortStudentsByMarks schuelerListe5) print
    putStrLn "\n------------------Ergebnisse-------------------"
    forM_ (generateCompleteAssignmentTable schulListe5 schuelerListe5 [0,1,2] []) print
    putStrLn ("\n*Die letzte Person bekommt keinen Platz mehr in der HWR.")


    --Die praeferierte Schule ist falsch geschrieben
    putStrLn "\n\n-----Die praeferierte Schule ist falsch geschrieben-------"
    let schulListe6 = [(School {nameSchool = "HWR", spots = 2})]
    let schuelerListe6 = [(Student {nameStudent="Falschschreiber", mark=4.0, preferences = ["HWRT", "HDW", "TH Wuldau"]})]
    putStrLn "\n---------------Liste der Schulen---------------"
    forM_ schulListe6 print
    putStrLn "\n---------------Liste der Schueler---------------"
    forM_ (sortStudentsByMarks schuelerListe6) print
    putStrLn "\n------------------Ergebnisse-------------------"
    forM_ (generateCompleteAssignmentTable schulListe6 schuelerListe6 [0,1,2] []) print
    putStrLn ("\n*Es erfolgt keine Zuweisung.\n")
