{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use head" #-}
data Schule = Schule
    {
        nameSchule :: [Char],
        plaetze :: Int
    } deriving (Show, Eq)

data Schueler = Schueler
    {
        nameSchueler :: [Char],
        note :: Float,
        praeferenz1 :: Schule,
        praeferenz2 :: Schule,
        praeferenz3 :: Schule
    } deriving Show

data Besetzung = Besetzung
    {
        schule :: Schule,
        liste :: [Schueler]

    } deriving Show

schuelerNachNotenSortieren :: [Schueler] -> [Schueler]
schuelerNachNotenSortieren [] = []
schuelerNachNotenSortieren (x:xs) = schuelerNachNotenSortieren smaller  ++ [x] ++ schuelerNachNotenSortieren larger where
    smaller = [s| s <- xs , note s <= note x]
    larger = [l | l<- xs , note l > note x]

schuelerNachPraeferenzenFiltern :: Schule -> [Schueler] -> Int -> [Schueler]
schuelerNachPraeferenzenFiltern _ [] _ = []
schuelerNachPraeferenzenFiltern schule liste p = preaf1 schule liste ++ preaf2 schule liste ++ preaf3 schule liste
    where 
        preaf1 _ [] = []
        preaf1 schule (x:xs)
            | praeferenz1 x == schule = x : preaf1 schule xs
            | otherwise = preaf1 schule xs
        preaf2 _ [] = []
        preaf2 schule (x:xs)
            | praeferenz2 x == schule = x : preaf2 schule xs
            | otherwise = preaf2 schule xs
        preaf3 _ [] = []
        preaf3 schule (x:xs)
            | praeferenz3 x == schule = x : preaf3 schule xs
            | otherwise = preaf3 schule xs

main = do
    let schulListe = [(Schule {nameSchule = "HWR", plaetze = 5 }),(Schule {nameSchule = "HTW", plaetze = 4 }),(Schule {nameSchule = "TH Wildau", plaetze = 2 }),(Schule {nameSchule = "xyz", plaetze = 1 })]
    --print schulListe --[Schule {nameSchule = "HWR", plaetze = 5},Schule {nameSchule = "HTW", plaetze = 4},Schule {nameSchule = "TH Wildau", plaetze = 3}]
    let schuelerListe = [
                        (Schueler {nameSchueler="Emilie", note=1.0, praeferenz1=schulListe!!0,praeferenz2=schulListe!!2,praeferenz3 =schulListe!!1}),
                        (Schueler {nameSchueler="Clemens", note=1.0, praeferenz1=schulListe!!1,praeferenz2=schulListe!!2,praeferenz3 =schulListe!!0}),
                        (Schueler {nameSchueler="Josephine", note=1.0, praeferenz1=schulListe!!0,praeferenz2=schulListe!!3,praeferenz3 =schulListe!!1}),

                        (Schueler {nameSchueler="Tom Vorlost", note=1.7, praeferenz1=schulListe!!3,praeferenz2=schulListe!!1,praeferenz3 =schulListe!!2}),
                        (Schueler {nameSchueler="Hermine", note=1.0, praeferenz1=schulListe!!2,praeferenz2=schulListe!!1,praeferenz3 =schulListe!!0}),
                        (Schueler {nameSchueler="Ron", note=3.7, praeferenz1=schulListe!!2,praeferenz2=schulListe!!0,praeferenz3 =schulListe!!1}),

                        (Schueler {nameSchueler="Harry", note=2.3, praeferenz1=schulListe!!0,praeferenz2=schulListe!!2,praeferenz3 =schulListe!!1}),
                        (Schueler {nameSchueler="Malfoy", note=2.3, praeferenz1=schulListe!!2,praeferenz2=schulListe!!1,praeferenz3 =schulListe!!0}),
                        (Schueler {nameSchueler="Hirka", note=2.7, praeferenz1=schulListe!!0,praeferenz2=schulListe!!1,praeferenz3 =schulListe!!2}),

                        (Schueler {nameSchueler="Rime", note=1.3, praeferenz1=schulListe!!0,praeferenz2=schulListe!!2,praeferenz3 =schulListe!!1}),
                        (Schueler {nameSchueler="Eragon", note=4.0, praeferenz1=schulListe!!1,praeferenz2=schulListe!!2,praeferenz3 =schulListe!!0}),
                        (Schueler {nameSchueler="Ezio Auditore", note=3.0, praeferenz1=schulListe!!1,praeferenz2=schulListe!!0,praeferenz3 =schulListe!!2})
                        ]
    --print schuelerListe --[Schueler {nameSchueler = "Emilie", note = 1.0, praeferenz1 = Schule {nameSchule = "HWR", plaetze = 5}, praeferenz2 = Schule {nameSchule = "TH Wildau", plaetze = 3}, praeferenz3 = Schule {nameSchule = "HTW", plaetze = 4}},Schueler {nameSchueler = "Clemens", note = 1.0, praeferenz1 = Schule {nameSchule = "HTW", plaetze = 4}, praeferenz2 = Schule {nameSchule = "TH Wildau", plaetze = 3}, praeferenz3 = Schule {nameSchule = "HWR", plaetze = 5}},Schueler {nameSchueler = "Josephine", note = 1.0, praeferenz1 = Schule {nameSchule = "HWR", plaetze = 5}, praeferenz2 = Schule {nameSchule = "TH Wildau", plaetze = 3}, praeferenz3 = Schule {nameSchule = "HTW", plaetze = 4}},Schueler {nameSchueler = "Tom Vorlost", note = 1.7, praeferenz1 = Schule {nameSchule = "HWR", plaetze = 5}, praeferenz2 = Schule {nameSchule = "HTW", plaetze = 4}, praeferenz3 = Schule {nameSchule = "TH Wildau", plaetze = 3}},Schueler {nameSchueler = "Hermine", note = 1.0, praeferenz1 = Schule {nameSchule = "TH Wildau", plaetze = 3}, praeferenz2 = Schule {nameSchule = "HTW", plaetze = 4}, praeferenz3 = Schule {nameSchule = "HWR", plaetze = 5}},Schueler {nameSchueler = "Ron", note = 3.7, praeferenz1 = Schule {nameSchule = "TH Wildau", plaetze = 3}, praeferenz2 = Schule {nameSchule = "HWR", plaetze = 5}, praeferenz3 = Schule {nameSchule = "HTW", plaetze = 4}},Schueler {nameSchueler = "Harry", note = 2.3, praeferenz1 = Schule {nameSchule = "HWR", plaetze = 5}, praeferenz2 = Schule {nameSchule = "TH Wildau", plaetze = 3}, praeferenz3 = Schule {nameSchule = "HTW", plaetze = 4}},Schueler {nameSchueler = "Malfoy", note = 2.3, praeferenz1 = Schule {nameSchule = "TH Wildau", plaetze = 3}, praeferenz2 = Schule {nameSchule = "HTW", plaetze = 4}, praeferenz3 = Schule {nameSchule = "HWR", plaetze = 5}},Schueler {nameSchueler = "Hirka", note = 2.7, praeferenz1 = Schule {nameSchule = "HWR", plaetze = 5}, praeferenz2 = Schule {nameSchule = "HTW", plaetze = 4}, praeferenz3 = Schule {nameSchule = "TH Wildau", plaetze = 3}},Schueler {nameSchueler = "Rime", note = 1.3, praeferenz1 = Schule {nameSchule = "HWR", plaetze = 5}, praeferenz2 = Schule {nameSchule = "TH Wildau", plaetze = 3}, praeferenz3 = Schule {nameSchule = "HTW", plaetze = 4}},Schueler {nameSchueler = "Eragon", note = 4.0, praeferenz1 = Schule {nameSchule = "HTW", plaetze = 4}, praeferenz2 = Schule {nameSchule = "TH Wildau", plaetze = 3}, praeferenz3 = Schule {nameSchule = "HWR", plaetze = 5}},Schueler {nameSchueler = "Ezio Auditore", note = 3.0, praeferenz1 = Schule {nameSchule = "HTW", plaetze = 4}, praeferenz2 = Schule {nameSchule = "HWR", plaetze = 5}, praeferenz3 = Schule {nameSchule = "TH Wildau", plaetze = 3}}]
    --print (schuelerNachNotenSortieren schuelerListe)
    print (schuelerNachPraeferenzenFiltern (schulListe!!3) schuelerListe 1)