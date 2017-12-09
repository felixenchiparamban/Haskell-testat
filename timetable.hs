import Data.List (groupBy, sortBy)
import Data.Function (on)

-- Timetable

-- define Module Type
data Module = OO | BSYS1 | BSYS2 | SE | PARAPROG | ENGPROJ | AN1 | AN2 | DM deriving (Show, Eq) 

-- define Class Type
data Class = Lecture (Day, Int, Int) | Exercise (Day, Int, Int) deriving Show

-- define Day Type
data Day = Day1 | Day2 | Day3 | Day4 | Day5 deriving Show

-- Knowledge Base
-- Module tuples
modulesTimetable = [
    ("OO", ("Day1", 8, 10), [("Day1", 10, 12), ("Day1", 13, 15), ("Day2", 8, 10)]), 
    ("SE", ("Day1", 10, 12), [("Day2", 10, 12), ("Day3", 8, 10), ("Day3", 10, 12)]),
    ("AN1", ("Day1", 13, 15), [("Day2", 10, 12), ("Day2", 13, 15), ("Day3", 15, 17)])]

-- find out lecture for the given module
-- lectures :: Module -> (Day, Int, Int)
lecture x = [l | (m, l, es) <- modulesTimetable, x == m]

-- find out lecture and exercise sessions of my modules
myModulesTimetable (x:xs) = [(m,l, es) | (m, l, es) <- modulesTimetable, m `elem` (x:xs)]

mapModuleExercises (m, l, es) = [ (m, (l, e)) | e <- es]
myModulePlanCombinations myModules = map mapModuleExercises (myModulesTimetable myModules)
-- myModulePlanCombinations ["OO", "SE"]
-- result would be [[(m, (l, e))]]
-- [[("OO",(("Day1",8,10),("Day1",10,12))),("OO",(("Day1",8,10),("Day1",13,15))),("OO",(("Day1",8,10),("Day2",8,10)))],
--  [("SE",(("Day1",10,12),("Day2",10,12))),("SE",(("Day1",10,12),("Day3",8,10))),("SE",(("Day1",10,12),("Day3",10,12)))]]

-- find out all possible timetables
allTimetables myModules = sequence (myModulePlanCombinations myModules)

-- for OO and SE, 9 possible timetables
-- [[("OO",(("Day1",8,10),("Day1",10,12))),("SE",(("Day1",10,12),("Day2",10,12)))],
-- [("OO",(("Day1",8,10),("Day1",10,12))),("SE",(("Day1",10,12),("Day3",8,10)))],
-- [("OO",(("Day1",8,10),("Day1",10,12))),("SE",(("Day1",10,12),("Day3",10,12)))],
-- [("OO",(("Day1",8,10),("Day1",13,15))),("SE",(("Day1",10,12),("Day2",10,12)))],
-- [("OO",(("Day1",8,10),("Day1",13,15))),("SE",(("Day1",10,12),("Day3",8,10)))],
-- [("OO",(("Day1",8,10),("Day1",13,15))),("SE",(("Day1",10,12),("Day3",10,12)))],
-- [("OO",(("Day1",8,10),("Day2",8,10))),("SE",(("Day1",10,12),("Day2",10,12)))],
-- [("OO",(("Day1",8,10),("Day2",8,10))),("SE",(("Day1",10,12),("Day3",8,10)))],
-- [("OO",(("Day1",8,10),("Day2",8,10))),("SE",(("Day1",10,12),("Day3",10,12)))]]


-- Merge------------

-- (m, (l, e)) --> (m, l), (m,e)
-- [[(a,b), (a, c)], [b, c]] --> [(a,b), (a,c), (b,c)]
mergeTwo :: Ord a => [a] -> [a] -> [a]
mergeTwo x [] = x
mergeTwo [] x = x
mergeTwo (x:xs) (y:ys) = if x < y
                          then x:(mergeTwo xs (y:ys))
                          else y:(mergeTwo (x:xs) ys)

mergeAll :: (Ord a) => [[a]] -> [a]
mergeAll [] = []
mergeAll ([]:xss) = mergeAll xss
mergeAll ((x:xs):xss) = x : mergeTwo xs (mergeAll xss)

-- map modules to timeslot
oneTimetable = [("OO",(("Day1",8,10),("Day1",10,12))),("SE",(("Day1",10,12),("Day2",10,12)))]
manyTimeTables = [[("OO",(("Day1",8,10),("Day1",10,12))),("SE",(("Day1",10,12),("Day2",10,12)))],
                  [("OO",(("Day1",8,10),("Day1",10,12))),("SE",(("Day1",10,12),("Day3",8,10)))]]

mapTimeSlot (m, (l, e)) = [(t, m) | t <- [l, e]]
-- mapped timeslot: map mapTimeSlot oneTimetable 
-- [[(("Day1",8,10),"OO"),(("Day1",10,12),"OO")],
--  [(("Day1",10,12),"SE"),(("Day2",10,12),"SE")]]

-- all time tables mapped to time slots
allTimetablesMappedWithTimeSlot (t:ts) = map (\aTimetable -> mergeAll (map mapTimeSlot aTimetable)) (t:ts)

-- [[(("Day1",8,10),"OO"),(("Day1",10,12),"OO"),(("Day1",10,12),"SE"),(("Day2",10,12),"SE")],
-- [(("Day1",8,10),"OO"),(("Day1",10,12),"OO"),(("Day1",10,12),"SE"),(("Day3",8,10),"SE")],
-- [(("Day1",8,10),"OO"),(("Day1",10,12),"OO"),(("Day1",10,12),"SE"),(("Day3",10,12),"SE")],
-- [(("Day1",8,10),"OO"),(("Day1",10,12),"SE"),(("Day1",13,15),"OO"),(("Day2",10,12),"SE")],
-- [(("Day1",8,10),"OO"),(("Day1",10,12),"SE"),(("Day1",13,15),"OO"),(("Day3",8,10),"SE")],
-- [(("Day1",8,10),"OO"),(("Day1",10,12),"SE"),(("Day1",13,15),"OO"),(("Day3",10,12),"SE")],
-- [(("Day1",8,10),"OO"),(("Day1",10,12),"SE"),(("Day2",8,10),"OO"),(("Day2",10,12),"SE")],
-- [(("Day1",8,10),"OO"),(("Day1",10,12),"SE"),(("Day2",8,10),"OO"),(("Day3",8,10),"SE")],
-- [(("Day1",8,10),"OO"),(("Day1",10,12),"SE"),(("Day2",8,10),"OO"),(("Day3",10,12),"SE")]]

result = allTimetablesMappedWithTimeSlot (allTimetables ["OO", "SE"])

-- give points to a timetable to compare with other timetables
xs = [("a",1),("b",2),("a",1)]
sx = [(n, s) | (s,n) <- xs]
mySort n = sortBy (compare `on` snd) n
fi = groupBy ((==) `on` snd) (mySort xs)