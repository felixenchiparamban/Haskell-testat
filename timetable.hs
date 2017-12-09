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

result1 = allTimetablesMappedWithTimeSlot (allTimetables ["OO", "SE"])

-- group timeslots
-- the list should be sorted in order to group
-- unsortedList = [(("Day2",10,12),"SE"),(("Day1",10,12),"OO"),(("Day1",8,10),"OO"),(("Day1",10,12),"SE")]
sortedList = [(("Day1",8,10),"OO"),(("Day1",10,12),"OO"),(("Day1",10,12),"SE"),(("Day2",10,12),"SE")]

-- xs = [("a",1),("b",2),("a",1)]
-- mySort n = sortBy (compare `on` snd) n
-- fi = groupBy ((==) `on` snd) (mySort xs)

-- [[(("Day1",8,10),"OO")],
--  [(("Day1",10,12),"OO"),(("Day1",10,12),"SE")],
--  [(("Day2",10,12),"SE")]]
groupByTimeSlot aSortedTimetable = groupBy ((==) `on` fst) aSortedTimetable

allTimetablesGrouppedByTimeSlot (t:ts) = map groupByTimeSlot (t:ts)

-- [[[(("Day1",8,10),"OO")],[(("Day1",10,12),"OO"),(("Day1",10,12),"SE")],[(("Day2",10,12),"SE")]],
-- [[(("Day1",8,10),"OO")],[(("Day1",10,12),"OO"),(("Day1",10,12),"SE")],[(("Day3",8,10),"SE")]],
-- [[(("Day1",8,10),"OO")],[(("Day1",10,12),"OO"),(("Day1",10,12),"SE")],[(("Day3",10,12),"SE")]],
-- [[(("Day1",8,10),"OO")],[(("Day1",10,12),"SE")],[(("Day1",13,15),"OO")],[(("Day2",10,12),"SE")]],
-- [[(("Day1",8,10),"OO")],[(("Day1",10,12),"SE")],[(("Day1",13,15),"OO")],[(("Day3",8,10),"SE")]],
-- [[(("Day1",8,10),"OO")],[(("Day1",10,12),"SE")],[(("Day1",13,15),"OO")],[(("Day3",10,12),"SE")]],
-- [[(("Day1",8,10),"OO")],[(("Day1",10,12),"SE")],[(("Day2",8,10),"OO")],[(("Day2",10,12),"SE")]],
-- [[(("Day1",8,10),"OO")],[(("Day1",10,12),"SE")],[(("Day2",8,10),"OO")],[(("Day3",8,10),"SE")]],
-- [[(("Day1",8,10),"OO")],[(("Day1",10,12),"SE")],[(("Day2",8,10),"OO")],[(("Day3",10,12),"SE")]]]

result2 = allTimetablesGrouppedByTimeSlot result1

-- give points to a timetable to compare with other timetables
-- the timetable with the lowest points is better
-- Rule 1) concurrent modules
-- only one module in the timeslot => 0 pts
-- n modules in the same timeslot => 10 * n pts

-- more rules can be defined as mentioned above

-- oneGrouppedTimetable = 
-- [[(("Day1",8,10),"OO")], --> 0
--  [(("Day1",10,12),"OO"),(("Day1",10,12),"SE")], --> 10 * 2 = 20
--  [(("Day2",10,12),"SE")]] --> 0

-- is it possible to define a constant inside a function and reuse it in if/else constructor
mapPointsToTimeSlot (ts:tss) = 
    if length(ts:tss) <= 1 
        then 0
        else length(ts:tss) * 10

-- apply Rule 1 to timetable
applyConcurrentModulesRule (t:ts) = sum (map mapPointsToTimeSlot (t:ts))

-- list all rules here
applyRules (t:ts) = applyConcurrentModulesRule (t:ts) 
                    -- + applyConcurrentModulesRule (t:ts)

allTimetablesMappedWithPoints (t:ts) = map applyRules (t:ts)

result3 = allTimetablesMappedWithPoints result2