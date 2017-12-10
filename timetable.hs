import Data.List (groupBy, sortBy)
import Data.Function (on)
import Data.Ord

-- define Module Type
data Module = OO | BSYS1 | BSYS2 | SE | PARAPROG | ENGPROJ | AN1 | AN2 | DM deriving (Eq, Ord, Show, Read) 

-- define Class Type
data Class = Lecture (Day, Int, Int) | Exercise (Day, Int, Int) deriving (Eq, Ord, Show, Read)

-- define Day Type
data Day = Day1 | Day2 | Day3 | Day4 | Day5 deriving (Eq, Ord, Show, Read)


-- Knowledge Base

-- the list should be sorted in order to group
-- unsortedList = [(("Day2",10,12),"SE"),(("Day1",10,12),"OO"),(("Day1",8,10),"OO"),(("Day1",10,12),"SE")]
-- sortedList = [(("Day1",8,10),"OO"),(("Day1",10,12),"OO"),(("Day1",10,12),"SE"),(("Day2",10,12),"SE")]

-- Module tuples
modulesPlan = [
    (OO, (Day1, 8, 10), [(Day1, 10, 12), (Day1, 13, 15), (Day2, 8, 10)]), 
    (SE, (Day1, 10, 12), [(Day2, 10, 12), (Day3, 8, 10), (Day3, 10, 12)]),
    (AN1, (Day1, 13, 15), [(Day2, 10, 12), (Day2, 13, 15), (Day3, 15, 17)])]

-- find out lecture and exercise sessions of my modules
getMyModulesPlan :: [Module]-> [(Module, (Day, Integer, Integer), [(Day, Integer, Integer)])]
getMyModulesPlan ms = [(m,l, es) | (m, l, es) <- modulesPlan, m `elem` ms]

generateAllTimeslots :: (m, l, [e]) -> [(m, (l, e))]
generateAllTimeslots (m, l, es) = [ (m, (l, e)) | e <- es]

mapMyModulePlanToTimeslots :: [(m, l, [e])] -> [[(m, (l, e))]]
mapMyModulePlanToTimeslots myModulePlan = map generateAllTimeslots myModulePlan
-- mapMyModulePlanToTimeslots ["OO", "SE"]
-- result would be [[(m, (l, e))]]
-- [[("OO",(("Day1",8,10),("Day1",10,12))),("OO",(("Day1",8,10),("Day1",13,15))),("OO",(("Day1",8,10),("Day2",8,10)))],
--  [("SE",(("Day1",10,12),("Day2",10,12))),("SE",(("Day1",10,12),("Day3",8,10))),("SE",(("Day1",10,12),("Day3",10,12)))]]

-- find out all possible timetables
generateAllTimetables :: (Traversable t, Monad m) => t (m a) -> m (t a)
generateAllTimetables  myModuleTimeslots = sequence myModuleTimeslots

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
convertToGenericTimeslot :: (m, (t, t)) -> [(t, m)]
convertToGenericTimeslot (m, (l, e)) = [(t, m) | t <- [l, e]]
-- mapped timeslot: map convertToGenericTimeslot oneTimetable 
-- [[(("Day1",8,10),"OO"),(("Day1",10,12),"OO")],
--  [(("Day1",10,12),"SE"),(("Day2",10,12),"SE")]]

-- all time tables mapped to time slots
mapTimetableWithGenericTimeslots :: (Ord t, Ord m) => [[(m, (t, t))]] -> [[(t, m)]]
mapTimetableWithGenericTimeslots (t:ts) = map (\aTimetable -> mergeAll (map convertToGenericTimeslot aTimetable)) (t:ts)

-- [[(("Day1",8,10),"OO"),(("Day1",10,12),"OO"),(("Day1",10,12),"SE"),(("Day2",10,12),"SE")],
-- [(("Day1",8,10),"OO"),(("Day1",10,12),"OO"),(("Day1",10,12),"SE"),(("Day3",8,10),"SE")],
-- [(("Day1",8,10),"OO"),(("Day1",10,12),"OO"),(("Day1",10,12),"SE"),(("Day3",10,12),"SE")],
-- [(("Day1",8,10),"OO"),(("Day1",10,12),"SE"),(("Day1",13,15),"OO"),(("Day2",10,12),"SE")],
-- [(("Day1",8,10),"OO"),(("Day1",10,12),"SE"),(("Day1",13,15),"OO"),(("Day3",8,10),"SE")],
-- [(("Day1",8,10),"OO"),(("Day1",10,12),"SE"),(("Day1",13,15),"OO"),(("Day3",10,12),"SE")],
-- [(("Day1",8,10),"OO"),(("Day1",10,12),"SE"),(("Day2",8,10),"OO"),(("Day2",10,12),"SE")],
-- [(("Day1",8,10),"OO"),(("Day1",10,12),"SE"),(("Day2",8,10),"OO"),(("Day3",8,10),"SE")],
-- [(("Day1",8,10),"OO"),(("Day1",10,12),"SE"),(("Day2",8,10),"OO"),(("Day3",10,12),"SE")]]

-- group timeslots

-- [[(("Day1",8,10),"OO")],
--  [(("Day1",10,12),"OO"),(("Day1",10,12),"SE")],
--  [(("Day2",10,12),"SE")]]
groupByTimeSlot :: Eq t => [(t, m)] -> [[(t, m)]]
groupByTimeSlot aSortedTimetable = groupBy ((==) `on` fst) aSortedTimetable

mapTimetablesGrouppedByTimeslot :: Eq t => [[(t, m)]] -> [[[(t, m)]]]
mapTimetablesGrouppedByTimeslot (t:ts) = map groupByTimeSlot (t:ts)

-- [[[(("Day1",8,10),"OO")],[(("Day1",10,12),"OO"),(("Day1",10,12),"SE")],[(("Day2",10,12),"SE")]],
-- [[(("Day1",8,10),"OO")],[(("Day1",10,12),"OO"),(("Day1",10,12),"SE")],[(("Day3",8,10),"SE")]],
-- [[(("Day1",8,10),"OO")],[(("Day1",10,12),"OO"),(("Day1",10,12),"SE")],[(("Day3",10,12),"SE")]],
-- [[(("Day1",8,10),"OO")],[(("Day1",10,12),"SE")],[(("Day1",13,15),"OO")],[(("Day2",10,12),"SE")]],
-- [[(("Day1",8,10),"OO")],[(("Day1",10,12),"SE")],[(("Day1",13,15),"OO")],[(("Day3",8,10),"SE")]],
-- [[(("Day1",8,10),"OO")],[(("Day1",10,12),"SE")],[(("Day1",13,15),"OO")],[(("Day3",10,12),"SE")]],
-- [[(("Day1",8,10),"OO")],[(("Day1",10,12),"SE")],[(("Day2",8,10),"OO")],[(("Day2",10,12),"SE")]],
-- [[(("Day1",8,10),"OO")],[(("Day1",10,12),"SE")],[(("Day2",8,10),"OO")],[(("Day3",8,10),"SE")]],
-- [[(("Day1",8,10),"OO")],[(("Day1",10,12),"SE")],[(("Day2",8,10),"OO")],[(("Day3",10,12),"SE")]]]

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

grantPointsToConcurrentModules :: [t] -> Int
grantPointsToConcurrentModules (ts:tss) = 
    if n <= 1 
        then 0
        else n * 10
    where n = length(ts:tss)

-- apply Rule 1 to timetable
applyConcurrentModulesRule :: [[t]] -> Int
applyConcurrentModulesRule (t:ts) = sum (map grantPointsToConcurrentModules (t:ts))

-- list all rules here
applyRules :: [[t]] -> Int
applyRules (t:ts) = applyConcurrentModulesRule (t:ts) 
                    -- + applyConcurrentModulesRule (t:ts)

mapIndex :: [Int] -> [(Int, Int)]
mapIndex (p:ps) = zip [0..n] (p:ps) where n = (length (p:ps) - 1)

mapPointsToTimetable :: [[[t]]] -> [(Int, Int)]
mapPointsToTimetable (t:ts) = mapIndex (map applyRules (t:ts))
-- [(0,20),(1,20),(2,20),(3,0),(4,0),(5,0),(6,0),(7,0),(8,0)]

sortByPoints :: [(Int, Int)] -> [(Int, Int)]
sortByPoints (p:ps) = sortBy (compare `on` snd) (p:ps)

getIndexOfLeastPoints :: [(Int, Int)] -> Int
getIndexOfLeastPoints (t:ts) = fst (head (sortByPoints (t:ts)))

-- [[(("Day1",8,10),"OO")],[(("Day1",10,12),"SE")],[(("Day1",13,15),"OO")],[(("Day2",10,12),"SE")]]
-- enough rules should be applied to get better timetable
getTimetableByIndex :: [t] -> Int -> t
getTimetableByIndex (t:ts) index = (t:ts) !! index

-- TODO 
result0 = getMyModulesPlan [OO, SE]
result01 = mapMyModulePlanToTimeslots result0
result1 = mapTimetableWithGenericTimeslots (generateAllTimetables result01)
result2 = mapTimetablesGrouppedByTimeslot result1
result3 = mapPointsToTimetable result2
result4 = getIndexOfLeastPoints result3
result5 = getTimetableByIndex result2 result4

getMyTimetable n  =  sum result6
                            where result6 = [1..n]