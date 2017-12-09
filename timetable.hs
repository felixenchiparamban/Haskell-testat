-- Timetable

-- define Module Type
data Module = OO | BSYS1 | BSYS2 | SE | PARAPROG | ENGPROJ | AN1 | AN2 | DM deriving (Show, Eq) 

-- define Class Type
data Class = Lecture (Day, Int, Int) | Exercise (Day, Int, Int) deriving Show

-- define Day Type
data Day = Day1 | Day2 | Day3 | Day4 | Day5 deriving Show

{-- 
OO Lecture(Day1, 8, 10), Exercises[(Day1, 10, 12), (Day1, 13, 15), (Day2, 8, 10)]
SE2 Lecture(Day1, 10, 12), Exercises[(Day2, 10, 12), (Day3, 8, 10), (Day3, 10, 12)]
AN1 Lecture(Day1, 13, 15), Exercises[(Day2, 10, 12), (Day2, 13, 15), (Day3, 15, 17)]
DM Lecture(Day1, 10, 12), Exercises[(Day1, 17, 19), (Day5, 13, 15), (Day3, 15, 17)]
--}

-- Knowledge Base
-- Module tuples
modulesTimetable = [
    (OO, (Day1, 8, 10), [(Day1, 10, 12), (Day1, 13, 15), (Day2, 8, 10)]), 
    (SE, (Day1, 10, 12), [(Day2, 10, 12), (Day3, 8, 10), (Day3, 10, 12)]),
    (AN1, (Day1, 13, 15), [(Day2, 10, 12), (Day2, 13, 15), (Day3, 15, 17)])]

-- find out lecture for the given module
-- lectures :: Module -> (Day, Int, Int)
lecture x = [l | (m, l, es) <- modulesTimetable, x == m]

-- find out lecture and exercise sessions of my modules
myModulesTimetable (x:xs) = [(m,l, es) | (m, l, es) <- modulesTimetable, m `elem` (x:xs)]

modulePlanCombination (m, l, es) = [ (m, l, e) | e <- es]
myModulePlanCombinations myModules = map modulePlanCombination (myModulesTimetable myModules)

-- enrolledModulesTimetable (x:xs)
mytest = [
    ("maths", ["mon", "tue"]),
    ("science", ["mon", "wed"])]

-- one combination
-- [(maths, mon), (science, mon)]

-- build all combinations
-- combinations = [[("maths", "mon"), ("science", "mon")],
--                  [("maths", "mon"), ("science", "wed")]
--                  -- etc.
--                  ]

-- [("maths","mon"),("maths","tue"),("science","mon"),("science","wed")]
flatSubjectDayArray (subject, days) = [ (subject, d) | d <- days]
mappedSubjectDays = map flatSubjectDayArray mytest

combinations = [ (m, e) | (m, z) <- mytest, e <- z]
combinations2 = [ [c] | c <- [1..2]]