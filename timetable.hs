-- Timetable

-- define Module Type
data Module = OO | BSYS1 | BSYS2 | SE1 | SE2 | PARAPROG | ENGPROJ | AN1 | AN2 | DM deriving Show

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
modulesTimetable = [(OO, )]