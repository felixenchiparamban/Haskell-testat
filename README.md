# Haskell Testat - Timetable

by Felix Varghese Enchiparambam & Keerthikan Thurairatnam

First we need to define a module plan which shows when a module's lecture and exercises are held, as shown below.

## Module Plan
```hs
modulesPlan = [
    (OO, (Day1, 8, 10), [(Day1, 10, 12), (Day1, 13, 15), (Day2, 8, 10)]), 
    (SE, (Day1, 10, 12), [(Day2, 10, 12), (Day3, 8, 10), (Day3, 10, 12)]),
    (AN1, (Day1, 13, 15), [(Day2, 10, 12), (Day2, 13, 15), (Day3, 15, 17)])]
```

You can pass your selection of modules (e.g. `[OO, SE]`) and our program will give you the best suitable timetable for the next semester.

The complete code is [here](timetable.hs).




