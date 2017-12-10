# Haskell Testat - Timetable

by Felix Varghese Enchiparambam & Keerthikan Thurairatnam

First we need to define a module plan which shows when the lecture and exercises of a module are held, as shown below.

## Module Plan

A modules plan is a list of tuples `(Module, Lecture, [Excercises])`.

```hs
modulesPlan = [
    (OO, (Day1, 8, 10), [(Day1, 10, 12), (Day1, 13, 15), (Day2, 8, 10)]), 
    (SE, (Day1, 10, 12), [(Day2, 10, 12), (Day3, 8, 10), (Day3, 10, 12)]),
    (AN1, (Day1, 13, 15), [(Day2, 10, 12), (Day2, 13, 15), (Day3, 15, 17)])]
```

## Get Timetable

You can pass your selection of modules (e.g. `[OO, SE]`) and our program will give you the best suited timetable for the next semester.

```hs
*Main> getMyTimetable  [OO, SE]

[[((Day1,8,10),OO)],[((Day1,10,12),SE)],[((Day1,13,15),OO)],[((Day2,10,12),SE)]]
```

In background, the `getMyTimetable` function calls the following functions sequentially to get the most suitable timetable. In the [Function Reference](#function-references-and-examples) section, each function is explained in detail with step by step instruction.

```hs
getMyTimetable (m:ms) =
    getTimetableByIndex step5 $
    getIndexOfLeastPoints $
    mapPointsToTimetable step5
        where {
            step5 = mapTimetablesGrouppedByTimeslot $
                    mapTimetableWithGenericTimeslots $
                    generateAllTimetables $
                    mapMyModulePlanToTimeslots $
                    getMyModulesPlan (m:ms)
        }
```

The complete code is [here](timetable.hs).

## Function References and Examples

In this section we explain the necessary steps how the functions can be used to generate a best suited timetable.

### Step 1: getMyModulesPlan

The `getMyModulesPlan` function returns all the timeslots (lecture and exercises) for the given modules.

```hs
*Main> step1 = getMyModulesPlan [OO, SE]

*Main> step1
[(OO,(Day1,8,10),[(Day1,10,12),(Day1,13,15),(Day2,8,10)]),
(SE,(Day1,10,12),[(Day2,10,12),(Day3,8,10),(Day3,10,12)])]
```

### Step 2: mapMyModulePlanToTimeslots

The function `mapMyModulePlanToTimeslots` generate all the possible tuples of lecture and exercise. 

A module has one lecture and multiple exercises. For instance, `OO` has a lecture on `(Day1,8,10)` and a list of 3 exercises on `[(Day1,10,12),(Day1,13,15),(Day2,8,10)]`

In this case, it generate 3 tuples as follows.
1. `(OO,((Day1,8,10),(Day1,10,12))`
2. `(OO,((Day1,8,10),(Day1,13,15))`
3. `(OO,((Day1,8,10),(Day2,8,10)))`

```hs
*Main> step2 = mapMyModulePlanToTimeslots step1

*Main> step2
[[(OO,((Day1,8,10),(Day1,10,12))),(OO,((Day1,8,10),(Day1,13,15))),(OO,((Day1,8,10),(Day2,8,10)))],
[(SE,((Day1,10,12),(Day2,10,12))),(SE,((Day1,10,12),(Day3,8,10))),(SE,((Day1,10,12),(Day3,10,12)))]]
```

### Step 3: generateAllTimetables

It generates all the possible timetables. In our example, it generates 9 different timetables for the selected 2 modules (OO, SE).

```hs
*Main> step3 = generateAllTimetables step2

*Main> step3
[[(OO,((Day1,8,10),(Day1,10,12))),(SE,((Day1,10,12),(Day2,10,12)))],
[(OO,((Day1,8,10),(Day1,10,12))),(SE,((Day1,10,12),(Day3,8,10)))],
[(OO,((Day1,8,10),(Day1,10,12))),(SE,((Day1,10,12),(Day3,10,12)))],
[(OO,((Day1,8,10),(Day1,13,15))),(SE,((Day1,10,12),(Day2,10,12)))],
[(OO,((Day1,8,10),(Day1,13,15))),(SE,((Day1,10,12),(Day3,8,10)))],
[(OO,((Day1,8,10),(Day1,13,15))),(SE,((Day1,10,12),(Day3,10,12)))],
[(OO,((Day1,8,10),(Day2,8,10))),(SE,((Day1,10,12),(Day2,10,12)))],
[(OO,((Day1,8,10),(Day2,8,10))),(SE,((Day1,10,12),(Day3,8,10)))],
[(OO,((Day1,8,10),(Day2,8,10))),(SE,((Day1,10,12),(Day3,10,12)))]]
```

### Step 4: mapTimetableWithGenericTimeslots

We want to convert the more specific module tuple `(Module, Lecture, Exercise)` to two generic tuples with timeslots `(Timeslot, Module)`.

For instance `(OO,((Day1,8,10),(Day1,10,12))` will be converted to
1. `((Day1,8,10),OO)`
2. `((Day1,10,12),OO)`

```hs
*Main> step4 = mapTimetableWithGenericTimeslots step3

*Main> step4
[[((Day1,8,10),OO),((Day1,10,12),OO),((Day1,10,12),SE),((Day2,10,12),SE)],
[((Day1,8,10),OO),((Day1,10,12),OO),((Day1,10,12),SE),((Day3,8,10),SE)],
[((Day1,8,10),OO),((Day1,10,12),OO),((Day1,10,12),SE),((Day3,10,12),SE)],
[((Day1,8,10),OO),((Day1,10,12),SE),((Day1,13,15),OO),((Day2,10,12),SE)],
[((Day1,8,10),OO),((Day1,10,12),SE),((Day1,13,15),OO),((Day3,8,10),SE)],
[((Day1,8,10),OO),((Day1,10,12),SE),((Day1,13,15),OO),((Day3,10,12),SE)],
[((Day1,8,10),OO),((Day1,10,12),SE),((Day2,8,10),OO),((Day2,10,12),SE)],
[((Day1,8,10),OO),((Day1,10,12),SE),((Day2,8,10),OO),((Day3,8,10),SE)],
[((Day1,8,10),OO),((Day1,10,12),SE),((Day2,8,10),OO),((Day3,10,12),SE)]]
```

### Step 5: mapTimetablesGrouppedByTimeslot

A timetable contains a list of tuples `(Timeslot, Module)`. We want to group the tuples by the timeslot.

For example, the following timetable has 4 tuples with 3 different timeslots `[((Day1,8,10),OO),((Day1,10,12),OO),((Day1,10,12),SE),((Day2,10,12),SE)]`.

After grouping, the timestable should contain 3 elements as follows.
1. `[((Day1,8,10),OO)]`
2. `[((Day1,10,12),OO),((Day1,10,12),SE)]`
3. `[((Day2,10,12),SE)]`

```hs
*Main> step5 = mapTimetablesGrouppedByTimeslot step4

*Main> step5
[[[((Day1,8,10),OO)],[((Day1,10,12),OO),((Day1,10,12),SE)],[((Day2,10,12),SE)]],
[[((Day1,8,10),OO)],[((Day1,10,12),OO),((Day1,10,12),SE)],[((Day3,8,10),SE)]],
[[((Day1,8,10),OO)],[((Day1,10,12),OO),((Day1,10,12),SE)],[((Day3,10,12),SE)]],
[[((Day1,8,10),OO)],[((Day1,10,12),SE)],[((Day1,13,15),OO)],[((Day2,10,12),SE)]],
[[((Day1,8,10),OO)],[((Day1,10,12),SE)],[((Day1,13,15),OO)],[((Day3,8,10),SE)]],
[[((Day1,8,10),OO)],[((Day1,10,12),SE)],[((Day1,13,15),OO)],[((Day3,10,12),SE)]],
[[((Day1,8,10),OO)],[((Day1,10,12),SE)],[((Day2,8,10),OO)],[((Day2,10,12),SE)]],
[[((Day1,8,10),OO)],[((Day1,10,12),SE)],[((Day2,8,10),OO)],[((Day3,8,10),SE)]],
[[((Day1,8,10),OO)],[((Day1,10,12),SE)],[((Day2,8,10),OO)],[((Day3,10,12),SE)]]]
```

### Step 6: mapPointsToTimetable

In order to compare the timetables, we give points to them by applying different rules.

* Rule 1 : *ConcurrentModules Rule*
  * If there is only one module in the timeslot => 0 pts
  * If there are several modules (n) in the same timeslot => 10 * n pts

> More rules can be defined as mentioned above

> More rules should be applied to get better result

The function `mapPointsToTimetable` applies all the defined rules and returns the points `(index, points)` for every timetable.

>> the lower the points the better the timetable.

```hs
*Main> step6 = mapPointsToTimetable step5

*Main> step6
[(0,20),(1,20),(2,20),(3,0),(4,0),(5,0),(6,0),(7,0),(8,0)]
```

### Step 7: getIndexOfLeastPoints

The function `getIndexOfLeastPoints` sorts the list `[(0,20),(1,20),(2,20),(3,0),(4,0),(5,0),(6,0),(7,0),(8,0)]` by points and returns the index with least points.

```hs
*Main> step7 = getIndexOfLeastPoints step6

*Main> step7
3
```

### Step 8: getTimetableByIndex

The function `getTimetableByIndex` returns the timetable at the given index.

```hs
*Main> step8 = getTimetableByIndex step5 step7

*Main> step8
[[((Day1,8,10),OO)],[((Day1,10,12),SE)],[((Day1,13,15),OO)],[((Day2,10,12),SE)]]
```