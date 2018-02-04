module MyLibrary

#light

//
// InputScores
//
// Given the complete filepath to a text file of exam scores, 
// inputs the scores and returns them as a list of integers.
//
let InputScores filepath = 
  let L = [ for line in System.IO.File.ReadAllLines(filepath) -> line ]
  List.map (fun score -> System.Int32.Parse(score)) L


//
// NumScores
//
// Recursively counts the # of scores in the list.
//
let rec NumScores L = 
    match L with
    | []    -> 0
    | hd::tl -> 1 + NumScores tl
                    


//
// FindMin
//
// Recursively finds the min score in the list.
//
let rec Min minSoFar L = 
  match L with
  | [] -> minSoFar
  | hd::tl -> if hd < minSoFar then
               Min hd tl
              else
               Min minSoFar tl
  
let FindMin L = 
  let hd = List.head L
  Min hd (List.tail L)


//
// FindMax
//
// Recursively finds the max score in the list.
//
let rec Max maxSoFar L =  
  match L with
  | [] -> maxSoFar
  | hd::tl -> if hd > maxSoFar then
                Max hd tl
              else
                Max maxSoFar tl
  
let FindMax L = 
  let hd = List.head L
  Max hd (List.tail L)



//
// Average
//
// Computes the average of a non-empty list of integers;
// the result is a real number (not an integer).
//

let rec NumScoresF L = 
    match L with
    | []    -> 0
    | hd::tl -> 1 + NumScoresF tl

let rec Sum L = 
    match L with
    | []    -> 0
    | hd::tl -> hd + Sum tl

let Average L = 
   float(Sum L) / float(NumScoresF L)
      


//
// Median
//
// Computes the median of a non-empty list of integers;
// the result is a real number (not an integer) since the 
// median may be the average of 2 scores if the # of scores
// is even.
//
let rec _median L skip isEven = 
  match skip with
  | 0 when isEven -> let first = List.head L
                     let second = List.head (List.tail L)
                     (first+second) / 2
  | 0 -> List.head L
  | _ -> _median (List.tail L) (skip-1) isEven

let Median L = 
  let skip = ((List.length L) - 1) / 2
  let isEven = ((List.length L) % 2) = 0
  _median L skip isEven



//
// StdDev
//
// Computes the standard deviation of a complete population
// defined by the integer list L.  Returns a real number.
//
let StdDev L = 
  let mean = Average L
  let var = L |> List.averageBy (fun x -> (float(x)-float(mean))*(float(x)-float(mean)))
  System.Math.Sqrt(var)


//
// Histogram
//
// Returns a list containing exactly 5 integers: [A;B;C;D;F].
// The integer A denotes the # of scores in L that fell in the
// range 90-100, inclusive.  B is the # of scores that fell in
// the range 80-89, inclusive.  C is the range 70-79, D is the
// range 60-69, and F is the range 0-59.
//
let rec histo acc high low L = 
    match L with
    | [] -> 0
    | hd::tl -> if (hd >= low) then
                    if(hd <= high) then
                        1 + (histo acc high low tl)
                    else
                        0 + (histo acc high low tl)
                else
                   0 + (histo acc high low tl)
                
let Histogram L = 
   let l1 = histo 0 100 90 L
   let l2 = histo 0 89 80 L
   let l3 = histo 0 79 70 L
   let l4 = histo 0 69 60 L
   let l5 = histo 0 59 0 L
   [l1; l2; l3; l4; l5]


//
// Trend
//
// Trend is given 3 lists of integer scores:  L1, L2, L3.  The lists are 
// non-empty, and |L1| = |L2| = |L3|.  L1 are the scores for exam 01, L2
// are the scores for exam 02, and L3 are the scores for exam 03.  The
// lists are in "parallel", which means student i has their scores at 
// position i in each list.  Example: the first exam in each list denote
// the exams for student 0.
//
// Trend returns a new list R such that for each student, R contains a '+'
// if the exam scores were score1 < score2 < score3 --- i.e. the scores
// are trending upward.  R contains a '-' if score1 > score2 > score3, i.e.
// the scores are trending downward.  Otherwise R contains '=' (e.g. if
// score1 < score2 but then score2 > score3).  
//
let rec _merge L1 L2 L3 acc = 
  match L1, L2, L3 with
  | [], [], [] -> (List.rev acc)
  | _, [], [] -> (List.rev acc) @ L3
  | [], [], _ -> (List.rev acc) @ L3
  | [], _, []  -> (List.rev acc) @ L2
  | _, [], _ -> (List.rev acc) @ L1
  | hd1::tl1, hd2::tl2, hd3::tl3 ->
    if ((hd1 < hd2) && (hd2 < hd3)) then
        '+'
        if ((hd1 > hd2) && (hd2 > hd3)) then
               ['-']
        else
            ['=']
    else 
        ['?']

    

                
                
    

let Trend L1 L2 L3 = _merge 
