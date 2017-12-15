(* Yixuan Hu, HW1 *)

(* is_older: 
    @inputs: two dates. 
    @returns: true/false. 
        True if the first argument is a date that comes before the second argument. 
        If the two dates are the same, the result is false.
 *)
fun is_older(date1: int*int*int, date2: int*int*int) =
    if (#1 date1) < (#1 date2) then true
    else if ((#1 date1) = (#1 date2) ) andalso ((#2 date1) < (#2 date2)) then true
    else if ((#1 date1) = (#1 date2) ) andalso ((#2 date1) = (#2 date2)) andalso ((#3 date1) < (#3 date2)) then true
    else false 

(* number_in_month: 
    @inputs: a list of dates and a month (i.e., an int) 
    @returns: how many dates in the list are in the given month.
    *)
fun number_in_month (dates: (int * int * int) list , month: int ) = 
    if null dates then 0
    else if ((#2 (hd dates)) = month) then 1 + number_in_month(tl dates, month)
    else number_in_month(tl dates, month)

(* number_in_months:
    @inputs: a list of dates and a list of months (i.e., an int list) 
    @returns: the number of dates in the list of dates that are 
        in any of the months in the list of months. 
        Assume the list of months has no number repeated. 
    *)
fun number_in_months (dates: (int * int * int) list, months: int list) =
    if null months then 0
    else number_in_month(dates, hd months) + number_in_months (dates, tl months)

(*  dates_in_month:
    @inputs: a list of dates and a month
    @returns: a list holding the dates from the argument list of dates that are in the month. 
    The returned list should contain dates in the order they were originally given.
    *)
fun dates_in_month (dates: (int * int * int) list, month: int) = 
    if null dates then []
    else if ((#2 (hd dates)) = month) then hd dates::dates_in_month(tl dates, month)
    else dates_in_month(tl dates, month)

(*  dates_in_months:
    @inputs: a list of dates and a list of months (i.e., an int list)
    @returns: a list holding the dates from the argument list of dates 
    that are in any of the months in the list of months. 
    Assume the list of months has no number repeated. 
    *)
fun dates_in_months (dates: (int * int * int) list, months: int list) = 
    if null months then []
    else dates_in_month(dates, hd months) @ dates_in_months(dates, tl months)

(*  get_nth:
    @inputs:a list of strings and an int n
    @returns: the nth element of the list where the head of the list is 1st. 
    *)
fun get_nth(strings: string list, n: int) = 
    if n = 1 then hd strings
    else get_nth(tl strings, n-1)

(*  date_to_string:
    @inputs: a date
    @returns: a string of the form January 20, 2013 (for example). 
    *)
fun date_to_string (date: int*int*int) = 
    let val months = ["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"]
    in
        get_nth(months, #2 date) ^ " " ^ Int.toString(#3 date) ^ ", " ^ Int.toString(#1 date)
    end

(*  number_before_reaching_sum:
    @inputs: an int called sum (positive), and an int list (all positive numbers)
    @returns: an int n, 
    such that the first n elements of the list add to less than sum, 
    but the first n + 1 elements of the list add to sum or more. 
    *)
fun number_before_reaching_sum (sum1: int, numList: int list)=
    if (hd numList >= sum1) then 0
    else 1 + number_before_reaching_sum (sum1 - hd numList, tl numList)

(* what_month:
    @input: a day of year (i.e., an int between 1 and 365) 
    @returns: what month that day is in (1 for January, 2 for February, etc.). 
    *)
fun what_month (day: int) =
    let val nDays=[31,28,31,30,31,30,31,31,30,31,30,31]
    in
        number_before_reaching_sum(day, nDays)+1
    end

(*month_range:
    @inpits: two days of the year day1 and day2
    @returns: an int list [m1,m2,...,mn] where m1 is the month of day1, 
        m2 is the month of day1+1, ..., 
        and mn is the month of day day2. 
    *)
fun month_range(day1: int, day2: int)=
    if (day1 > day2) then []
    else if (day1 = day2) then what_month(day1)::[]
    else what_month(day1)::month_range(day1+1,day2)

(* oldest:
    @inputs: a list of dates 
    @returns: evaluates to an (int*int*int) option. 
        It evaluates to NONE if the list has no dates and 
        SOME d if the date d is the oldest date in the list.
    *)
fun oldest (dates: (int * int * int) list)=
    if null dates then NONE
    else         
        let val tl_ans = oldest(tl dates)
        in  if isSome tl_ans andalso is_older(valOf tl_ans, hd dates)
            then tl_ans
            else SOME (hd dates)
        end


(********* C H A L L E N G E S ********)       


(* is_leap_year:
    @input: a date 
    @returns: whether the date is in a leap year
*)
fun is_leap_year (date: int*int*int) =
    if (#1 date mod 100 <> 0 andalso #1 date mod 4 = 0) then true
    else false

(* get_nth_int:
    @input: an integer list
    @returns: the n-th integer of the list
    *)
fun get_nth_int (nums: int list, n: int) = 
    if n = 1 then hd nums
    else get_nth_int(tl nums, n-1)

(* reasonable_date:
    @input: a date
    @returns: true/false indicating whether the date is valid
    *)
fun reasonable_date (date: int*int*int) = 
    if (#1 date <= 0) then false
    else if (#2 date > 12 orelse #2 date < 1) then false
    else
        let 
            val nDays = [31,28,31,30,31,30,31,31,30,31,30,31]
            val nDays_leap = [31,29,31,30,31,30,31,31,30,31,30,31]
            val days_of_month  = 
                if is_leap_year(date) then get_nth_int(nDays_leap, #2 date)
                else get_nth_int(nDays, #2 date)
        in 
            if (#3 date <= days_of_month andalso #3 date > 0) then true
            else false
        end
