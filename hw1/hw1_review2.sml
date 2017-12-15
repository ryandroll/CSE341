val months = [
  "January", "February", "March", "April", "May", "June",
  "July", "August", "September", "October", "November", "December"
]

val days_count_in_months = [
  31, 28, 31, 30, 31, 30,
  31, 31, 30, 31, 30, 31
]

(* e.g. val test1 = is_older ((1,2,3),(2,3,4)) = true *)
(* date format: (year * month * day) *)
fun is_older (t1: (int * int * int), t2: (int * int * int)): bool =
  if (#1 t1) < (#1 t2)
  then true
  else
    if (#1 t1) = (#1 t2)
    then
        if (#2 t1) < (#2 t2)
        then true
        else
          if (#2 t1) = (#2 t2)
          then (#3 t1) < (#3 t2)
          else false
    else
      false
(* TODO [improvement] use 'orelse' and 'andalso' *)

fun get_older_one (t1: (int * int * int), t2: (int * int * int)): (int * int * int) =
  if is_older(t1, t2)
  then t1
  else t2

fun contains (num: int, nums2: int list) =
  if null nums2
  then false
  else ((hd nums2) = num) orelse contains(num, tl(nums2))

(* e.g. val test5 = dates_in_months ([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[2,3,4]) = [(2012,2,28),(2011,3,31),(2011,4,28)] *)
fun dates_in_months (ts: (int * int * int) list, nums: int list): (int * int * int) list =
  let
    fun f (ts2: (int * int * int) list) =
      if null ts2
      then []
      else let
        val item = (#2 (hd ts2));
      in
        if contains (item,  nums)
        then (hd ts2) :: f (tl(ts2))
        else f (tl(ts2))
      end
  in
    f (ts)
  end

(* e.g. val test4 = dates_in_month ([(2012,2,28),(2013,12,1)],2) = [(2012,2,28)] *)
fun dates_in_month (ts: (int * int * int) list, num: int): (int * int * int) list =
  dates_in_months (ts, num :: [])

fun size_of_list (xs: (int * int * int) list): int =
  if null xs
  then 0
  else 1 + size_of_list(tl(xs))

(* e.g. val test3 = number_in_months ([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[2,3,4]) = 3 *)
fun number_in_months (ts: (int * int * int) list, nums: int list) =
  size_of_list(dates_in_months (ts, nums))

(* e.g. val test2 = number_in_month ([(2012,2,28),(2013,12,1)],2) = 1 *)
fun number_in_month (ts: (int * int * int) list, num: int): int =
  number_in_months(ts, num :: [])


(* TODO fix list index error problem. *)
(* e.g. val test6 = get_nth (["hi", "there", "how", "are", "you"], 2) = "there" *)
fun get_nth (items: 'a list, target_idx: int): 'a =
  let
    fun f (current_items: 'a list, current_idx: int) =
      if (target_idx = current_idx)
      then hd(current_items)
      else f(tl(current_items), current_idx + 1)
  in
    f (items, 1)
  end
(* TODO [improvement] use reducing target_idx instead *)

(* e.g. val test7 = date_to_string (2013, 6, 1) = "June 1, 2013" *)
fun date_to_string (year_int: int, month_int: int, day_int: int): string =
  get_nth(months, month_int) ^ " " ^ Int.toString(day_int) ^ ", " ^ Int.toString(year_int)

(* e.g. val test8 = number_before_reaching_sum (10, [1,2,3,4,5]) = 3 *)
fun number_before_reaching_sum (target_sum: int, nums: int list): int =
  let
    fun f (curr_nums: int list, curr_sum: int, prev_idx: int): int =
      if ((curr_sum + hd(curr_nums)) >= target_sum)
      then prev_idx
      else f(tl(curr_nums), curr_sum + hd(curr_nums), prev_idx + 1)
  in
    f(nums, 0, 0)
  end
(* TODO [improvement] modify target_sum instead *)

(* e.g. val test9 = what_month 70 = 3 *)
fun what_month (days_count: int): int =
  number_before_reaching_sum(days_count, days_count_in_months) + 1

(* e.g. val test10 = month_range (31, 34) = [1,2,2,2] *)
fun month_range (day1: int, day2: int): int list =
  let
    fun f (from_day: int, to_day: int): int list =
       if (from_day > to_day)
       then []
       else
         what_month(from_day) :: f(from_day + 1, to_day)
  in
    f(day1, day2)
  end
(* TODO [improvement] merge #month_range and local #f *)

(* e.g. val test11 = oldest([(2012,2,28),(2011,3,31),(2011,4,28)]) = SOME (2011,3,31) *)
fun oldest (days: (int * int * int) list) =
  let
    fun f1(oldest_day: (int * int * int), remains_days: (int * int * int) list): (int * int * int) =
      if null remains_days
      then oldest_day
      else
        f1(
          get_older_one(oldest_day, hd(remains_days)),
          tl(remains_days)
        )
  in
    if null days
    then NONE
    else SOME(f1(hd(days), tl(days)))
  end
(* NOTE yes, my version is better as the suggestion tells. *)

fun remove_duplicates (nums: int list): int list =
  let
    fun f2 (prev_nums: int list, curr_num: int, next_nums: int list): int list =
      let
        val prev_nums2 = if contains(curr_num, prev_nums)
                         then prev_nums
                         else curr_num :: prev_nums
      in
        if null next_nums
        then prev_nums2
        else f2 (prev_nums2, hd(next_nums), tl(next_nums))
      end
  in
    if null nums
    then []
    else f2 ([], hd(nums), tl(nums))
  end
(* TODO [improvement] use remove_duplicates itself *)

(* e.g. val test12 = number_in_months_challenge * ([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[2,3,4,2]) = 3 *)
fun number_in_months_challenge (ts: (int * int * int) list, nums: int list) =
  number_in_months (ts, remove_duplicates(nums))

fun dates_in_months_challenge (ts: (int * int * int) list, nums: int list): (int * int * int) list =
  dates_in_months (ts, remove_duplicates(nums))

fun is_leap_year(year: int): bool =
  ((year mod 400) = 0) orelse ( ((year mod 4) = 0) andalso ((year mod 100) <> 0) )

fun reasonable_date(t1: (int * int * int)): bool =
  let
    val year = #1 t1
    val month = #2 t1
    val day = #3 t1
  in
    if (year <= 0) orelse (not ((month >= 1) andalso (month <= 12)))
    then false
    else
      let
        val is_leap = is_leap_year(year)
        val max_days_count_in_current_month = if (is_leap andalso (month = 2))
                                              then 29
                                              else get_nth(days_count_in_months, month)
      in
        (day >= 1) andalso (day <= max_days_count_in_current_month)
      end
  end


