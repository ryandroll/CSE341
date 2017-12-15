(*Homework 1*)

fun is_older (date_01 : int * int * int, date_02 : int * int * int) =
  if #1 date_01 < #1 date_02 then true
  else if #1 date_01 > #1 date_02 then false
  else if #2 date_01 < #2 date_02 then true
  else if #2 date_01 > #2 date_02 then false
  else if #3 date_01 < #3 date_02 then true
  else false

fun number_in_month (date_list : (int * int * int) list, month : int) =
  if null date_list then 0
  else if #2 (hd date_list) = month
  then 1 + number_in_month (tl date_list, month)
  else number_in_month (tl date_list, month)

fun number_in_months (date_list : (int * int * int) list,
                      month_list : int list) =
  if null month_list then 0
  else number_in_month (date_list, hd month_list) +
       number_in_months (date_list, tl month_list)

fun dates_in_month (date_list : (int * int * int) list, month : int) =
  if  null date_list then []
  else if #2 (hd date_list) = month
  then (hd date_list) :: dates_in_month (tl date_list, month)
  else dates_in_month (tl date_list, month)

fun dates_in_months (date_list : (int * int * int) list,
                     month_list : int list) =
  if null month_list then []
  else dates_in_month (date_list, hd month_list) @
       dates_in_months (date_list, tl month_list)

fun  get_nth (str_list : string list, n : int)=
  if n = 1 then hd str_list
  else get_nth (tl str_list, n - 1)

fun date_to_string (date : int * int * int) =
  let
      val month_alias = ["January", "February", "March",
                         "April", "May", "June",
                         "July", "August", "September",
                         "October", "November", "December"]
      fun month_name (n : int) =
          get_nth (month_alias, n)
  in
      month_name (#2 date) ^
      " " ^ Int.toString (#3 date) ^
      ", " ^ Int.toString (#1 date)
  end

fun number_before_reaching_sum (sum : int, num_list : int list) =
  let
     fun ans_rec (num_list : int list, accu : int, position : int) =
       if accu >= sum then position - 1
       else ans_rec (tl num_list, accu + (hd num_list), position + 1)
  in
      ans_rec (num_list, 0, 0)
  end

fun what_month (day : int) =
  let
      val days_month_list = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
  in
      number_before_reaching_sum (day, days_month_list) + 1
  end

fun month_range (day1 : int, day2 : int) =
  if day1 > day2 then []
  else
     what_month (day1) :: month_range (day1 + 1, day2)

fun oldest (date_list : (int * int * int) list) =
    if null date_list then NONE
    else
        let
            fun ans_rec (temp_oldest : (int * int * int),
                         date_list : (int * int * int) list) =
                if null date_list then temp_oldest
                else if is_older (temp_oldest, hd date_list)
                then ans_rec (temp_oldest, tl date_list)
                else ans_rec (hd date_list, tl date_list)
        in
           SOME (ans_rec (hd date_list, tl date_list))
        end

fun remove_multiple (num_list : int list) =
  let
      fun del_element (num : int, num_list : int list) =
        if null num_list then []
        else if num = hd num_list
        then del_element (num, tl num_list)
        else hd num_list :: del_element (num, tl num_list)

      fun ans_rec (num_list : int list) =
        if null num_list then []
        else hd num_list :: ans_rec (del_element (hd num_list, tl num_list))
  in
      ans_rec (num_list)
  end

fun number_in_months_challenge (date_list : (int * int * int) list,
                                month_list : int list) =
  number_in_months (date_list, remove_multiple (month_list))

fun dates_in_months_challenge (date_list : (int * int * int) list,
                               month_list : int list) =
  dates_in_months (date_list, remove_multiple (month_list))

fun reasonable_date (date : int * int * int) =
  let
      val year = #1 date;
      val month = #2 date;
      val day = #3 date;

      fun year_verified (year : int) =
        year > 0
      fun month_verified (month : int) =
        month > 0 andalso month < 13
      fun leap_year_check (year : int) =
        year mod 400 = 0 orelse (year mod 4 = 0 andalso not (year mod 100 = 0))
      fun get_nth (num_list : int list, n : int)=
        if n = 1
        then hd num_list
        else get_nth (tl num_list, n - 1)

      val common = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
      val leap = [31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]

      fun day_verified (year: int, month: int, day : int) =
        if  day < 1
        then false
        else
            if leap_year_check (year)
            then day <= get_nth (leap, month)
            else day <= get_nth (common, month)
  in
      year_verified (year) andalso
      month_verified (month) andalso
      day_verified (year, month, day)
  end
