(* returns true if date d1 is older than date d2, false otherwise *)
(*1*)
fun is_older (d1: (int * int * int), d2: (int * int * int)) =
  if #1 (d1) > #1 (d2)
  then false
  else if #1 (d2) > #1 (d1)
  then true
  else
      if #2 (d1) > #2 (d2)
      then false
      else if #2 (d2) > #2 (d1)
      then true
      else
	  if #3 (d2) > #3 (d1)
	  then true
	  else false



(* returns the number of dates in list l1 that have the month mo *)
(*2*)
fun number_in_month (l1 : (int * int * int) list , mo: int) =

      if null l1
      then 0
      else
	  let
	      val cnt = number_in_month( tl(l1), mo);
          in
	      if ((#2 (hd (l1))) = mo)
              then cnt + 1
              else cnt

	  end
(* helper function to append two date lists together *)
(*3*)
fun append_lists (l1 : (int * int * int) list, l2 : (int* int* int) list ) =
  if null l1
  then l2
  else (hd l1) ::  append_lists ((tl l1), l2)

(*4*)
(*returns the number of dates from list l_date that have a month from list l_mo *)
fun number_in_months (l_date: (int * int * int) list, l_mo: int list) =


      if null (l_mo)
      then 0
      else
      (

	let
	    val mo = hd(l_mo);
	    val cnt = number_in_month(l_date, mo);
	    val total_cnt = cnt +  number_in_months(l_date, tl(l_mo));
        in

            total_cnt

        end
      )
(*returns a list of dates from an input list of dates that have the month mo*)
fun dates_in_month (l1 : (int* int* int) list, mo) =

       if null l1
       then []
       else(
	   let
	       val date = hd l1;
	       val date_list = dates_in_month(tl (l1), mo)

           in
               if ((date_list = []) andalso ((#2 date) <> mo))
	       then date_list
	       else if((#2 date) = mo)
	       then
	       (
		   append_lists([date], date_list)
	       )
	       else date_list


	   end
       )
(*returns a list of dates from an input list of dates that have the months listed in l_mo *)
fun dates_in_months (l_date: (int * int * int) list, l_mo: int list) =


      if null (l_mo)
      then let val ls : (int * int * int) list = []
	   in
	       ls
           end
      else
      (

	let
	    val mo = hd(l_mo);
	    val list = dates_in_month(l_date, mo)

        in
	    append_lists(list, dates_in_months(l_date, tl(l_mo)))
        end
      )
(* gets the n index string from a list of strings l1*)
fun get_nth (l1: string list, n: int) =
  let
      val index = 1
  in
      if (index = n)
      then hd l1
      else get_nth (tl l1, n - 1)
  end

(*returns a numerical date as a string date, utilizing get_nth function from above*)
fun date_to_string(d1 : (int * int * int)) =
  let
      val mo_list = ["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"]
  in
      get_nth(mo_list, #2 d1) ^ " " ^ Int.toString(#3 d1) ^ ", " ^ Int.toString(#1 d1)
  end

(*given sum and a input list, returns the last index for which the sum of all previous indices is less than sum *)
fun number_before_reaching_sum (sum: int, l1: int list) =

    if null l1
    then 0
    else
	let
	    fun inc (i: int, partial_sum: int, xs : int list) =

	      if partial_sum <= 0 then i
	      else inc(i+1, partial_sum - hd(xs), tl(xs))
        in

	    inc(0,sum, l1)
	end

(*returns what month a day of the year falls into*)
fun what_month (day: int) =
  let
      val days_in_mos = [31,28,31,30,31,30,31,31,30,31,30,31];
  in
      number_before_reaching_sum(day, days_in_mos)
  end

(*returns a list of months for each day of the year between day1 and day2*)
fun month_range (day1 : int, day2: int) =
  if  day1 > day2
  then []
  else if day1 = day2
  then [what_month(day1)]
  else
      let
          fun count(from: int, to: int) =
	    if from >= to then []
	    else from::count(from+1, to)

	  fun append(day_list: int list) =
	    if null day_list then []
	    else what_month(hd day_list)::append(tl(day_list))

	  val day_list = count(day1,day2)
      in
	  append(day_list)@[what_month(day2)]
      end
(*returns the oldest of a list of dates*)
fun oldest (dl : (int * int * int) list ) =
  if null dl
  then NONE
  else
      let
	  fun compare(date: (int * int * int), dl: (int * int * int) list) =
	    if null (tl(dl)) then date
	    else if is_older(date, hd(tl dl)) then
            (
		compare(date, tl(dl))
	    )
	    else
	    (
		compare (hd(tl dl), (tl(dl)))
            )
      in
	  SOME(compare(hd dl, dl))
      end
