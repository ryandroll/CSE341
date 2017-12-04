(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)
fun all_except_option (keyword, slst) =
  let
      fun rec_checker slst =
        case slst
         of [] => []
          | (x :: xs') => if same_string (x, keyword) then xs'
                         else x :: (rec_checker xs')
      val post_slst = rec_checker slst
  in
      if post_slst = slst then NONE
      else SOME post_slst
  end

fun get_substitutions1 (slstlst, keyword) =
  case slstlst
   of [] => []
    | x :: xs' => case (all_except_option (keyword, x))
                  of NONE => (get_substitutions1 (xs', keyword))
                   | SOME s => (get_substitutions1 (xs', keyword)) @ s

fun get_substitutions2 (slstlst, keyword) =
  let
      fun tail_helper (post_slstlst, answer) =
        case post_slstlst
         of [] => answer
          | x :: xs' => case (all_except_option (keyword, x))
                        of NONE => tail_helper (xs', answer)
                         | SOME s => tail_helper (xs', answer @ s)
  in
      tail_helper (slstlst, [])
  end

fun similar_names (sub_name_table, {first = f, middle = m, last = l}) =
  let
      fun rec_helper lst =
        case lst
         of [] => []
          | x :: xs' => {first = x, middle = m, last = l} :: rec_helper xs'
  in
      rec_helper(f :: get_substitutions2 (sub_name_table, f))
  end

(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw

exception IllegalMove

(* put your solutions for problem 2 here *)

fun card_color (c : card) : color =
  case c
   of (Clubs, _) => Black
    | (Diamonds, _) => Red
    | (Hearts, _) => Red
    | (Spades, _) => Black

fun card_value (c : card) : int  =
  case c
   of (_, Ace) => 11
    | (_, Num x) => x
    | (_, _) => 10

fun remove_card (cs : card list, c : card, e : exn) : card list =
  case cs
   of [] => raise e
    | x :: xs' => if x = c then xs'
                 else remove_card (xs', c, e)

fun all_same_color (cs : card list) : bool =
  case cs
   of [] => true
    | x :: [] => true
    | x1 :: x2 :: xs' => (card_color x1 = card_color x2) andalso
                        all_same_color (x2 ::xs')

fun sum_cards (cs : card list) : int =
  let
      fun tail_helper (cs, sum_val) =
        case cs
         of [] => sum_val
          | x :: xs' => tail_helper (xs', (card_value x) + sum_val)
  in
      tail_helper(cs, 0)
  end

fun score (cs : card list, goal : int) : int =
  let val sum = sum_cards cs
      val prelim_score = if sum > goal then 3 * (sum-goal)
                         else goal-sum
  in
      if all_same_color cs then prelim_score div 2
      else prelim_score
  end
