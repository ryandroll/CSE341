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
                 else x :: remove_card (xs', c, e)

fun all_same_color (cs : card list) : bool =
  case cs
   of [] => true
    | x :: [] => true
    | x1 :: x2 :: xs' => (card_color x1 = card_color x2) andalso
                        all_same_color (x2 ::xs')

fun sum_cards (cs : card list) : int =
  foldl (fn (a, b) => a + b) 0 (map (fn x => card_value x) cs)

fun score (cs : card list, goal : int) : int =
  let
      val sum = sum_cards cs
      val prelim_score = if sum > goal then 3 * (sum-goal)
                         else goal - sum
  in
      if all_same_color cs then prelim_score div 2
      else prelim_score
  end

fun officiate (cl : card list, ml : move list, goal) =
  let
      fun tail_helper (cl : card list , hl : card list , ml : move list) =
        case ml
         of [] => hl
          | Discard m :: ms' => tail_helper(cl, remove_card(hl, m, IllegalMove), ms')
          | Draw :: ms' =>
            case cl of
                [] => hl
              | c :: cs' =>
                if  sum_cards (c :: hl) > goal then c :: hl
                else tail_helper (cs', c :: hl , ms')
  in
      score (tail_helper(cl, [], ml), goal)
  end

fun score_challenge (cs : card list, goal : int) : int =
  let
      val ace_count = length(List.filter (fn (su, ra) => ra = Ace) cs)
      val sum = sum_cards cs
      fun prelim_score sum =
        if sum > goal then 3 * (sum-goal)
        else goal - sum
      fun alt_score ace_count =
        case ace_count
         of 0 => []
          | i => prelim_score(sum - 10 * i) :: alt_score(i - 1)
      val min_prelim_score = foldl (fn (a, b) => if a < b then a else b)
                                   (prelim_score sum)
                                   (alt_score ace_count)
  in
      if all_same_color cs then min_prelim_score div 2
      else min_prelim_score
  end

fun officiate_challenge (cl: card list, ml: move list, goal: int) =
  let
      fun min_card_value (c : card) : int  =
        case c
         of (_, Ace) => 1
          | (_, Num x) => x
          | (_, _) => 10
      fun min_sum_cards (cl : card list) : int =
        foldl (fn (a, b) => a + b) 0 (map (fn x => min_card_value x) cl)
      fun tail_helper (cl: card list, hl: card list, ml: move list) =
        case ml of
            [] => hl
          | Discard m :: ms' => tail_helper(cl, remove_card(hl, m, IllegalMove), ms')
          | Draw :: ms' =>
            case cl of
                [] => hl
              | c :: cs' =>
                if  min_sum_cards (c :: hl) > goal then c :: hl
                else tail_helper (cs', c :: hl , ms')
  in
      score_challenge(tail_helper(cl, [], ml), goal)
  end

fun careful_player (cl : card list, goal : int) : move list =
  (*Method: remove the largest value card*)
  let
      fun add_card_sort (cl : card list, c : card) : card list =
        case cl
         of [] => [c]
          | x :: xs' => if card_value c >= card_value x then c :: x :: xs'
                       else x :: add_card_sort (xs', c)

      fun card_finder (cl : card list, v : int) =
        case cl
         of [] => NONE
          | x :: xs' => if card_value x = v then SOME x
                       else card_finder (xs', v)

      fun tail_helper (status : {cl : card list, hl : card list, ml : move list}) =
        case status
         of {cl = [], hl, ml} => ml
          | {cl = c :: cs', hl = [], ml} =>
            if goal > card_value c
            then tail_helper {cl = cs', hl = [c], ml = ml @ [Draw]}
            else if goal = card_value c then ml @ [Draw]
            else ml
          | {cl = c :: cs', hl = h :: hs', ml} =>
            let
                val goal_diff = goal - (sum_cards (h :: hs'))
            in
                if goal_diff > card_value c
                then tail_helper {cl = cs', hl = add_card_sort (h :: hs', c),
                                  ml = ml @ [Draw]}
                else if goal_diff = card_value c then ml @ [Draw]
                else if goal_diff + card_value h >= card_value c
                then case card_finder (h :: hs', (card_value c) - goal_diff)
                      of NONE => tail_helper {cl = c :: cs', hl = hs',
                                             ml = ml @ [Discard h]}
                       | SOME a => ml @ [Discard a, Draw]
                else ml
            end
  in
      tail_helper {cl = cl, hl = [], ml = []}
  end
