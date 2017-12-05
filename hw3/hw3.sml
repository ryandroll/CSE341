(* Coursera Programming Languages, Homework 3, Provided Code *)

exception NoAnswer

datatype pattern = Wildcard
		         | Variable of string
		         | UnitP
		         | ConstP of int
		         | TupleP of pattern list
		         | ConstructorP of string * pattern

datatype valu = Const of int
	          | Unit
	          | Tuple of valu list
	          | Constructor of string * valu

fun g f1 f2 p =
  let
	  val r = g f1 f2
  in
	  case p of
	      Wildcard          => f1 ()
	    | Variable x        => f2 x
	    | TupleP ps         => List.foldl (fn (p,i) => (r p) + i) 0 ps
	    | ConstructorP(_,p) => r p
	    | _                 => 0
  end

(**** for the challenge problem only ****)

datatype typ = Anything
	         | UnitT
	         | IntT
	         | TupleT of typ list
	         | Datatype of string

(**** you can put all your code here ****)

val only_capitals = List.filter (Char.isUpper o (fn x => String.sub (x, 0)))

fun longest_string1 (lst : string list) : string =
  foldl (fn (ele, reg) => if String.size ele > String.size reg then ele
                         else reg)
        ""
        lst

fun longest_string2 (lst : string list) : string =
  foldl (fn (ele, reg) => if String.size ele >= String.size reg then ele
                         else reg)
        ""
        lst

fun longest_string_helper ruler lst =
  foldl (fn (ele, reg) => if (ruler (String.size ele, String.size reg)) then ele
                         else reg)
        ""
        lst

val longest_string3 = longest_string_helper (fn (x, y) => x > y)
val longest_string4 = longest_string_helper (fn (x, y) => x >= y)
val longest_capitalized = longest_string1 o only_capitals
val rev_string = implode o rev o explode

fun first_answer checker alst =
  case alst
   of [] => raise NoAnswer
    | x :: xs' => case checker x
                  of SOME b => b
                   | NONE => first_answer checker xs'

fun all_answers checker alst =
  let
     fun rec_helper (tail_ans : {aug_lst : 'a list option list, ans_lst : 'a list option}) =
        case tail_ans
         of {aug_lst = [], ans_lst = a} => a
          | {aug_lst = (NONE :: xs'), ans_lst = SOME []} => rec_helper {aug_lst = xs', ans_lst = NONE}
          | {aug_lst = (NONE :: xs'), ans_lst = a} => rec_helper {aug_lst = xs', ans_lst = a}
          | {aug_lst = (SOME x :: xs'), ans_lst = NONE} => rec_helper {aug_lst = xs', ans_lst = SOME x}
          | {aug_lst = (SOME x :: xs'), ans_lst = SOME a} => rec_helper {aug_lst = xs', ans_lst = SOME (a@x)}
  in
      rec_helper {aug_lst = (map checker alst), ans_lst = SOME []}
  end
