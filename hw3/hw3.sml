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
     fun tail_helper (alst, blst) =
       case alst
        of [] => SOME blst
         | x :: xs' => case checker x
                       of NONE => NONE
                        | SOME a  => tail_helper (xs', blst @ a)
  in
      tail_helper (alst, [])
  end

fun same_string(s1 : string, s2 : string) =
  s1 = s2

val count_wildcards = g (fn () => 1) (fn x => 0)
val count_wild_and_variable_lengths =  g (fn () => 1) (fn x => String.size x)
fun count_some_var (var : string, pat : pattern) =
  g (fn () => 0) (fn x => if same_string (x, var) then 1 else 0) pat

fun check_pat (pat : pattern) =
  let
      fun var_lst (pat : pattern list) =
        case pat
         of [] => []
          | TupleP plst :: xs' => (var_lst plst) @ (var_lst xs')
          | ConstructorP (_, p) :: xs' => (var_lst [p]) @ (var_lst xs')
          | Variable str :: xs' => str :: (var_lst xs')
          | _ :: xs' => var_lst xs'
      fun dupl_checker (stl : string list) =
        case stl
         of [] => true
          | x :: xs' => (List.all (fn a => not (a = x)) xs')
                       andalso (dupl_checker xs')
  in
     (dupl_checker o var_lst) [pat]
  end

fun match (v: valu, p : pattern) : (string * valu) list option =
  case (v,p)
   of (_, Wildcard) => SOME []
    | (_, Variable s)  => SOME [(s, v)]
    | (Unit, UnitP) => SOME []
    | (Const n1, ConstP n2) => if n1 = n2 then SOME [] else NONE
    | (Tuple vs, TupleP ps) => if length vs <> length ps then NONE
                              else all_answers match (ListPair.zip (vs, ps))
    | (Constructor(s2,v), ConstructorP (s1,p)) => if s1 = s2 then match (v, p)
                                                 else NONE
    | (_, _) => NONE

fun first_match (v : valu) (plst : pattern list) : (string * valu) list option =
  SOME (first_answer (fn x => match (v,x)) plst)
  handle NoAnswer => NONE

fun typecheck_patterns ((c : string, v : string, ty : typ) option ) patt =
  let 
      fun pat_chk patt =
        case patt
         of Wildcard => SOME [Anything]
          | Variable _ => SOME [Anything]
          | UnitP => SOME [UnitT]
          | ConstP _ => SOME [IntT]
          | TupleP plst => (case all_answers pat_chk plst
                            of SOME x => SOME [TupleT x]
                             | NONE => NONE)
          | ConstructorP (c1, p1) => case s
                                     of NONE => NONE
                                      | SOME {c, v, ty} => if c1 <> c then NONE
                                                          else (case pat_chk p1
                                                                 of SOME ty => SOME [Datatype v]
                                                                  | NONE => NONE)
  in
      pat_chk patt
  end

fun lenient_type (typ1, typ2) =
  case (typ1, typ2)
   of (Anything, x) => SOME [x]
    | (x, Anything) => SOME [x]
    | (UnitT, UnitT) => SOME [UnitT]
    | (IntT, IntT) => SOME [IntT]
    | (Datatype x, Datatype y) => if x = y then SOME [Datatype x] else NONE
    | (TupleT xlst, TupleT ylst) => (if length xlst <> length ylst then NONE
                                    else case all_answers lenient_type (ListPair.zip (xlst, ylst))
                                          of SOME x => SOME [TupleT x]
                                           | NONE => NONE)
    | _ => NONE



(*
datatype pattern = Wildcard
		         | Variable of string
		         | UnitP
		         | ConstP of int
		         | TupleP of pattern list
		         | ConstructorP of string * pattern

datatype typ = Anything
	         | UnitT
	         | IntT
	         | TupleT of typ list
	         | Datatype of string
*)

val test_typecheck_patterns_1 = lenient_type (
       TupleT [Anything,TupleT [Anything, IntT]],
       TupleT [IntT,TupleT [IntT,Anything]])


val test_typecheck_patterns_11 = typecheck_patterns NONE
                                      (TupleP[Wildcard, TupleP [ConstP 3, Wildcard]])

val test_typecheck_patterns_21 = typecheck_patterns (SOME {c="foo2",v="bar2",ty=UnitT}) (ConstructorP("foo1", Variable "x"))
(*
val test_typecheck_patterns_22 = typecheck_patterns SOME ("c", "t", IntT) (TupleP [Wildcard, TupleP [Variable "x", UnitP]])
val test_typecheck_patterns_23 = typecheck_patterns SOME ("c", "t", IntT) (TupleP [ConstructorP("c", ConstP 13), Wildcard])

val testlist = all_answers (fn x => typecheck_patterns SOME ("c", "t", IntT) x ) [TupleP [Wildcard, TupleP [ConstP 3, Wildcard]],
                                                                                 TupleP [Wildcard, TupleP [Variable "x", UnitP]],
                                                                                 TupleP [ConstructorP("c", ConstP 13), Wildcard]]
*)
                           (*
fun all_answers checker alst =
  let
      fun tail_helper (alst, blst) =
        case alst
         of [] => SOME blst
          | x :: xs' => case checker x
                        of NONE => NONE
                         | SOME a  => tail_helper (xs', a @ blst)

                                *)
