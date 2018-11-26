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

val only_capitals : string list -> string list =
    List.filter (Char.isUpper o (fn x => String.sub (x, 0)))

val longest_string1 : string list -> string =
    foldl (fn (ele, reg) => if String.size ele > String.size reg
                           then ele
                           else reg)
          ""
val longest_string2 : string list -> string =
    foldl (fn (ele, reg) => if String.size ele >= String.size reg
                           then ele
                           else reg)
          ""

fun longest_string_helper (ruler : int * int -> bool) : string list -> string =
    foldl (fn (ele, reg) => if (ruler (String.size ele, String.size reg))
                           then ele
                           else reg)
          ""

val longest_string3 : string list -> string =
    longest_string_helper (fn (x, y) => x > y)
                                            
val longest_string4 : string list -> string =
    longest_string_helper (fn (x, y) => x >= y)
                          
val longest_capitalized : string list -> string =
    longest_string1 o only_capitals

val rev_string : string -> string =
    implode o rev o explode

fun first_answer (checker : 'a -> 'b option) (alst : 'a list) : 'b =
  case alst
   of [] => raise NoAnswer
    | x :: xs' => case checker x
                  of SOME b => b
                   | NONE => first_answer checker xs'

fun all_answers (checker : 'a -> 'b list option)
                (lst : 'a list)
    : 'b list option =
  let
     fun aux (alst, blst) =
       case alst
        of [] => SOME blst
         | x :: xs => case checker x
                       of NONE => NONE
                        | SOME a  => aux (xs,a @ blst)
  in
      aux (lst, [])
  end

fun same_string (s1 : string, s2 : string) =
    s1 = s2

val count_wildcards : pattern -> int =
    g (fn () => 1) (fn x => 0)

val count_wild_and_variable_lengths : pattern -> int  =
    g (fn () => 1) (fn x => String.size x)
      
fun count_some_var (var : string, pat : pattern) : int =
    g (fn () => 0) (fn x => if same_string (x, var) then 1 else 0) pat

fun check_pat (pat : pattern) =
    let
        fun lst p =
	        case p
             of Variable x        => [x]
	          | TupleP ps         => List.foldl (fn (p,i) => i @ (lst p)) [] ps
	          | ConstructorP(_,p) => lst p
	          | _                 => []
        fun checker (stl : string list) =
            case stl
             of [] => true
              | x :: xs' => (List.all (fn a => not (a = x)) xs')
                           andalso (checker xs')      
    in
        (checker o lst) pat
    end

fun match (v: valu, p : pattern) : (string * valu) list option =
    case (v, p)
     of (Tuple vs, TupleP ps) =>
        (all_answers match (ListPair.zipEq (vs, ps))
         handle UnequalLengths => NONE)
      | (Constructor(s2,v), ConstructorP (s1,p)) =>
        if s1 = s2 then match (v, p)
        else NONE                
      | (Const n1, ConstP n2) =>  if n1 = n2 then SOME [] else NONE
      | (Unit, UnitP) => SOME []
      | (_, Wildcard) => SOME []
      | (_, Variable s)  => SOME [(s, v)]
      | (_, _) => NONE                    
      
fun first_match (v : valu) (plst : pattern list) : (string * valu) list option =
  SOME (first_answer (fn x => match (v,x)) plst)
  handle NoAnswer => NONE

fun typecheck_patterns_ (tylst, plst) =
    let 
        fun ptoty (tylst, p) =
            case p
             of Wildcard => Anything
              | Variable s => Anything
              | UnitP => UnitT
              | ConstP n1 => IntT
              | TupleP pst => TupleT (map (fn x => ptoty (tylst, x)) pst)
              | ConstructorP(s1, p1) =>
                (case tylst
                  of (s, v, ty) :: xs' =>
                     if s = s1 andalso ty = ptoty (tylst, p1)
                     then Datatype v
                     else ptoty (xs', p)
                   | [] => raise NoAnswer)            
        fun tyred (ty1, ty2) =
            case (ty1, ty2)
             of (TupleT tlst1, TupleT tlst2) =>
                TupleT ((map tyred (ListPair.zipEq (tlst1, tlst2)))
                        handle UnequalLengths => raise NoAnswer)
             | (Anything, ty2) => ty2
             | (ty1, Anything) => ty1
             | (ty1, ty2) => if ty1 = ty2 then ty1
                            else raise NoAnswer
    in
        case plst
         of [] => raise NoAnswer
          | x :: xs' => List.foldl tyred
                                  (ptoty (tylst, x))
                                  (map (fn x => ptoty (tylst, x)) xs')
    end

fun typecheck_patterns (tylst, plst) =
    SOME (typecheck_patterns_ (tylst, plst))
    handle NoAnswer => NONE
