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

fun only_capitals sl =
    List.filter (fn s => Char.isUpper(String.sub(s, 0))) sl

fun longest_string1 sl =
    List.foldl (fn (s, acc) => if String.size(s) > String.size(acc)
			       then s
			       else acc)
	       ""
	       sl

fun longest_string2 sl =
    List.foldl (fn (s, acc) => if String.size(s) >= String.size(acc)
			       then s
			       else acc)
	       ""
	       sl

fun longest_string_helper f sl =
    List.foldl (fn (s, acc) => if f(String.size(s), String.size(acc))
			       then s
			       else acc)
	       ""
	       sl

fun longest_string3 sl =
    let
	val helper = longest_string_helper (fn (a, b) => a > b)
    in
	helper sl
    end

fun longest_string4 sl =
    let
	val helper = longest_string_helper (fn (a, b) => a >= b)
    in
	helper sl
    end

fun longest_capitalized sl =
    (longest_string1 o only_capitals) sl

fun rev_string s =
    (String.implode o (List.rev o String.explode)) s

fun first_answer f l =
    case l of
	[] => raise NoAnswer
      | x::xs => case f(x) of
		     SOME v => v
		   | NONE => first_answer f xs

fun all_answers f l =
    let
	fun helper xs acc =
	    case xs of
		[] => SOME acc
	      | x::xs' => case f(x) of
			      NONE => NONE
			    | SOME v => helper xs' (acc@v)
    in
	helper l []
    end

fun count_wildcards p =
    g (fn () => 1) (fn x => 0) p

fun count_wild_and_variable_lengths p =
    g (fn () => 1) (fn x => String.size(x)) p

fun count_some_var (s, p) =
    g (fn () => 0) (fn x => if x = s then 1 else 0) p

fun check_pat p =
    let
	fun get_var_strs(p, acc) =
	    case p of
		Variable x => x::acc
	      | TupleP ps => List.foldl get_var_strs acc ps
	      | _ => acc
	fun all_unique sl =
	    case sl of
		[] => true
	      | s::ss => not (List.exists (fn x => x = s) ss) andalso all_unique(ss)
    in
	(all_unique o get_var_strs) (p, [])
    end

fun match (v, p) =
    case (v, p) of
	(_, Wildcard) => SOME []
      | (_, Variable s) => SOME [(s, v)]
      | (Unit, UnitP) => SOME []
      | (Const x, ConstP y) => if x = y then SOME [] else NONE
      | (Tuple vs, TupleP ps) => if List.length(ps) = List.length(vs)
				 then all_answers match (ListPair.zip(vs, ps))
				 else NONE
      | (Constructor(s2, vc), ConstructorP(s1, pc)) =>
	if s1 = s2 then match(vc,pc) else NONE
      | _ => NONE

fun first_match v lp =
    SOME (first_answer match (List.map (fn p => (v, p)) lp))
    handle NoAnswer => NONE
