(* Homework3 Simple Test*)
(* These are basic test cases. Passing these tests does not guarantee that your code will pass the actual homework grader *)
(* To run the test, add a new line to the top of this file: use "homeworkname.sml"; *)
(* All the tests should evaluate to true. For example, the REPL should say: val test1 = true : bool *)

use "hw3provided.sml";

val test1 = only_capitals ["A","B","C"] = ["A","B","C"]
val test1a = only_capitals ["aBcd", "Cwwe", "AAie"] = ["Cwwe", "AAie"]
val test1b = only_capitals ["a","b","c"] = [];
val test1c = only_capitals [] = [];


val test2 = longest_string1 ["A","bc","C"] = "bc";
val test2a = longest_string1 [] = "";
val test2b = longest_string1 ["a","b","c"] = "a";

val test3 = longest_string2 ["A","bc","C"] = "bc";
val test3a = longest_string2 ["a","b","c"] = "c";


val test4a= longest_string3 ["A","bc","C"] = "bc";
val test4ab = longest_string1 ["a","b","c"] = "a";

val test4b= longest_string4 ["A","B","C"] = "C";
val test4bb = longest_string2 ["a","b","c"] = "c";


val test5 = longest_capitalized ["A","bc","C"] = "A";
val test5a = longest_capitalized [] = "";
val test5b = longest_capitalized ["a","b"] = "";

val test6 = rev_string "abc" = "cba";
val test6a = rev_string "" = "";

val test7 = first_answer (fn x => if x > 3 then SOME x else NONE) [1,2,3,4,5] = 4;

val test8 = all_answers (fn x => if x = 1 then SOME [x] else NONE) [2,3,4,5,6,7] = NONE;
val test8a = all_answers (fn x => if x > 1 then SOME [x] else NONE) [2,3,4,5,6,7] =
	     SOME [2,3,4,5,6,7];


val test9a = count_wildcards Wildcard = 1;
val test9aa = count_wildcards(TupleP [Wildcard, TupleP [Wildcard, Wildcard]]) = 3;

val test9b = count_wild_and_variable_lengths (Variable("a")) = 1;
val test9ba = count_wild_and_variable_lengths (
	TupleP [Wildcard, TupleP [Wildcard, Wildcard], Variable("abc")]) = 6;

val test9c = count_some_var ("x", Variable("x")) = 1;
val test9ca = count_some_var ("x",  TupleP[Variable("x"), Variable("x"), Wildcard]) = 2;
val test9cb = count_some_var (
	"x",  TupleP[Variable("x"), ConstructorP("x", Wildcard), Wildcard]) = 1;
val test9cc = count_some_var (
	"x",  TupleP[
	    Variable("x"), ConstructorP("x", Wildcard), Wildcard, TupleP[Variable("x")]])
	      = 2;

val test10 = check_pat (Variable("x")) = true;
val test10a = check_pat (TupleP[Variable("x"), Variable("x")]) = false;
val test10b = check_pat (Wildcard) = true;
val test10c = check_pat (TupleP[Variable("x"), TupleP[Variable("x"),Wildcard]]) = false;

val test11 = match (Const(1), UnitP) = NONE;
val test11a = match (Unit, Wildcard) = SOME [];
val test11b = match (Unit, Variable "foo") = SOME [("foo", Unit)];
val test11c = match (Unit, UnitP) = SOME [];
val test11d = match (Const 123, ConstP 123) = SOME [];
val test11e = match (Const 123, ConstP 456) = NONE;
val test11f = match (Constructor("foo", Unit), ConstructorP("foo", Variable "blah")) =
	      SOME [("blah", Unit)];
val test11g = match (Constructor("foo", Unit), ConstructorP("bar", UnitP)) = NONE;
val test11h = match (Tuple([Const(123), Unit, Tuple[Unit], Constructor("test", Unit)]),
		     TupleP([ConstP(123), UnitP, TupleP[Variable "foo"], ConstructorP("test", UnitP)])) =
	      SOME [("foo", Unit)];
val test11i = match (Tuple([Const(123), Unit, Tuple[Unit], Constructor("test", Unit)]),
		     TupleP([ConstP(123), UnitP, TupleP[Variable "foo"], ConstructorP("test1", UnitP)])) =
	      NONE;

val test12 = first_match Unit [UnitP] = SOME []
val test12a = first_match Unit [] = NONE;
val test12b = first_match Unit [ConstP 123] = NONE
val test12c = first_match (Tuple[Const 123]) [UnitP, (TupleP[Variable "foo"])] =
	      match(Tuple[Const 123], TupleP[Variable "foo"])
val test12d = first_match Unit [Variable "foo", Wildcard] = SOME [("foo", Unit)];
