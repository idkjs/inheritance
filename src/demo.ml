

let () = Js.log "Hello, BuckleScript"

type grade = Pass | No_pass



(* This is a more typesafe approach than a string, which will assure us that grade is either Pass or No_pass - nothing else. Client code that use this type definition can't mess up, e.g. by mistake writing "Pas" or "no_pass".

Next is our student record type: *)

type student = {
   name : string;
   tests : int list;
   grade : grade option;
   compute_grade : student -> grade
}
(* The name field explains itself. Instead of int array for tests, we will use the for OCaml more natural int list. Grade is marked as option so grade will be either "Some grade" or "None", excluding the Not_graded we saw in the second Java example from the grade type definition. The last field, compute_grade, is the type of a function that takes a student as argument and produces a grade. The compute_grade function for graduate student will then look something like this: *)

(**
 Computes grading for a graduate student.

 @param s student record
 @return grade
*)
let graded_compute s =
   let tests = s.tests in
   let total = List.fold_left (+) 0 tests in
   match total / List.length tests with
      | q when q >= 80 -> Pass
      | _ -> No_pass



(* Ok, let's analyze this. Line 8 is only for convenience. The idiomatic way to sum an int list in OCaml is not using a for-loop, but a fold, so what line 9 is saying is: "sum all the elements in the list tests using function (+), starting from 0". It's possible to concatenate string lists - or indeed any kind of list - in the same way, using function (^) (for strings), starting from empty string "", etc.

Next thing that happens is the match statement - ML's switch-statement on steroids - to branch on the quota. If the quota is equal to or higher than 80, the student passed. Yey! Otherwise, the function returns No_pass. Since this is the last expression in the function, it means this is what the function returns. Notice also how we get the length from the list "tests": "List.length tests". Since "tests" is not an object, we do not write "tests.length" as in Java or Javascript, but instead call the length function from the List module, "List.length", and feed it with a list to get the result.

The undergraduate computation will of course be similar: *)

(**
 Computes grading for an undergraduate student.

 @param s student record
 @return grade
*)
let undergraded_compute s =
   let tests = s.tests in
   let total = List.fold_left (+) 0 tests in
   match total / List.length tests with
      | q when q >= 70 -> Pass
      | _ -> No_pass




let compute_aux limit s =
   let tests = s.tests in
   let total = List.fold_left (+) 0 tests in
   match total / List.length tests with
      | q when q >= limit -> Pass
      | _ -> No_pass

let graded_compute = compute_aux 80
let undergraded_compute = compute_aux 70



let student1 = {
   name = "Foo Barsson";
   tests = [78; 70; 92];
   grade = None;
   compute_grade = graded_compute;
}

let student2 = {
   name = "Ronald Tolkien";
   tests = [65; 32];
   grade = None;
   compute_grade = undergraded_compute;
}

let roster = [
   student1;
   student2;
]




let graded_roster =  List.map (fun s ->
   {s with grade =
      Some (s.compute_grade s)
   }) roster


(* println graded_roster *)

