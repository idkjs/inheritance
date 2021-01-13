/* # From Java to OCaml - problem-based tutorials

   This blog will cover intermediate topics in the OCaml language. Prior knowledge in Java and basic OCaml syntax is assumed.
   s ndag 21 juli 2013

   ## Alternatives to subtyping, Part 2

   ## Part two of Alternatives to inheritance (read part one here).

   Modules instead of classes

   Classes in Java serve a number of different purposes. One is to extend present classes with new code and types to avoid code duplication and reuse work done by earlier developers. The example we saw in the last blog (student, graduate students and undergraduate students) is an example of this. OCaml have classes, too, but we will instead look at how the module system can help us achieve some of these goals. In any case you should learn about OCaml's module language before continuing to the object system.

   As mentioned we have a class hierarchy consisting of a base class "student" and two subclasses "graduate student" and "undergraduate student". We will use the module feature to include another module to model this: */
/** Same grade type as before */
type grade =
  | Pass
  | No_pass;

/** For printing later on */
let string_of_grade =
  fun
  | Some(Pass) => "Pass"
  | Some(No_pass) => "No pass"
  | None => "Not graded";

module Student = {
  type t = {
    /* Naming the main type of the module 't' is conventional */
    name: string,
    tests: list(int),
    course_grade: option(grade),
  };
  /**  Makes a student
  @param name string
  @return t with name, no tests and no course_grade */
  let make = name => {name, tests: [], course_grade: None};
  /** Returns a string representation of a student */
  let to_string = s =>
    Printf.sprintf(
      "Student: %s; number of tests made: %d; course grade: %s",
      s.name,
      List.length(s.tests),
      string_of_grade(s.course_grade),
    );
};

/*
 The "student" type is exactly like in the last blog entry, but with the function field removed that computed the grade. We have a "make" function to make an instance of a student ("new" is a reserved word used by the object system), and even a small "to_string" function that does what's exptected from it. So how can we "extend" this type for customized computation (compute the grade)?

 Simple. We make a new module and include the old in it:
  */
module GraduateStudent = {
  include Student;
  let compute_course_grade = s => {
    let tests = s.tests;
    if (List.length(tests) > 0) {
      let total = List.fold_left((+), 0, tests);
      let grade =
        switch (total / List.length(tests)) {
        | q when q >= 80 => Pass
        | _ => No_pass
        };
      {...s, course_grade: Some(grade)};
    } else {
      s;
    };
  };
};

module UndergraduateStudent = {
  include Student;
  let compute_course_grade = s => {
    let tests = s.tests;
    if (List.length(tests) > 0) {
      let total = List.fold_left((+), 0, tests);
      let grade =
        switch (total / List.length(tests)) {
        | q when q >= 70 => Pass
        | _ => No_pass
        };
      {...s, course_grade: Some(grade)};
    } else {
      s;
    };
  };
};

/* Can we say that GraduateStudent "inherited" Student? Yes and no. Some code is reused. E.g., you don't have to recode the make- or to_string-function from the first module.
   On the other hand there's no subtyping going on here;
    the type system only knows that the type t from the first module is in the two later modules.
     For example, if you type */
/*
 let student1 = GraduateStudent.make "Ronald" and
     student2 = UndergraduateStudent.make "John";;
 [student1; student2;];;  (* type = GraduateStudent.t list *)
  */
/* in the toplevel, the list will have type GraduateStudent.t, disregarding UndergraduateStudent.
   How funny is that?
   This means trouble:
   there's nothing that prevents us from computing the course grade for a undergraduate student using a graduate student type! */
/*
 let student1 = GraduateStudent.make "Ronald";;
   (* This is not what we want! *)
 UndergraduateStudent.compute_course_grade student1;; */
/* A way to go around this is to "tag" the types with variants, like this: */
/* type student_type = Graduate Student.student | Undergraduate Student.student */
/* But you still have to keep the bookkeeping yourself; the type system won't help you from mixing things up. So how do we tell the compiler that the graduate and undergraduate students are in fact two different types, and still keep the code reuse? The answer is private type abbriviations. This is a somewhat advanced feature of the module language, so hold on to your hats.

   OCaml will make as much as the structure "public" as possible, which, in our case where we're not using signatures, is everything. That's why the type inference system "knows" that GraduateStudent.t is in fact the same type as UndergraduateStudent.t. The way to limit this is with a module signature, which you might already know. The signature is pretty much the interface of the module; in it you can define what will be visible from the outside. For example, we might want to keep the definition of the student for ourselves, so that client code don't pattern match on it and creates students without our admirable make-function:
    */
module SecretStudent: {
  type t; /* Only show the name of the type in the signature */
  let make: string => t; /* We want this to be shown, too! */
  let to_string: t => string; /* As this one */
} = {
  type t = {
    /* Here we declare the type fully */
    name: string,
    tests: list(int),
    course_grade: option(grade),
  };
  let make = name => {
    /* Everything else as usual */
    name,
    tests: [],
    course_grade: None,
  };
  /** Returns a string representation of a student */
  let to_string = s =>
    Printf.sprintf(
      "Name: %s, grade: %s",
      s.name,
      string_of_grade(s.course_grade),
    );
};

/* As exptected, printing this in the top level and then making a student shows that the student is abstract: */
/*

 open SecretStudent;;
 let s = make "Thorald Dickinson";;
 print_endline (to_string s) (* This will work *)
 print_endline s.name;;   (* Error: Unbound record field label name *)
  */
/* Obviously, the type system doesn't allow this. Everything in perfect order.

   Returning to our original problem, we will redefine the student type in the module signature, adding the private keyword. Using private is the middle-ground between fully including t from Student, and making it abstract (not visible outside the module). It allows pattern matching, but makes sure the student types in the two modules are treated as different types. Since we're defining our own signatures, we must include the signature of the student module into our new module, using the language construct include module type of Student, where "module type" means the signature of "Student". One more thing: To allow type coersion (explained below) We will explicitly denote that the type t in the included signature should not simply be copy-pasted, but reused. This is done with with type t = Student.t
    */
/* open SecretStudent;; */
module PrivateGradStudent: {
  include  (module type of Student) with type t = Student.t; /* Include sig of Student */
  type t' = pri t; /* Private type abbreviation */
  let make: string => t'; /* Redifine make to produce t', not t */
  let to_string: t' => string; /* t' here too */
  let compute_course_grade: t' => t'; /* and here */
} = {
  include Student; /* Include struct of Student */
  type t' = t; /* t' = Student.t */
  let compute_course_grade = s => {
    /* As before */
    let tests = s.tests;
    if (List.length(tests) > 0) {
      let tests = s.tests;
      let total = List.fold_left((+), 0, tests);
      let grade =
        switch (total / List.length(tests)) {
        | q when q >= 80 => Pass
        | _ => No_pass
        };
      {...s, course_grade: Some(grade)};
    } else {
      s;
    };
  };
};

module PrivateUndergradStudent: {
  /* As above */
  include  (module type of Student) with type t = Student.t;
  type t' = pri t;
  let make: string => t';
  let to_string: t' => string;
  let compute_course_grade: t' => t';
} = {
  include Student;
  type t' = t;
  let compute_course_grade = s => {
    let tests = s.tests;
    if (List.length(tests) > 0) {
      let total = List.fold_left((+), 0, tests);
      let grade =
        switch (total / List.length(tests)) {
        | q when q >= 70 => Pass
        | _ => No_pass
        };
      {...s, course_grade: Some(grade)};
    } else {
      s;
    };
  };
};

/* Now constructing two students and putting them in a list raises an error: */
/*
 let s1 = PrivateGradStudent.make "Foo";;
 let s2 = PrivateUndergradStudent.make "Bar";;
 [s1; s2];; (* <-- Error: Not the same types *)
  */
/* It's not possible to compute the grade wrong, either:

   PrivateGradStudent.compute_course_grade s2;; (_ <-- Error: Wrong type again _)

   As before, we can tag the types to include them in one list:
    */
let s1 = PrivateGradStudent.make("Foo");

let s2 = PrivateUndergradStudent.make("Bar");

type student_types =
  | Grad(PrivateGradStudent.t')
  | Undergrad(PrivateUndergradStudent.t');

let l = [Grad(s1), Undergrad(s2)];

/* To print them we can do something like:
 */
let print_list = l => List.iter(s => print_endline(s), l);

let sl =
  List.map(
    s =>
      switch (s) {
      | Grad(s) => PrivateGradStudent.to_string(s)
      | Undergrad(s) => PrivateUndergradStudent.to_string(s)
      },
    l,
  );

print_list(sl);

/* If we know we won't do anything else than printing these students, there's another trick in the OCaml hat: coersing (or type casting). We can upcast the private t' to its supertype t, like this: */
let s1' = (s1 :> Student.t); /* Coercion */

let s2' = (s2 :> Student.t);

let l = [s1', s2']; /* This is ok now */

let sl = List.map(s => Student.to_string(s), l);

print_list(sl);

/* PrivateGradStudent.compute_course_grade s1';; (* Won't work, s1' is of wrong type! *)
 */
/* Once you have coerced a value to its supertype, you can't coerce or cast it "back" like you could do in C/Java.
   Why? Because it's unsafe as hell, that's why. Type safety is honorary in OCaml.

     What have we achieved so far? We have added additional behaviour to a base module by including it, and made sure we can coerce an instance of it back to its supertype when we're done with all the specialized work. One inevitably wonders - can I add additional type information this way too? No. You can't modify a defined record type afterwords. It's not "open" in that sence.
      If you want that, you'd have to use the object system in OCaml.
       Or make a new type, possibly in the new module, though this more resembles aggregation (have-a) than inheritance (is-a).

     Another shortcoming - in an OO sence of reasoning - is the lack of "protected" members.
      E.g., one might want to factor out the point limit from the two compute_course_grade functions (80 and 70), and put an auxiliary function in the Student module, hidden from outside but visible from modules which include it. This is not possible. Functions are either completely hidden or visible for everyone.

     Same thing, but with functors

     A functor - in OCaml phrase book -
      is "a module parameterized with another module",
       or "a function from module to module".

    Basically, it's a way to smash two (or more) implementations together,
     just like the include command. The difference is, with a functor,
      you will know beforehand what signature the other module will have.
      This allows for some interesting concepts.
       Check out this easy example, where the functor Square assumes a number module which could be
        int, float, complex or whatever we choose:
      */
module type NUMBER = {
  /* Module types (signatures) are conventionally written with capitals */
  type t /* This could be anything */;
  let times: (t, t) => t; /* Multiplication */
};

module Square = (N: NUMBER) => {
  /* Functor accepts a NUMBER module as input */
  type t = {
    width: N.t, /* Instance of a number */
    height: N.t,
  };
  let area = t => N.times(t.width, t.height); /* Using the times function from N */
};

/*
 The module that is fed to the functor can be either named or anonymous, like below: */
module IntSquare =
  Square({
    /* Anonymous struct */
    type t = int;
    let times = (i, j) => i * j;
  });

module Float = {
  /* Defining a module */
  type t = float;
  let times = (i, j) => i *. j;
};

module FloatSquare = Square(Float); /* Named module */

/* Short example of usage: */
open FloatSquare;

let square = {width: 10., height: 5.5};

let _ = area(square); /* Returns 55. */

/* In the world of students,
we want a student functor that accepts another module with a grade computation function.
We will customize the student module with another module that carries the special computations.

   First, define the computation module signature: */
module type GRADE_COMPUTATION = {
  let compute_course_grade: list(int) => option(grade); /* int list = test results */
};

/* Next our functor: */
module StudentFunctor = (G: GRADE_COMPUTATION) => {
  type t = {
    /* Same as before */
    name: string,
    tests: list(int),
    course_grade: option(grade),
  };
  let make = name => {name, tests: [], course_grade: None};
  let to_string = s =>
    Printf.sprintf(
      "Name: %s; grade: %s",
      s.name,
      string_of_grade(s.course_grade),
    );
  let compute_course_grade = s => {
    ...s,
    course_grade: G.compute_course_grade(s.tests),
  };
};

/*
 Nothing surprising here, right? Notice how compute_course_grade are using G.compute_course_grade.

 I will save us some typing and define a help function for our next example.
  */
let compute_aux = (limit, tests) =>
  if (List.length(tests) > 0) {
    let total = List.fold_left((+), 0, tests);
    let grade =
      switch (total / List.length(tests)) {
      | q when q >= limit => Pass /* limit will be 80 and 70 */
      | _ => No_pass
      };
    Some(grade);
  } else {
    None;
  };

/* Now we can create our two student types like this:
 */
module GradStudent =
  StudentFunctor({
    let compute_course_grade = tests => compute_aux(80, tests);
  });

module UndergradStudent =
  StudentFunctor({
    let compute_course_grade = tests => compute_aux(70, tests);
  });

/*
 Easy as one, two, three!

 As usual we can make new students:
  */
let s = GradStudent.make("Foo");

let t = UndergradStudent.make("Bar");
/*

 Printing and computing course grade is exactly the same as above. Note that type coersion is not enabled, you'd have to do some signature trickery to do that.

 Ok, that's it. In my next blog, I might cover the actual object system in OCaml and give some brief explanation of the theories behind it. Thanks for reading! */
