let id = (x) => x;

type r = {f: 'a .'a => 'a};

// If you don't mind passing the same arguments multiple times,
//  you can add as much arguments as necessary to have only
//  monomorphic arguments:


// We can avoid both record wrapping and argument multiplication using Obj.magic to cheat with the type system. This solution may be the most effective, but it completely bypasses the type checker and I would not use it.
let map_pair = (f: 'a => 'a, (p1, p2)) => (f(p1), (Obj.magic(f): 'b => 'b)(p2));

let sample = map_pair(id, (1, true));
Js.log(sample)

