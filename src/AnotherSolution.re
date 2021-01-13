let id = (x) => x;

type r = {f: 'a .'a => 'a};

// If you don't mind passing the same arguments multiple times,
//  you can add as much arguments as necessary to have only
//  monomorphic arguments:
let map_pair = (f1, f2, (p1, p2)) => (f1(p1), f2(p2));

let sample = map_pair(id, id, (1, true));
