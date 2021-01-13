

# http://ocamltutorials.blogspot.com/2013/06/alternatives-to-subtyping.html
# http://devmusings.legiasoft.com/blog/2008/05/23/higher-rank_polymorphism_in_ocaml
```reason
let id = (x) => x;

type r = {f: 'a .'a => 'a};

let map_pair = (r, (p1, p2)) => (r.f(p1), r.f(p2));

let sample = map_pair({f: id}, (3, true));

/* let map_pair f (p1, p2) =
   let r = { f = f } in
   (r.f p1, r.f p2) */
let map_pair = (f1, f2, (p1, p2)) => (f1(p1), f2(p2));

let sample = map_pair(id, id, (1, true));

let map_pair = (f: 'a => 'a, (p1, p2)) => (f(p1), (Obj.magic(f): 'b => 'b)(p2));

let sample = map_pair(id, (1, true));
```
