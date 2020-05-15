Expansions
==========

1.

type t [@resources : int] =
  Int of int
| Bool of bool
| Cons of t * t

----->

type t =
  {t' : t';
   resources : int}

and t' =
  Int of int
| Bool of bool
| Cons of t * t 

2.

let rec flip = function
  Cons (x, y) -> Cons (flip x, flip y)
| r -> r

----->

let rec flip a =
  match a.t' with
    Cons (x, y) -> {a with t' = Cons (flip y, flip x)}
  | r -> a

A. Change match a --> match a.t'
B. type t' on RHS must be expanded to t

3.

let rec total_count a =
  match a with
    Cons (x, y) -> total_count x + total_count y
  | _ -> a.resources

----->

let rec total_count a =
  match a.t' with
    Cons (x, y) -> total_count x + total_count y
  | _ -> a.resources

