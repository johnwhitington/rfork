type t =
  {t' : t';
   resources : int}

and t' =
  Int of int
| Bool of bool
| Cons of t * t 

(* original *)
let rec total a =
  match a.t' with
    Int i -> i
  | Bool _ -> 0
  | Cons (x, y) -> total x + total y

let rec total_count a =
  let total =
    match a.t' with
      Cons (x, y) -> total_count x + total_count y
    | _ -> 0
  in
    a.resources + total

let rec flip a =
  match a.t' with
    Cons (x, y) -> {a with t' = Cons (flip y, flip x)}
  | r -> a

(* new. *)
type t [@resources : int] =
  Int of int
| Bool of bool
| Cons of t * t

(* a actually means a.t when deconstructing *)
let rec total a =
  match a with
    Int i -> i
  | Bool _ -> 0
  | Cons (x, y) -> total x + total y (* but when applying, means the whole thing *)

(* or even *)
let rec total = function
  Int i -> i
| Bool _ -> 0
| Cons (x, y) -> total x + total y


(* to get at resources, use .resources *)
let rec total_count a =
  match a with
    Cons (x, y) -> total_count x + total_count y
  | _ -> a.resources


(* new flip *)
let rec flip = function
  Cons (x, y) -> Cons (flip x, flip y)
| r -> r


