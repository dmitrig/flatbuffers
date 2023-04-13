type offset = int

type _ tag =
  | TScalar : 'a Primitives.ty -> 'a tag
  | TRef : offset tag
  | TStruct :
      { sz : int
      ; align : int
      }
      -> offset tag

let[@inline] get_val (type a b) (t : a tag) (prim : b Primitives.t) (b : b) (i : int) : a =
  match t with
  | TScalar t' -> Primitives.get_scalar t' prim b i
  | TRef -> i + Primitives.get_uoffset prim b i
  | TStruct _ -> i
;;

(* get offset of a table via vtable *)
let[@inline] get_indirect p b i voff =
  let vi = i - Primitives.get_soffset p b i in
  let vsz = Primitives.get_voffset p b vi in
  if voff < vsz
  then (
    let foff = Primitives.get_voffset p b (vi + voff) in
    if foff <> 0 then i + foff else -1)
  else -1
;;

(* generic *)
let[@inline] read_table t p b i n =
  let i' = get_indirect p b i n in
  if i' < 0 then invalid_arg "required field not set" else get_val t p b i'
;;

(* scalars *)
let[@inline] read_table_default t p b i n ~default =
  let i' = get_indirect p b i n in
  if i' < 0 then default else Primitives.get_scalar t p b i'
;;

let[@inline] read_table_opt t p b i n =
  let i' = get_indirect p b i n in
  if i' < 0 then None else Some (Primitives.get_scalar t p b i')
;;

(* ref *)
let[@inline] read_table_opt_ref p b i n =
  let i' = get_indirect p b i n in
  if i' < 0 then i' else get_val TRef p b i'
;;

(* struct *)
let[@inline] read_table_struct p b i n =
  let i' = get_indirect p b i n in
  if i' < 0 then invalid_arg "required field not set" else i'
;;

let[@inline] read_table_opt_struct p b i n = get_indirect p b i n

(* vector *)
let[@inline] sz_val (type a) : a tag -> int = function
  | TScalar t -> Primitives.size_scalar t
  | TRef -> 4
  | TStruct { sz; _ } -> sz
;;

let[@inline] unsafe_get_vec t p b i j = get_val t p b (i + 4 + (sz_val t * j))
let[@inline] length_vec p b i = Primitives.get_uoffset p b i

let[@inline] get_vec t p b i j =
  if j < length_vec p b i
  then unsafe_get_vec t p b i j
  else invalid_arg "index out of bounds"
;;

let[@inline] to_list_vec t p b i =
  List.init (length_vec p b i) (fun j -> unsafe_get_vec t p b i j)
;;

let[@inline] to_array_vec t p b i =
  Array.init (length_vec p b i) (fun j -> unsafe_get_vec t p b i j)
;;

let[@inline] to_seq_vec t p b i =
  let len = length_vec p b i in
  let rec aux j () =
    if j < len
    then (
      let x = unsafe_get_vec t p b i j in
      Seq.Cons (x, aux (j + 1)))
    else Seq.Nil
  in
  aux 0
;;

let[@inline] iter_vec t p b f i =
  for j = 0 to length_vec p b i - 1 do
    f (unsafe_get_vec t p b i j)
  done
;;

let[@inline] get_string (type b) (prim : b Primitives.t) (b : b) (i : offset) : string =
  let len = length_vec prim b i in
  Primitives.get_string prim b ~off:(i + 4) ~len
;;
