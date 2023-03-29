open Generated.Monster
open MyGame.Sample

let read_vec3 buf p =
  ignore (Vec3.x buf p);
  ignore (Vec3.y buf p);
  ignore (Vec3.z buf p)
;;

let read_string buf s = Rt.String.iter buf ignore s

let read_weapon buf w =
  Weapon.name buf w |> Rt.Option.get |> read_string buf;
  Weapon.damage buf w |> ignore
;;

let read_uint8_vec buf v = Rt.UByte.Vector.iter buf ignore v
let read_weapon_vec buf v = Weapon.Vector.iter buf (read_weapon buf) v
let read_vec3_vec buf v = Vec3.Vector.iter buf (read_vec3 buf) v

let read_monster (buf, m) =
  Monster.pos buf m |> Rt.Option.get |> read_vec3 buf;
  Monster.mana buf m |> ignore;
  Monster.hp buf m |> ignore;
  Monster.name buf m |> Rt.Option.get |> read_string buf;
  Monster.inventory buf m |> Rt.Option.get |> read_uint8_vec buf;
  Monster.color buf m |> ignore;
  Monster.weapons buf m |> Rt.Option.get |> read_weapon_vec buf;
  Monster.equipped_type buf m |> ignore;
  Monster.equipped buf m ~weapon:(read_weapon buf) ~default:ignore;
  Monster.path buf m |> Rt.Option.get |> read_vec3_vec buf
;;

let write_monster b =
  Rt.Builder.reset b;
  let name = Rt.String.create b "Orc" in
  let inv = Rt.UByte.Vector.create b (Array.init 10 Char.chr) in
  let axe_name = Rt.String.create b "axe" in
  let bow_name = Rt.String.create b "bow" in
  let axe = Weapon.Builder.(start b |> add_name axe_name |> add_damage 100 |> finish) in
  let bow = Weapon.Builder.(start b |> add_name bow_name |> add_damage 90 |> finish) in
  let weapons = Weapon.Vector.create b [| axe; bow |] in
  let path = Vec3.Vector.create b [| 1., 2., 3.; 4., 5., 6.; 7., 8., 9. |] in
  let orc =
    Monster.Builder.(
      start b
      |> add_pos (1.0, 2.0, 3.0)
      |> add_hp 300
      |> add_name name
      |> add_color Color.blue
      |> add_inventory inv
      |> add_weapons weapons
      |> add_equipped_weapon bow
      |> add_path path
      |> finish)
  in
  orc
;;

let allocated_since_last =
  let prev = ref 0. in
  fun () ->
    let allocated_bytes = Gc.allocated_bytes () in
    let since_prev = allocated_bytes -. !prev in
    prev := allocated_bytes;
    since_prev
;;

let print_allocated ~repeat iter =
  let allocated = allocated_since_last () in
  Printf.printf "Allocated bytes: %#d\n" (Float.to_int allocated);
  let n = Int64.(to_float (mul (of_int repeat) iter)) in
  Printf.printf "Per iteration: %#d\n" (Float.to_int (allocated /. n))
;;

let repeat = 3
let read_iter = 10_000_000L
let write_iter = 2_000_000L

let () =
  Option.iter (Printf.printf "MEMTRACE: %s\n") (Sys.getenv_opt "MEMTRACE");
  Memtrace.trace_if_requested ();
  (* set up monster data and builder *)
  let b = Rt.Builder.create () in
  let buf = write_monster b |> Monster.finish_buf Flatbuffers.Primitives.Bigstring b in
  Printf.printf "Buffer size: %d\n" (Bigstringaf.length buf);
  let (Rt.Root (buf, m)) = Monster.root Flatbuffers.Primitives.Bigstring buf in
  (* benchmark read *)
  ignore (allocated_since_last ());
  let res = Benchmark.latencyN ~repeat read_iter [ "read", read_monster, (buf, m) ] in
  Benchmark.tabulate res;
  print_allocated ~repeat read_iter;
  (* benchmark write *)
  ignore (allocated_since_last ());
  let res = Benchmark.latencyN ~repeat write_iter [ "write", write_monster, b ] in
  Benchmark.tabulate res;
  print_allocated ~repeat write_iter
;;
