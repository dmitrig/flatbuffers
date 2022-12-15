let check_monsterdata ?(size_prefixed = false) buf =
  let open Fixtures.Monster_test in
  let open MyGame.Example in
  let open Rt in
  Alcotest.(check bool) "has_ident" true (Monster.has_identifier ~size_prefixed buf);
  let (Root (buf, m)) = Monster.root ~size_prefixed buf in
  Alcotest.(check int) "hp" 80 (Monster.hp buf m);
  Alcotest.(check int) "mana" 150 (Monster.mana buf m);
  Alcotest.(check string) "name" "MyMonster" (Monster.name buf m |> String.to_string buf);
  Alcotest.(check bool) "color" true (Monster.color buf m == Color.blue);
  let pos = Monster.pos buf m in
  Alcotest.(check bool) "pos is some" true (Monster.pos buf m |> Option.is_some);
  let pos = Option.get pos in
  Alcotest.(check (float 0.)) "pos.x" 1.0 (Vec3.x buf pos);
  Alcotest.(check (float 0.)) "pos.y" 2.0 (Vec3.y buf pos);
  Alcotest.(check (float 0.)) "pos.z" 3.0 (Vec3.z buf pos);
  Alcotest.(check (float 0.)) "pos.test1" 3.0 (Vec3.test1 buf pos);
  Alcotest.(check char) "pos.test2" (Color.green :> char) (Vec3.test2 buf pos :> char);
  let test3 = Vec3.test3 buf pos in
  Alcotest.(check int) "pos.test3.a" 5 (Test.a buf test3);
  Alcotest.(check int) "pos.test3.b" 6 (Test.b buf test3);
  let test_type = Monster.test_type buf m in
  Alcotest.(check char) "test_type" (Any.monster :> char) (test_type :> char);
  let monster2 =
    Monster.test buf m ~monster:Fun.id ~default:(fun _ -> failwith "unexpected union")
  in
  Alcotest.(check string)
    "test.name"
    "Fred"
    (Monster.name buf monster2 |> String.to_string buf);
  let inventory = Monster.inventory buf m in
  Alcotest.(check bool) "inventory is some" true (Option.is_some inventory);
  let inventory = Option.get inventory in
  Alcotest.(check (array char))
    "inventory"
    [| '\x00'; '\x01'; '\x02'; '\x03'; '\x04' |]
    (UByte.Vector.to_array buf inventory);
  let vec_of_doubles = Monster.vector_of_doubles buf m in
  Alcotest.(check bool) "vector_of_doubles is some" true (Option.is_some vec_of_doubles);
  let vec_of_doubles = Option.get vec_of_doubles in
  Alcotest.(check (array (float 0.)))
    "vector_of_doubles"
    [| -1.7976931348623157e+308; 0.; 1.7976931348623157e+308 |]
    (Double.Vector.to_array buf vec_of_doubles);
  let test4 = Monster.test4 buf m in
  Alcotest.(check bool) "test4 is some" true (Option.is_some test4);
  let test4 = Option.get test4 in
  Alcotest.(check int) "test4 length" 2 (Test.Vector.length buf test4);
  (* don't check order since cpp vs. python example data write these differently *)
  Alcotest.(check int)
    "test4 sum"
    100
    (Test.Vector.to_seq buf test4
    |> Seq.fold_left (fun a t -> a + Test.a buf t + Test.b buf t) 0);
  let aos = Monster.testarrayofstring buf m in
  Alcotest.(check bool) "testarrayofstring is some" true (Option.is_some aos);
  Alcotest.(check (list string))
    "testarrayofstring"
    [ "test1"; "test2" ]
    (Option.get aos |> String.Vector.to_list buf |> List.map (String.to_string buf));
  let aot = Monster.testarrayoftables buf m in
  Alcotest.(check bool) "testarrayoftables is none" true (Option.is_none aot)
;;

let enum_name () =
  let open Fixtures.Monster_test in
  let open MyGame.Example in
  Alcotest.(check string) "Color.to_string" "red" Color.(to_string red);
  Alcotest.(check string) "Color.to_string" "blue" Color.(to_string blue);
  Alcotest.(check string) "Color.to_string" "green" Color.(to_string green)
;;

let check_serialized path () = check_monsterdata (Fixtures.bytes_of_file path)

let check_generated ?(size_prefixed = false) () =
  let open Fixtures.Monster_test in
  let open MyGame.Example in
  let b = Rt.Builder.create () in
  let buf = Fixtures.create_example_monster b |> Monster.finish_buf ~size_prefixed b in
  check_monsterdata ~size_prefixed buf
;;

let check_generated_matches ?(size_prefixed = false) path () =
  let open Fixtures.Monster_test in
  let open MyGame.Example in
  let b = Rt.Builder.create () in
  let buf = Fixtures.create_example_monster b |> Monster.finish_buf ~size_prefixed b in
  let buf' = Fixtures.bytes_of_file path in
  Fixtures.check_bytes_chunked buf' buf
;;

let check_generated_reset () =
  let open Fixtures.Monster_test in
  let open MyGame.Example in
  let b = Rt.Builder.create () in
  (* build first example *)
  let buf = Fixtures.create_example_monster b |> Monster.finish_buf b in
  Rt.Builder.reset b;
  (* write another monster *)
  Fixtures.create_monster
    b
    ~pos:(0., 0., 0., 0., MyGame.Example.Color.blue, (0, 0))
    ~hp:42
    ~name:"AnotherMonster"
    ~inv:(Array.make 100 '\xff')
    ~name2:"ChildMonsterName"
    ~test4:[||]
    ~strings:[||]
    ~longs:[||]
    ~doubles:[||]
  |> ignore;
  Rt.Builder.reset b;
  (* write second example *)
  Fixtures.create_example_monster b
  |> Monster.finish_buf b
  |> Fixtures.check_bytes_chunked buf
;;

let check_defaults_not_written () =
  let open Fixtures.Monster_test in
  let open MyGame.Example in
  let b = Rt.Builder.create () in
  let buf = Monster.Builder.(start b |> finish) |> Monster.finish_buf b in
  let buf' =
    Monster.Builder.(
      start b
      |> add_hp 100
      |> add_mana 150
      |> add_color Color.blue
      |> add_testf2 3.
      |> add_testhashs32_fnv1 0l
      |> add_testhashu64_fnv1 0L
      |> add_inf_default infinity
      |> add_nan_default nan
      |> finish)
    |> Monster.finish_buf b
  in
  Alcotest.(check bytes) "buffers are identical" buf buf'
;;

let check_default_layout ?(size_prefixed = false) () =
  let open Fixtures.Monster_test in
  (* 0 of 62 fields populated *)
  let n_fields = 0 in
  let vtable_size =
    (n_fields * 2) (* vtable field offsets *)
    + 2 (* uint16 vtable size *)
    + 2 (* uint16 inline size *)
  in
  (* just soffset to vtable *)
  let monster_inline_size = 4 in
  (* initial offset to monster table plus identifier *)
  let fb_header_size = 8 in
  (* don't need to pad for initial offset alignment *)
  Alcotest.(check int) "no padding" 0 ((monster_inline_size + vtable_size) mod 4);
  let monster_size = fb_header_size + vtable_size + monster_inline_size in
  let buffer_size = monster_size + if size_prefixed then 4 else 0 in
  (* manually construct a default monster buffer *)
  let layout = Bytes.make buffer_size '\000' in
  let monster_table_start = buffer_size - monster_inline_size in
  Bytes.set_int32_le layout monster_table_start (Int32.of_int vtable_size);
  let vtable_start = monster_table_start - vtable_size in
  Bytes.set_uint16_le layout vtable_start vtable_size;
  Bytes.set_uint16_le layout (vtable_start + 2) monster_inline_size;
  (* construct header *)
  let buf_start = if size_prefixed then 4 else 0 in
  let monster_offset = monster_size - monster_inline_size in
  Bytes.set_int32_le layout buf_start (Int32.of_int monster_offset);
  Bytes.blit_string "MONS" 0 layout (buf_start + 4) 4;
  if size_prefixed then Bytes.set_int32_le layout 0 (Int32.of_int monster_size);
  (* check against builder output *)
  let b = Rt.Builder.create () in
  let buf =
    MyGame.Example.Monster.(Builder.(start b |> finish) |> finish_buf ~size_prefixed b)
  in
  Alcotest.(check int) "buffer size" (Bytes.length layout) (Bytes.length buf);
  Alcotest.(check bytes) "buffer layout" layout buf
;;

let check_monster_extra_floats () =
  let open Fixtures.Monster_extra in
  let open MyGame in
  let b = Rt.Builder.create () in
  let buf = MonsterExtra.(Builder.(start b |> finish) |> finish_buf b) in
  let (Rt.Root (buf, mon)) = MonsterExtra.root buf in
  Alcotest.(check bool) "is_nan d0" true (Float.is_nan (MonsterExtra.d0 buf mon));
  Alcotest.(check bool) "is_nan d1" true (Float.is_nan (MonsterExtra.d1 buf mon));
  Alcotest.(check (float 0.)) "d2" Float.infinity (MonsterExtra.d2 buf mon);
  Alcotest.(check (float 0.)) "d3" Float.neg_infinity (MonsterExtra.d3 buf mon);
  Alcotest.(check bool) "is_nan d0" true (Float.is_nan (MonsterExtra.f0 buf mon));
  Alcotest.(check bool) "is_nan d1" true (Float.is_nan (MonsterExtra.f1 buf mon));
  Alcotest.(check (float 0.)) "d2" Float.infinity (MonsterExtra.f2 buf mon);
  Alcotest.(check (float 0.)) "d3" Float.neg_infinity (MonsterExtra.f3 buf mon)
;;

let check_extension_ident () =
  let open Fixtures.Monster_test in
  let open MyGame.Example in
  Alcotest.(check (option string)) "extension" (Some "mon") Monster.extension;
  Alcotest.(check (option string)) "identifier" (Some "MONS") Monster.identifier
;;

let gold_monsterdata = "../../tests/monsterdata_test.mon"
let python_monsterdata = "../../tests/monsterdata_python_wire.mon"

let test_cases =
  Alcotest.
    [ test_case "Read gold monsterdata" `Quick (check_serialized gold_monsterdata)
    ; test_case "Read python monsterdata" `Quick (check_serialized python_monsterdata)
    ; test_case "Read generated monster" `Quick check_generated
    ; test_case "Read gen size-prefixed" `Quick (check_generated ~size_prefixed:true)
    ; test_case "Check nan and inf float defaults" `Quick check_monster_extra_floats
    ; test_case "Reusing builder produces same output" `Quick check_generated_reset
    ; test_case "Check default scalars not written" `Quick check_defaults_not_written
    ; test_case "Check default monster layout" `Quick check_default_layout
    ; test_case
        "Check size-prefixed layout"
        `Quick
        (check_default_layout ~size_prefixed:true)
    ; test_case
        "Check gen matches python"
        `Quick
        (check_generated_matches python_monsterdata)
    ; test_case "Check enum names" `Quick enum_name
    ]
;;
