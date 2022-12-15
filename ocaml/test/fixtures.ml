module Monster_test = Generated.Monster_test.Make (Flatbuffers.BytesRuntime)
module Monster_extra = Generated.Monster_extra.Make (Flatbuffers.BytesRuntime)

let create_monster ~pos ~hp ~name ~inv ~name2 ~test4 ~strings ~longs ~doubles b =
  let open Monster_test in
  let open MyGame.Example in
  let string = Rt.String.create b name in
  let strings = Array.map (fun s -> Rt.String.create b s) strings in
  let name2 = Rt.String.create b name2 in
  let inv = Rt.UByte.Vector.create b inv in
  let mon2 = Monster.Builder.(start b |> add_name name2 |> finish) in
  let test4 = Test.Vector.create b test4 in
  let array_of_strings = Rt.String.Vector.create b strings in
  let vector_of_longs = Rt.Long.Vector.create b longs in
  let vector_of_doubles = Rt.Double.Vector.create b doubles in
  Monster.Builder.(
    start b
    |> add_pos pos
    |> add_hp hp
    |> add_name string
    |> add_inventory inv
    |> add_test_monster mon2
    |> add_test4 test4
    |> add_testarrayofstring array_of_strings
    |> add_vector_of_longs vector_of_longs
    |> add_vector_of_doubles vector_of_doubles
    |> finish)
;;

let create_example_monster =
  create_monster
    ~pos:(1.0, 2.0, 3.0, 3.0, Monster_test.MyGame.Example.Color.green, (5, 6))
    ~hp:80
    ~name:"MyMonster"
    ~inv:(Array.init 5 Char.chr)
    ~name2:"Fred"
    ~test4:[| 30, 40; 10, 20 |]
    ~strings:[| "test1"; "test2" |]
    ~longs:[| 1L; 100L; 10_000L; 1_000_000L; 100_000_000L |]
    ~doubles:[| -1.7976931348623157e+308; 0.; 1.7976931348623157e+308 |]
;;

let check_bytes_chunked ?(chunk_size = 16) b b' =
  Alcotest.(check int) "same length" (Bytes.length b) (Bytes.length b');
  let buf_len = Bytes.length b in
  for i = buf_len / chunk_size downto 0 do
    let start = i * chunk_size in
    let len = Int.min chunk_size (buf_len - start) in
    Alcotest.(check bytes)
      (Printf.sprintf "(%d) bytes %d:%d" i start (start + len - 1))
      (Bytes.sub b start len)
      (Bytes.sub b' start len)
  done
;;

let bigstring_map_file path =
  let fd = Unix.(openfile path [ O_RDONLY ] 0) in
  let ga = Unix.map_file fd Char C_layout false [| -1 |] in
  Bigarray.array1_of_genarray ga
;;

let bigstring_of_file fbfile =
  In_channel.with_open_bin fbfile (fun ic ->
    let s = In_channel.input_all ic in
    Bigstringaf.of_string ~off:0 ~len:(String.length s) s)
;;

let string_of_file fbfile =
  In_channel.with_open_bin fbfile (fun ic -> In_channel.input_all ic)
;;

let bytes_of_file fbfile =
  In_channel.with_open_bin fbfile (fun ic ->
    let s = In_channel.input_all ic in
    Bytes.of_string s)
;;
