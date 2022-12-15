(* see: fb_bench.cpp *)
open Generated.Bench.Make (Flatbuffers.BigstringRuntime)
open BenchmarksFlatbuffers

let encode b =
  Rt.Builder.reset b;
  let vec =
    Array.init 3 (fun i ->
      let foo =
        ( Int64.(add 0xABADCAFEABADCAFEL (of_int i))
        , 10000 + i
        , Char.(code '@' + i)
        , Int32.(add 1000000l (of_int i)) )
      in
      let bar =
        foo, Int32.(add 123456l (of_int i)), 3.14159 +. Float.of_int i, 10000 + i
      in
      let name = Rt.String.create b "Hello, world!" in
      FooBar.Builder.(
        start b
        |> add_sibling bar
        |> add_name name
        |> add_rating (3.1415432432445543543 +. Float.of_int i)
        |> add_postfix Char.(chr (code '!' + i))
        |> finish))
  in
  let location = Rt.String.create b "http://google.com/flatbuffers/" in
  let foobarvec = FooBar.Vector.create b vec in
  FooBarContainer.Builder.(
    start b
    |> add_list foobarvec
    |> add_initialized true
    |> add_fruit Enum.bananas
    |> add_location location
    |> finish)
;;

let use buf =
  let sum = ref 0 in
  let (Rt.Root (buf, fbc)) = FooBarContainer.root buf in
  sum := !sum + (FooBarContainer.initialized buf fbc |> Bool.to_int);
  sum := !sum + (FooBarContainer.location buf fbc |> Rt.Option.get |> Rt.String.length buf);
  sum := !sum + (FooBarContainer.fruit buf fbc :> int);
  let list = FooBarContainer.list buf fbc |> Rt.Option.get in
  for i = 0 to FooBar.Vector.length buf list - 1 do
    let fb = FooBar.Vector.get buf list i in
    sum := !sum + (FooBar.name buf fb |> Rt.Option.get |> Rt.String.length buf);
    sum := !sum + Char.code (FooBar.postfix buf fb);
    sum := !sum + Float.to_int (FooBar.rating buf fb);
    let b = FooBar.sibling buf fb |> Rt.Option.get in
    sum := !sum + Float.to_int (Bar.ratio buf b);
    sum := !sum + Bar.size buf b;
    sum := !sum + Int32.to_int (Bar.time buf b);
    let f = Bar.parent buf b in
    sum := !sum + Foo.count buf f;
    sum := !sum + Int64.to_int (Foo.id buf f);
    sum := !sum + Int32.to_int (Foo.length buf f);
    sum := !sum + Foo.prefix buf f
  done;
  !sum
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
  let buf = encode b |> FooBarContainer.finish_buf b in
  Printf.printf "Buffer size: %d\n" (Bigstringaf.length buf);
  (* check sum *)
  assert (use buf = 218812692406581874);
  (* benchmark read *)
  ignore (allocated_since_last ());
  let res = Benchmark.latencyN ~repeat read_iter [ "read", use, buf ] in
  Benchmark.tabulate res;
  print_allocated ~repeat read_iter;
  (* benchmark write *)
  ignore (allocated_since_last ());
  let res = Benchmark.latencyN ~repeat write_iter [ "write", encode, b ] in
  Benchmark.tabulate res;
  print_allocated ~repeat write_iter
;;
