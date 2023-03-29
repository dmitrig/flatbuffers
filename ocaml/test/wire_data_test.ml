open Fixtures.Monster_test
open MyGame.Example

let () =
  let b = Rt.Builder.create () in
  Fixtures.create_example_monster b
  |> Monster.finish_buf Flatbuffers.Primitives.Bytes b
  |> Out_channel.output_bytes stdout
;;
