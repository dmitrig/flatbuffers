(* see: samples/sample_binary.cpp *)
open Generated.Monster
open MyGame.Sample

let main () =
  let b = Rt.Builder.create () in
  let sword_name = Rt.String.create b "Sword" in
  let axe_name = Rt.String.create b "Axe" in
  let sword = Weapon.Builder.(start b |> add_name sword_name |> add_damage 3 |> finish) in
  let axe = Weapon.Builder.(start b |> add_name axe_name |> add_damage 5 |> finish) in
  let weapons = Weapon.Vector.create b [| sword; axe |] in
  let name = Rt.String.create b "MyMonster" in
  let inventory = Rt.UByte.Vector.create b (Array.init 10 Char.chr) in
  let orc =
    Monster.Builder.(
      start b
      |> add_pos (1., 2., 3.)
      |> add_mana 150
      |> add_hp 80
      |> add_name name
      |> add_inventory inventory
      |> add_color Color.red
      |> add_weapons weapons
      |> add_equipped_weapon axe
      |> finish)
  in
  let buf = Monster.finish_buf Flatbuffers.Primitives.String b orc in
  (* read back serialized data *)
  let (Rt.Root (b, monster)) = Monster.root Flatbuffers.Primitives.String buf in
  assert (Monster.hp b monster = 80);
  assert (Monster.mana b monster = 150);
  assert (Monster.name b monster |> Rt.Option.get |> Rt.String.to_string b = "MyMonster");
  (* position *)
  assert (Monster.pos b monster |> Rt.Option.is_some);
  let pos = Monster.pos b monster |> Rt.Option.get in
  assert ((Vec3.x b pos, Vec3.y b pos, Vec3.z b pos) = (1., 2., 3.));
  (* inventory *)
  let inv = Monster.inventory b monster in
  assert (Rt.Option.is_some inv);
  assert (Rt.Option.get inv |> Rt.UByte.Vector.to_array b = Array.init 10 Char.chr);
  (* weapons *)
  assert (Monster.weapons b monster |> Rt.Option.is_some);
  let weapons = Monster.weapons b monster |> Rt.Option.get in
  assert (Weapon.Vector.length b weapons = 2);
  let sword = Weapon.Vector.get b weapons 0 in
  assert (Weapon.name b sword |> Rt.Option.get |> Rt.String.to_string b = "Sword");
  assert (Weapon.damage b sword = 3);
  let axe = Weapon.Vector.get b weapons 1 in
  assert (Weapon.name b axe |> Rt.Option.get |> Rt.String.to_string b = "Axe");
  assert (Weapon.damage b axe = 5);
  (* eqipment *)
  assert (Monster.equipped_type b monster = Equipment.weapon);
  let equipped =
    Monster.equipped b monster ~weapon:Fun.id ~default:(fun e ->
      failwith @@ "Unexpected union member: " ^ Equipment.to_string e)
  in
  assert (Weapon.name b equipped |> Rt.Option.get |> Rt.String.to_string b = "Axe");
  assert (Weapon.damage b equipped = 5);
  Printf.printf "The FlatBuffer was successfully created and verified!\n"
;;

main ()
