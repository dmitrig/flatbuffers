module Builder = struct
  (* flatbuffers constructed back-to-front. Offsets are relative to the end of the buffer *)
  type offset = int

  (* dynamic array of offsets, sorted by a compare function. Used to dedupe vtables *)
  module IndCache = struct
    type t =
      { mutable buf : offset array
      ; mutable length : int
      ; compare : offset -> offset -> int
      }

    let make n compare = { buf = Array.make n (-1); length = 0; compare }

    let grow s =
      let new_len = Array.length s.buf * 2 in
      let new_buf = Array.make new_len 0 in
      Array.blit s.buf 0 new_buf 0 s.length;
      s.buf <- new_buf
    ;;

    let find s x =
      let rec loop s x lo hi =
        if hi <= lo
        then -(lo + 1)
        else (
          let mid = (lo + hi) / 2 in
          let cmp = s.compare x s.buf.(mid) in
          if cmp == 0
          then mid
          else if cmp < 0
          then loop s x lo mid
          else loop s x (mid + 1) hi)
      in
      loop s x 0 s.length
    ;;

    let insert s i x =
      if s.length == Array.length s.buf then grow s;
      Array.blit s.buf i s.buf (i + 1) (s.length - i);
      s.buf.(i) <- x;
      s.length <- s.length + 1
    ;;

    let find_or_insert s x =
      let ind = find s x in
      if ind < 0
      then (
        insert s (-ind - 1) x;
        x)
      else s.buf.(ind)
    ;;

    let reset s = s.length <- 0
  end

  let compare_vtable_offsets b o p =
    let b = !b in
    let o = Bytes.length b - o in
    let p = Bytes.length b - p in
    let leno = Bytes.get_int16_le b o in
    let lenp = Bytes.get_int16_le b p in
    let cmp = Int.compare leno lenp in
    if cmp != 0
    then cmp
    else (
      let cmp = ref 0 in
      let i = ref 2 in
      while !i < leno && !cmp == 0 do
        cmp := Char.compare (Bytes.get b (o + !i)) (Bytes.get b (p + !i));
        incr i
      done;
      !cmp)
  ;;

  type t =
    { buf : bytes ref
    ; mutable length : int
    ; mutable cur_vtable : int array
    ; mutable cur_vtable_len : int
    ; mutable minalign : int
    ; mutable nested_start : int (* offset of table/vector start *)
    ; strings : (string, int) Hashtbl.t
    ; vtables : IndCache.t
    }

  let create ?(init_capacity = 1024) () =
    let buf = ref (Bytes.create (Int.max init_capacity 16)) in
    { buf
    ; length = 0
    ; cur_vtable = [||]
    ; cur_vtable_len = 0
    ; minalign = 1
    ; nested_start = -1
    ; strings = Hashtbl.create 0
    ; vtables = IndCache.make 16 (compare_vtable_offsets buf)
    }
  ;;

  (* TODO: option to shrink buffer? *)
  let reset b =
    b.length <- 0;
    b.cur_vtable_len <- 0;
    b.minalign <- 1;
    b.nested_start <- -1;
    Hashtbl.reset b.strings;
    IndCache.reset b.vtables
  ;;

  let assert_nested b t = assert (t == (b.nested_start != -1))

  let set_nested b t =
    assert_nested b (not t);
    if t then b.nested_start <- b.length else b.nested_start <- -1
  ;;

  let ensure_capacity b n =
    let old_len = Bytes.length !(b.buf) in
    if old_len < n
    then (
      let new_len = ref old_len in
      while !new_len < n do
        new_len := 2 * !new_len
      done;
      let buf' = Bytes.extend !(b.buf) (!new_len - old_len) 0 in
      b.buf := buf')
  ;;

  (* Current index (for writing into buf) *)
  let current b = Bytes.length !(b.buf) - b.length

  (* current offset (index from end of buf) *)
  let current_offset b = b.length

  (* Add padding so that, after writing [additional_bytes], the buffer size is a
     multiple of [align]. Ensures space for [additional_bytes] more bytes *)
  let prealign b ?(additional_bytes = 0) align =
    b.minalign <- Int.max align b.minalign;
    let pad_bytes = -(b.length + additional_bytes) land (align - 1) in
    ensure_capacity b (b.length + additional_bytes + pad_bytes);
    if pad_bytes != 0
    then (
      b.length <- b.length + pad_bytes;
      Bytes.fill !(b.buf) (current b) pad_bytes '\x00')
  ;;

  (* TODO: probably more useful api than prealign, replace? *)
  let prep ~align ~bytes b =
    prealign b align ~additional_bytes:bytes;
    b.length <- b.length + bytes
  ;;

  (* TODO: could check id vs n_fields from table start *)
  let save_slot ~id b =
    assert_nested b true;
    if id >= b.cur_vtable_len then b.cur_vtable_len <- id + 1;
    b.cur_vtable.(id) <- b.length
  ;;

  let set_bool b i x = Primitives.set_bool !(b.buf) (current b + i) x
  let set_byte b i x = Primitives.set_byte !(b.buf) (current b + i) x
  let set_ubyte b i x = Primitives.set_ubyte !(b.buf) (current b + i) x
  let set_short b i x = Primitives.set_short !(b.buf) (current b + i) x
  let set_ushort b i x = Primitives.set_ushort !(b.buf) (current b + i) x
  let set_int b i x = Primitives.set_int !(b.buf) (current b + i) x
  let set_uint b i x = Primitives.set_uint !(b.buf) (current b + i) x
  let set_long b i x = Primitives.set_long !(b.buf) (current b + i) x
  let set_ulong b i x = Primitives.set_ulong !(b.buf) (current b + i) x
  let set_float b i x = Primitives.set_float !(b.buf) (current b + i) x
  let set_double b i x = Primitives.set_double !(b.buf) (current b + i) x
  let set_string b i s = Bytes.blit_string s 0 !(b.buf) (current b + i) (String.length s)
  let set_padding b i n = Bytes.fill !(b.buf) (current b + i) n '\000'

  (* convert offset to relative *)
  let set_uoffset b i o =
    let i' = current b + i in
    let b' = !(b.buf) in
    Bytes.set_int32_le b' i' (Int32.of_int (Bytes.length b' - o - i'))
  ;;

  let find_shared_string b s = Hashtbl.find_opt b.strings s
  let add_shared_string b s o = Hashtbl.add b.strings s o

  (* size of vector length field *)
  let vector_len_size = 4

  let start_vector b ~n_elts ~elt_size =
    prep b ~align:(Int.max vector_len_size elt_size) ~bytes:(n_elts * elt_size);
    (* TODO: hack? *)
    ensure_capacity b vector_len_size;
    Bytes.set_int32_le !(b.buf) (current b - vector_len_size) (Int32.of_int n_elts);
    (* set_uint b (-vector_len_size) (Int32.of_int n_elts); *)
    set_nested b true
  ;;

  let end_vector b =
    (* TODO: check we haven't moved since start_vector. Does this make sense? *)
    (* TODO: rename? or separate bit of state for checking n_elts *)
    assert (b.nested_start = b.length);
    set_nested b false;
    (* skip over size added in start_vector *)
    b.length <- b.length + vector_len_size;
    current_offset b
  ;;

  let start_table b ~n_fields =
    if Array.length b.cur_vtable < n_fields then b.cur_vtable <- Array.make n_fields 0;
    b.cur_vtable_len <- 0;
    Array.fill b.cur_vtable 0 n_fields 0;
    set_nested b true;
    b
  ;;

  let create_vtable b =
    assert_nested b true;
    let table_offset = b.length in
    let table_len = table_offset - b.nested_start in
    let vtable_len = 2 * (2 + b.cur_vtable_len) in
    prealign b 2 ~additional_bytes:vtable_len;
    b.length <- b.length + vtable_len;
    let ind = current b in
    let buf = !(b.buf) in
    Bytes.set_int16_le buf ind vtable_len;
    Bytes.set_int16_le buf (ind + 2) table_len;
    for i = 0 to b.cur_vtable_len - 1 do
      let wip_offset = b.cur_vtable.(i) in
      let real_offset = if wip_offset = 0 then 0 else table_offset - wip_offset in
      Bytes.set_int16_le buf (ind + 4 + (i * 2)) real_offset
    done;
    b.length
  ;;

  let end_table b =
    (* add vtable (signed) offset, to be patched later *)
    prep ~align:4 ~bytes:4 b;
    let table_offset = b.length in
    (* serialize vtable *)
    let vt_offset' = create_vtable b in
    (* reuse an existing vtable if found, removing extra vtable *)
    let vt_offset = IndCache.find_or_insert b.vtables vt_offset' in
    if vt_offset != vt_offset' then b.length <- table_offset;
    (* patch the *backwards* offset from table to vtable *)
    Bytes.set_int32_le
      !(b.buf)
      (Bytes.length !(b.buf) - table_offset)
      (Int32.of_int (vt_offset - table_offset));
    b.cur_vtable_len <- 0;
    set_nested b false;
    table_offset
  ;;

  let finish ?identifier ?(size_prefixed = false) b o =
    assert_nested b false;
    let ident_length = Option.fold identifier ~none:0 ~some:String.length in
    let offset_length = 4 in
    let prefix_length = if size_prefixed then 4 else 0 in
    let header_size = prefix_length + offset_length + ident_length in
    prep ~align:(Int.max 4 b.minalign) ~bytes:header_size b;
    (match identifier with
     | None -> ()
     | Some s -> set_string b (prefix_length + offset_length) s);
    set_uoffset b prefix_length o;
    if size_prefixed
    then Bytes.set_int32_le !(b.buf) (current b) (Int32.of_int (b.length - prefix_length));
    let res = Primitives.buf_of_bytes !(b.buf) ~off:(current b) ~len:b.length in
    reset b;
    res
  ;;
end
