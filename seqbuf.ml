(* 

How do I make seqbufs threadsafe?
At least, use of seqbufs shouldn't segfault, even if I implement the ops unsafely.
Furthermore, we shouldn't see stale data. 

The difficult case is an exhausted buffer.
When a buffer is exhausted, I write 'dummy' to the location.

Wait, why must the buf be nulled out?
That is only of use if we hang on to stale buffers forever.
We can reuse them even though they're GC-live. That's fine.

So, buf is going to be immutable, which is nice.

OK, so now the memory-safety guarantee is that reads and writes are inside length,
which is fine. Woo!

*)
module B = Bigarray.Array1
type bigstring = (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) B.t

type seqbuf = {
  mutable pos : int;
  buf : bigstring;
  length : int
}
type builder = seqbuf
type buffer = seqbuf

exception BuilderFinished
exception Overflow
exception Underflow

let fail ty sb =
  if sb.pos == sb.length + 1 then
    raise BuilderFinished
  else match ty with
  | `Builder -> raise Overflow
  | `Buffer -> raise Underflow

let advance ty ({ pos; length; _ } as sb) n =
  if pos + n <= length then
    (sb.pos <- pos + n; pos)
  else
    fail ty sb

let finish bil =
  let { buf; pos; length } = bil in
  bil.pos <- bil.length + 1;
  { buf; pos = 0; length = pos }

let default_size = 1 lsl 16
let reuse buf = ()

let advance_buf ({ pos; length; _} as buf) n f =
  let pos' = pos + n in
  if pos' > length then
    raise Underflow;
  buf.pos <- pos';
  let r = f pos in
  if pos' == length then
    reuse buf;
  r

let create ?(min_size = 0) () =
  let size = if min_size < default_size then default_size else min_size in
  { buf = B.create Bigarray.Char Bigarray.C_layout size;
    pos = 0;
    length = size }

let remaining b =
  b.length - b.pos

let space_remaining = remaining
let bytes_remaining = remaining

external swap_16 : int -> int = "%bswap16"
external swap_32 : int32 -> int32 = "%bswap_int32"
external swap_64 : int64 -> int64 = "%bswap_int64"

let be_16 n = if Sys.big_endian then n else swap_16 n
let be_32 n = if Sys.big_endian then n else swap_32 n
let be_64 n = if Sys.big_endian then n else swap_64 n
let le_16 n = if Sys.big_endian then swap_16 n else n
let le_32 n = if Sys.big_endian then swap_32 n else n
let le_64 n = if Sys.big_endian then swap_64 n else n

external get_16 : bigstring -> int -> int = "%caml_bigstring_get16u"
external get_32 : bigstring -> int -> int32 = "%caml_bigstring_get32u"
external get_64 : bigstring -> int -> int64 = "%caml_bigstring_get64u"
external set_16 : bigstring -> int -> int -> unit = "%caml_bigstring_set16u"
external set_32 : bigstring -> int -> int32 -> unit = "%caml_bigstring_set32u"
external set_64 : bigstring -> int -> int64 -> unit = "%caml_bigstring_set64u"

(* Put operations *)

let put_char b c =
  let n = advance `Builder b 1 in
  B.set b.buf n c

let put_int8 b c =
  put_char b (Char.chr c)

let put_int16_nat b n =
  let p = advance `Builder b 2 in
  set_16 b.buf p n

let put_int32_nat b n =
  let p = advance `Builder b 4 in
  set_32 b.buf p n

let put_int64_nat b n =
  let p = advance `Builder b 8 in
  set_64 b.buf p n

let put_int16_le b n = put_int16_nat b (le_16 n)
let put_int32_le b n = put_int32_nat b (le_32 n)
let put_int64_le b n = put_int64_nat b (le_64 n)
let put_float32_le b f = put_int32_nat b (le_32 (Int32.bits_of_float f))
let put_float64_le b f = put_int64_nat b (le_64 (Int64.bits_of_float f))
let put_int16_be b n = put_int16_nat b (be_16 n)
let put_int32_be b n = put_int32_nat b (be_32 n)
let put_int64_be b n = put_int64_nat b (be_64 n)
let put_float32_be b f = put_int32_nat b (be_32 (Int32.bits_of_float f))
let put_float64_be b f = put_int64_nat b (be_64 (Int64.bits_of_float f))

let put_bytes_sub_checked b s off len =
  let p = advance `Builder b len in
  for i = 0 to len - 1 do
    B.set b.buf (p + i) (s.[off + i])
  done

let put_bytes b s =
  put_bytes_sub_checked b s 0 (String.length s)

let put_string b s =
  put_bytes_sub_checked b (Bytes.unsafe_of_string s) 0 (Bytes.length s)

let put_bytes_sub b s off len =
  if off < 0 || len < 0 || off > String.length s - len then
    invalid_arg "Seqbuf.put_string_sub / Seqbuf.put_bytes_sub"
  else
    put_bytes_sub_checked b s off len

let put_string_sub b s off len =
  put_bytes_sub b (Bytes.unsafe_of_string s) off len


(* Get operations *)

let get_char b =
  let p = advance `Buffer b 1 in
  B.get b.buf p

let get_int8 b = Char.code (get_char b)

let get_int16_nat b =
  let p = advance `Buffer b 2 in
  get_16 b.buf p

let get_int32_nat b =
  let p = advance `Buffer b 4 in
  get_32 b.buf p

let get_int64_nat b =
  let p = advance `Buffer b 8 in
  get_64 b.buf p

let get_int16_le b = get_int16_nat b |> le_16
let get_int32_le b = get_int32_nat b |> le_32
let get_int64_le b = get_int64_nat b |> le_64
let get_float32_le b = get_int32_nat b |> le_32 |> Int32.float_of_bits
let get_float64_le b = get_int64_nat b |> le_64 |> Int64.float_of_bits
let get_int16_be b = get_int16_nat b |> be_16
let get_int32_be b = get_int32_nat b |> be_32
let get_int64_be b = get_int64_nat b |> be_64
let get_float32_be b = get_int32_nat b |> be_32 |> Int32.float_of_bits
let get_float64_be b = get_int64_nat b |> be_64 |> Int64.float_of_bits

let get_string b n =
  let p = advance `Buffer b n in
  String.init n (fun i -> B.get b.buf (p+i))

let get_bytes b n =
  let p = advance `Buffer b n in
  Bytes.init n (fun i -> B.get b.buf (p+i))

let to_string b = get_string b (bytes_remaining b)
let to_bytes b = get_bytes b (bytes_remaining b)
let of_bytes s =
  let b = create ~min_size:(Bytes.length s * 2) () in
  put_string b s;
  b
let of_string s = of_bytes (Bytes.unsafe_of_string s)

let skip b n =
  let _ = advance `Buffer b n in
  ()

let discard b = skip b (bytes_remaining b)

let index_char b c =
  let rec search buf c i lim =
    if i < lim then
      if B.get buf i = c then
        Some i
      else
        search buf c (i+1) lim
    else
      None in
  search b.buf c b.pos b.length

let move dst src n =
  failwith "unimplemented"

let move_new src =
  failwith "unimplemented"

(* Raw *)

let put_raw ({ buf; pos; length } as bil) f =
  let produced = f buf ~off:pos ~len:(length - pos) in
  let newpos = pos + produced in
  if newpos >= length then failwith "put_raw produced more data than space available";
  bil.pos <- newpos;
  produced

let get_raw ({ buf; pos; length } as bf) f =
  let consumed = f buf ~off:pos ~len:(length - pos) in
  let newpos = pos + consumed in
  if newpos >= length then failwith "get_raw consumed more data than available";
  bf.pos <- newpos;
  consumed


