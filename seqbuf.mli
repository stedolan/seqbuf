(** Efficient byte buffers, suitable for I/O.

    There are two types of buffer provided by [Seqbuf]: bytes are
    appended sequentially to [builder]s, and consumed sequentially
    from [buffer]s. Both [builder]s and [buffer]s are single-use: once
    a [builder] is full it cannot be reset, and once a [buffer] is
    empty it cannot be rewound. Internally, the memory of exhausted
    [buffer]s is efficiently reused. *)

(** {2 Builders} 

    A [builder] is an append-only sequence of bytes with a fixed
    capacity. Builders can only be written once: when one is filled,
    it cannot be reset.

    Builders are allocated with {!Seqbuf.create}, filled in with
    {!put_char} and friends, and finally turned into {!buffer}s by
    {!finish} *)

type builder

(** [create ()] creates a new builder, initially empty.

    A minimum capacity can be specified, if needed. [create ()] is a
    cheap operation, as space is internally reused from exhausted buffers. *)
val create : ?min_size:int -> unit -> builder

val space_remaining : builder -> int
(** [space_remaining b] is the number of bytes which can still be appended to
    [b] before subsequent put operations overflow. *)

(* FIXME: specify default min size in create? *)


(** If a put operation is given a builder with insufficient space,
    then [Overflow] is raised and the builder remains unchanged. *)
exception Overflow

(** The put operations below append a fixed-size datum to a builder,
    and several come in both big-endian and little-endian forms.  If
    the builder has insufficient space, they raise [Overflow] without
    appending any bytes *)

val put_char : builder -> char -> unit
val put_int8 : builder -> int -> unit
val put_int16_le : builder -> int -> unit
val put_int16_be : builder -> int -> unit
val put_int32_le : builder -> int32 -> unit
val put_int32_be : builder -> int32 -> unit
val put_int64_le : builder -> int64 -> unit
val put_int64_be : builder -> int64 -> unit
val put_float32_le : builder -> float -> unit
val put_float32_be : builder -> float -> unit
val put_float64_le : builder -> float -> unit
val put_float64_be : builder -> float -> unit
val put_string : builder -> string -> unit
val put_bytes : builder -> bytes -> unit

val put_string_sub : builder -> string -> int -> int -> unit
(** [put_string_sub b s off len] is [put_string b (String.sub s off len)], but faster *)

val put_bytes_sub : builder -> string -> int -> int -> unit
(** [put_bytes_sub b s off len] is [put_bytes b (Bytes.sub s off len)], but faster *)


(** {2 Buffers}

    A [buffer] is a read-only sequence of bytes, which are consumed
    sequentially. Buffers cannot be rewound: bytes may be consumed
    from a buffer only once. (In fact, as soon as a buffer is
    exhausted its backing memory is reused).

    Buffers are obtained by {!finish}-ing a [builder], or directly
    using {!of_string} or {!of_bytes}. Their contents are consumed
    using {!get_char} and friends. *)

type buffer

exception BuilderFinished
val finish : builder -> buffer
(** [finish b] is a buffer containing the contents of [b]. Once
    [finish b] has been called, [b] may no longer be appended to:
    subsequent [put] operations raise [BuilderFinished] *)

val of_string : string -> buffer
val of_bytes : bytes -> buffer

val bytes_remaining : buffer -> int
(** [bytes_remaining b] is the number of bytes which can still be
    consumed from [b] before subsequent get operations underflow. *)

(** If a get operations is given a buffer with too few bytes
    remaining, then [Underflow] is raised and no bytes are consumed. *)
exception Underflow

(** The get operations below consume a fixed number of bytes from a
    buffer, and several come in both big-endian and little-endian
    forms. If the buffer has too few bytes, they raise [Underflow]
    without consuming any bytes. *)

val get_char : buffer -> char
val get_int8 : buffer -> int
val get_int16_le : buffer -> int
val get_int16_be : buffer -> int
val get_int32_le : buffer -> int32
val get_int32_be : buffer -> int32
val get_int64_le : buffer -> int64
val get_int64_be : buffer -> int64
val get_float32_le : buffer -> float
val get_float32_be : buffer -> float
val get_float64_le : buffer -> float
val get_float64_be : buffer -> float

val get_string : buffer -> int -> string
(** [get_string b n] consumes and returns n bytes of b. Never returns
    a shorter string, but may raise [Underflow]. *)

val get_bytes : buffer -> int -> bytes
(** [get_bytes b n] consumes and returns n bytes of b. Never returns a
    shorter result, but may raise [Underflow]. *)

val to_string : buffer -> string
(** Consumes and returns the entire buffer. Cannot underflow. *)

val to_bytes : buffer -> bytes
(** Consumes and returns the entire buffer. Cannot underflow. *)

val skip : buffer -> int -> unit
(** [skip b n] consumes and ignores [n] bytes of [b]. Raises [Underflow]
    if [b] has less than [n] bytes remaining *)

val discard : buffer -> unit
(** [discard b] consumes and ignores all bytes of [b], leaving it empty. Cannot underflow *)

val index_char : buffer -> char -> int option
(** [index_char b c] returns the first index of the character [c] in
    the buffer, or [None] if it is not present. This function consumes no
    bytes and cannot underflow *)
  
(*
val move : builder -> buffer -> int -> unit
(** [move a b n] consumes [n] bytes from [b] and appends them to
    [a]. It may raise either [Underflow] or [Overflow], if [b] is too
    empty or [a] is too full. (It may also raise [BuilderFinished] if
    [finish a] has already occurred) *)

val move_new : buffer -> builder
(** [move_new b] returns a new builder which already contains the remaining contents of b *)
*)

(** {2 Raw Bigstring access} *)

type bigstring = (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

val put_raw : builder -> (bigstring -> off:int -> len:int -> int) -> int
(** [put_raw b f] calls [f] to store some bytes into the [builder]'s
    internal [bigstring]. [f] must not write outside the specified
    offset and length, and must return the number of bytes it inserts. *)

val get_raw : buffer -> (bigstring -> off:int -> len:int -> int) -> int
(** [get_raw b f] calls [f] to consume some bytes from the [buffer]'s
    internal [bigstring]. [f] must not read outside the specified
    offset and length (nor mutate it), and must return the number of
    bytes it consumes. *)


(*


(** {2 I/O} *)

type file_descr

val read_fd : file_descr -> buffer option
(** [read_fd fd] either returns a buffer read from [fd], or [None] if
    [fd] is at end-of-file, or raises [Sys_error] *)

val write_fd : buffer -> file_descr -> unit
(** [write_fd b fd] consumes bytes from [b] and writes them to [fd],
    raising [Sys_error] if an I/O error occurs. If no I/O error
    occurs, then all of [b] is consumed. *)

val raw_read_fd : builder -> file_descr -> int
(** [raw_read_fd b fd] appends some bytes read from [fd] to [b]. The
    builder may or may not be full afterwards, and the return value is
    the number of bytes read and appended to [b]. If an error occurs,
    [raw_read_fd] raises [Sys_error] and appends no bytes to [b]. If
    end-of-file is reached, [raw_read_fd] returns 0. *)

val raw_write_fd : buffer -> file_descr -> int
(** [raw_write_fd b fd] consumes some bytes from [b] and writes them
    to [fd]. The buffer may or may not be empty afterwards, and the
    return value is the number of bytes consumed and written to
    [b]. If an error occurs, [raw_write_fd] raises [Sys_error] and
    consumes no bytes from [b]. *)

*)
