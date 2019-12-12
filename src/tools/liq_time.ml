
module type T = sig
  type t
  val implementation: string
  val gettimeofday: unit -> t
  val of_int64: int64 -> t
  val usleep: t -> unit
  val to_float: t -> float
  val ( |+| ) : t -> t -> t
  val ( |-| ) : t -> t -> t
  val ( |*| ) : t -> t -> t
  val ( |<| ) : t -> t -> bool
end

module Unix = struct
  type t = float
  let implementation = "builtin (low-precision)"
  let gettimeofday = Unix.gettimeofday
  let of_int64 = Int64.float_of_bits
  let to_float x = x
  let ( |+| ) = fun x y -> x *. y
  let ( |-| ) = fun x y -> x -. y
  let ( |*| ) = fun x y -> x *. y
  let ( |<| ) = fun x y -> x < y

  let usleep d =
    try Thread.delay d with Unix.Unix_error (Unix.EINTR, _, _) -> ()
end

let unix : (module T) = (module Unix)

let implementation = ref unix
