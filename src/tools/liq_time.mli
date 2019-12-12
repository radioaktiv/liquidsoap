
module type T = sig
  type t
  val implementation : string
  val gettimeofday: unit -> t
  val of_int64: int64 -> t
  val usleep: t -> unit
  val to_float: t -> float
  val ( |+| ) : t -> t -> t
  val ( |-| ) : t -> t -> t
  val ( |*| ) : t -> t -> t
  val ( |<| ) : t -> t -> bool
end

val implementation: (module T) ref
