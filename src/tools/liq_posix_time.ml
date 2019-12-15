open Posix_time
open Liq_time

module Sys_time = struct
  type t = timespec

  let implementation = "native (high-precision)"
  let time () = clock_gettime `Monotonic

  let of_float d =
    {
      tv_sec = Int64.of_float d;
      tv_nsec = Int64.of_float ((d -. floor d) *. 1_000_000_000.);
    }

  let to_float { tv_sec; tv_nsec } =
    Int64.to_float tv_sec +. (Int64.to_float tv_nsec /. 1_000_000_000.)

  let normalize { tv_sec; tv_nsec } =
    {
      tv_sec = Int64.add tv_sec (Int64.div tv_nsec 1_000_000_000L);
      tv_nsec = Int64.rem tv_nsec 1_000_000_000L;
    }

  let apply fn x y =
    normalize
      { tv_sec = fn x.tv_sec y.tv_sec; tv_nsec = fn x.tv_nsec y.tv_nsec }

  let ( |+| ) = apply Int64.add
  let ( |-| ) = apply Int64.sub

  let ( |*| ) x y =
    normalize
      {
        tv_sec = Int64.mul x.tv_sec y.tv_sec;
        tv_nsec =
          Int64.add
            (Int64.add
               (Int64.mul x.tv_sec y.tv_nsec)
               (Int64.mul x.tv_nsec y.tv_sec))
            (Int64.div (Int64.mul x.tv_nsec y.tv_nsec) 1_000_000_000L);
      }

  let ( |<| ) x y =
    if Int64.equal x.tv_sec y.tv_sec then x.tv_nsec < y.tv_nsec
    else x.tv_sec < y.tv_sec

  let ( |<=| ) x y =
    if Int64.equal x.tv_sec y.tv_sec then x.tv_nsec <= y.tv_nsec
    else x.tv_sec <= y.tv_sec

  let sleep = nanosleep
end

let posix_time : (module T) = (module Sys_time)
let () = implementation := posix_time
