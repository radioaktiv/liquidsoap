open Sys_time
open Liq_time

module Sys_time = struct
  type t = timeval

  let implementation = "native (high-precision)"

  let gettimeofday = gettimeofday

  let of_float d = {
    tv_sec  = Int64.of_float d;
    tv_usec = Int64.of_float ((d -. floor d) *. 1_000_000.)
  }

  let to_float {tv_sec; tv_usec} =
    Int64.to_float tv_sec +.
      (Int64.to_float tv_usec /. 1_000_000.)

  let normalize {tv_sec;tv_usec} =
    { tv_sec = Int64.add tv_sec (Int64.div tv_usec 1_000_000L);
      tv_usec = Int64.rem tv_usec 1_000_000L }

  let apply fn x y = normalize {
    tv_sec = fn x.tv_sec y.tv_sec;
    tv_usec = fn x.tv_usec y.tv_usec
  }

  let ( |+| ) = apply Int64.add
  let ( |-| ) = apply Int64.sub
  let ( |*| ) = fun x y -> normalize {
    tv_sec  = Int64.mul x.tv_sec y.tv_sec;
    tv_usec =
      Int64.add
        (Int64.add
          (Int64.mul x.tv_sec y.tv_usec)
          (Int64.mul x.tv_usec y.tv_sec))
        (Int64.div
          (Int64.mul x.tv_usec y.tv_usec)
          1_000_000L)
  }

  let ( |<| ) = fun x y ->
    if Int64.equal x.tv_sec y.tv_sec then
      x.tv_usec < y.tv_usec
    else
      x.tv_sec < y.tv_sec

  let usleep d =
    try ignore(select [] [] [] d) with Unix.Unix_error (Unix.EINTR, _, _) -> ()
end

let sys_time : (module T) = (module Sys_time)

let () =
  implementation := sys_time
