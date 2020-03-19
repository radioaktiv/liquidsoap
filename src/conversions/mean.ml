(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2019 Savonet team

  This program is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details, fully stated in the COPYING
  file at the root of the liquidsoap distribution.

  You should have received a copy of the GNU General Public License
  along with this program; if not, write to the Free Software
  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301  USA

 *****************************************************************************)

open Source

class mean ~kind source =
  let base = new Conversion.base ~audio:true () in
  object
    inherit operator kind [source] ~name:"mean"

    method stype = source#stype

    method is_ready = source#is_ready

    method remaining = source#remaining

    method abort_track = source#abort_track

    method seek = source#seek

    method self_sync = source#self_sync

    method private get_frame frame =
      let tmp_frame = base#get_frame frame in
      base#copy_frame frame tmp_frame;
      let start = AFrame.position tmp_frame in
      source#get tmp_frame;
      let stop = AFrame.position tmp_frame in
      let content = AFrame.content tmp_frame in
      (* Compute the mean of audio channels *)
      Frame.set_audio frame
        [| Audio.to_mono (Audio.sub content start (stop - start)) |];
      (* Copy the rest of the frame's content. *)
      base#copy_frame tmp_frame frame
  end

let () =
  let in_kind = Lang.kind_type_of_kind_format Lang.any in
  let out_kind =
    let { Frame.audio = _; video = v; midi = m } =
      Lang.of_frame_kind_t in_kind
    in
    Lang.frame_kind_t ~audio:(Lang.succ_t Lang.zero_t) ~video:v ~midi:m
  in
  Lang.add_operator "mean"
    [("", Lang.source_t in_kind, None, None)]
    ~kind:(Lang.Unconstrained out_kind) ~category:Lang.Conversions
    ~descr:"Produce mono audio by taking the mean of all audio channels."
    (fun p kind ->
      let s = Lang.to_source (Lang.assoc "" 1 p) in
      (new mean ~kind s :> Source.source))
