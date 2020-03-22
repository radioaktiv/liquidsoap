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

(** Muxing takes a main and an auxiliary source.
  * The auxiliary source streams only one kind of content,
  * the main has no channel of that kind, anything for the others.
  *
  * Both sources should be infallibles and track markers are taken
  * from both sources. *)

class mux ~kind ~mode ~main ~aux =
  let audio, video =
    match mode with `Add_audio -> (false, true) | `Add_video -> (true, false)
  in

  let main_source = Lang.to_source main in
  let () =
    if main_source#stype <> Source.Infallible then
      raise (Lang_errors.Invalid_value (main, "Main source cannot be fallible"))
  in

  let main_base =
    new Conversion.base
      ~audio ~video
      ~converter:(fun ~frame:_ _ -> ())
      main_source
  in

  let aux_source = Lang.to_source aux in
  let () =
    if aux_source#stype <> Source.Infallible then
      raise
        (Lang_errors.Invalid_value (aux, "Auxiliary source cannot be fallible"))
  in

  let () =
    if aux_source#self_sync && main_source#self_sync then
      raise
        (Lang_errors.Invalid_value
           (main, "Main and auxiliary sources cannot be both self-sync"))
  in

  let aux_base =
    new Conversion.base
      ~audio:(not audio) ~video:(not video)
      ~converter:(fun ~frame:_ _ -> ())
      aux_source
  in
  object (self)
    inherit Source.operator ~name:"mux" kind [main_source; aux_source]

    method self_sync = main_source#self_sync || aux_source#self_sync

    method stype = Source.Infallible

    method is_ready = main_source#is_ready && aux_source#is_ready

    method abort_track = main_source#abort_track

    method remaining = main_source#remaining

    (* 2 possible cases:
     * - [None]: both streams are in sync
     * - [Some data]: stream has pending data *)
    method private add_diff frame =
      function
      | None -> ()
      | Some (content, offset, length) ->
          Frame.blit_content content offset frame.Frame.content 0 length;
          Frame.add_break frame length

    val mutable main_frame = None

    val mutable main_diff = None

    method private main_frame frame =
      match main_frame with
        | Some f -> f
        | None ->
            let f = main_base#get_tmp_frame in
            main_base#copy_frame frame f;
            self#add_diff f main_diff;
            main_frame <- Some f;
            f

    val mutable aux_frame = None

    val mutable aux_diff = None

    method private aux_frame frame =
      match aux_frame with
        | Some f -> f
        | None ->
            let f = aux_base#get_tmp_frame in
            aux_base#copy_frame frame f;
            self#add_diff f aux_diff;
            aux_frame <- Some f;
            f

    method private get_frame frame =
      let position = Frame.position frame in
      let metadata = Frame.get_all_metadata frame in

      let main_frame = self#main_frame frame in

      if Frame.is_partial main_frame then main_source#get main_frame;

      let aux_frame = self#aux_frame frame in

      if Frame.is_partial aux_frame then aux_source#get aux_frame;

      main_base#copy_frame ~markers:false main_frame frame;
      aux_base#copy_frame ~markers:false aux_frame frame;

      (* Each filling operation should add exactly one break so we take
       * the earliest of both here and store the diff. *)
      let new_pos =
        match (Frame.position main_frame, Frame.position aux_frame) with
          | p, p' when p = p' ->
              aux_diff <- None;
              main_diff <- None;
              p
          | p, p' when p < p' ->
              aux_diff <- Some (aux_frame.Frame.content, p, p' - p);
              main_diff <- None;
              p
          | p, p' when p' < p ->
              aux_diff <- None;
              main_diff <- Some (aux_frame.Frame.content, p', p - p');
              p'
          | _ -> assert false
      in
      Frame.add_break frame new_pos;

      (* Set metadata from both frames. *)
      let new_metadata =
        List.filter
          (fun (x, _) -> position <= x)
          (List.sort
             (fun (x, _) (y, _) -> compare x y)
             ( Frame.get_all_metadata main_frame
             @ Frame.get_all_metadata aux_frame ))
      in
      Frame.set_all_metadata frame (new_metadata @ metadata)

    method advance =
      main_frame <- None;
      aux_frame <- None;
      self#advance
  end

let () =
  let out_t = Lang.kind_type_of_kind_format Lang.any in
  let { Frame.audio; video; midi } = Lang.of_frame_kind_t out_t in
  let main_t = Lang.frame_kind_t ~audio ~video:Lang.zero_t ~midi in
  let aux_t = Lang.frame_kind_t ~audio:Lang.zero_t ~video ~midi:Lang.zero_t in
  Lang.add_operator "mux_video" ~category:Lang.Conversions
    ~descr:
      "Add video channnels to a stream. Both sources need to be infallible. \
       Track marks and metadata are taken from both sources."
    ~kind:(Lang.Unconstrained out_t)
    [
      ("video", Lang.source_t aux_t, None, None);
      ("", Lang.source_t main_t, None, None);
    ]
    (fun p kind ->
      let main = List.assoc "" p in
      let aux = List.assoc "video" p in
      new mux ~kind ~mode:`Add_video ~main ~aux)

let () =
  let out_t = Lang.kind_type_of_kind_format Lang.any in
  let { Frame.audio; video; midi } = Lang.of_frame_kind_t out_t in
  let main_t = Lang.frame_kind_t ~audio:Lang.zero_t ~video ~midi in
  let aux_t = Lang.frame_kind_t ~audio ~video:Lang.zero_t ~midi:Lang.zero_t in
  Lang.add_operator "mux_audio" ~category:Lang.Conversions
    ~descr:
      "Mux an audio stream into an audio-free stream. Both sources need to be \
       infallible. Track marks and metadata are taken from both sources."
    ~kind:(Lang.Unconstrained out_t)
    [
      ("audio", Lang.source_t aux_t, None, None);
      ("", Lang.source_t main_t, None, None);
    ]
    (fun p kind ->
      let main = List.assoc "" p in
      let aux = List.assoc "audio" p in
      new mux ~kind ~mode:`Add_audio ~main ~aux)
