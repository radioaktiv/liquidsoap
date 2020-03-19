(* Conversion base class. contents marked as [true] are modified
 * by the converter using this class. Other contents are untouched. *)
class base ?(audio = false) ?(video = false) ?(midi = false) () =
  object (self)
    (* The tmp_frame is intended to be filled by the underlying
     * source. Content untouched by the converter are replaced by
     * by content from the calling frame. Touched content get their
     * own layer. *)
    val mutable tmp_frame = None

    method private content_type frame =
      let content_type = Frame.content_type frame in
      {
        Frame.audio = (if audio then content_type.Frame.audio else 0);
        video = (if video then content_type.Frame.video else 0);
        midi = (if midi then content_type.Frame.midi else 0);
      }

    method get_frame frame =
      match tmp_frame with
        | Some f -> f
        | None ->
            let f = Frame.create_type (self#content_type frame) in
            tmp_frame <- Some f;
            f

    method copy_frame ?(markers = true) src dst =
      if markers then (
        Frame.set_breaks dst (Frame.breaks src);
        Frame.set_all_metadata dst (Frame.get_all_metadata src) );
      if not audio then Frame.set_audio dst (Frame.audio src);
      if not video then Frame.set_video dst (Frame.video src);
      if not midi then Frame.set_midi dst (Frame.midi src)
  end
