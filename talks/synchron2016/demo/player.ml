open Firebug
open Dom_html2


let to_min_sec t =
  let sec = int_of_float t in
  let min = sec / 60 in
  let sec = sec mod 60 in
  (min, sec)

let max_slide = 1000.

let set_visible b elt =
  let visibility = if b then "visible" else "hidden"
  in elt##.style##.visibility := Js.string visibility

(* Updates the progress bar value proportionnaly to current time *)
let update_slider slider media =
  slider##.value := Js.string @@ Format.sprintf "%0.f" (
      if media##.duration = 0. then 0.
      else media##.currentTime /. media##.duration *. max_slide)


(* Sets the current time of the media tag in proportion *)
let update_media media slider =
  media##.currentTime := (Js.parseFloat slider##.value) /. max_slide *. media##.duration


(* Apply the state switching to the video by calling pause/play action *)
let update_state state media button =
  if state then media##play else media##pause


(* Update the displayed current time *)
let update_time_a media time_a =
  let cmin, csec = to_min_sec media##.currentTime in
  let tmin, tsec = to_min_sec media##.duration in
  time_a##.textContent := str @@ Format.sprintf "%2d:%0d / %0d:%0d" cmin csec tmin tsec

(* Switch the text on the button *)
let update_content elt b =
  elt##.textContent := Js.some @@ Js.string @@ if b then "Pause" else "Play"


open Dom_html

let%sync reactive_player =
  input play_pause;
  input progress_bar;
  input media;
  input time_a;

  let state = Js.to_bool media##.autoplay in
  let seek = () in

  loop begin
    present play_pause##onclick (emit state (not !!state))
    ||
    present state !(
      update_state !!state media play_pause;
      update_content play_pause !!state
    );
    pause
  end
  ||
  loop begin
    present progress_bar##onmousedown begin
      trap t begin
        loop begin
          emit seek
        ; present progress_bar##onmouseup begin
            !(update_media media progress_bar);
            exit t
          end
        ; pause
        end
      end
    end
  ; pause
  end
  ||
  loop begin
    present media##ontimeupdate begin
      !(update_time_a media !!time_a)
      ||
      present seek nothing
        !(update_slider progress_bar media)
    end;
    pause
  end


let main _ =
  let play_button = "reactiveplayer_play" @> Coerce.button in
  let progress_bar = "reactiveplayer_progress" @> Coerce.input in
  let media = "reactiveplayer_media" @> Coerce.media in
  let time = "reactiveplayer_timetxt" @> Coerce.a in
  reactive_player (play_button, progress_bar, media, time);
  Js._false


let () = Dom_html.(window##.onload := handler main)
