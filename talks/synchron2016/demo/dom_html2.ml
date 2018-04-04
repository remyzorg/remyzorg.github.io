
include Dom_html
open Js
class type _mediaElement = object
  inherit mediaElement
  method onprogress : (_mediaElement t, mouseEvent t) event_listener writeonly_prop
  method ontimeupdate : (_mediaElement t, mouseEvent t) event_listener writeonly_prop
  method onplay : (_mediaElement t, mouseEvent t) event_listener writeonly_prop
  method onpause : (_mediaElement t, mouseEvent t) event_listener writeonly_prop
  method onloadeddata : (_mediaElement t, mouseEvent t) event_listener writeonly_prop
end
module Coerce = struct
  include CoerceTo
  let unsafeCoerce tag (e : #element t) = Js.some (Js.Unsafe.coerce e)
  let media :  #element t -> _mediaElement t opt = fun e -> unsafeCoerce "media" e
end

(* Jsoo boilerplate code *)

let error f = Printf.ksprintf
    (fun s -> Firebug.console##error (Js.string s); failwith s) f
let debug f = Printf.ksprintf
    (fun s -> Firebug.console##log(Js.string s)) f
let alert f = Printf.ksprintf
    (fun s -> Dom_html.window##alert(Js.string s); failwith s) f

let (@>) s coerce =
  Js.Opt.get (coerce @@ Dom_html.getElementById s)
    (fun () -> error "can't find element %s" s)

let str s = Js.some @@ Js.string s

(* ======================== *)

