

type 'a tree = Empty | Tree of ('a * 'a tree * 'a tree)


module type OrderedType =
sig
  type t
  val compare : t -> t -> int
end

module type S =
sig
  type key
  type 'a t
  val create : unit -> 'a t
  val add : 'a t -> key -> 'a t -> 'a t
  val find : 'a t -> key -> 'a
end

(* module Make : functor (Ord: OrderedType) -> S with type key = Ord.t *)

module Make (Ord: OrderedType) = struct

  type key = Ord.t

  type 'a t = Empty | Tree of ('a * 'a t * 'a t)

  let create () = Empty

  let rec add m k v =
    match m with
    | Empty -> Tree ((k, v), Empty, Empty)
    | Tree ((k', v'), l, r) when Ord.compare k k' < 0 -> Tree ((k', v'), add l k v, r)
    | Tree ((k', v'), l, r) when Ord.compare k k' > 0 -> Tree ((k', v'), l, add r k v)
    | Tree _ -> m

  let rec find m k =
    match m with
    | Empty -> raise Not_found
    | Tree ((k', v), l, r) ->
      if Ord.compare k k' = 0 then v
      else if Ord.compare k k' < 0 then find l k
      else find r k

end

module MapString = Make (String)



type fmark = Bold | Italic | Underlined
type fchar = (char * fmark list)
type fline = fchar list

let set_mark = List.cons
let unset_mark a = List.filter ((<>) a)
let present_mark = List.mem

let change_mark a l =
  if present_mark a l then
    unset_mark a l
  else set_mark a l


module type INPUT = sig
  type param
  type t
  exception End
  val create : param -> t
  val line : t -> fline
  val close : t -> unit
end

module type OUTPUT = sig
  type param
  type t
  val create : param -> t
  val line : t -> fline -> unit
  val close : t -> unit
end

module type PROC = sig
  type input_param
  type output_param
  val process : input_param -> output_param -> unit
end

module Processor (Input : INPUT)
    (Output : OUTPUT) : (PROC with type input_param = Input.param
                               and type output_param = Output.param) =
struct
  type input_param = Input.param
  type output_param = Output.param
  let process input_param output_param =
    let input = Input.create input_param in
    let output = Output.create output_param in
    try
      while true do
        Output.line output (Input.line input)
      done
    with
    | Input.End ->
      Input.close input ;
      Output.close output
end

module VerbatimFileInput : INPUT with type param = string = struct
  type param = string
  type t = in_channel
  exception End

  let create = open_in
  let close = close_in

  let line c =
    match input_line c with
    | exception End_of_file -> raise End
    | s ->
      List.rev @@
      Seq.fold_left (fun acc c -> List.cons (c, []) acc) [] @@ String.to_seq s
end

module VerbatimFileOutput : OUTPUT with type param = string = struct
  type param = string
  type t = out_channel

  let create = open_out
  let close = close_out

  let line chan line =
    List.iter (fun (c, _) -> output_char chan c) line;
    output_string chan "\n"

end

module VerbatimProcessor = Processor (VerbatimFileInput)(VerbatimFileOutput)

(* let () =
 *   VerbatimProcessor.process "td5.ml" "td32.ml" *)
