(* open Effect *)

type 'a resumer = 'a -> unit
type _ Effect.t += Suspend : ('a resumer -> unit) -> 'a Effect.t
(* val suspend_fn : ('a resumer -> unit) -> 'a Lwt.t  *)

