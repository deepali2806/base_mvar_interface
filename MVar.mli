    (* type 'a t
    val create : 'a -> 'a t
    val create_empty : unit -> 'a t
    val put : 'a -> 'a t -> unit
    val take : 'a t -> 'a
    val put_monad : 'a -> 'a t -> unit Lwt.t
    val take_monad : 'a t -> 'a Lwt.t *)



(* module type MVarSig = sig *)
    type 'a t
    val create : 'a -> 'a t
    val create_empty : unit -> 'a t
    val put : 'a -> 'a t -> unit
    val take : 'a t -> 'a
(* end *)



(* val put_monad : 'a -> 'a t -> unit Lwt.t
val take_monad : 'a t -> 'a Lwt.t *)
