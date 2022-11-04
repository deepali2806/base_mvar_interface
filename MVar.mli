module type MVarSig = sig
    type 'a t
    val create : 'a -> 'a t
    val create_empty : unit -> 'a t
    val put : 'a -> 'a t -> unit
    val take : 'a t -> 'a
end

module type Base_Sched = 
  sig
    type 'a t 
    val return : 'a -> 'a t
  end

module type Base_mvar = 
  sig
    type 'a t
    type 'a u
    val take_monad :  'a u-> 'a t
    val put_monad : 'a -> 'a u -> unit t
  end

module MVar: MVarSig

module Make_monad (S : Base_Sched) : (Base_mvar with type 'a t = 'a S.t and type 'a u = 'a MVar.t)

(* val put_monad : 'a -> 'a t -> unit Lwt.t
val take_monad : 'a t -> 'a Lwt.t *)
