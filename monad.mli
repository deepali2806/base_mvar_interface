(* open MVar *)

module type Base_Sched = 
  sig
    type 'a t 
    val return : 'a -> 'a t
  end

module type Base_mvar = 
  sig
    type 'a t
    val take_monad :  'a MVar.t-> 'a t
    val put_monad : 'a -> 'a MVar.t -> unit t
  end

module Make_monad (S : Base_Sched) : (Base_mvar with type 'a t = 'a S.t)

