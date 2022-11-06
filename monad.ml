open Effect

module type Base_Sched = 
  sig
    type 'a t 
    val return : 'a -> 'a t
  end


module type Base_mvar = 
  sig
    type 'a t
    (* type 'a u *)
    val take_monad :  'a MVar.t -> 'a t
    val put_monad : 'a -> 'a MVar.t -> unit t
  end
  
module Make_monad (S : Base_Sched) : (Base_mvar with type 'a t = 'a S.t)
= 
struct
  type 'a t = 'a S.t
  (* type 'a u = 'a MVar.t *)
  let take_monad m = 
  let p  = (try S.return (MVar.take m) with
          | Unhandled (Sched.Suspend f) -> 
                          Lwt.incr_suspend_count ();
                          let (promise, resolver) = Lwt.task () in
                          let resumer v = (Lwt.decr_suspend_count ();
                                           Lwt.wakeup resolver v; ()) in
                          f resumer;
                          Obj.magic promise
                  ) 
  in p

let put_monad v m = 
  let v = (try S.return (MVar.put v m) with
          | Unhandled (Sched.Suspend f) ->
                          Lwt.incr_suspend_count ();
                          let (promise, resolver) = Lwt.task () in
                          let resumer v = ( Lwt.decr_suspend_count ();
                                            Lwt.wakeup resolver v; ()) in
                          f resumer;
                          Obj.magic promise
                          ) 
          in v
end

module Final_monad = Make_monad (struct 
                                  type 'a t = 'a Lwt.t
                                  let return = Lwt.return
                                  end
                                ) 

