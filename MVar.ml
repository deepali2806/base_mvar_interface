open Effect
(* 
module type MVarSig = sig
  type 'a t
  val create : 'a -> 'a t
  val create_empty : unit -> 'a t
  val put : 'a -> 'a t -> unit
  val take : 'a t -> 'a
end *)


(* module MVar: MVarSig = struct *)
type 'a mv_state =
   | Full  of 'a * ('a * unit Sched.resumer) Queue.t
   | Empty of 'a Sched.resumer Queue.t
 
 type 'a t = 'a mv_state ref
 
 let create_empty () = ref (Empty (Queue.create ()))
 
 let create v = ref (Full (v, Queue.create ()))


 let take mv =
  match !mv with
  | Empty q -> perform (Sched.Suspend (fun r -> Queue.push r q)) 
  | Full (v, q) ->
      if Queue.is_empty q then
        (mv := Empty (Queue.create ()); v)
      else begin
        let (v', resume) = Queue.pop q in
        mv := Full (v', q);
        resume ();
        v
      end
      

 let put v mv =
   match !mv with
   | Full (_, q) -> perform (Sched.Suspend (fun r -> Queue.push (v,r) q))
   | Empty q ->
       if Queue.is_empty q then
         mv := Full (v, Queue.create ())
       else
         let resume = Queue.pop q in
         resume v

(* end *)

(* open Effect
(* open Printf *)


type 'a mv_state =
   | Full  of 'a * ('a * unit Sched.resumer) Queue.t
   | Empty of 'a Sched.resumer Queue.t
 
 type 'a t = 'a mv_state ref
 
 let create_empty () = ref (Empty (Queue.create ()))
 
 let create v = ref (Full (v, Queue.create ()))


 let take mv =
  match !mv with
  | Empty q -> perform (Sched.Suspend (fun r -> Queue.push r q)) 
  | Full (v, q) ->
      if Queue.is_empty q then
        (mv := Empty (Queue.create ()); v)
      else begin
        let (v', resume) = Queue.pop q in
        mv := Full (v', q);
        resume ();
        v
      end
      

 let put v mv =
   match !mv with
   | Full (_, q) -> perform (Sched.Suspend (fun r -> Queue.push (v,r) q))
   | Empty q ->
       if Queue.is_empty q then
         mv := Full (v, Queue.create ())
       else
         let resume = Queue.pop q in
         resume v


  let take_monad m = 
          let p  = (try Lwt.return (take m) with
                  | Unhandled (Sched.Suspend f) -> 
                                  Lwt.incr_suspend_count ();
                                  let (promise, resolver) = Lwt.task () in
                                  let resumer v = (Lwt.decr_suspend_count ();
                                                    Lwt.wakeup resolver v) in
                                  f resumer;
                                  Obj.magic promise
                          ) 
          in p
        
  let put_monad v m = 
          let v = (try Lwt.return (put v m) with
                  | Unhandled (Sched.Suspend f) ->
                                  Lwt.incr_suspend_count ();
                                  let (promise, resolver) = Lwt.task () in
                                  let resumer v = ( Lwt.decr_suspend_count ();
                                                    Lwt.wakeup resolver v) in
                                  f resumer;
                                  Obj.magic promise
                                  ) 
                  in v 
 *)







