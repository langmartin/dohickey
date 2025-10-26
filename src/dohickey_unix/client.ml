open Dohickey

type t = {
  username : string;
  queue : (Item.t list) Queue.t;
  conn : Dream.websocket;
  table : string;
  mutable ready : bool
}

let create username table conn =
  {username; table; conn; queue = Queue.create(); ready = true}

let send_to (items : Item.t list) client =
  (* TODO locking? *)
  Queue.add items client.queue

let take client = Queue.take_opt client.queue
