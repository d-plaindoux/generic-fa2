#import "../data/token.mligo" "Token"
#import "../data/ledger.mligo" "Ledger"
#import "../data/storage.mligo" "Storage"

type storage = Storage.t

type request = {
      owner    : address;
      token_id : Token.t;
   }

type callback = [@layout:comb] {
      request : request;
      balance : nat;
   }

type balance_of = [@layout:comb] {
      requests : request list;
      callback : callback list contract;
   }

let balance_of (type a) (b: balance_of) (s: a storage) : operation list * a storage =
   let {requests;callback} = b in
   let get_balance_info (request : request) : callback =
      let {owner;token_id} = request in
      let ()          = Storage.assert_token_exist  s token_id in
      let balance_    = Ledger.get_for_user s.ledger owner token_id in
      {request=request;balance=balance_}
   in
   let callback_param = List.map get_balance_info requests in
   let operation = Tezos.transaction callback_param 0tez callback in
   ([operation]: operation list),s
