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

type t = balance_of

let get_balance_info (type a) (storage: a storage)  (request : request) : callback =
   let {owner;token_id} = request in
   let ()          = Storage.assert_token_exist storage token_id in
   let balance_    = Ledger.get_for_user storage.ledger owner token_id in
   {request=request;balance=balance_}

let balance_of (type a) (balance: balance_of) (storage: a storage) : operation list * a storage =
   let {requests;callback} = balance in
   let callback_param = List.map (get_balance_info storage) requests in
   let operation = Tezos.transaction callback_param 0tez callback in
   [ operation ], storage
