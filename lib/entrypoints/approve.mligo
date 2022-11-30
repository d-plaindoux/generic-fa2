#import "../data/token.mligo" "Token"
#import "../data/ledger.mligo" "Ledger"
#import "../data/storage.mligo" "Storage"

type storage = Storage.t

type single_approve = [@layout:comb] {
      owner     : address;
      spender   : address;
      token_id  : Token.t;
      old_value : nat;
      new_value : nat;
   }   

type approvements = single_approve list

type t = approvements

let approve (type a) (_approvements: approvements) (storage: a storage) : operation list * a storage =
   ([]: operation list), storage