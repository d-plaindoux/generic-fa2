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

let main (_approvements: approvements) (storage: storage) : operation list * storage =
   ([]: operation list), storage