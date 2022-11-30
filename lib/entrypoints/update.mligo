#import "../data/errors.mligo" "Errors"
#import "../data/operators.mligo" "Operators"
#import "../data/ledger.mligo" "Ledger"
#import "../data/storage.mligo" "Storage"

type storage = Storage.t

type operator = [@layout:comb] {
      owner    : address;
      operator : address;
      token_id : nat;
   }

type unit_update      = 
   | Add_operator of operator 
   | Remove_operator of operator

type update_operators = unit_update list

type t = update_operators

let update_ops (type a) (updates: update_operators) (storage: a storage) (operators: Operators.t) : operation list * a storage =
   let update_operator (operators,update : Operators.t * unit_update) = match update with
      Add_operator    {owner=owner;operator=operator;token_id=token_id} -> Operators.add_operator    operators owner operator token_id
   |  Remove_operator {owner=owner;operator=operator;token_id=token_id} -> Operators.remove_operator operators owner operator token_id
   in
   let operators = List.fold_left update_operator operators updates in
   ([]: operation list),Storage.set_operators storage operators

let update_ops (type a) (updates: update_operators) (storage: a storage) : operation list * a storage =   
   match Storage.get_operators storage with
   | Some operators -> update_ops updates storage operators
   | None -> failwith Errors.storage_has_no_operators
