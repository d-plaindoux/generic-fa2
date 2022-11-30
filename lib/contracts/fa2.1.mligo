#import "../data/token.mligo" "Token"
#import "../data/ledger.mligo" "Ledger"
#import "../data/storage.mligo" "Storage"
#import "../data/tokenMetadata.mligo" "TokenMetadata"
#import "../entrypoints/transfer.mligo" "Transfer"
#import "../entrypoints/balance_of.mligo" "Balance_of"
#import "../entrypoints/update.mligo" "Update"
#import "../entrypoints/import_ticket.mligo" "Import_ticket"

type storage = Storage.t
type extended_storage = unit storage

type parameter = [@layout:comb]
   | Transfer of Transfer.transfer
   | Balance_of of Balance_of.balance_of
   | Update_operators of Update.update_operators
   | Import_ticket of Import_ticket.import_ticket

let main ((p,s):(parameter * extended_storage)): operation list * extended_storage = match p with
   | Transfer         p -> Transfer.transfer   p s
   | Balance_of       p -> Balance_of.balance_of p s
   | Update_operators p -> Update.update_ops p s

(*
   Views corner
*)

[@view] let balance_of : ((address * Token.t) * extended_storage) -> nat =
   fun (parameter, storage : (address * Token.t) * extended_storage) ->
      let (owner, token_id) = parameter in
      Ledger.get_for_user storage.ledger owner token_id

[@view] let total_supply : (Token.t * extended_storage) -> nat =
   fun ((token_id, s) : (nat * extended_storage)) ->
      let () = Storage.assert_token_exist s token_id in
      1n (* ? *)
