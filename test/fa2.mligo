#import "../lib/data/storage.mligo" "Storage"
#import "../lib/data/tokenMetadata.mligo" "TokenMetadata"
#import "../lib/entrypoints/transfer.mligo" "Transfer"
#import "../lib/entrypoints/balance_of.mligo" "Balance_of"
#import "../lib/entrypoints/update.mligo" "Update"

type storage = Storage.t
type extended_storage = string storage

type parameter = [@layout:comb]
   | Transfer of Transfer.transfer
   | Balance_of of Balance_of.balance_of
   | Update_operators of Update.update_operators

let main ((p,s):(parameter * extended_storage)): operation list * extended_storage = match p with
   | Transfer         p -> Transfer.transfer   p s
   | Balance_of       p -> Balance_of.balance_of p s
   | Update_operators p -> Update.update_ops p s
