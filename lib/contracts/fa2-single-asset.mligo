#import "../data/errors.mligo" "Errors"
#import "../data/metadata.mligo" "Metadata"
#import "../data/token.mligo" "Token"
#import "../data/ledger.mligo" "Ledger"
#import "../data/operators.mligo" "Operators"
#import "../data/approvals.mligo" "Approvals"
#import "../data/tokenMetadata.mligo" "TokenMetadata"
#import "../data/storage.mligo" "Storage"

#import "../entrypoints/transfer.mligo" "Transfer"
#import "../entrypoints/balance_of.mligo" "Balance_of"
#import "../entrypoints/update.mligo" "Update"

#import "./fa2-generic.mligo" "FA2"

type parameter = FA2.parameter
type parametric_storage = Storage.t

type ledger_kind = Ledger.Single_asset.t
let ledger_make = Ledger.Single_asset.ledger_and_make

type storage = (unit, ledger_kind) parametric_storage

let main (p:(FA2.parameter * storage)): operation list * storage = 
   FA2.main ledger_make p

(*
   Views corner
*)

[@view] let balance_of : ((address * Token.t) * storage) -> nat =
   FA2.balance_of ledger_make

[@view] let total_supply : (Token.t * storage) -> nat =
   FA2.total_supply