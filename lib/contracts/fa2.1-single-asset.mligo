#import "../data/errors.mligo" "Errors"
#import "../data/metadata.mligo" "Metadata"
#import "../data/token.mligo" "Token"
#import "../data/ledger.mligo" "Ledger"
#import "../data/operators.mligo" "Operators"
#import "../data/approvals.mligo" "Approvals"
#import "../data/tokenMetadata.mligo" "TokenMetadata"
#import "../data/storage.mligo" "Storage"

#import "./fa2.1-generic.mligo" "FA2_1"

type parametric_storage = Storage.t

type ledger_kind = Ledger.Multi_asset.t
let ledger_make = Ledger.Multi_asset.ledger_and_make

type storage = (unit, ledger_kind) parametric_storage

let main (p:(FA2_1.parameter * storage)): operation list * storage = 
   FA2_1.main ledger_make p

(*
   Views corner
*)

[@view] let balance_of : ((address * Token.t) * storage) -> nat =
   FA2_1.balance_of ledger_make

[@view] let total_supply : (Token.t * storage) -> nat =
   FA2_1.total_supply
