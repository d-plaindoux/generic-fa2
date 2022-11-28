#import "errors.mligo" "Errors"
#import "metadata.mligo" "Metadata"
#import "token.mligo" "Token"
#import "operators.mligo" "Operators"
#import "tokenMetadata.mligo" "TokenMetadata"
#import "ledger.mligo" "Ledger"

type 'a t = {
  metadata: Metadata.t;
  ledger : Ledger.t;
  token_metadata : TokenMetadata.t;
  operators : Operators.t;
  extension : 'a;
}

let assert_token_exist (type a) (s: a t) (token_id : Token.t) : unit  =
  let _ = Option.unopt_with_error (Big_map.find_opt token_id s.token_metadata)
     Errors.undefined_token in
  ()

let set_ledger (type a) (s: a t) (ledger:Ledger.t) = {s with ledger = ledger}

let get_operators (type a) (s: a t) = s.operators
let set_operators (type a) (s: a t) (operators:Operators.t) = {s with operators = operators}
