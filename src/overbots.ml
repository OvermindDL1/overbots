open Tea

type msg =
  | NothingYet
  [@@bs.deriving {accessors}]

type model = {
  notUsedYet : int;
}

let init () =
  let model ={
    notUsedYet = 42;
  } in
  (model, Cmd.none)

let update model = function
  | NothingYet -> (model, Cmd.none)

let subscriptions _model =
  Sub.none

let view model =
  let open Html in
  div
    []
    [ text (string_of_int model.notUsedYet)
    ]

let main =
  App.standardProgram {
    init;
    update;
    view;
    subscriptions;
  }
