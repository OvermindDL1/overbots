open Tea.Html
open Overbots_types


let view_container enabled id title children =
  if not enabled then noNode else
    div
      [ class' ("container container-" ^ id)]
      [ div [ class' "title"] [ text title ]
      ; div [ class' ("scroller " ^ id) ] children
      ]


let view_resources model = []


let view_buttons model = []


let view_scanner model = []


let view_msgs model = []


let view model =
  div
    [ class' "overbots" ]
    [ div [ class' "header" ] [ text "OverBots" ]
    ; div [ class' "body" ]
        [ view_container true "resources" "Resources" (view_resources model)
        ; view_container true "actions" "Actions" (view_buttons model)
        ; view_container true "scanner" "Scanner" (view_scanner model)
        ]
    ; view_container true "msgs" "Messages" (view_msgs model)
    ]
