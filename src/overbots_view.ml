open Tea.Html
open Overbots_types


let view_container enabled id title children =
  if not enabled then noNode else
    div
      [ class' ("container container-" ^ id)]
      [ div [ class' "title"] [ text title ]
      ; div [ class' ("scroller " ^ id) ] children
      ]


let format_value value =
  if value < 10000.0 then
    let str = string_of_float value in
    let str = String.sub str 0 (min (String.length str) 6) in
    str ^ String.make (6 - String.length str) '0'
  else
    string_of_int (int_of_float value)

let view_resources_category_resource model (rid, name, id) =
  let r = Overbots_resource.get_resource_module rid in
  let module R = (val r) in
  if not (R.shown model) then [] else
  let value = format_value (Overbots_resource.get_resource_value rid model) in
  let delta = format_value (ResourceMap.find rid model.cache.resource_deltas) in
  [ div
      [ class' ("resource resource-"^id) ]
      [ div [ class' "resource-name" ] [ text name ]
      ; div [ class' "resource-value" ] [ text value ]
      ; div [ class' "resource-delta" ] [ text delta; text "/s" ]
      ]
  ]

let view_resources_categories model (name, id, resources) =
  let children = List.map (view_resources_category_resource model) resources |> List.flatten in
  if children == [] then [] else
  let children = if name = "" then children else div [ class' "category-title" ] [ text name ] :: children in
  [ div [ class' ("resource-category resource-category-"^id) ] children ]

let view_resources model =
  List.map (view_resources_categories model) Overbots_resource.displayed_resources
  |> List.flatten


let view_button model (bid, id, name) =
  let open Overbots_buttons in
  if button_enabled model bid then
    [ button
        [ onClick (ActionButtonClicked bid)
        ; Attributes.disabled (button_temporarily_disabled model bid)
        ; class' ("action action-" ^ id)
        ] [ text name ]
    ]
  else
    []

let view_buttons_category model (name, id, buttons) =
  let children = List.map (view_button model) buttons |> List.flatten in
  if children == [] then [] else
  let children = div [ class' "category-title" ] [ text name ] :: children in
  [ div [ class' ("action-category action-category-"^id) ] children ]

let view_buttons model =
  List.map (view_buttons_category model) Overbots_buttons.displayed_buttons
  |> List.flatten


let view_scanner _model = []


let view_msg _model = function
  | TimeMsg (at, msg) -> div [ class' "msg" ] [text ((0.5 +. at) |> int_of_float |> string_of_int); text ": "; text msg]

let view_msgs model =
  List.map (view_msg model) model.msgs


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
