import Html exposing (..)
import Html.App as Html
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Json
import Task
import String


main : Program Never
main =
  Html.program
    { init = init "cats"
    , view = view
    , update = update
    , subscriptions = subscriptions
    }



-- MODEL

type alias Record =
  { label : String
  , value : String
  }

type alias Records = List Record

type alias Model =
  { found_records : Records
  , selected_record : Record
  , message : String
  } 


init : String -> (Model, Cmd Msg)
init s =
  ( Model [] (Record "" "") ("INIT with " ++ s)
  , Cmd.none
  )


-- UPDATE


type Msg
  = Search String
  | FetchSucceed Records
  | FetchFail Http.Error
  | SelectRecord Record
  | ClearSelectedValue



update : Msg -> Model -> (Model, Cmd Msg)
update action model =
  case action of
    Search s ->
      let 
        m = Model [] (Record "" "") "SEARCHING"
      in
        if (String.length s) > 2 then 
          (m, apiSearch s)
        else
          ({ m | message = "" }, Cmd.none)

    FetchSucceed records ->
      (Model records (Record "" "") "OK", Cmd.none)

    FetchFail _ ->
      ({model | message = "FAIL"}, Cmd.none)

    SelectRecord r ->
      ({ model | found_records = [], selected_record = r }, Cmd.none)

    ClearSelectedValue ->
      (Model [] (Record "" "") "CLEARED", Cmd.none)



-- VIEW


view : Model -> Html Msg
view model =
  div [ class ""] [
    div [ class "row" ] [
      Html.form [ class ""] [
        div [ class "form-group" ] [
          (searchInput model)
        ]
        , div [ class "form-group" ] [
          div [] [(recordList model.found_records)]
        ]
      ]
    ]
    , br [] []
    , div [ class "row" ] [ text model.message ]
    , br [] []
    , div [] [ text "SELECTED: "
              , text model.selected_record.label
              , text model.selected_record.value 
              ]
  ]


searchInput : Model -> Html Msg
searchInput model = 
  if (String.length model.selected_record.value) == 0 then
    input [ class "search-input form-control col-xs-3 ", onInput Search ] []
  else
    div [ class "selected-value" ] [ text ("SELECTED: " ++ model.selected_record.label) 
    , text " "
    , div [ class "btn btn-default", onClick ClearSelectedValue ] [ text "X"]
    ]



recordList : Records -> Html Msg
recordList records = 
  if (List.length records) > 0 then
    div [ class "found-records" ] (List.map foundRecord records)
  else
    div [] []
  

foundRecord : Record -> Html Msg
foundRecord record = 
  div [ class "record", onClick (SelectRecord record) ] [ text record.label ]




-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none



-- HTTP

apiSearch : String -> Cmd Msg
apiSearch s =
  let
    url =
      "list.json?" ++ s
  in
    Task.perform FetchFail FetchSucceed (Http.get decodeResult url)


decodeResult : Json.Decoder Records
decodeResult =
  Json.list decodeRecord


decodeRecord : Json.Decoder Record
decodeRecord = 
  Json.object2 Record
    (Json.at ["label"] Json.string)
    (Json.at ["value"] Json.string)


