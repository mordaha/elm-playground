import Html exposing (..)
import Html.App as Html
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Json
import Task
import String


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



-- VIEW


view : Model -> Html Msg
view model =
  div []
    [ input [ onInput Search ] []
    , br [] []
    , div [] [(recordList model.found_records)]
    , div [] [ text model.message ]
    , br [] []
    , div [] [ text "SELECTED: "
              , text model.selected_record.label
              , text model.selected_record.value 
              ]
    ]


recordList : Records -> Html Msg
recordList records = 
  div [ class "found-records" ] (List.map foundRecord records)
  

foundRecord : Record -> Html Msg
foundRecord record = 
  div [ class "record", onClick (SelectRecord record), recordStyle ] [ text record.label ]


recordStyle =
  style
    [ ("padding", "10px 0")
    , ("cursor", "pointer")
    , ("background-color", "#eef")
    ]





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


