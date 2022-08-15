module Water exposing (..)

import Browser
import Html exposing (Html)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Svg.Events exposing (..)

type alias Model = {conf: Conf, state: State}
type alias Conf = List Bin
type alias Bin = List Color
type alias Color = Int

type State = ChoosingSrc
           | ChoosingDst Int

type Msg = SrcSelected Int
         | DstSelected Int

main = Browser.element {init = init
                       ,update = update
                       ,view = view
                       ,subscriptions = subscriptions
                       }

init : () -> (Model, Cmd Msg)
init _ = ({conf = [[3,1,1],[3,2,2],[1,3,4],[2,4,4],[]]
          ,state = ChoosingSrc
          }
         ,Cmd.none
         )

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        SrcSelected i ->
            let
                dummy = Debug.log "selected" i
            in
                ({model|state = ChoosingDst i}
                ,Cmd.none
            )
        DstSelected j ->
            case model.state of
                ChoosingDst i -> ({model | conf = pour model.conf i j
                                  ,state = ChoosingSrc}
                                 ,Cmd.none)
                _ -> ({model|state = ChoosingSrc}
                     ,Cmd.none)

pour : Conf -> Int -> Int -> Conf
pour conf i j =
    let
        binI = Maybe.withDefault [] <| List.head <| List.drop (i) conf
        binJ = Maybe.withDefault [] <| List.head <| List.drop (j) conf
    in
        if (List.length binJ) == 0 ||
            ((List.head binI)==(List.head binJ) && (List.length binJ) < h) then
            let
                srcmodified = (List.take (i) conf) ++
                              [List.drop 1 binI] ++
                                  (List.drop (i+1) conf)
            in
                (List.take (j) srcmodified) ++
                    [(Maybe.withDefault 0 <| List.head binI) :: binJ] ++
                                  (List.drop (j+1) srcmodified)
        else
            conf
                    
view : Model -> Html Msg
view model =
    Html.div[]
        [svg [width "600"
             ,height (String.fromInt (unit*(h+1)))
             ]
             (List.indexedMap (\i bin -> binView i bin model.state)
                  model.conf)
        ]

color: Color -> String
color col =
    case col of
        1 -> "gray"
        2 -> "yellow"
        3 -> "orange"
        4 -> "pink"
        _ -> "none"

unit = 40
h = 3
    
binView : Int -> Bin -> State ->  Svg Msg
binView i bin state =
    let
        strokeColor = case state of
                          ChoosingSrc -> "black"
                          ChoosingDst src -> if i==src then
                                                 "red"
                                             else
                                                 "black"
        msg = case state of
                  ChoosingSrc -> SrcSelected i
                  ChoosingDst src -> DstSelected i
    in
    g [onClick msg]
        (if (List.length bin) > 0 then
             List.indexedMap (\j c ->
                                  rect [x (String.fromInt (2*(i+1)*unit))
                                       ,y (String.fromInt (((h-(List.length bin))+j+1)*unit))
                                       ,width (String.fromInt unit)
                                       ,height (String.fromInt unit)
                                       ,fill (color c)
                                       ,stroke strokeColor
                                       ]
                                  []
                             )
             bin
         else
             [rect [x (String.fromInt (2*(i+1)*unit))
                   ,y (String.fromInt (unit))
                   ,width (String.fromInt unit)
                   ,height (String.fromInt ((h)*unit))
                   ,stroke "black"
                   ,fill "white"
                   ]
                  []
             ]
        )
            
subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
