module Main exposing (main)

import Bootstrap.Button as Button
import Bootstrap.ButtonGroup as ButtonGroup
import Bootstrap.CDN as CDN
import Bootstrap.Card as Card
import Bootstrap.Card.Block as Block
import Bootstrap.Form.Input as Input
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Grid.Row as Row
import Bootstrap.Navbar as Navbar
import Bootstrap.Table as Table
import Bootstrap.Text as Text
import Bootstrap.Utilities.Spacing as Spacing
import Browser
import DnDList
import Html exposing (div, img, text)
import Html.Attributes exposing (class, href, src, style)
import Svg
import Svg.Attributes as SvgAtt


flip : (a -> b -> c) -> b -> a -> c
flip function argB argA =
    function argA argB


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- Data


type alias Point =
    { x : Float, y : Float }


type alias Model =
    { userPoints : List Point
    , userPointsDragAndDrop : DnDList.Model
    }



-- Drag and drop configurations and system


config : DnDList.Config Point
config =
    { beforeUpdate = \_ _ list -> list
    , movement = DnDList.Free
    , listen = DnDList.OnDrag
    , operation = DnDList.Rotate
    }


system : DnDList.System Point Msg
system =
    DnDList.create config PointDrop



-- Model


init _ =
    ( initialModel, Cmd.none )


initialPoints =
    [ { x = 10, y = 10 }
    , { x = 20, y = 20 }
    ]


initialModel : Model
initialModel =
    { userPoints = initialPoints
    , userPointsDragAndDrop = system.model
    }



-- Update


type Msg
    = PointDrop DnDList.Msg
    | NavbarMsg Navbar.State


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        PointDrop msg ->
            let
                ( dnd, userPoints ) =
                    system.update msg model.userPointsDragAndDrop model.userPoints
            in
            ( { model | userPointsDragAndDrop = dnd, userPoints = userPoints }
            , system.commands model.userPointsDragAndDrop
            )

        NavbarMsg msg ->
            ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    system.subscriptions model.userPointsDragAndDrop



-- view


withBootstrapStyleSheet : List (Html.Html Msg) -> List (Html.Html Msg)
withBootstrapStyleSheet =
    (++)
        [ CDN.fontAwesome
        , Html.node "link"
            [ Html.Attributes.rel "stylesheet"
            , href "/css/bootstrap.min.css"
            ]
            []
        ]


joinWith : String -> ( String, String ) -> String
joinWith char stringsTuple =
    flip (++) (Tuple.second stringsTuple) <| (++) (Tuple.first stringsTuple) char


createSvgPath : List ( Float, Float ) -> String
createSvgPath wayPoints =
    List.foldr
        (\current ->
            Tuple.mapBoth String.fromFloat String.fromFloat current
                |> joinWith " "
                |> (++)
                << (++) "l"
        )
        ""
        wayPoints


mainToolbarButtons : List (ButtonGroup.ButtonItem msg)
mainToolbarButtons =
    [ ButtonGroup.button [ Button.light ] [ text "Save" ]
    , ButtonGroup.button [ Button.light ] [ text "Load" ]
    , ButtonGroup.button [ Button.light ] [ text "Undo" ]
    , ButtonGroup.button [ Button.light ] [ text "Redo" ]
    ]


optionsCardView : String -> List (Html.Html Msg) -> Html.Html Msg
optionsCardView header content =
    Card.config
        [ Card.primary
        , Card.textColor Text.white
        , Card.attrs [ Spacing.mb3 ]
        ]
        |> Card.header [] [ text header ]
        |> Card.block [ Block.light, Block.textColor Text.success ]
            [ Block.custom <|
                div [] content
            ]
        |> Card.view


view : Model -> Html.Html Msg
view model =
    div [] <|
        withBootstrapStyleSheet
            [ Navbar.config NavbarMsg
                |> Navbar.withAnimation
                |> Navbar.primary
                |> Navbar.brand [ href "#" ] [ text "Path Finder" ]
                |> Navbar.items
                    [ Navbar.itemLink []
                        [ ButtonGroup.toolbar []
                            [ ButtonGroup.buttonGroupItem [ ButtonGroup.attrs [ Spacing.ml1 ] ] mainToolbarButtons
                            ]
                        ]
                    ]
                |> Navbar.view (Tuple.first <| Navbar.initialState NavbarMsg)
            , Grid.container [ Spacing.my5 ]
                [ Grid.row [ Row.centerMd ]
                    [ Grid.col [ Col.mdAuto ]
                        [ div [ style "background-color" "#0B0", style "width" "50vw", style "height" "25vw" ]
                            [ Svg.svg
                                [ SvgAtt.x "0"
                                , SvgAtt.y "0"
                                , SvgAtt.viewBox "0 0 323.141 322.95"
                                ]
                                [ Svg.path
                                    [ SvgAtt.d <| "M30 50 " ++ createSvgPath [ ( 20, 20 ), ( 20, 30 ), ( 50, 60 ) ]
                                    , SvgAtt.fill "transparent"
                                    , SvgAtt.stroke "black"
                                    , SvgAtt.strokeLinecap "round"

                                    -- , SvgAtt.strokeLinejoin "round"
                                    , SvgAtt.strokeWidth "2px"
                                    ]
                                    []
                                ]
                            ]
                        ]
                    ]
                , Grid.row []
                    [ Grid.col [ Col.xs5 ]
                        [ optionsCardView "Options" []
                        ]
                    , Grid.col []
                        [ Button.button [ Button.primary ] [ text "hello" ]
                        ]
                    , Grid.col [ Col.xs5 ]
                        [ model.userPoints
                            |> List.indexedMap (pointDataView model.userPointsDragAndDrop)
                            |> optionsCardView "Points"
                        ]
                    ]
                , ghostPointDataView model.userPointsDragAndDrop model.userPoints
                ]
            ]


pointDataView : DnDList.Model -> Int -> Point -> Html.Html Msg
pointDataView dnd index point =
    let
        itemId : String
        itemId =
            (++) "id-" <| String.fromFloat <| .x point
    in
    case system.info dnd of
        Just { dragIndex } ->
            if dragIndex /= index then
                Html.p
                    (Html.Attributes.id itemId :: system.dropEvents index itemId)
                    [ Html.text itemId ]

            else
                Html.p
                    [ Html.Attributes.id itemId ]
                    [ Html.text "[---------]" ]

        Nothing ->
            Html.p
                (Html.Attributes.id itemId :: system.dragEvents index itemId)
                [ Html.text itemId ]


ghostPointDataView : DnDList.Model -> List Point -> Html.Html Msg
ghostPointDataView dnd items =
    let
        maybeDragItem : Maybe Point
        maybeDragItem =
            system.info dnd
                |> Maybe.andThen (\{ dragIndex } -> items |> List.drop dragIndex |> List.head)
    in
    case maybeDragItem of
        Just point ->
            Html.div
                (system.ghostStyles dnd)
                [ Html.text <| String.fromFloat <| .x point ]

        Nothing ->
            Html.text ""
