module Main exposing (main)

import Browser
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)
import Json.Decode as Decode
import Random


type alias Flags =
    { cards : List String
    , items : List String
    , modifiers : List String
    }


flagsDecoder : Decode.Decoder Flags
flagsDecoder =
    Decode.map3 Flags
        (Decode.field "cards" (Decode.list cardDecoder))
        (Decode.field "items" (Decode.list itemDecoder))
        (Decode.field "modifiers" (Decode.list modifierDecoder))


cardDecoder : Decode.Decoder String
cardDecoder =
    Decode.field "image" Decode.string


itemDecoder : Decode.Decoder String
itemDecoder =
    Decode.field "image" Decode.string


modifierDecoder : Decode.Decoder String
modifierDecoder =
    Decode.field "image" Decode.string



---- MODEL ----


type alias URL =
    String



-- Cards


type CardStatus
    = Active
    | Held
    | Discarded
    | Lost


type alias Card =
    { id : Int
    , image : URL
    , status : CardStatus
    }


type alias Hand =
    List Card


filterByStatus : CardStatus -> Hand -> List Card
filterByStatus status =
    List.filter (\card -> card.status == status)


type alias Modifier =
    { id : Int
    , image : String
    , autoDestroy : Bool
    }


blessing : Int -> Modifier
blessing id =
    { id = id, image = "Modifiers/am-p-bless.png", autoDestroy = True }


curse : Int -> Modifier
curse id =
    { id = id, image = "Modifiers/am-p-curse.png", autoDestroy = True }


type ItemStatus
    = Unused
    | Used
    | Flipped


type alias Item =
    { id : Int
    , image : URL
    , status : ItemStatus
    }


type alias Model =
    { hand : Hand
    , modifierDeck : List Modifier
    , modifierDiscard : List Modifier
    , items : List Item
    , health : Int
    , experience : Int
    , gold : Int
    }


init : Decode.Value -> ( Model, Cmd Msg )
init encodedFlags =
    case Decode.decodeValue flagsDecoder encodedFlags of
        Err _ ->
            Debug.todo "wow"

        Ok flags ->
            ( { hand =
                    flags.cards
                        |> List.indexedMap
                            (\index cardImage ->
                                { id = index
                                , image = cardImage
                                , status = Held
                                }
                            )
              , modifierDeck =
                    flags.modifiers
                        |> List.indexedMap
                            (\index cardImage ->
                                { id = index
                                , image = cardImage
                                , autoDestroy = False
                                }
                            )
              , modifierDiscard = []
              , items =
                    flags.items
                        |> List.indexedMap
                            (\index cardImage ->
                                { id = index
                                , image = cardImage
                                , status = Unused
                                }
                            )
              , health = 10
              , experience = 0
              , gold = 0
              }
            , Cmd.none
            )



---- UPDATE ----


type Msg
    = NoOp
    | Discard Card
    | Restore Card
    | Destroy Card
    | Choose Card
    | CompleteRound
    | LongRest
    | ShortRest
    | DrawModifier
    | ShuffleModifiers
    | ResetModifiers
    | ModifierDrawn Modifier
    | AddBlessing
    | AddCurse
    | UseItem Item
    | FlipItem Item
    | RestoreItem Item
    | ResetItems
    | ChangeHealth Int
    | ChangeExperience Int
    | ChangeGold Int


sendToStatus : CardStatus -> Card -> Hand -> Hand
sendToStatus status { id } hand =
    List.map
        (\card ->
            if card.id == id then
                { card | status = status }

            else
                card
        )
        hand


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        CompleteRound ->
            ( { model
                | hand =
                    List.map
                        (\card ->
                            if card.status == Active then
                                { card | status = Discarded }

                            else
                                card
                        )
                        model.hand
              }
            , Cmd.none
            )

        LongRest ->
            ( { model
                | hand =
                    List.map
                        (\card ->
                            if card.status == Discarded then
                                { card | status = Held }

                            else
                                card
                        )
                        model.hand
              }
            , Cmd.none
            )

        ShortRest ->
            let
                discardedCards =
                    List.filter (\card -> card.status == Discarded) model.hand

                newHand =
                    List.map
                        (\card ->
                            if card.status == Discarded then
                                { card | status = Held }

                            else
                                card
                        )
                        model.hand
            in
            ( { model
                | hand = newHand
              }
            , case discardedCards of
                [] ->
                    Cmd.none

                first :: rest ->
                    Random.generate Destroy (Random.uniform first rest)
            )

        Discard card ->
            ( { model
                | hand = sendToStatus Discarded card model.hand
              }
            , Cmd.none
            )

        Restore card ->
            ( { model
                | hand = sendToStatus Held card model.hand
              }
            , Cmd.none
            )

        Destroy card ->
            ( { model
                | hand = sendToStatus Lost card model.hand
              }
            , Cmd.none
            )

        Choose card ->
            ( { model
                | hand = sendToStatus Active card model.hand
              }
            , Cmd.none
            )

        ModifierDrawn modifier ->
            ( { model
                | modifierDiscard = modifier :: model.modifierDiscard
                , modifierDeck = List.filter (\{ id } -> id /= modifier.id) model.modifierDeck
              }
            , Cmd.none
            )

        AddBlessing ->
            let
                nextId =
                    1
                        + (List.map .id (model.modifierDeck ++ model.modifierDiscard)
                            |> List.sort
                            |> List.reverse
                            |> List.head
                            |> Maybe.withDefault 0
                          )
            in
            ( { model | modifierDeck = blessing nextId :: model.modifierDeck }, Cmd.none )

        AddCurse ->
            let
                nextId =
                    1
                        + (List.map .id (model.modifierDeck ++ model.modifierDiscard)
                            |> List.head
                            |> Maybe.withDefault 0
                          )
            in
            ( { model | modifierDeck = curse nextId :: model.modifierDeck }, Cmd.none )

        DrawModifier ->
            case model.modifierDeck of
                [] ->
                    ( model, Cmd.none )

                first :: rest ->
                    ( model, Random.generate ModifierDrawn (Random.uniform first rest) )

        ShuffleModifiers ->
            ( { model | modifierDeck = model.modifierDeck ++ List.filter (not << .autoDestroy) model.modifierDiscard, modifierDiscard = [] }, Cmd.none )

        ResetModifiers ->
            ( { model | modifierDeck = List.filter (not << .autoDestroy) (model.modifierDeck ++ model.modifierDiscard), modifierDiscard = [] }, Cmd.none )

        UseItem item ->
            ( { model
                | items =
                    List.map
                        (\i ->
                            if i.id == item.id then
                                { i | status = Used }

                            else
                                i
                        )
                        model.items
              }
            , Cmd.none
            )

        FlipItem item ->
            ( { model
                | items =
                    List.map
                        (\i ->
                            if i.id == item.id then
                                { i | status = Flipped }

                            else
                                i
                        )
                        model.items
              }
            , Cmd.none
            )

        RestoreItem item ->
            ( { model
                | items =
                    List.map
                        (\i ->
                            if i.id == item.id then
                                { i | status = Unused }

                            else
                                i
                        )
                        model.items
              }
            , Cmd.none
            )

        ResetItems ->
            ( { model | items = List.map (\item -> { item | status = Unused }) model.items }, Cmd.none )

        ChangeHealth change ->
            ( { model | health = model.health + change }, Cmd.none )

        ChangeExperience change ->
            ( { model | experience = model.experience + change }, Cmd.none )

        ChangeGold change ->
            ( { model | gold = model.gold + change }, Cmd.none )



---- VIEW ----


button : List (Attribute Msg) -> { onPress : Maybe Msg, label : Element Msg } -> Element Msg
button attributes =
    Input.button ([ Border.rounded 4, paddingXY 12 8, Background.color (rgb 0 0 0), Font.size 16, Font.color (rgb 1 1 1) ] ++ attributes)


renderCard : List (Element Msg) -> Card -> Element Msg
renderCard buttons card =
    column [ spacing 8 ]
        [ image [ width (px 300) ] { src = card.image, description = "" }
        , wrappedRow [ spacing 8 ] buttons
        ]


changeStatusButton : Msg -> String -> Element Msg
changeStatusButton msg label =
    button []
        { onPress = Just msg
        , label = text label
        }


discardButton : Card -> Element Msg
discardButton card =
    changeStatusButton (Discard card) "Discard"


restoreButton : Card -> Element Msg
restoreButton card =
    changeStatusButton (Restore card) "Restore"


destroyButton : Card -> Element Msg
destroyButton card =
    changeStatusButton (Destroy card) "Destroy"


chooseButton : Card -> Element Msg
chooseButton card =
    changeStatusButton (Choose card) "Choose"


cardRow : List (Attribute Msg) -> List (Element Msg) -> Element Msg
cardRow attrs =
    row ([ spacing 16 ] ++ attrs)


h2 : String -> Element Msg
h2 string =
    el [ Font.size 20, Font.bold ] (text string)


section : List (Attribute Msg) -> Element Msg -> Element Msg
section attrs =
    el ([ padding 16, Background.color (rgb255 255 255 255), width fill, Border.rounded 8, scrollbarX ] ++ attrs)


view : Model -> Html Msg
view model =
    layout [ padding 32, Background.color (rgb255 245 235 230) ]
        (column
            [ spacing 32, centerX, width fill ]
            [ el [ Font.size 36, Font.extraBold ] (text "Gloom E-Boy")
            , row [ spacing 32, width fill ]
                [ section [ width fill, height fill ] <|
                    column [ spacing 8 ]
                        [ h2 "Chosen Cards"
                        , cardRow []
                            (List.map
                                (\card ->
                                    renderCard
                                        [ restoreButton card ]
                                        card
                                )
                                (filterByStatus Active model.hand)
                            )
                        , button [] { onPress = Just CompleteRound, label = text "Complete Round" }
                        ]
                , section [ height fill ]
                    (column [ spacing 8 ]
                        [ h2 "Health"
                        , row [ spacing 16 ]
                            [ button [] { onPress = Just (ChangeHealth -1), label = text "-1" }
                            , el [ Font.bold ] (model.health |> String.fromInt |> text)
                            , button [] { onPress = Just (ChangeHealth 1), label = text "+1" }
                            ]
                        , h2 "Experience"
                        , row [ spacing 16 ]
                            [ button [] { onPress = Just (ChangeExperience -1), label = text "-1" }
                            , el [ Font.bold ] (model.experience |> String.fromInt |> text)
                            , button [] { onPress = Just (ChangeExperience 1), label = text "+1" }
                            ]
                        , h2 "Gold"
                        , row [ spacing 16 ]
                            [ button [] { onPress = Just (ChangeGold -1), label = text "-1" }
                            , el [ Font.bold ] (model.gold |> String.fromInt |> text)
                            , button [] { onPress = Just (ChangeGold 1), label = text "+1" }
                            ]
                        ]
                    )
                ]
            , section [] <|
                column [ spacing 8 ]
                    [ h2 "Hand"
                    , cardRow []
                        (List.map
                            (\card ->
                                renderCard
                                    [ chooseButton card, discardButton card, destroyButton card ]
                                    card
                            )
                            (filterByStatus Held model.hand)
                        )
                    ]
            , section [] <|
                column [ spacing 8 ]
                    [ h2 "Modifier Deck"
                    , el [ Font.size 16 ] (text <| (List.length model.modifierDeck |> String.fromInt) ++ " Remaining")
                    , row [ spacing 8 ]
                        [ button [] { onPress = Just DrawModifier, label = text "Draw Modifier" }
                        , button [] { onPress = Just AddBlessing, label = text "Add Blessing" }
                        , button [] { onPress = Just AddCurse, label = text "Add Curse" }
                        , button [] { onPress = Just ShuffleModifiers, label = text "Shuffle Discard" }
                        , button [] { onPress = Just ResetModifiers, label = text "Reset" }
                        ]
                    , row [ spacing 4 ]
                        (List.map
                            (\modifier ->
                                image
                                    [ width (px 200) ]
                                    { src = modifier.image, description = "" }
                            )
                            model.modifierDiscard
                        )
                    ]
            , section [] <|
                column [ spacing 8 ]
                    [ h2 "Items"
                    , row [ spacing 16 ]
                        (List.map
                            (\item ->
                                column [ spacing 4 ]
                                    [ image
                                        ((if item.status == Used then
                                            [ alpha 0.5 ]

                                          else
                                            []
                                         )
                                            ++ [ width (px 200) ]
                                        )
                                        { src =
                                            if item.status == Flipped then
                                                "Modifiers/back.jpg"

                                            else
                                                item.image
                                        , description = ""
                                        }
                                    , wrappedRow [ spacing 4 ]
                                        [ button [] { onPress = Just (UseItem item), label = text "Use" }
                                        , button [] { onPress = Just (FlipItem item), label = text "Flip" }
                                        , button [] { onPress = Just (RestoreItem item), label = text "Restore" }
                                        ]
                                    ]
                            )
                            model.items
                        )
                    , button [] { onPress = Just ResetItems, label = text "Restore All Items" }
                    ]
            , row [ width fill, spacing 32 ]
                [ section [ alignTop, height fill ] <|
                    column [ spacing 8, height fill ]
                        [ h2 "Discarded Cards"
                        , cardRow []
                            (List.map
                                (\card ->
                                    renderCard
                                        [ restoreButton card, destroyButton card ]
                                        card
                                )
                                (filterByStatus Discarded model.hand)
                            )
                        , row [ spacing 8, alignBottom ]
                            [ button [] { onPress = Just LongRest, label = text "Long Rest" }
                            , button [] { onPress = Just ShortRest, label = text "Short Rest" }
                            ]
                        ]
                , section [ alignTop, height fill ] <|
                    column [ spacing 8, height fill ]
                        [ h2 "Lost Cards"
                        , cardRow []
                            (List.map
                                (\card ->
                                    renderCard
                                        [ restoreButton card ]
                                        card
                                )
                                (filterByStatus Lost model.hand)
                            )
                        ]
                ]
            ]
        )



---- PROGRAM ----


main : Program Decode.Value Model Msg
main =
    Browser.element
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }
