module Api.Generated exposing
    ( Widget(..)
    , widgetEncoder
    , widgetDecoder
    , NavBarContext
    , navBarContextEncoder
    , navBarContextDecoder
    , Violation(..)
    , violationEncoder
    , violationDecoder
    , FlashMessage(..)
    , flashMessageEncoder
    , flashMessageDecoder
    , User
    , userEncoder
    , userDecoder
    )

import Json.Decode
import Json.Decode.Pipeline
import Json.Encode


type Widget 
    = NavBarWidget NavBarContext
    | AboutWidget 
    | HomeWidget 
    | LoginWidget 
    | NewUserWidget 
    | FlashMessageWidget FlashMessage


widgetEncoder : Widget -> Json.Encode.Value
widgetEncoder a =
    case a of
        NavBarWidget b ->
            Json.Encode.object [ ("tag" , Json.Encode.string "NavBarWidget")
            , ("contents" , navBarContextEncoder b) ]

        AboutWidget ->
            Json.Encode.object [("tag" , Json.Encode.string "AboutWidget")]

        HomeWidget ->
            Json.Encode.object [("tag" , Json.Encode.string "HomeWidget")]

        LoginWidget ->
            Json.Encode.object [("tag" , Json.Encode.string "LoginWidget")]

        NewUserWidget ->
            Json.Encode.object [("tag" , Json.Encode.string "NewUserWidget")]

        FlashMessageWidget b ->
            Json.Encode.object [ ("tag" , Json.Encode.string "FlashMessageWidget")
            , ("contents" , flashMessageEncoder b) ]


widgetDecoder : Json.Decode.Decoder Widget
widgetDecoder =
    Json.Decode.field "tag" Json.Decode.string |>
    Json.Decode.andThen (\a -> case a of
        "NavBarWidget" ->
            Json.Decode.succeed NavBarWidget |>
            Json.Decode.Pipeline.required "contents" navBarContextDecoder

        "AboutWidget" ->
            Json.Decode.succeed AboutWidget

        "HomeWidget" ->
            Json.Decode.succeed HomeWidget

        "LoginWidget" ->
            Json.Decode.succeed LoginWidget

        "NewUserWidget" ->
            Json.Decode.succeed NewUserWidget

        "FlashMessageWidget" ->
            Json.Decode.succeed FlashMessageWidget |>
            Json.Decode.Pipeline.required "contents" flashMessageDecoder

        _ ->
            Json.Decode.fail "No matching constructor")


type alias NavBarContext  =
    { loggedIn : Bool }


navBarContextEncoder : NavBarContext -> Json.Encode.Value
navBarContextEncoder a =
    Json.Encode.object [("loggedIn" , Json.Encode.bool a.loggedIn)]


navBarContextDecoder : Json.Decode.Decoder NavBarContext
navBarContextDecoder =
    Json.Decode.succeed NavBarContext |>
    Json.Decode.Pipeline.required "loggedIn" Json.Decode.bool


type Violation 
    = TextViolation { message : String }
    | HtmlViolation { message : String }


violationEncoder : Violation -> Json.Encode.Value
violationEncoder a =
    case a of
        TextViolation b ->
            Json.Encode.object [ ("tag" , Json.Encode.string "TextViolation")
            , ("message" , Json.Encode.string b.message) ]

        HtmlViolation b ->
            Json.Encode.object [ ("tag" , Json.Encode.string "HtmlViolation")
            , ("message" , Json.Encode.string b.message) ]


violationDecoder : Json.Decode.Decoder Violation
violationDecoder =
    Json.Decode.field "tag" Json.Decode.string |>
    Json.Decode.andThen (\a -> case a of
        "TextViolation" ->
            Json.Decode.map TextViolation (Json.Decode.succeed (\b -> { message = b }) |>
            Json.Decode.Pipeline.required "message" Json.Decode.string)

        "HtmlViolation" ->
            Json.Decode.map HtmlViolation (Json.Decode.succeed (\b -> { message = b }) |>
            Json.Decode.Pipeline.required "message" Json.Decode.string)

        _ ->
            Json.Decode.fail "No matching constructor")


type FlashMessage 
    = SuccessFlashMessage String
    | ErrorFlashMessage String


flashMessageEncoder : FlashMessage -> Json.Encode.Value
flashMessageEncoder a =
    case a of
        SuccessFlashMessage b ->
            Json.Encode.object [ ("tag" , Json.Encode.string "SuccessFlashMessage")
            , ("contents" , Json.Encode.string b) ]

        ErrorFlashMessage b ->
            Json.Encode.object [ ("tag" , Json.Encode.string "ErrorFlashMessage")
            , ("contents" , Json.Encode.string b) ]


flashMessageDecoder : Json.Decode.Decoder FlashMessage
flashMessageDecoder =
    Json.Decode.field "tag" Json.Decode.string |>
    Json.Decode.andThen (\a -> case a of
        "SuccessFlashMessage" ->
            Json.Decode.succeed SuccessFlashMessage |>
            Json.Decode.Pipeline.required "contents" Json.Decode.string

        "ErrorFlashMessage" ->
            Json.Decode.succeed ErrorFlashMessage |>
            Json.Decode.Pipeline.required "contents" Json.Decode.string

        _ ->
            Json.Decode.fail "No matching constructor")


type alias User  =
    { errors : List (String , Violation) }


userEncoder : User -> Json.Encode.Value
userEncoder a =
    Json.Encode.object [ ("errors" , Json.Encode.list (\b -> case b of
        (c , d) ->
            Json.Encode.list identity [ Json.Encode.string c
            , violationEncoder d ]) a.errors) ]


userDecoder : Json.Decode.Decoder User
userDecoder =
    Json.Decode.succeed User |>
    Json.Decode.Pipeline.required "errors" (Json.Decode.list (Json.Decode.map2 Tuple.pair (Json.Decode.index 0 Json.Decode.string) (Json.Decode.index 1 violationDecoder)))