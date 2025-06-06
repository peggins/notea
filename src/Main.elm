port module Main exposing (..)

import Browser
import File.Download as FDownload
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Decode
import Json.Encode as Encode
import List exposing (map)
import Task



{- PORTS FOR JS INTERACTION
   These ports handle communication between Elm and JavaScript:
   - saveNote: Sends a note (title, content) to JS for storage
   - deleteNote: Requests JS to remove a specific note
   - requestNotes: Asks JS to send back all saved notes
   - receiveNotes: Subscribes to incoming notes from JS
-}


port saveNote : ( String, String, String ) -> Cmd msg


port deleteNote : ( String, String, String ) -> Cmd msg


port requestNotes : () -> Cmd msg


port receiveNotes : (Decode.Value -> msg) -> Sub msg



{- MODELS
   Defines the structure of our application state:
   - title: Current note title being edited
   - content: Current note content being edited
   - savedNotes: List of all persisted notes
   - selectedFont: List of all fonts family
-}


type alias Model =
    { title : String
    , content : String
    , savedNotes : List Note
    , selectedFont : String
    }


systemFonts : List String
systemFonts =
    [ "Arial"
    , "Courier New"
    , "Georgia"
    , "Times New Roman"
    , "Verdana"
    , "Sans-serif"
    , "Serif"
    , "Helvetica"
    , "Tahoma"
    , "Trebuchet MS"
    ]



{- NOTE TYPE
   Represents the structure of a saved note with:
   - title: The note's title/identifier
   - content: The note's textual content
-}


type alias Note =
    { title : String
    , content : String
    , font : String
    }


initialModel : Model
initialModel =
    { title = ""
    , content = ""
    , savedNotes = []
    , selectedFont = "Arial"
    }



{- INITIALIZATION
   Sets up the application by:
   1. Starting with the initialModel
   2. Immediately requesting saved notes from JS storage
-}


init : () -> ( Model, Cmd Msg )
init _ =
    ( initialModel, requestNotes () )



{- MESSAGES
   Defines all possible actions in the application:
   - UpdateTitle/UpdateContent: Track form field changes
   - Save: Persist current note
   - Download: Export note as file
   - NotesReceived: Handle incoming notes from JS
   - LoadNote: Populate form with existing note
   - DeleteNote: Remove a saved note
-}


type Msg
    = UpdateTitle String
    | UpdateContent String
    | Save
    | Download
    | NotesReceived Decode.Value
    | LoadNote Note
    | DeleteNote Note
    | ChangeFont String



{- UPDATES
   Handles state transitions for each message type:
   - ChangeFont: Select fonts and change them in the system
   - Field updates: Modify model directly
   - Save: Validate then persist via ports
   - Download: Create file with proper naming
   - NotesReceived: Decode and store incoming notes
   - LoadNote: Populate editor fields
   - DeleteNote: Remove from both model and storage
-}


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangeFont font ->
            ( { model | selectedFont = font }, Cmd.none )

        UpdateTitle title ->
            ( { model | title = title }, Cmd.none )

        UpdateContent content ->
            ( { model | content = content }, Cmd.none )

        Save ->
            if String.isEmpty model.title || String.isEmpty model.content then
                ( model, Cmd.none )

            else
                ( model
                , Cmd.batch
                    [ saveNote ( model.title, model.content, model.selectedFont )
                    , requestNotes ()
                    ]
                )

        Download ->
            let
                filename =
                    if String.isEmpty model.title then
                        "notea-file.txt"

                    else
                        model.title ++ ".txt"
            in
            ( model
            , FDownload.string filename "text/plain" model.content
            )

        NotesReceived json ->
            case Decode.decodeValue savedNotesDecoder json of
                Ok notes ->
                    ( { model | savedNotes = notes }, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )

        LoadNote note ->
            ( { model | title = note.title, content = note.content, selectedFont = note.font }, Cmd.none )

        DeleteNote note ->
            let
                newNotes =
                    List.filter (\n -> n /= note) model.savedNotes
            in
            ( { model | savedNotes = newNotes }
            , Cmd.batch
                [ deleteNote ( note.title, note.content, note.font )
                , requestNotes ()
                ]
            )



{- JSON DECODER
   Converts JSON values into our Note type:
   - Expects an array of objects
   - Each object must have "title", "content" and "font" string fields
-}


savedNotesDecoder : Decode.Decoder (List Note)
savedNotesDecoder =
    Decode.list <|
        Decode.map3 Note
            (Decode.field "title" Decode.string)
            (Decode.field "content" Decode.string)
            (Decode.field "font" (Decode.string |> Decode.maybe |> Decode.map (Maybe.withDefault "Arial")))



-- VIEWS


view : Model -> Html Msg
view model =
    div [ class "notea-app" ]
        [ div [ class "header-controls" ]
            [ input
                [ type_ "text"
                , placeholder "Insert your title..."
                , value model.title
                , onInput UpdateTitle
                , class "title-input"
                , style "font-family" model.selectedFont
                ]
                []
            , select
                [ onInput ChangeFont
                , class "font-selector"
                , value model.selectedFont
                ]
                (List.map
                    (\font ->
                        option
                            [ value font
                            , style "font-family" font
                            ]
                            [ text font ]
                    )
                    systemFonts
                )
            ]
        , textarea
            [ placeholder "Lorem ipsum dolor sit amet..."
            , value model.content
            , onInput UpdateContent
            , class "content-textarea"
            , style "font-family" model.selectedFont
            ]
            []
        , div [ class "buttons" ]
            [ button
                [ onClick Save
                , class "save-btn"
                , disabled (String.isEmpty model.title || String.isEmpty model.content)
                ]
                [ text "Save" ]
            , button [ onClick Download, class "download-btn" ] [ text "Download" ]
            ]
        , ul [ class "notes-list" ] (List.map renderNote model.savedNotes)
        ]


renderNote : Note -> Html Msg
renderNote note =
    li [ onClick (LoadNote note), class "note-item" ]
        [ p [] [ text note.title ]
        , div [ class "note-content" ]
            [ button
                [ stopPropagationOn "click" (Decode.succeed ( DeleteNote note, True ))
                , class "delete-btn"
                ]
                [ text "Delete" ]
            ]
        ]



{- SUBSCRIPTIONS
   Listens for incoming notes from JavaScript
   through the receiveNotes port
-}


subscriptions : Model -> Sub Msg
subscriptions _ =
    receiveNotes NotesReceived



{- BOOTSTRAP
   Standard Elm application setup wiring together:
   - Initialization
   - View rendering
   - Update logic
   - Subscriptions
-}


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
