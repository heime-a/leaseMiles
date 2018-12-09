port module Main exposing (Model, Msg(..), Status(..), formatResult,
 formatStatus, init, main, saveLocal, subscriptions, update, validate, view, viewInput)

import Array exposing (..)
import Browser
import Browser.Navigation as Nav
import Date exposing (..)
import FormatNumber exposing (format)
import FormatNumber.Locales exposing (spanishLocale, usLocale)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import Task
import Time exposing (..)
import Url exposing (..)



-- MAIN


main =
    Browser.application
        { init = init
        , onUrlChange = LinkClicked
        , onUrlRequest = UrlChanged
        , update = update
        , view = view
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { startDate : String
    , startDateStatus : Status
    , currentDate : String
    , currentDateStatus : Status
    , milesPerYear : String
    , milesPerYearStatus : Status
    }


type Status
    = Empty
    | Valid
    | Invalid String


init : Array String -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    let
        start =
            Maybe.withDefault "" (get 0 flags)

        curr =
            Maybe.withDefault "" (get 1 flags)

        miles =
            Maybe.withDefault "" (get 2 flags)
    in
    ( Model start Valid curr Valid miles Valid, Cmd.none )



-- UPDATE


type Msg
    = StartDate String
    | CurrentDate String
    | MilesPerYear String
    | LinkClicked Url.Url
    | UrlChanged Browser.UrlRequest
    | Noop
    | HandleTime (Result String Posix)


port saveLocal : String -> Cmd msg
port saveMiles : String -> Cmd msg


update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
    case msg of
        StartDate startDate ->
            ( validate { model | startDate = startDate }, saveLocal startDate )

        CurrentDate currentDate ->
            ( validate { model | currentDate = currentDate }, Cmd.none )

        MilesPerYear milesPerYear ->
            ( validate { model | milesPerYear = milesPerYear }, saveMiles milesPerYear )

        LinkClicked urlRequest ->
            case urlRequest of
                _ ->
                    ( model, Cmd.none )

        UrlChanged url ->
            ( model, Cmd.none )

        Noop ->
            ( model, Cmd.none )

        HandleTime result ->
            ( model, Cmd.none )


validate : Model -> Model
validate model =
    let
        errDate =
            Date.fromCalendarDate 1980 Jan 1

        startDate =
            case Date.fromIsoString model.startDate of
                Ok dt ->
                    dt

                Err msg ->
                    errDate

        startDateStatus =
            if startDate == errDate then
                Invalid "Please Enter YYYY-MM-DD"

            else
                Valid

        currentDate =
            case Date.fromIsoString model.currentDate of
                Ok dt ->
                    dt

                Err msg ->
                    errDate

        currentDateStatus =
            if currentDate == errDate then
                Invalid "Please Enter YYYY-MM-DD"

            else
                Valid

        milesPerYearStatus =
            if String.toInt model.milesPerYear == Nothing then
                Invalid "Enter a number"

            else
                Valid
    in
    { model
        | startDateStatus = startDateStatus
        , currentDateStatus = currentDateStatus
        , milesPerYearStatus = milesPerYearStatus
    }



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


formatStatus : Status -> Html Msg
formatStatus status =
    case status of
        Invalid msg ->
            div [ style "color" "red" ] [ text msg ]
            
        _ ->
            div [] []


formatResult : Model -> Float -> Html Msg
formatResult model result =
    let
        bad =
            model.startDateStatus /= Valid || model.currentDateStatus /= Valid || model.milesPerYearStatus /= Valid
    in
    if bad then
        div [] []

    else
        div [ style "color" "green" ] [ text (format usLocale result) ]


view : Model -> Browser.Document Msg
view model =
    let
        milesPerYear =
            String.toInt model.milesPerYear

        sdate =
            case Date.fromIsoString model.startDate of
                Ok dt ->
                    dt

                Err msg ->
                    Date.fromCalendarDate 1980 Jan 1

        cdate =
            case Date.fromIsoString model.currentDate of
                Ok dt ->
                    dt

                Err msg ->
                    Date.fromCalendarDate 1980 Jan 1

        daysDiff =
            Date.diff Days sdate cdate

        yearFrac =
            toFloat daysDiff / 365.0

        milesShouldBe =
            toFloat (Maybe.withDefault 0 milesPerYear) * yearFrac
    in
    { title = "Lease Calculator"
    , body =
        [ div []
            [ h1 [] [ text "Lease Calculator" ]
            , viewInput "text" "Start Date" model.startDate StartDate
            , formatStatus model.startDateStatus
            , viewInput "text" "Current Date" model.currentDate CurrentDate
            , formatStatus model.currentDateStatus
            , viewInput "text" "Miles Per Year" model.milesPerYear MilesPerYear
            , formatStatus model.milesPerYearStatus
            , formatResult model milesShouldBe
            ]
        ]
    }


viewInput : String -> String -> String -> (String -> msg) -> Html msg
viewInput t p v toMsg =
    input [ type_ t, placeholder p, value v, onInput toMsg ] []
