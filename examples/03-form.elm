import Char exposing (isDigit, isLower, isUpper)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)


main =
  Html.beginnerProgram
    { model = model
    , view = view
    , update = update
    }



-- MODEL


type alias Model =
  { name : String
  , password : String
  , passwordAgain : String
  }


model : Model
model =
  Model "" "" ""



-- UPDATE


type Msg
    = Name String
    | Password String
    | PasswordAgain String


update : Msg -> Model -> Model
update msg model =
  case msg of
    Name name ->
      { model | name = name }

    Password password ->
      { model | password = password }

    PasswordAgain password ->
      { model | passwordAgain = password }



-- VIEW


view : Model -> Html Msg
view model =
  div []
    [ input [ type_ "text", placeholder "Name", onInput Name ] []
    , input [ type_ "password", placeholder "Password", onInput Password ] []
    , input [ type_ "password", placeholder "Re-enter Password", onInput PasswordAgain ] []
    , viewValidation model
    ]


viewValidation : Model -> Html msg
viewValidation model =
  let
    (color, message) =
      if not (isPasswordLengthValid model.password) then
        ("red", "The password is too short")
      else if not (passwordContainsDifferentCharacterClasses model.password) then
        ("red", "The password needs to contain upper case, lower case, and numeric characters.")
      else if model.password == model.passwordAgain then
        ("green", "OK")
      else
        ("red", "Passwords do not match!")
  in
    div [ style [("color", color)] ] [ text message ]

isPasswordLengthValid : String -> Bool
isPasswordLengthValid password =
  String.length password >= 8

passwordContainsDifferentCharacterClasses : String -> Bool
passwordContainsDifferentCharacterClasses password =
  let
    charList = String.toList password
    hasUpper = List.any Char.isUpper charList
    hasLower = List.any Char.isLower charList
    hasNumeric = List.any Char.isDigit charList
  in
    hasUpper && hasLower && hasNumeric
