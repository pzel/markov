import Debug
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Random exposing (Generator(..), Seed, int, initialSeed, step)
import Tuple exposing (second)

main =
  Html.programWithFlags
    { init = init
    , view = view
    , update = update
    , subscriptions = always Sub.none
    }
type alias Config = { seed : Int }
type alias RNG = (Generator Int, Seed)
type Cleaned = Cleaned -- phantom
type Raw = Raw         -- phantom
type Text a = Text String
type alias Token = String
type alias Corpus = List Token
type alias Digrams = Dict (Token,Token) (List Token)

type alias Model =
  { input : Text Raw
  , desiredLength : Int
  , output : Maybe String
  , rng : RNG
  }

type Msg
  = Input String
  | SetDesired String
  | Generate


init : Config -> (Model, Cmd Msg)
init config =
  ({ input = rawString ""
   , desiredLength = 1000
   , output = Nothing
   , rng =  (initSeed config)}
  ,
    Cmd.none)

initSeed : Config -> RNG
initSeed c =
  (int 0 999999, initialSeed c.seed)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  let nextRng = newRng model.rng
  in case msg of
       Input s ->
         {model
           | input = rawString s
           , output = Nothing} ! []
       SetDesired s ->
         case String.toInt s of
           Err _ -> model ! []
           Ok i -> {model | desiredLength = i} ! []
       Generate ->
         {model
           | output = generate model.input model.desiredLength nextRng
           , rng = (newRng nextRng)} ! []

view : Model -> Html Msg
view model =
  div [] [
     button [onClick Generate] [ text "Generate" ]
    , input [ name "desired"
            , value (model.desiredLength |> Basics.toString)
            , onInput SetDesired
            ] []
    , span [] [text "words"]
    , div [] []
    , textarea [id "main-input"
               , autofocus True
               , onInput Input
               , value (fromRaw model.input)] []
    , div [id "main-output"] (outputFor model.output)
    ]


outputFor : Maybe String -> List (Html Msg)
outputFor o =
  case o of
    Nothing -> [text "I need at least 3 words of input"]
    Just i -> [text i]

rawString : String -> Text Raw
rawString = Text

fromRaw : Text Raw -> String
fromRaw (Text s) = s

clean : Text Raw -> Corpus
clean (Text s) =
  String.map replacePunctuation s
    |> String.words

replacePunctuation : Char -> Char
replacePunctuation c =
  if List.member c (String.toList "…+„”*—-:;\"()[]»«_")
     then ' ' else c

buildDigrams : Corpus -> Digrams
buildDigrams tokens =
  List.foldl
    (\(a,b,c) d -> Dict.update (a,b) (updateList c) d)
    Dict.empty
    (triplets tokens)

updateList : Token -> Maybe (List Token) -> Maybe (List Token)
updateList t m = case m of
                   Nothing -> Just [t]
                   Just l ->  Just (t :: l)

zip3 : List a -> List b -> List c -> List (a,b,c)
zip3 = List.map3 (\x y z -> (x,y,z))

triplets : List Token -> List (Token, Token, Token)
triplets l =
  case List.tail l of
    Nothing -> []
    Just t1 -> case List.tail t1 of
                 Nothing -> []
                 Just t2 -> zip3 l t1 t2

generate : Text Raw -> Int -> RNG -> Maybe String
generate raw length rng =
  let corpus = clean raw
      digrams = buildDigrams corpus
  in if (List.length corpus) < 3
     then Nothing
     else
       let ((t1,t2), newRng) = randomInitialSequence corpus rng
       in Just (fromDigrams digrams 100 rng (t1,t2) [])

fromDigrams : Digrams -> Int -> RNG -> (Token, Token) -> List Token -> String
fromDigrams d left rng (last1,last2) acc =
  if (left == 0)
  then (List.reverse(acc) |> String.join(" "))
  else
    case findNext d (last1,last2) rng of
      Just (t, newRng) ->
        fromDigrams d (left-1) newRng (last2,t) (last1::acc)
      Nothing ->
        (List.reverse(last2::last1::acc) |> String.join(" "))

findNext : Digrams -> (Token, Token) -> RNG -> Maybe (Token, RNG)
findNext d k (gen,seed) =
  case Dict.get k d of
    Nothing -> Nothing
    Just tokens ->
      let (v, newSeed) = Random.step gen seed
      in case List.head (List.drop (v % (List.length tokens)) tokens) of
           Just h -> Just (h, (gen,newSeed))
           Nothing -> Nothing

newRng : RNG -> RNG
newRng (gen,seed) =
  let (_, newSeed) = Random.step gen seed
  in (gen,newSeed)


randomInitialSequence : Corpus -> RNG -> ((Token,Token),RNG)
randomInitialSequence corpus (gen,seed) =
  let (v, newSeed) = Random.step gen seed
      idx = v % (List.length corpus)
      candidates = (List.drop v corpus |> List.take 2)
  in case candidates of
       [a,b] -> ((a,b), (gen,newSeed))
       _ -> randomInitialSequence corpus (gen,newSeed) -- shouldn't happen
