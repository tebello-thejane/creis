{-# LANGUAGE OverloadedStrings, OverloadedLists #-}

import Data.List.Split (splitOn)
import System.Console.Haskeline
import qualified Data.Sequence as Sequence
import qualified Data.Text as Text
import Data.Text (Text)
import Data.Monoid ((<>))
import qualified Data.Foldable as Fold (foldr)
import Data.Char (toUpper)
import System.IO.Unsafe (unsafePerformIO) --this will most probably bite me...
import qualified Text.Regex.Posix as RE
import Text.Regex.Posix.String
import Control.Monad.State

simpleREMatch :: Text -> Text -> Bool
simpleREMatch rx str =
  let
    regex = RE.makeRegex (Text.unpack rx) :: Regex
  in
    RE.matchTest regex (Text.unpack str)

narrowReplacements :: Sequence.Seq (Text, Text)
narrowReplacements = Sequence.fromList [
      ("\148", "i"),
      ("ö", "ny"),
      ("O", "ɔ"),
      ("N", "ng"),
      ("qh", "kg"),
      ("tSh", "ch"),
      ("X", "g"),
      ("E", "ɛ"),
      ("¿", "ʊ"),
      ("é", "ɪ"),
      ("tÂ", "tl"),
      ("Ù", "\768"),
      ("Ú", "\769"),
      ("\143", "tš"),
      ("S", "sh")
  ]

broadReplacements :: Sequence.Seq (Text, Text)
broadReplacements = Sequence.fromList [
      ("ɔ", "o"),
      ("qh", "kg"),
      ("tSh", "ch"),
      ("X", "g"),
      ("ɛ", "e"),
      ("ʊ", "o"),
      ("ɪ", "e"),
      ("\768", ""),
      ("\769", ""),
      ("tš", "ts"),
      ("ngk", "nk"),
      ("nyny", "nny"),
      ("ngng", "nng"),
      ("nych", "nch"),
      ("nytš", "ntš")
  ]

cleanEntry :: [Text] -> Text
cleanEntry entry =
  head cleaned <> "  --  " <>
  (cleaned !! 1) <> "  --  " <>
  (cleaned !! 2) <> " " <> cleaned !! 3 <> "  --  " <>
  cleaned !! 4
  where
    cleaned = entry

searchableEntry :: [Text] -> [Text]
searchableEntry entry = [
    (Text.reverse . Text.tail . Text.reverse $ (entry !! 1)) <> transformEntry broadReplacements cleaned,
    cleaned,
    entry !! 3,
    entry !! 4,
    head entry
  ]
  where
    cleaned = transformEntry narrowReplacements (entry !! 2)

searchableEntries :: [[Text]]
searchableEntries =
  map searchableEntry entries

substs :: Sequence.Seq (Text, Text) ->  Sequence.Seq (Text -> Text)
substs = fmap (uncurry Text.replace)

transformEntry :: Sequence.Seq (Text, Text) -> Text -> Text
transformEntry reps entry = Fold.foldr (\f el -> f el ) entry (substs reps)

data InputChoice = Exit | Find Text | Line Int

data LanguageChoice = Sesotho | English

promptText :: LanguageChoice -> String
promptText Sesotho = "\n(F)umana mantswe (ka diRegEx), e-ya mo(l)eng, kapa o kgaots(e) (enter X to switch to English)\n:"
promptText English = "\n(F)ind words (using RegExs), go to a (l)ine, or (e)xit (tlanya X ho fetola puo ho Sesotho)\n:"

mistakeText :: LanguageChoice -> String
mistakeText Sesotho = "O entse phoso."
mistakeText English = "You made a mistake."

switchLang :: LanguageChoice -> LanguageChoice
switchLang Sesotho = English
switchLang English = Sesotho

doPrompt :: StateT LanguageChoice (InputT IO) InputChoice
doPrompt = do
  curLang <- get
  inp <- (lift . getInputLine) . promptText $ curLang
  case inp of
    Nothing -> return Exit
    Just x ->
      case toUpper . head $ x of
        'E' -> return Exit
        'F' -> return (Find (Text.strip . Text.pack . tail $ x))
        'L' -> return (Line ((read . tail $ x)::Int))
        'X' -> do
          let newLang = switchLang curLang
          put newLang
          doPrompt
        _   -> do
          (lift . outputStrLn) . mistakeText $ curLang
          doPrompt

findEntry :: Text -> InputT IO ()
findEntry s = do
  let srch = filter (simpleREMatch s . head) searchableEntries
  case srch of
    [] -> outputStrLn "Ha le yo."
    _  -> mapM_ (outputStrLn . Text.unpack . cleanEntry) srch

entries :: [[Text]]
{-# NOINLINE entries #-}
entries = unsafePerformIO $ do
  content <- readFile "Tswana.Creissels 1996.txt"
  return . map (map Text.pack . splitOn "\t") . tail . lines $ content

cleanEntries :: [Text]
cleanEntries =
  map cleanEntry entries

main :: IO ()
main =
  runInputT defaultSettings (evalStateT loop Sesotho)

loop :: StateT LanguageChoice (InputT IO) ()
loop = do
  inp <- doPrompt
  case inp of
    Exit -> return ()
    Line n -> do
      let entry = cleanEntries !! n
      lift . outputStrLn . Text.unpack $ entry
      loop
    Find s -> do
      lift . findEntry $ s
      loop

