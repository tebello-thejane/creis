{-# LANGUAGE OverloadedStrings, OverloadedLists #-}

import Data.List.Split (splitOn)
import System.Console.Haskeline
import qualified Data.Sequence as Sequence
import qualified Data.Text as Text
import Data.Text (Text)
import Data.Monoid ((<>))
import qualified Data.Foldable as Fold (foldl)
import Data.Char (toUpper)
import System.IO.Unsafe (unsafePerformIO) --this will most probably bite me...
import qualified Text.Regex.Posix as RE
import Text.Regex.Posix.String
import Control.Monad.State
import Safe (headMay)
import Text.Read (readMaybe)

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
      ("ny\768", "n\768"),
      ("ng\768", "n\768"),
      ("ny\769", "n\769"),
      ("ng\769", "n\769"),
      ("\143", "tš"),
      ("S", "sh"),
      ("Â", "tl"),
      ("j", "y"),
      ("\152", "j")
  ]

broadReplacements :: Sequence.Seq (Text, Text)
broadReplacements = Sequence.fromList [
      ("ɔ", "o"),
      ("ɛ", "e"),
      ("ʊ", "o"),
      ("ɪ", "e"),
      ("\768", ""),
      ("\769", ""),
      ("tš", "tsh")
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
transformEntry reps entry = Fold.foldl (\cur subst -> subst cur) entry (substs reps)

data InputChoice = Exit | Find Text | Line Int

data LanguageChoice = Sesotho | English

data MessageType = Prompt | Mistake | NotFound

messageText :: MessageType -> LanguageChoice -> String
messageText Prompt Sesotho = "\n(F)umana mantswe (ka diRegEx), e-ya mo(l)eng, kapa o kgaots(e) (enter X to switch to English)\n:"
messageText Prompt English = "\n(F)ind words (using RegExs), go to a (l)ine, or (e)xit (tlanya X ho fetola puo ho Sesotho)\n:"
messageText Mistake Sesotho = "O entse phoso."
messageText Mistake English = "You made a mistake."
messageText NotFound Sesotho = "Ha le yo."
messageText NotFound English = "Not found."

switchLang :: LanguageChoice -> LanguageChoice
switchLang Sesotho = English
switchLang English = Sesotho

doPrompt :: StateT LanguageChoice (InputT IO) InputChoice
doPrompt = do
  curLang <- get
  inp <- (lift . getInputLine) . messageText Prompt $ curLang
  case inp of
    Nothing -> return Exit
    Just x -> do
      let comm = headMay x
      case comm of
        Nothing -> doPrompt --ignore empty input
        Just y  ->
          case toUpper y of
            'E' -> return Exit
            'F' -> return (Find (Text.strip . Text.pack . tail $ x))
            'L' -> do
              let lineNum = readMaybe . tail $ x :: Maybe Int
              case lineNum of
                Nothing -> errorAndRetry
                Just k  -> return (Line k)
            'X' -> do
              let newLang = switchLang curLang
              put newLang
              doPrompt
            _   -> errorAndRetry
  where
    errorAndRetry = do
      curLang <- get
      (lift . outputStrLn) . messageText Mistake $ curLang
      doPrompt

findEntry :: Text -> StateT LanguageChoice (InputT IO) ()
findEntry s = do
  let srch = filter (simpleREMatch s . head) searchableEntries
  curLang <- get
  case srch of
    [] -> lift . outputStrLn . messageText NotFound $ curLang
    _  -> mapM_ (lift . outputStrLn . Text.unpack . cleanEntry) srch

{-# NOINLINE entries #-}
entries :: [[Text]]
entries = unsafePerformIO $ do
  content <- readFile "Tswana.Creissels1996.txt"
  return . map (map Text.pack . splitOn "\t") . tail . lines $ content

cleanEntries :: [Text]
cleanEntries =
  map cleanEntry searchableEntries

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
      findEntry s
      loop

