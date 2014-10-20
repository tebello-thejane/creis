{-# LANGUAGE OverloadedStrings, OverloadedLists #-}

import Data.List.Split (splitOn)
import System.Console.Haskeline
import qualified Data.Sequence as Sequence
import qualified Data.Text as Text
import Data.Text (Text)
import Data.Monoid ((<>))
import qualified Data.Foldable as Fold (foldr)
import Data.Char (toUpper)
import Data.Function (on)
import Data.List (find)
import System.IO.Unsafe (unsafePerformIO) --this will most probably bite me...

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
  (head cleaned) <> "  --  " <>
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
--transformLine :: [Text] -> [Text]
--transformLine ln = toList . update 2 (transformEntry (ln !! 2)) . fromList $ ln

data InputChoice = Exit | Find Text | Line Int

doPrompt :: InputT IO InputChoice
doPrompt = do
  inp <- getInputLine "(F)umana lentswe (ka diRegEx), e-ya mo(l)eng, kapa o kgaots(e)\n:"
  case inp of
    Nothing -> return Exit
    Just x ->
      case toUpper . head $ x of
        'E' -> return Exit
        'F' -> return (Find (Text.strip . Text.pack . tail . tail $ x))
        'L' -> return (Line ((read . tail $ x)::Int))
        _   -> do
          outputStrLn "O entse phoso.\n"
          doPrompt

findEntry :: Text -> InputT IO ()
findEntry s = do
  let srch = find (\el -> head el == s ) searchableEntries
  case srch of
    Nothing -> outputStrLn "Ha le yo."
    Just x -> outputStrLn . Text.unpack . cleanEntry $ x

entries :: [[Text]]
entries = unsafePerformIO $ do
  content <- readFile "Tswana.Creissels1996.txt"
  return . map (map Text.pack . splitOn "\t") . tail . lines $ content

cleanEntries :: [Text]
cleanEntries =
  map cleanEntry entries

main :: IO ()
main =
  runInputT defaultSettings loop

loop :: InputT IO ()
loop = do
  inp <- doPrompt
  case inp of
    Exit -> return ()
    Line n -> do
      let entry = cleanEntries !! n
      outputStrLn . Text.unpack $ entry
      loop
    Find s -> do
      findEntry s
      loop

