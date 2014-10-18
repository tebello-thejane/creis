{-# LANGUAGE OverloadedStrings, OverloadedLists #-}

import Data.List.Split (splitOn)
import System.Console.Haskeline
import Data.Sequence (fromList)
import qualified Data.Sequence as Sequence
import qualified Data.Text as Text
import Data.Text (Text)
import Data.Monoid ((<>))
import qualified Data.Foldable as Fold (foldr)

narrowReplacements :: Sequence.Seq (Text, Text)
narrowReplacements = fromList [
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
broadReplacements = fromList [
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
  (Text.reverse . Text.tail . Text.reverse $ (entry !! 1)) <> transformEntry broadReplacements cleaned <> "  --  " <>
  cleaned <> "  --  " <>
  entry !! 3 <> " " <> entry !! 4 <> "  --  " <>
  head entry
  where
    cleaned = transformEntry narrowReplacements (entry !! 2)

substs :: Sequence.Seq (Text, Text) ->  Sequence.Seq (Text -> Text)
substs = fmap (uncurry Text.replace)

transformEntry :: Sequence.Seq (Text, Text) -> Text -> Text
transformEntry reps entry = Fold.foldr (\f el -> f el ) entry (substs reps)

--transformLine :: [Text] -> [Text]
--transformLine ln = toList . update 2 (transformEntry (ln !! 2)) . fromList $ ln

main :: IO ()
main = do
  content <- readFile "Tswana.Creissels1996.txt"
  let entries = map (map Text.pack . splitOn "\t") . tail . lines $ content
  runInputT defaultSettings (loop entries)

loop :: [[Text]] -> InputT IO ()
loop entries = do
  inp <- getInputLine "Mola: "
  case inp of
    Nothing -> return ()
    Just x -> do
      let n = read x :: Int
      let entry = entries !! n
      outputStrLn . Text.unpack . cleanEntry $ entry
      loop entries

