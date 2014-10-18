{-# LANGUAGE OverloadedStrings, OverloadedLists #-}
import Data.List.Split (splitOn)
import System.Console.Haskeline
import Data.Sequence (fromList, update)
import qualified Data.Sequence as Sequence
import Data.Foldable (toList)
import qualified Data.Text as Text
import Data.Text (Text)
import Data.Monoid ((<>))
import qualified Data.Foldable as Fold (foldr)

replacements :: Sequence.Seq (Text, Text)
replacements = fromList [
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

substs :: Sequence.Seq (Text -> Text)
substs = fmap (uncurry Text.replace) replacements

transformEntry :: Text -> Text
transformEntry entry = Fold.foldr (\f el -> f el ) entry substs

transformLine :: [Text] -> [Text]
transformLine ln = toList . update 2 (transformEntry (ln !! 2)) . fromList $ ln

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
      let transformedLine = transformLine . (!! n) $ entries
      outputStrLn . Text.unpack $ (head transformedLine <> " -- " <> (transformedLine !! 2) <> "    " <> (transformedLine !! 3) <> "  " <> (transformedLine !! 4))
      loop entries
      --putStrLn . (!!2) . (splitOn "\t") . (!!200) $ entries


