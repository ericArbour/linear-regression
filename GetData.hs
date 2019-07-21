module GetData ( getData ) where

split :: Char -> String -> (String,String)
split delim s = (fst', tail snd')
  where breakOn = break (==delim)
        (fst',snd') = breakOn s

readTup :: (String, String) -> (Double, Double)
readTup (a,b) = (read a, read b)

parseCSV :: String -> [(Double,Double)]
parseCSV = map (readTup . split ',') . tail . words

getData :: String -> IO [(Double,Double)]
getData filePath = do
  contents <- readFile filePath
  return $ parseCSV contents
