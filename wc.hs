module Main where

import Control.Monad (void)
import Data.Bits (Bits ((.&.), (.|.)))
import Data.Char (isSpace)
import Data.Either (fromRight)
import Data.Foldable (traverse_)
import Distribution.Compat.CharParsing (option)
import System.Environment (getArgs)
import System.IO (FilePath, Handle, IOMode (ReadMode), hClose, hGetChar, openFile, stdin)
import System.IO.Error (isEOFError, tryIOError)

type ErrString = String

data CommandOption = Option Int String

lineOption = Option 1 "l"

wordOption = Option 2 "w"

byteOption = Option 4 "c"

allOption = Option 7 "lwc"

noOption = Option 0 ""

addOption :: CommandOption -> CommandOption -> CommandOption
addOption (Option x a) (Option y b) = Option (x .|. y) $ if (x .&. y) == x then a else a <> b

haveOption :: CommandOption -> CommandOption -> Bool
haveOption (Option x _) (Option y _) = x .&. y == y


type Count = Either IOError (Int, Int, Int)

readLoop :: Handle -> Int -> Int -> Int -> Bool -> IO Count
readLoop file lines words chars inWord = do
  result <- tryIOError $ hGetChar file
  let wordCount = if not inWord then words + 1 else words
      charCount = chars + 1
      lineCount = lines + 1
      counter c
        | c == '\n' = readLoop file lineCount words charCount False
        | isSpace c = readLoop file lines words charCount False -- covers for \t as well
        | otherwise = readLoop file lines wordCount charCount True
   in case result of
        Left err -> return $ if isEOFError err then Right (lines, words, chars) else Left err
        Right c -> counter c

parseArgs :: [String] -> Either ErrString CommandOption -> ([FilePath], CommandOption, Maybe ErrString)
parseArgs args _options
  | not (null args) && head (head args) == '-' =
      let currentArg = drop 1 $ head args
          nextOptionCalc option optionVal
            | option == 'l' = addOpt lineOption
            | option == 'w' = addOpt wordOption
            | option == 'c' = addOpt byteOption
            | otherwise = Left $ "illegal argument " ++ show option
            where
              addOpt = Right . addOption optionVal

          parseOption options option = case options of
            Left _ -> options
            Right optionVal -> nextOptionCalc option optionVal
          nextArgs = drop 1 args
          nextOptions = foldl parseOption _options currentArg
       in parseArgs nextArgs nextOptions
  | otherwise = case _options of
      Left err -> ([], noOption, Just err)
      Right optionVal ->
        ( args,
          if haveOption optionVal noOption
            then allOption
            else optionVal,
          Nothing
        )

readFile' :: Handle -> IO Count
readFile' file = do
  count <- readLoop file 0 0 0 False
  hClose file
  return count

main :: IO ()
main = do
  args <- getArgs
  case parseArgs args $ Right noOption of
    (_, _, Just err) -> putStrLn err
    (filePaths, _option, Nothing) -> do
      files <- getFiles filePaths
      iteratedResult <- mapM iterateFile files

      let finalFilePaths =
            if null filePaths
              then [""] -- represents stdin
              else filePaths
          result = zipWith (getOutput _option) finalFilePaths iteratedResult
          defaultCount = fromRight (0, 0, 0)
          sumFn (a, b, c) (d, e, f) = (a + d, b + e, c + f)
          totalCalc x y =
            Right . (sumFn . defaultCount) x $ defaultCount y
          totalCount = foldl1 totalCalc iteratedResult
          finalResult
            | length files <= 1 = result
            | otherwise = result ++ [getOutput _option "total" totalCount]
       in traverse_ putStrLn finalResult

iterateFile :: Either IOError Handle -> IO Count
iterateFile (Right handle) = readFile' handle
iterateFile (Left err) = return (Left err)

getFiles :: [FilePath] -> IO [Either IOError Handle]
getFiles [] = return [Right stdin]
getFiles files = mapM getFile files -- not a lazy operatioss

getFile :: FilePath -> IO (Either IOError Handle)
getFile filePath = tryIOError $ openFile filePath ReadMode

getOutput :: CommandOption -> FilePath -> Count -> String
getOutput _ _ (Left err) = show err
getOutput _option filePath (Right (lines, words, chars)) =
  let filterFn (option, _) = haveOption _option option
      outputFn output (_, count) = output ++ show count ++ " "
      fn = foldl outputFn "" . filter filterFn
   in fn [(lineOption, lines), (wordOption, words), (byteOption, chars)] ++ filePath