import System.Environment
import System.IO
import Assembler
import Machine
import Parsers
import qualified Data.ByteString as BS

main :: IO ()
main = do
  hSetBuffering stdin LineBuffering
  hSetBuffering stdout NoBuffering
  args <- getArgs
  case args of
    [fn] -> buildAndExecute fn
    ["-e", fn] -> execute fn
    ["-b", fn, outFile] -> build fn outFile
    _ -> putStrLn "invalid parameters"

buildAndExecute fn = do
  asm <- readFile fn
  case (assemble . lines) asm of
    (Left err) -> putStrLn err
    (Right byteCode) -> do
      machine <- makeMachine byteCode
      runMachine machine

build fn outFile = do
  asm <- readFile fn
  case (assemble . lines) asm of
    (Left err) -> putStrLn err
    (Right byteCode) -> do
      let bs = BS.pack byteCode
      BS.writeFile outFile bs

execute fn = do
  byteCodeString <- BS.readFile fn
  let byteCode = BS.unpack byteCodeString
  machine <- makeMachine byteCode
  runMachine machine
