import System.Console.ANSI
import Control.Concurrent 
import Control.Monad(when)

pStars i = do
  putStr "*"
  threadDelay 1000000
  when (i > 0) $ pStars $ i-1 

main = do
  pStars 8
  cursorBackward 9
  putStr "         "
  cursorBackward 10
  main
