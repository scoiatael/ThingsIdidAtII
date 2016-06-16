import qualified StawyCLI

import Graphics.UI.Gtk.Glade
import Graphics.UI.Gtk

import Paths_stawy(getDataFileName)

import Control.Concurrent
import Control.Monad(void)
import Control.Concurrent.MVar
import System.IO

main = do
  gladepaths <- getDataFileName "stawy.glade"
  initGUI
  timeoutAddFull (yield >> return True) priorityDefaultIdle 100
  gui <- loadGlade gladepaths
  connectGUI gui
  widgetShowAll (startWindow gui)
  mainGUI


data GUI = GUI { startWindow :: Window, sWLabel :: Label, sWEntry :: Entry,sWCancel :: Button, sWAccept :: Button, 
  waitWindow :: Window, wWLabel :: Label, wWProgressBar :: ProgressBar, wWAbandon :: Button, 
  resultWindow :: Window, rWLabel :: Label, rWTextView :: TextView, rWQuit :: Button, rWAgain :: Button}
 
loadGlade path = do
  Just xml <- xmlNew path
  sw <- xmlGetWidget xml castToWindow "startWindow"
  rw <- xmlGetWidget xml castToWindow "resultWindow"
  ww <- xmlGetWidget xml castToWindow "waitWindow"
  [swc, swa, wwa, rwq, rwa] <- mapM (xmlGetWidget xml castToButton) ["sWCancel", "sWAccept", "wWAbandon", "rWQuit", "rWAgain"]
  [swl, wwl, rwl] <- mapM (xmlGetWidget xml castToLabel) ["sWLabel", "wWLabel", "rWLabel"]
  swe <- xmlGetWidget xml castToEntry "sWEntry"
  wwpb <- xmlGetWidget xml castToProgressBar "wWProgressBar"
  rwtv <- xmlGetWidget xml castToTextView "rWTextView"
  return $ GUI sw swl swe swc swa ww wwl wwpb wwa rw rwl rwtv rwq rwa

connectGUI gui = do
--startWindow: label, entry, cancel, accept
  thrdidcont <- newEmptyMVar
  onDestroy (startWindow gui) mainQuit 
  onClicked (sWCancel gui) mainQuit
  onClicked (sWAccept gui) (moveToWait thrdidcont)
  onEntryActivate (sWEntry gui) (moveToWait thrdidcont)
--waitWindow: label, progressbar, abandon
  onDestroy (waitWindow gui) mainQuit
  onClicked (wWAbandon gui) (auxKillThreadFromCont thrdidcont)
--resultWindow: label, textview, quit, again
  onDestroy (resultWindow gui) mainQuit
  onClicked (rWQuit gui) mainQuit
  onClicked (rWAgain gui) (moveToStart)
  where
    moveToWait tcont = do
      widgetHideAll $ startWindow gui
      widgetHideAll $ resultWindow gui
      widgetShowAll $ waitWindow gui
      filename <- entryGetText (sWEntry gui)
      entrySetText (sWEntry gui) "" 
      clock <- newEmptyMVar
      thrid <- forkIO (StawyCLI.refresh clock)
      t <- forkFinally (stawy filename (StawyCLI.notify (progressBarPulse (wWProgressBar gui)) clock)) (\a -> do { t <- tryTakeMVar tcont; killThread thrid; moveToResult a; })
      putMVar tcont t
      putStrLn "done"
 
    auxKillThreadFromCont tcont = do
      t <- tryTakeMVar tcont
      auxKillThread t

    auxKillThread t = case t of 
      Just tid -> killThread tid
      otherwise -> putStr ""

    moveToResult ewyniki = do
      rWTextBuffer <- textViewGetBuffer (rWTextView gui)
      let wyniki = is_error ewyniki
      if (fst wyniki == False) 
        then do 
          textBufferSetText rWTextBuffer $ snd wyniki
          widgetHideAll $ waitWindow gui
          widgetHideAll $ startWindow gui
          widgetShowAll $ resultWindow gui
        else do
          hPutStrLn stderr (snd wyniki)
          moveToStart
      

    is_error e = case e of
        Left exc -> (True, "Sorry, some error occured: " ++ (show exc))
        Right str -> (False, str)

    moveToStart = do
      widgetHideAll $ resultWindow gui
      widgetHideAll $ waitWindow gui
      widgetShowAll $ startWindow gui

stawy :: FilePath -> IO () -> IO String
stawy file move = do 
  content <- readFile file
  let wdane = StawyCLI.wczytajDane content
  rozwiazanie <- StawyCLI.rozwiaz move wdane
  return $ concatMap (++"\n") $ map (StawyCLI.ladnieFormatuj (StawyCLI.doWstawienia wdane)) $ filter StawyCLI.dobreRozwiazanie $ rozwiazanie

