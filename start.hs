{-# LANGUAGE RecordWildCards, DeriveDataTypeable, ScopedTypeVariables #-}

module Main where 

import           Control.Category
import           Control.Lens hiding ((<.>))
import qualified Data.ByteString.Char8 as B 
import           Data.IORef 
import           Data.Monoid 
import           Data.Time.Clock 
import           Database.CouchDB
import           Graphics.Rendering.Cairo 
import           Graphics.UI.Gtk hiding (get,set)
import           System.Directory 
import           System.FilePath
import           Text.JSON
import           Text.JSON.Pretty
import           Text.JSON.Generic
-- 
import           Data.Hoodle.BBox
import           Data.Hoodle.Simple
import           Graphics.Hoodle.Render
import           Graphics.Hoodle.Render.Item 
import           Hoodle.Script
import           Hoodle.Script.Hook
import           Hoodle.StartUp
-- 
import Prelude hiding ((.),id)


data RecentPath = RecentPath { rpPath :: FilePath 
                             }
                deriving (Show, Typeable, Data)



hoodledb = db "hoodle"

rpDoc = doc "recent_path"


-- |
main :: IO ()
main = do  
    cdir <- getCurrentDirectory 
    mr <- runCouchDB' (getDoc hoodledb rpDoc)
    (rev1,dirinfo) <- 
      case mr of 
        Nothing -> do 
          Right trev<- runCouchDB' (newNamedDoc hoodledb rpDoc (toJSON (RecentPath cdir)))
          return (trev,cdir)
        Just (_,trev,j) -> do 
          print j 
          case fromJSON j of 
            Ok (RecentPath ndir) -> return (trev,ndir) 
            Error err -> error err
    pathref <- newIORef (dirinfo :: FilePath) 
    hoodleStartMain defaultScriptConfig 
           { message = Just welcomeMessage 
           , hook = Just (newhook pathref) } 
    lastpath <- readIORef pathref
    runCouchDB' $ updateDoc hoodledb (rpDoc,rev1) (toJSON (RecentPath lastpath))
    return () 
       -- { message = welcomeMessage } 

-- | 
newhook :: IORef FilePath -> Hook 
newhook ref = 
  defaultHook { saveAsHook = Nothing -- sahk 
              , afterSaveHook = Just (memoPath ref) -- Nothing -- Just ashk
              , afterOpenHook = Nothing 
              , afterUpdateClipboardHook = Just auch 
              , customContextMenuTitle = Just "take a memo"
              , customContextMenuHook = Just custommenuhook 
              , fileNameSuggestionHook = Just fnamesuggest
              , recentFolderHook = Just (giveMemoed ref)
              , embedPredefinedImageHook = Just (return "/home/wavewave/test.png")
              } 


memoPath :: IORef FilePath -> FilePath -> Hoodle -> IO () 
memoPath ref fp _ = do 
  prev <- readIORef ref
  putStrLn $ "in memopath, prev = " ++ prev
  let (dir,fn) = splitFileName fp 
  writeIORef ref dir
  putStrLn $ "in memopath, dir = " ++ dir 
  
  
giveMemoed :: IORef FilePath -> IO FilePath   
giveMemoed ref = readIORef ref 

-- |
makesvg :: [Item] -> FilePath -> IO () 
makesvg itms fp = do 
  mr <- renderitems itms 
  case mr of 
    Nothing -> return () 
    Just (BBox (ulx,uly) (lrx,lry),r) -> 
      withSVGSurface fp (lrx-ulx) (lry-uly) $ \s -> renderWith s r

-- |
makepng :: [Item] -> FilePath -> IO () 
makepng itms fp = do 
  mr <- renderitems itms 
  case mr of 
    Nothing -> return () 
    Just (BBox (ulx,uly) (lrx,lry),r) -> 
      withImageSurface FormatARGB32 (floor (lrx-ulx)) (floor (lry-uly)) $ 
        \s -> do 
          renderWith s r 
          surfaceWriteToPNG s fp 

-- |
renderitems :: [Item] -> IO (Maybe (BBox, Render ()))
renderitems itms = do
  ritms <- mapM cnstrctRItem itms 
  let ulbbox = unUnion . mconcat . fmap (Union . Middle . getBBox) $ ritms
  case ulbbox of 
    Middle (bbox@(BBox (ulx,uly) (lrx,lry))) -> 
      let r = do translate (-ulx) (-uly)  
                 mapM_ renderRItem ritms  
      in return (Just (bbox,r))
    _ -> return Nothing 


-- |
custommenuhook :: [Item] -> IO ()
custommenuhook is = do 
  ctime <- getCurrentTime
  -- makesvg is ("/home/wavewave/Dropbox/memos" </> show ctime <.> "svg") 
  makepng is ("/home/wavewave/Dropbox/memos" </> show ctime <.> "png") 

-- |
fnamesuggest :: IO FilePath 
fnamesuggest = do 
  ctime <- getCurrentTime
  return (show ctime <.> "hdl")

          
-- | after update clipboard hook
auch :: [Item] -> IO ()
auch itms = putStrLn ("length of strokes = " ++ show (length itms))


-- | 
sahk :: Hoodle -> IO () 
sahk hdl = do 
  let ttl = view title hdl
  putStrLn $ show ttl 
  putStrLn "Save As Hook called"

-- |
ashk :: Hoodle -> IO ()
ashk hdl = do 
  putStrLn "AfterSaveHook called"
  dialog <- messageDialogNew Nothing [DialogModal] 
              MessageQuestion ButtonsOkCancel 
              "Do you want to upload your file now?"
  res <- dialogRun dialog 
  case res of
    ResponseOk -> do 
      widgetDestroy dialog
      putStrLn "you pushed ok."      
      B.putStrLn (view title hdl)
      return ()
    _ -> do 
      widgetDestroy dialog
      return ()
  
  
-- |
welcomeMessage :: String 
welcomeMessage = 
   " ===================================================\n\
   \ =                                                 =\n\ 
   \ =        welcome to hoodle                        =\n\
   \ =                                                 =\n\
   \ =           Copyright 2011-2013  Ian-Woo Kim      =\n\
   \ =                                                 =\n\
   \ =                            Date: 2013.01.03     =\n\
   \ =                                                 =\n\
   \ ===================================================\n"
