-- Gyhe - a graphical user interface for Haskell
-- Part of the Yhc project
-- All code written by Mike Dodds <miked@cs.york.ac.uk>

module Main where

import Interact
import Evaluate
import YheIcon

import IO
import System
import Numeric
import Directory
import Data.IORef
import Data.Word
import Graphics.UI.Gtk

#ifdef GTKSOURCEVIEW
import Graphics.UI.Gtk.SourceView
#endif

name :: String
name = "Gyhe"

version :: String
version = "0.1"

main = do 
  initGUI

  -- add a timer to allow other threads to run
  -- timeoutAddFull (yield >> return True) priorityDefaultIdle 20

  -- load the configuration file
  home <- getEnv "HOME"
  let configfile = home ++ "/.gyhe"
  b <- doesFileExist configfile
  config <- if b then do cont <- readFile configfile
                         case (reads cont::[(ConfigFormat,String)]) of
                           [] -> return defaultConfig
                           x  -> return (read cont)
                 else return defaultConfig

  stateref <- initGuiState config

  -- Set up the main window 
  window <- createMainWindow stateref config
  
  windowbox <- vBoxNew False 0
  containerAdd window windowbox 

  -- Set up the menu bar
  menubar <- createMenuBar stateref  

  -- Set up the main box
  mainpane <- createPaned stateref

  -- pack the main box and menu bar
  boxPackStart windowbox menubar PackNatural 0
  boxPackStart windowbox mainpane PackGrow 0 

  -- show everything
  widgetShowAll window

  -- Perform post-show rearrangement
  currstate <- readIORef stateref
  if oneLineMode currstate 
    then moveMainPaneDivider stateref 1.0
    else moveMainPaneDivider stateref 0.8

  focusInputbox stateref

  mainGUI


{--
type IORefSafe a = ( IORef a, IORef Bool )

readIORefSafe :: IORefSafe a -> a 
readIORefSafe ( contentref, guardref ) = do
  writeIORef guardref True 
  readIORef contentref

writeIORefSafe :: IORefSafe a -> a -> IO ()
writeIORefSafe ( contentref, guardref ) newcont = do
  guardval <- readIORef guardref 
  if guardval then do writeIORef guardref False  
                      writeIORef contentref newcont
              else error "ERROR! IORefSafe became unsafe!"
--}


------------------------------------
-- The structures used in the gui --
------------------------------------

-- the GUI state - this is passed around in an IORef
data GuiState = GuiState {
  -- internal state
  guiStructure :: GuiStructure,
  interpState :: Interact,
  cmdHistory :: ([String], [String], [String]), 
  clickLinks :: [(TextTag, String, (String -> IO ()) )],

  -- preferences
  textboxFont :: FontDescription,
  guiIcon :: Pixbuf, 
  oneLineMode :: Bool,
  textEditor :: String,

  -- text colors
  inputColor :: Color,
  outputColor :: Color, 
  errorColor :: Color
  } 


-- A datatype holding gui elements which we need to manipulate
data GuiStructure = GuiStructure {
  mainWindow :: Maybe Window,
#ifdef GTKSOURCEVIEW
  inputBox :: Maybe SourceView,
#endif 
#ifndef GTKSOURCEVIEW
  inputBox :: Maybe TextView,
#endif
  outputBox :: Maybe TextView,
  mainPane :: Maybe VPaned,
  outputScrWindow :: Maybe ScrolledWindow
  }

defaultStructure :: GuiStructure 
defaultStructure = GuiStructure {
                      mainWindow = Nothing,
                      inputBox = Nothing,
                      outputBox = Nothing,
                      mainPane = Nothing,
                      outputScrWindow = Nothing
                      }

-- A format for reading and recording configuration data
-- All of the datatypes used have to be show-able - no FontDescrips!
data ConfigFormat = ConfigFormat {
  textboxFontString :: String,
  guiHeight :: Int,
  guiWidth :: Int,
  guiXRoot :: Int,
  guiYRoot :: Int,
  oneLineModeConf :: Bool,
  guiTextEditor :: String,
  incol :: (Word16,Word16,Word16),
  outcol :: (Word16,Word16,Word16),
  errcol :: (Word16,Word16,Word16)
  } deriving (Read, Show)

{-
instance Show Color where
  show (Color red green blue) = show (red,green,blue)

instance Read Color where
  read string = (Color red green blue)
    where 
      --red :: Word16
      --green :: Word16
      --blue :: Word16
      (red, green, blue) = read (Word16,Word16,Word16)::string
-}


-- set up the default values in case there isn't a config file
defaultConfig :: ConfigFormat
defaultConfig = ConfigFormat { 
                  textboxFontString = "Monospace 13",
                  guiWidth = 600, guiHeight = 400,
                  guiXRoot = 0, guiYRoot = 0,
                  oneLineModeConf = True, guiTextEditor = "",
                  incol = (0,0,0xFFFF),
                  outcol = (0,0,0),
                  errcol = (0xFFFF,0,0)
                  }


-- initialise the gui state 
initGuiState :: ConfigFormat -> IO (IORef GuiState)
initGuiState config = do 
  icon <- pixbufNewFromXPMData yheicon
  font <- fontDescriptionFromString (textboxFontString config)

  let (inr, ing, inb) = incol config
      (outr, outg, outb) = outcol config
      (errr, errg, errb) = errcol config

  let state = GuiState {
        guiStructure = defaultStructure, 
        interpState = emptyInteract,
        cmdHistory = ([],[],[]),
        clickLinks = [],

        textboxFont = font,
        guiIcon = icon,
        oneLineMode = oneLineModeConf config,
        textEditor = guiTextEditor config, 

        inputColor = Color inr ing inb,
        outputColor = Color outr outg outb,
        errorColor = Color errr errg errb
        }
  
  stateref <- newIORef state

  -- Have to do this after creating the stateref because of circular reference
  let interactstate = emptyInteract { 
                            interactExpr = evalYhc,
                            interactWriteOut = (writeOutput stateref),
                            interactWriteErr = (writeError stateref)
                            } 
      
  writeIORef stateref $ state { interpState = interactstate }
  return stateref


-- Save the status out to a file
saveConfig stateref = do
  currstate <- readIORef stateref
  let Just window = mainWindow ( guiStructure currstate )

  -- FIXME!!! this is a hacky way of ensuring we don't end up with a
  -- huge window after maximization
  windowUnmaximize window
  
  fontString <- fontDescriptionToString ( textboxFont currstate )
  (width, height) <- windowGetSize window
  (xroot, yroot) <- windowGetPosition window

  let Color inr inb ing = inputColor currstate
      Color outr outb outg = outputColor currstate
      Color errr errb errg = errorColor currstate

  let config = ConfigFormat { 
                 textboxFontString = fontString,
                 guiWidth = width,
                 guiHeight = height,
                 guiXRoot = xroot,
                 guiYRoot = yroot,
                 oneLineModeConf = oneLineMode currstate, 
                 guiTextEditor = textEditor currstate,
                 incol = (inr,inb,ing),
                 outcol = (outr,outb,outg),
                 errcol = (errr,errb,errg)
                 }

  home <- getEnv "HOME"
  writeFile ( home ++ "/.gyhe") (show config)


------------------------------------
-- Create the elements of the Gui --
------------------------------------

-- Create the main window
createMainWindow :: IORef GuiState -> ConfigFormat -> IO Window
createMainWindow stateref config = do
  window <- windowNew

  currstate <- readIORef stateref
  let newstruct = (guiStructure currstate) { mainWindow = Just window }
  writeIORef stateref (currstate { guiStructure = newstruct })
  
  windowSetIcon window (guiIcon currstate)
  window `onDestroy` (quitGui stateref)
  windowSetDefaultSize window (guiWidth config) (guiHeight config) 
  windowMove window (guiXRoot config) (guiYRoot config)
  windowSetTitle window name

  return window


-- Create a menu bar
createMenuBar stateref = do
  menubar <- menuBarNew

  -- The file menu 
  fileitem <- menuItemNewWithLabel "File" 
  filemenu <- menuNew 
  menuItemSetSubmenu fileitem filemenu
  
  fontselect <- menuItemNewWithLabel "Preferences"
  fontselect `onActivateLeaf` (popupPreferencesDialog stateref)
  containerAdd filemenu fontselect

{--
  texteditor <- menuItemNewWithLabel "Popup Text Editor"
  texteditor `onActivateLeaf` (openTextEditor stateref "" 0 0 )
  containerAdd filemenu texteditor 
--}

  quit <- menuItemNewWithLabel "Quit"
  quit `onActivateLeaf` (quitGui stateref)
  containerAdd filemenu quit 

  -- the help menu
  helpitem <- menuItemNewWithLabel "Help" 
  helpmenu <- menuNew 
  menuItemSetSubmenu helpitem helpmenu

  about <- menuItemNewWithLabel "About"
  onActivateLeaf about (popupAbout stateref)
  containerAdd helpmenu about 

  containerAdd menubar fileitem
  containerAdd menubar helpitem 
  return menubar


-- Create a paned view for the two text boxes
createPaned :: IORef GuiState -> IO VPaned 
createPaned stateref = do
  currstate <- readIORef stateref

  -- set up the output box 
  outputbox <- createOutputBox currstate
  widgetModifyFont outputbox (Just (textboxFont currstate) )
  outputbox `onButtonPress` (catchMouseClick stateref)

  scrtop <- scrolledWindowNew Nothing Nothing
  scrolledWindowSetPolicy scrtop PolicyAutomatic PolicyAlways
  scrolledWindowSetShadowType scrtop ShadowIn  
  containerAdd scrtop outputbox  

  inputbox <- createInputBox 
  widgetModifyFont inputbox (Just (textboxFont currstate) )
  inputbox `onKeyPress` (catchCommandKeys stateref)

  scrbottom <- scrolledWindowNew Nothing Nothing
  scrolledWindowSetPolicy scrbottom PolicyAutomatic PolicyAutomatic
  scrolledWindowSetShadowType scrbottom ShadowIn  
  containerAdd scrbottom inputbox 

  evalbutton <- buttonNew
  evallabel <- labelNew (Just "Evaluate!")
  miscSetPadding evallabel 10 5 
  containerAdd evalbutton evallabel
  evalbutton `afterClicked` (processInput stateref)

  bottombox <- hBoxNew False 0 
  boxPackStart bottombox scrbottom PackGrow 0
  boxPackStart bottombox evalbutton PackNatural 0
  
  pane <- vPanedNew 
  panedPack1 pane scrtop True True 
  panedPack2 pane bottombox False True 

  set pane [ (panedChildShrink bottombox) := False ]

  currstate <- readIORef stateref
  let newstruct = (guiStructure currstate) {
                      inputBox = Just inputbox,
                      outputBox = Just outputbox,
                      mainPane = Just pane,
                      outputScrWindow = Just scrbottom
                      }
      newstate = currstate { guiStructure = newstruct }
  writeIORef stateref newstate

  setOutputBoxMinSize stateref
  
  return pane


-- Create the output box
createOutputBox :: GuiState -> IO TextView
createOutputBox currstate = do
  tagtable <- createTagTable currstate
  outputbuffer <- textBufferNew (Just tagtable)

  textBufferInsertAtCursor outputbuffer "\n "
  end <- textBufferGetEndIter outputbuffer
  textBufferInsertPixbuf outputbuffer end (guiIcon currstate)
  textBufferInsertAtCursor outputbuffer "  The York Haskell Evaluator\n\n"

  outputbox <- textViewNewWithBuffer outputbuffer
  textViewSetEditable outputbox False 
  textViewSetCursorVisible outputbox False 
  textViewSetWrapMode outputbox WrapWordChar
  textViewSetIndent outputbox 5
  
  return outputbox


createTagTable :: GuiState -> IO TextTagTable
createTagTable currstate = do
  table <- textTagTableNew 

  inputtag <- textTagNew (Just "input")
  set inputtag [ textTagForeground := (colorToString $ inputColor currstate) ]
  textTagTableAdd table inputtag 

  outputtag <- textTagNew (Just "output")
  set outputtag [ textTagForeground := (colorToString $ outputColor currstate) ]
  textTagTableAdd table outputtag 

  errortag <- textTagNew (Just "error")
  set errortag [ textTagForeground := (colorToString $ errorColor currstate) ]
  textTagTableAdd table errortag 

  return table


  

-- Create the inputbox, either as a textview or sourceview box
#ifdef GTKSOURCEVIEW
createInputBox :: IO SourceView
createInputBox = do 
  lm <- sourceLanguagesManagerNew
  langM <- sourceLanguagesManagerGetLanguageFromMimeType lm "text/x-haskell"
  lang <- case langM of
    (Just lang) -> return lang
    Nothing -> error ("Haskell language def missing\n")

  inputbuffer <- sourceBufferNewWithLanguage lang
  sourceBufferSetHighlight inputbuffer True

  inputbox <- sourceViewNewWithBuffer inputbuffer
  textViewSetIndent inputbox 5
  textViewSetPixelsAboveLines inputbox 5 
  return inputbox
#endif
#ifndef GTKSOURCEVIEW
createInputBox :: IO TextView
createInputBox = do
  inputbox <- textViewNew
  textViewSetIndent inputbox 5
  textViewSetPixelsAboveLines inputbox 5 
  return inputbox 
#endif


-- Create a font button with the correct font loaded into it
createFontButton :: GuiState -> IO FontButton
createFontButton currstate = do 
  currfontname <- fontDescriptionToString (textboxFont currstate)
  fontbut <- fontButtonNewWithFont currfontname 
  fontButtonSetUseFont fontbut True
--  fontButtonSetShowSize fontbut False
  fontButtonSetShowStyle fontbut False
  return fontbut


-------------------
-- Handle events --
-------------------

-- Perform cleanup and quit Gyhe
quitGui :: IORef GuiState -> IO ()
quitGui stateref = do   
  saveConfig stateref
  mainQuit


-- catch magic key-sequences from the input box 
-- FIXME!!! It would be nice if these were configurable in some way
catchCommandKeys :: IORef GuiState -> Event -> IO Bool
catchCommandKeys stateref Key { eventKeyName = key, 
                                eventModifier = [Control] } = do
  case key of 
    "Return" -> do processInput stateref
                   return True 
    "k" -> do clearInputBox stateref
              return True 
    "Up" -> do historyPrev stateref
               return True 
    "Down" -> do historyNext stateref
                 return True 
    _ -> return False 
catchCommandKeys stateref Key { eventKeyName = key,
                                eventModifier = [] } = do
  currstate <- readIORef stateref
  case key of 
    "Page_Up" -> do historyPrev stateref
                    return True 
    "Page_Down" -> do historyNext stateref
                      return True 
    "Up" | (oneLineMode currstate) -> do historyPrev stateref
                                         return True
         | False -> return False
    "Down" | (oneLineMode currstate) -> do historyNext stateref
                                           return True 
           | False -> return False
    "Return" | (oneLineMode currstate) -> do processInput stateref
                                             return True 
             | False -> return False
    _ -> return False
catchCommandKeys _ _ = return False 


-- Catch a mouse click in the output buffer, for use with menus etc.
catchMouseClick :: IORef GuiState -> Event -> IO Bool
catchMouseClick stateref Button { eventX = x, eventY = y,
                                  eventModifier = [],
                                  eventButton = LeftButton,
                                  eventClick = SingleClick } = do
    currstate <- readIORef stateref
    let Just view = outputBox $ guiStructure currstate
    (bufx, bufy) <- textViewWindowToBufferCoords view TextWindowText (round x, round y)
    clickpos <- textViewGetIterAtLocation view bufx bufy
  
    res <- findTextTag clickpos $ clickLinks currstate
    case res of 
      Just (tag, location, handler) -> handler location
      Nothing -> do putStrLn "Click caught - not in a tag"
                    return ()
    return False
  where 
    findTextTag _ [] = return Nothing
    findTextTag itr (ln@(tag,_,_):xs) = do
      intag <- textIterHasTag itr tag 
      if intag then return $ Just ln else findTextTag itr xs
    
catchMouseClick stateref _ = return False  


-- grab focus for the inputbox
focusInputbox :: IORef GuiState -> IO ()
focusInputbox stateref = do
  currstate <- readIORef stateref
  let (Just inputbox) = inputBox $ guiStructure currstate  
  widgetGrabFocus inputbox


-- Set one-line mode
setOneLineMode :: IORef GuiState -> Bool -> IO ()
setOneLineMode stateref True = do
  currstate <- readIORef stateref
  if not (oneLineMode currstate) 
    then do moveMainPaneDivider stateref 1.0
            currstate <- readIORef stateref
            writeIORef stateref ( currstate { oneLineMode = True } )
    else return () 
setOneLineMode stateref False = do
  currstate <- readIORef stateref
  if (oneLineMode currstate) 
    then do moveMainPaneDivider stateref 0.8
            currstate <- readIORef stateref
            writeIORef stateref ( currstate { oneLineMode = False } )
    else return () 


-- Get input from the inputbox and send it for processing 
processInput :: IORef GuiState -> IO ()
processInput stateref = do
  currstate <- readIORef stateref
  let Just inputbox = inputBox $ guiStructure currstate
  input <- getAllText inputbox
  setAllText inputbox ""

  writeInput stateref (input ++ "\n")
  doPendingEvents

  res <- doInteraction (interpState currstate) input
  case (quitNow res) of 
    True -> (quitGui stateref)
    False -> do 
        currstate <- readIORef stateref
        let newstate = (updateHistory currstate input) { interpState = res }
        writeIORef stateref newstate 
        return ()


-- Clear the input box
clearInputBox :: IORef GuiState -> IO ()
clearInputBox stateref = do
  currstate <- readIORef stateref
  let (Just inputbox) = inputBox $ guiStructure currstate
  buf <- textViewGetBuffer inputbox 
  textBufferSetText buf ""


-- Move the main pane divider to a third of the way up
moveMainPaneDivider :: IORef GuiState -> Float -> IO ()
moveMainPaneDivider stateref val = do
  currstate <- readIORef stateref 
  let (Just pane) = mainPane (guiStructure currstate) 
  max <- get pane panedMaxPosition 
  panedSetPosition pane $ round ((fromIntegral max) * val)


-- Popup a preferences dialog
popupPreferencesDialog :: IORef GuiState -> IO ()
popupPreferencesDialog stateref = do
  currstate <- readIORef stateref
  let (Just mainwindow) = mainWindow $ guiStructure currstate
  
  -- set up the appearence frame
  fontbut <- createFontButton currstate
  fontbutlabel <- labelNew (Just "Font:")
  fontselect <- hBoxNew False 10 
  boxPackStart fontselect fontbutlabel PackNatural 20
  boxPackEnd fontselect fontbut PackNatural 20

  inpcolorbut <- colorButtonNewWithColor $ inputColor currstate
  inpcolorbutlabel <- labelNew (Just "Input Echo:")
  inpcolorselect <- hBoxNew False 10 
  boxPackStart inpcolorselect inpcolorbutlabel PackNatural 20
  boxPackEnd inpcolorselect inpcolorbut PackNatural 20

  outcolorbut <- colorButtonNewWithColor $ outputColor currstate
  outcolorbutlabel <- labelNew (Just "Output Text:")
  outcolorselect <- hBoxNew False 10 
  boxPackStart outcolorselect outcolorbutlabel PackNatural 20 
  boxPackEnd outcolorselect outcolorbut PackNatural 20

  errcolorbut <- colorButtonNewWithColor $ errorColor currstate
  errcolorbutlabel <- labelNew (Just "Error Text:")
  errcolorselect <- hBoxNew False 10 
  boxPackStart errcolorselect errcolorbutlabel PackNatural 20
  boxPackEnd errcolorselect errcolorbut PackNatural 20

  appearbox <- vBoxNew False 10 
  containerSetBorderWidth appearbox 10
  boxPackStart appearbox fontselect PackNatural 0
  boxPackStart appearbox inpcolorselect PackNatural 0
  boxPackStart appearbox outcolorselect PackNatural 0
  boxPackStart appearbox errcolorselect PackNatural 0

  appearframe <- frameNew 
  frameSetLabel appearframe "Appearence"
  containerSetBorderWidth appearframe 5
  containerAdd appearframe appearbox 

  -- set up the configuration frame
  editorbox <- entryNew
  entrySetText editorbox (textEditor currstate)
  editorlabel <- labelNew (Just "Text Editor:")
  editorselect <- hBoxNew False 10 
  boxPackStart editorselect editorlabel PackNatural 0
  boxPackEnd editorselect editorbox PackGrow 0

  onelinetoggle <- checkButtonNewWithLabel "Single-line Mode"
  toggleButtonSetActive onelinetoggle (oneLineMode currstate)

  configbox <- vBoxNew False 10 
  containerSetBorderWidth configbox 10
  boxPackStart configbox editorselect PackNatural 0
  boxPackStart configbox onelinetoggle PackNatural 0

  configframe <- frameNew
  frameSetLabel configframe "Behavior"
  containerSetBorderWidth configframe 5
  containerAdd configframe configbox 

  -- pack the two frames into the preferences dialog
  prefs <- dialogNew 
  windowSetTitle prefs "Preferences"
  windowSetResizable prefs False 
  windowSetModal prefs True
  windowSetTransientFor prefs mainwindow 
  windowSetPosition prefs WinPosCenterOnParent
  dialogAddButton prefs "Ok" ResponseOk
  dialogAddButton prefs "Cancel" ResponseCancel

  topbox <- dialogGetUpper prefs 
  boxPackStart topbox appearframe PackNatural 0
  boxPackStart topbox configframe PackNatural 0

  -- set up a response to closing the box in various ways
  let prefsResponse ResponseOk = do
        -- write out a new state to the IORef 
        editorstring <- entryGetText editorbox 
        inpcolor <- colorButtonGetColor inpcolorbut
        outcolor <- colorButtonGetColor outcolorbut
        errcolor <- colorButtonGetColor errcolorbut
        currstate <- readIORef stateref
        writeIORef stateref ( currstate {
                                  textEditor = editorstring,
                                  inputColor = inpcolor,
                                  outputColor = outcolor,
                                  errorColor = errcolor } ) 

        -- perform other operations
        setTextColors currstate inpcolor outcolor errcolor

        fontstring <- fontButtonGetFontName fontbut
        setGuiFont stateref fontstring
        doPendingEvents
        onelinestatus <- (toggleButtonGetActive onelinetoggle)
        setOneLineMode stateref onelinestatus

        widgetDestroy prefs
      prefsResponse ResponseCancel = widgetDestroy prefs
      prefsResponse ResponseClose = widgetDestroy prefs
      prefsResponse _ = return () 
  
  prefs `onResponse` prefsResponse 

  widgetShowAll prefs


-- Open the selected Text Editor
openTextEditor :: IORef GuiState -> String -> Int -> Int -> IO () 
openTextEditor stateref file row column = do
  currstate <- readIORef stateref 
  system $ (textEditor currstate) ++ " " ++ file
  return () 


-- Set the font in the two textboxes
setGuiFont :: IORef GuiState -> String -> IO ()
setGuiFont stateref fontstring = do
  newfont <- fontDescriptionFromString fontstring 
  currstate <- readIORef stateref
  writeIORef stateref ( currstate { textboxFont = newfont } )

  let struct = guiStructure currstate
      Just inputbox = inputBox struct
      Just outputbox = outputBox struct
  widgetModifyFont outputbox (Just newfont)
  widgetModifyFont inputbox (Just newfont )

  setOutputBoxMinSize stateref


-- Set the minimum size for the output box from the font
setOutputBoxMinSize :: IORef GuiState -> IO ()
setOutputBoxMinSize stateref = do
  currstate <- readIORef stateref 
  let font = textboxFont currstate
      Just outputscrwindow = outputScrWindow (guiStructure currstate)
  res <- lineHeightFromFont font
  case res of 
    Just size -> widgetSetSizeRequest outputscrwindow (-1) size 
    Nothing -> return () 


-- update the history of a GuiState following evaluation of an input 
updateHistory :: GuiState -> String -> GuiState
updateHistory state input = state { cmdHistory = (newhistory, newhistory, []) }
  where
    (history,_,_) = cmdHistory state
    newhistory = case history of 
                   [] -> [input] 
                   (h:xs) | h /= input -> (input:h:xs) 
                          | True -> (h:xs)


-- Pop up an about dialog
popupAbout :: IORef GuiState -> IO ()
popupAbout stateref = do
  currstate <- readIORef stateref
  let (Just mainwindow) = mainWindow $ guiStructure currstate

  about <- aboutDialogNew
  aboutDialogSetName about name
  aboutDialogSetVersion about version
  aboutDialogSetComments about "The York Haskell Evaluator.\nA Haskell GUI based on YHC"
  aboutDialogSetWebsite about "http://www.haskell.org/haskellwiki/Yhc"

  windowSetTransientFor about mainwindow 
  windowSetPosition about WinPosCenterOnParent

  widgetShowAll about


-- Replace the current contents of the inputbox with the previous command
historyPrev stateref = do
  currstate <- readIORef stateref
  let Just inputbox = inputBox $ guiStructure currstate
      (history,past,future) = cmdHistory currstate
  case past of  
    [] -> return () 
    (prev:xs) -> do 
      curr <- getAllText inputbox 
      setAllText inputbox prev 
      currstate <- readIORef stateref
      writeIORef stateref $ currstate { cmdHistory = (history,xs,curr:future) }
  
-- Replace the current contents of the inputbox with the next command
historyNext stateref = do
  currstate <- readIORef stateref
  let Just inputbox = inputBox $ guiStructure currstate
      (history,past,future) = cmdHistory currstate
  case future of  
    [] -> return () 
    (next:xs) -> do 
      curr <- getAllText inputbox 
      setAllText inputbox next 
      currstate <- readIORef stateref
      writeIORef stateref $ currstate { cmdHistory = (history,curr:past,xs) }


writeInput :: IORef GuiState -> String -> IO () 
writeInput stateref str = do 
  currstate <- readIORef stateref
  let Just view = outputBox $ guiStructure currstate
  buf <- textViewGetBuffer view 
  bufend <- textBufferGetEndIter buf 
  leftmark <- textBufferCreateMark buf Nothing bufend True
  rightmark <- textBufferCreateMark buf Nothing bufend False
  textBufferInsert buf bufend str

  textstart <- textBufferGetIterAtMark buf leftmark
  textend <- textBufferGetIterAtMark buf rightmark
  textBufferApplyTagByName buf "input" textstart bufend

  doPendingEvents
  textViewScrollToIter view textend 0.0 Nothing 
  return ()


writeOutput :: IORef GuiState -> String -> IO () 
writeOutput stateref str = do 
  currstate <- readIORef stateref
  let Just view = outputBox $ guiStructure currstate
  buf <- textViewGetBuffer view 
  bufend <- textBufferGetEndIter buf 
  leftmark <- textBufferCreateMark buf Nothing bufend True
  rightmark <- textBufferCreateMark buf Nothing bufend False
  textBufferInsert buf bufend str

  textstart <- textBufferGetIterAtMark buf leftmark
  textend <- textBufferGetIterAtMark buf rightmark
  textBufferApplyTagByName buf "output" textstart bufend

  doPendingEvents
  textViewScrollToIter view textend 0.0 Nothing 
  return ()


writeError :: IORef GuiState -> String -> IO () 
writeError stateref str = do 
  currstate <- readIORef stateref
  let Just view = outputBox $ guiStructure currstate
  buf <- textViewGetBuffer view 
  bufend <- textBufferGetEndIter buf 
  leftmark <- textBufferCreateMark buf Nothing bufend True
  rightmark <- textBufferCreateMark buf Nothing bufend False
  textBufferInsert buf bufend str

  textstart <- textBufferGetIterAtMark buf leftmark
  textend <- textBufferGetIterAtMark buf rightmark
  textBufferApplyTagByName buf "error" textstart bufend

  -- temporary code to test the clickable link code 
  linktag <- textTagNew Nothing
  table <- textBufferGetTagTable buf
  textTagTableAdd table linktag
  textBufferApplyTag buf linktag textstart bufend
  let link = ( linktag, "Clicked on an error", putStrLn ) 

  currstate <- readIORef stateref
  writeIORef stateref ( currstate { clickLinks = (link:(clickLinks currstate)) } )
  -- end temp code 

  doPendingEvents
  textViewScrollToIter view textend 0.0 Nothing 
  return ()


setTextColors :: GuiState -> Color -> Color -> Color -> IO () 
setTextColors currstate inpcolor outcolor errcolor = do
  let Just view = outputBox $ guiStructure currstate
  buf <- textViewGetBuffer view
  tagtable <- textBufferGetTagTable buf
  
  Just inptag <- textTagTableLookup tagtable "input"
  set inptag [ textTagForeground := (colorToString inpcolor) ]

  Just outtag <- textTagTableLookup tagtable "output"
  set outtag [ textTagForeground := (colorToString outcolor) ]

  Just errtag <- textTagTableLookup tagtable "error"
  set errtag [ textTagForeground := (colorToString errcolor) ]

  return () 

----------------------
-- Helper Functions --
----------------------

-- Append text to the end of a textview
appendText :: TextView -> String -> IO ()
appendText view "" = return () 
appendText view text = do
  buf <- textViewGetBuffer view 
  end <- textBufferGetEndIter buf 
  textBufferInsert buf end text
  doPendingEvents
  end <- textBufferGetEndIter buf
  textViewScrollToIter view end 0.0 Nothing 
  return ()


-- Get all of the text in a textview 
getAllText :: TextViewClass self => self -> IO String 
getAllText view = do
  buf <- textViewGetBuffer view
  start <- textBufferGetStartIter buf
  end <- textBufferGetEndIter buf 
  text <- textBufferGetText buf start end False 
  return text 


-- Set text in a textview
setAllText :: TextViewClass self => self -> String ->  IO ()
setAllText view string = do
  buf <- textViewGetBuffer view
  textBufferSetText buf string 


-- Do all of the pending events before proceeding
doPendingEvents :: IO ()
doPendingEvents = do 
  e <- eventsPending 
  if e /= 0 then do res <- mainIterationDo False 
                    doPendingEvents 
            else return ()


-- FIXME!!! this function is crap in several ways:
--  * it will only work on 96dpi screens, which is most of them atm, but still...
--  * it uses the font height rather than the line height
lineHeightFromFont :: FontDescription -> IO (Maybe Int)
lineHeightFromFont font = do
    res <- fontDescriptionGetSize font 
    case res of
      Just size -> do let pixelheight = pointsToPixels $ fromIntegral size 
  			  unscaledborder = 8
			  scaledborder = round $ (fromIntegral (unscaledborder * pixelheight)) 
			 				/ (fromIntegral $ pointsToPixels 12)
		      return $ Just (pixelheight + scaledborder)
      Nothing -> return Nothing
  where 
    pointsToPixels :: Int -> Int
    pointsToPixels points = round ((fromIntegral points) * (96/72))


colorToString :: Color -> String 
colorToString (Color red green blue) = "#" ++ rstr ++ gstr ++ bstr
  where
    rstr = pad "00" (showHex (div red 0x100) "")
    gstr = pad "00" (showHex (div green 0x100) "")
    bstr = pad "00" (showHex (div blue 0x100) "")


{--
stringToColor :: String -> Color
stringToColor ['#',r1,r2,g1,g2,b1,b2] = Color (256*red)  (256*green) (256*blue)
  where
    [(red, "")] = readHex [r1,r2]
    [(green, "")] = readHex [g1,g2]
    [(blue, "")] = readHex [b1,b2]
stringToColor _ = Color 0 0 0
--}


pad :: String -> String -> String 
pad padding str = reverse $ prv (reverse padding) (reverse str)
  where prv [] [] = []
        prv [] ys = ys
        prv xs [] = xs
        prv (x:xs) (y:ys) = y : (prv xs ys)
