module Main (main) where

--------------------------------------------------------------------------------

import Control.Concurrent.STM    (TQueue, atomically, newTQueueIO, tryReadTQueue, writeTQueue)
import Control.Monad             (unless, when, void)
import Control.Monad.RWS.Strict  (RWST, ask, asks, evalRWST, get, liftIO, modify, put)
import Data.Maybe                (catMaybes, fromMaybe)

import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.UI.GLFW          as GLFW
import Control.Monad.Reader (ReaderT, runReaderT)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import qualified Data.Map.Strict as M

import qualified Graphics.ShaderLoader as ShaderLoader
import Graphics.Renderer (foo)

--------------------------------------------------------------------------------

data WindowInfo = WindowInfo {
      windowInfoWidth  :: !Int
    , windowInfoHeight :: !Int    
    , windowInfoGLFW   :: !GLFW.Window
} deriving Show

data Env = Env { 
      envEventsChan:: !(TQueue Event)
    , envWindowInfo:: !WindowInfo
    , envShaders::    !(M.Map B.ByteString GL.Program)
}

type GameEnv a = ReaderT Env IO a

--------------------------------------------------------------------------------

data Event =
    EventError           !GLFW.Error !String
  | EventWindowPos       !GLFW.Window !Int !Int
  | EventWindowSize      !GLFW.Window !Int !Int
  | EventWindowClose     !GLFW.Window
  | EventWindowRefresh   !GLFW.Window
  | EventWindowFocus     !GLFW.Window !Bool
  | EventWindowIconify   !GLFW.Window !Bool
  | EventFramebufferSize !GLFW.Window !Int !Int
  | EventMouseButton     !GLFW.Window !GLFW.MouseButton !GLFW.MouseButtonState !GLFW.ModifierKeys
  | EventCursorPos       !GLFW.Window !Double !Double
  | EventCursorEnter     !GLFW.Window !GLFW.CursorState
  | EventScroll          !GLFW.Window !Double !Double
  | EventKey             !GLFW.Window !GLFW.Key !Int !GLFW.KeyState !GLFW.ModifierKeys
  | EventChar            !GLFW.Window !Char
  deriving Show

--------------------------------------------------------------------------------

main :: IO ()
main = do
    let width  = 640
        height = 480

    eventsChan <- newTQueueIO :: IO (TQueue Event)

    withWindow width height "GLFW-b-demo" $ \win -> do
        GLFW.setErrorCallback               $ Just $ errorCallback           eventsChan
        GLFW.setWindowPosCallback       win $ Just $ windowPosCallback       eventsChan
        GLFW.setWindowSizeCallback      win $ Just $ windowSizeCallback      eventsChan
        GLFW.setWindowCloseCallback     win $ Just $ windowCloseCallback     eventsChan
        GLFW.setWindowRefreshCallback   win $ Just $ windowRefreshCallback   eventsChan
        GLFW.setWindowFocusCallback     win $ Just $ windowFocusCallback     eventsChan
        GLFW.setWindowIconifyCallback   win $ Just $ windowIconifyCallback   eventsChan
        GLFW.setFramebufferSizeCallback win $ Just $ framebufferSizeCallback eventsChan
        GLFW.setMouseButtonCallback     win $ Just $ mouseButtonCallback     eventsChan
        GLFW.setCursorPosCallback       win $ Just $ cursorPosCallback       eventsChan
        GLFW.setCursorEnterCallback     win $ Just $ cursorEnterCallback     eventsChan
        GLFW.setScrollCallback          win $ Just $ scrollCallback          eventsChan
        GLFW.setKeyCallback             win $ Just $ keyCallback             eventsChan
        GLFW.setCharCallback            win $ Just $ charCallback            eventsChan

        GLFW.swapInterval 1

        shaders <- ShaderLoader.compileShaders

        let windowEnv = WindowInfo
                {
                  windowInfoWidth  = width
                , windowInfoHeight = height
                , windowInfoGLFW   = win
                }

        let env = Env
              { 
                  envEventsChan = eventsChan
                , envWindowInfo = windowEnv
                , envShaders    = shaders
              }

        runIO env

    putStrLn "ended!"

--------------------------------------------------------------------------------

-- GLFW-b is made to be very close to the C API, so creating a window is pretty
-- clunky by Haskell standards. A higher-level API would have some function
-- like withWindow.

withWindow :: Int -> Int -> String -> (GLFW.Window -> IO ()) -> IO ()
withWindow width height title f = do
    GLFW.setErrorCallback $ Just simpleErrorCallback
    r <- GLFW.init
    when r $ do
        m <- GLFW.createWindow width height title Nothing Nothing
        case m of
          (Just win) -> do
              GLFW.makeContextCurrent m
              f win
              GLFW.setErrorCallback $ Just simpleErrorCallback
              GLFW.destroyWindow win
          Nothing -> return ()
        GLFW.terminate
  where
    simpleErrorCallback e s =
        putStrLn $ unwords [show e, show s]

--------------------------------------------------------------------------------

-- Each callback does just one thing: write an appropriate Event to the events
-- TQueue.

errorCallback           :: TQueue Event -> GLFW.Error -> String                                                            -> IO ()
windowPosCallback       :: TQueue Event -> GLFW.Window -> Int -> Int                                                       -> IO ()
windowSizeCallback      :: TQueue Event -> GLFW.Window -> Int -> Int                                                       -> IO ()
windowCloseCallback     :: TQueue Event -> GLFW.Window                                                                     -> IO ()
windowRefreshCallback   :: TQueue Event -> GLFW.Window                                                                     -> IO ()
windowFocusCallback     :: TQueue Event -> GLFW.Window -> Bool                                                             -> IO ()
windowIconifyCallback   :: TQueue Event -> GLFW.Window -> Bool                                                             -> IO ()
framebufferSizeCallback :: TQueue Event -> GLFW.Window -> Int -> Int                                                       -> IO ()
mouseButtonCallback     :: TQueue Event -> GLFW.Window -> GLFW.MouseButton   -> GLFW.MouseButtonState -> GLFW.ModifierKeys -> IO ()
cursorPosCallback       :: TQueue Event -> GLFW.Window -> Double -> Double                                                 -> IO ()
cursorEnterCallback     :: TQueue Event -> GLFW.Window -> GLFW.CursorState                                                 -> IO ()
scrollCallback          :: TQueue Event -> GLFW.Window -> Double -> Double                                                 -> IO ()
keyCallback             :: TQueue Event -> GLFW.Window -> GLFW.Key -> Int -> GLFW.KeyState -> GLFW.ModifierKeys            -> IO ()
charCallback            :: TQueue Event -> GLFW.Window -> Char                                                             -> IO ()

errorCallback           tc e s            = atomically $ writeTQueue tc $ EventError           e s
windowPosCallback       tc win x y        = atomically $ writeTQueue tc $ EventWindowPos       win x y
windowSizeCallback      tc win w h        = atomically $ writeTQueue tc $ EventWindowSize      win w h
windowCloseCallback     tc win            = atomically $ writeTQueue tc $ EventWindowClose     win
windowRefreshCallback   tc win            = atomically $ writeTQueue tc $ EventWindowRefresh   win
windowFocusCallback     tc win fa         = atomically $ writeTQueue tc $ EventWindowFocus     win fa
windowIconifyCallback   tc win ia         = atomically $ writeTQueue tc $ EventWindowIconify   win ia
framebufferSizeCallback tc win w h        = atomically $ writeTQueue tc $ EventFramebufferSize win w h
mouseButtonCallback     tc win mb mba mk  = atomically $ writeTQueue tc $ EventMouseButton     win mb mba mk
cursorPosCallback       tc win x y        = atomically $ writeTQueue tc $ EventCursorPos       win x y
cursorEnterCallback     tc win ca         = atomically $ writeTQueue tc $ EventCursorEnter     win ca
scrollCallback          tc win x y        = atomically $ writeTQueue tc $ EventScroll          win x y
keyCallback             tc win k sc ka mk = atomically $ writeTQueue tc $ EventKey             win k sc ka mk
charCallback            tc win c          = atomically $ writeTQueue tc $ EventChar            win c

--------------------------------------------------------------------------------

runIO :: Env -> IO ()
runIO env = do
    void $ runReaderT run env

run :: GameEnv ()
run = do
    win <- asks (windowInfoGLFW . envWindowInfo)

    draw
    liftIO $ do
        GLFW.swapBuffers win
        GL.flush  -- not necessary, but someone recommended it
        GLFW.pollEvents
    processEvents

    mt <- liftIO GLFW.getTime

    q <- liftIO $ GLFW.windowShouldClose win
    unless q run

processEvents :: GameEnv ()
processEvents = do
    tc <- asks envEventsChan
    me <- liftIO $ atomically $ tryReadTQueue tc
    case me of
      Just e -> do
          processEvent e
          processEvents
      Nothing -> return ()

processEvent :: Event -> GameEnv ()
processEvent ev =
    case ev of
      (EventError e s) -> do
          liftIO $ printEvent "error" [show e, show s]
          win <- asks (windowInfoGLFW . envWindowInfo)
          liftIO $ GLFW.setWindowShouldClose win True

      (EventWindowPos _ x y) ->
          liftIO $ printEvent "window pos" [show x, show y]

      (EventWindowSize _ width height) ->
          liftIO $ printEvent "window size" [show width, show height]

      (EventWindowClose _) ->
          liftIO $ printEvent "window close" []

      (EventWindowRefresh _) ->
          liftIO $ printEvent "window refresh" []

      (EventWindowFocus _ fs) ->
          liftIO $ printEvent "window focus" [show fs]

      (EventWindowIconify _ is) ->
          liftIO $ printEvent "window iconify" [show is]

      (EventFramebufferSize _ width height) -> do
          liftIO $ printEvent "framebuffer size" [show width, show height]

      (EventMouseButton _ mb mbs mk) -> do
          liftIO $ printEvent "mouse button" [show mb, show mbs, showModifierKeys mk]

      (EventCursorPos _ x y) -> do
          let x' = round x :: Int
              y' = round y :: Int
          liftIO $ printEvent "cursor pos" [show x', show y']

      (EventCursorEnter _ cs) ->
          liftIO $ printEvent "cursor enter" [show cs]

      (EventScroll _ x y) -> do
        liftIO $ print "Event scroll"
          

      (EventKey win k scancode ks mk) -> do
          liftIO $ printEvent "key" [show k, show scancode, show ks, showModifierKeys mk]
          when (ks == GLFW.KeyState'Pressed) $ do
              -- Q, Esc: exit
              when (k == GLFW.Key'Q || k == GLFW.Key'Escape) $
                liftIO $ GLFW.setWindowShouldClose win True

      (EventChar _ c) ->
          liftIO $ printEvent "char" [show c]

draw :: GameEnv ()
draw = do
    env   <- ask
    let m_program = M.lookup (B8.pack "TriangleShader") (envShaders env)
    liftIO $ do
        GL.clear [GL.ColorBuffer, GL.DepthBuffer]

        foo (fromMaybe (error "No program here") m_program)

isPress :: GLFW.KeyState -> Bool
isPress GLFW.KeyState'Pressed   = True
isPress GLFW.KeyState'Repeating = True
isPress _                       = False

printEvent :: String -> [String] -> IO ()
printEvent cbname fields =
    liftIO $ putStrLn $ cbname ++ ": " ++ unwords fields

showModifierKeys :: GLFW.ModifierKeys -> String
showModifierKeys mk =
    "[mod keys: " ++ keys ++ "]"
  where
    keys = if null xs then "none" else unwords xs
    xs = catMaybes ys
    ys = [ if GLFW.modifierKeysShift   mk then Just "shift"   else Nothing
         , if GLFW.modifierKeysControl mk then Just "control" else Nothing
         , if GLFW.modifierKeysAlt     mk then Just "alt"     else Nothing
         , if GLFW.modifierKeysSuper   mk then Just "super"   else Nothing
         ]