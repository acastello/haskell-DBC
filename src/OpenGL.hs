{-# LANGUAGE
    FlexibleInstances
  , ScopedTypeVariables
  , GADTs #-}

module OpenGL
  ( vertex
  , Vertex2 (..), Vertex3 (..)
  , Bidimensional (..)
  , Draw (..)
  , breadth
  , draw
  ) where

import Control.Concurrent
import Control.Monad

import Data.Fixed
import Data.Int
import qualified Data.IntMap as M
import Data.IORef

import Graphics.UI.GLUT
import Graphics.UI.GLUT.Objects

import Text.Printf

instance Vertex a => Vertex [a] where
    vertex = mapM_ vertex
    vertexv = undefined

instance Vertex a => Vertex (M.IntMap a) where
    vertex = mapM_ vertex
    vertexv = undefined

class Bidimensional a where
    vertp :: a -> [(Double, Double)]
    vert :: a -> IO ()
    bounds :: a -> (Double, Double, Double, Double)
    vert e = mapM_ (vertex . uncurry Vertex2) $ vertp e

center2 :: Bidimensional a => a -> (Double, Double)
center2 e = ((x+x')/2, (y+y')/2) where
    (x,y,x',y') = bounds e

breadth xs = max (x' - x) (y' - y) where
    (x, y, x', y') = bounds xs

instance Bidimensional (Double, Double) where
    vertp e = [e]
    bounds (x,y) = (x-0.5, y-0.5, x+0.5, y+0.5)

instance Bidimensional a => Bidimensional [a] where
    vertp xs = vertp `foldMap` xs
    bounds xs = (x, y, x', y') where
        x = minimum $ _1 <$> bs
        y = minimum $ _2 <$> bs
        x' = maximum $ _3 <$> bs
        y' = maximum $ _4 <$> bs
        _1 (e,_,_,_) = e
        _2 (_,e,_,_) = e
        _3 (_,_,e,_) = e
        _4 (_,_,_,e) = e
        bs = bounds <$> xs

instance Bidimensional a => Bidimensional (M.IntMap a) where
    vertp e = vertp `foldMap` (M.elems e)
    bounds = bounds . M.elems

data Draw where
    P :: Bidimensional a => a -> Draw
    L :: Bidimensional a => a -> Draw

circle :: Bidimensional a => a -> Double -> Draw
circle e r = L $ do
    p <- vertp e
    [(r * cos a, r * sin a) | a <- (pi*2*) <$> [0,0.01..1]]

drawP (P a) = vertp a
drawP (L a) = vertp a

drawV (P a) = vert a
drawV (L a) = vert a

drawB (P a) = bounds a
drawB (L a) = bounds a

drawC (P a) = center2 a
drawC (L a) = center2 a

instance Bidimensional Draw where
    vertp = drawP
    vert  = drawV
    bounds = drawB

renderDraw e = renderPrimitive pm (drawV e) where
    pm = case e of
        P _ -> Points
        L _ -> LineStrip

data Refs = Refs
  { zoom    :: IORef Double
  , center  :: IORef (Double, Double)
  , pos     :: IORef (Double, Double)
  , lastPos :: IORef (Maybe (Double, Double, Int32, Int32))
  , ratios  :: IORef (Double, Double)
  }

newRefs = do
    z <- newIORef 0.1
    c <- newIORef (0, 0)
    p <- newIORef (0, 0)
    lp <- newIORef Nothing
    rs <- newIORef (100, 100)
    return $ Refs z c p lp rs

draw :: [Draw] -> IO ()
draw xs = do
    refs <- newRefs
    let (x,y) = center2 xs
    pos refs $= (-x, -y)
    zoom refs $= 1 / breadth xs
    getArgsAndInitialize
    initialDisplayMode $= [ RGBAMode, DoubleBuffered ]
    initialWindowSize $= Size 900 900
    createWindow "OpenGL Canvas"
    reshapeCallback $= Just (reshapeCB refs)
    depthFunc $= Just Less
    displayCallback $= displayCB refs
    idleCallback $= Just (postRedisplay Nothing)
    keyboardMouseCallback $= Just (keyboardMouseCB refs)
    motionCallback $= Just (motionCB refs)
    actionOnWindowClose $= MainLoopReturns
    mainLoop
    where
        pairs = zip (hsvRange (length xs)) xs
        displayCB refs @ Refs { zoom = zoom, pos = pos } = do
            z <- get zoom
            (x,y) <- get pos
            (x', y') <- get (center refs)
            clear [ColorBuffer]
            loadIdentity
            matrixMode $= Modelview 0

            scale z z (1.0 :: Double)
            translate $ Vector3 x y 0

            forM_ pairs $ \(c,d) -> do
                color (hsv2rgb c)
                renderDraw d
            swapBuffers

        keyboardMouseCB refs c ks m p @ (Position x y) = do
            z <- get (zoom refs)
            case c of
                Char 'q' -> leaveMainLoop
                MouseButton WheelDown -> do
                    z <- get (zoom refs)
                    (rx, ry) <- get (ratios refs)
                    zoom refs $= z * 0.9
                    center refs $= ( (fromIntegral x - rx) / rx / z
                                   , (fromIntegral y - ry) / ry / z)
                MouseButton WheelUp -> do
                    z <- get (zoom refs)
                    (rx, ry) <- get (ratios refs)
                    zoom refs $= z / 0.9
                    center refs $= ( (fromIntegral x - rx) / rx / z
                                   , (fromIntegral y - ry) / ry / z)
                MouseButton LeftButton -> do
                    if ks == Down then do
                        (x', y') <- get (pos refs)
                        lastPos refs $= Just (x', y', x, y)
                    else do
                        lastPos refs $= Nothing

                _ -> return ()

        reshapeCB refs size @ (Size w h) = do
            viewport $= (Position 0 0, size)
            ratios refs $= (fromIntegral w / 2, fromIntegral h / 2)

        motionCB refs p @ (Position x y) = do
            -- (_, s) <- get viewport
            (x', y') <- get (pos refs)
            (rx, ry) <- get (ratios refs)
            z <- get (zoom refs)
            p'' <- get (lastPos refs)
            forM_ p'' $ \(x', y', x'', y'') -> do
                -- printf "motion: (%+d, %d, %d) (%d, %d, %d)\n" x x' x'' y y' y'' :: IO ()
                let offx = fromIntegral (x - x'') / rx / z
                    offy = fromIntegral (y'' - y) / ry / z
                pos refs $= (x' + offx, y' + offy)
                -- viewport $= (Position (x' + x - x'') (y' + y'' - y), s)
            postRedisplay Nothing

avg xs = (\(x,y) -> (x/l, y/l)) $ foldr (\(x,y) (x',y') -> (x+x',y+y')) (0,0) xs
             where l = fromIntegral $ length xs

hsvRange :: Int -> [Color3 Double]
hsvRange n = [Color3 ((i / n' * 360) `mod'` 360) 0.7 1 | i <- [0 .. n' - 1]]
  where n' = fromIntegral n

hsv2rgb :: Color3 Double -> Color3 Double
hsv2rgb (Color3 h s v) = Color3 (r + m) (g + m) (b + m)
  where
    h' = h `mod'` 360
    c = v * s
    x = c * (1 - abs (((h' / 60) `mod'` 2) - 1))
    m = v - c
    (r, g, b)
      | 0 <= h' && h' < 60    = (,,) c x 0
      | 60 <= h' && h' < 120  = (,,) x c 0
      | 120 <= h' && h' < 180 = (,,) 0 c x
      | 180 <= h' && h' < 240 = (,,) 0 x c
      | 240 <= h' && h' < 300 = (,,) x 0 c
      | 240 <= h' && h' < 360 = (,,) c 0 x

test :: IO ()
test = do
    getArgsAndInitialize
    initialDisplayMode $= [SingleBuffered, RGBMode]
    createWindow "test"
    shadeModel $= Flat
    displayCallback $= displayCB
    -- reshapeCallback $= Just reshapeCB
    -- matrixMode $= Projection
    -- loadIdentity
    -- frustum (-1) 1 (-1) 1 1.5 7
    -- matrixMode $= Modelview 0
    mainLoop
    where
        displayCB = do
            clear [ColorBuffer]
            loadIdentity
            -- lookAt (Vertex3 0 0 5) (Vertex3 0 0 0) (Vector3 0 1 0)
            scale 1.5 2 (1 :: Double)
            renderObject Wireframe (Cube 1.0)
            flush
        reshapeCB s = do
            viewport $= (Position 0 0,  s)
            matrixMode $= Projection
            loadIdentity
            frustum (-1) 1 (-1) 1 1.5 20
            matrixMode $= Modelview 0

v1 = (LineStrip, [ Vertex3 0 0 (-1000), Vertex3 0.75 0.75 0, Vertex3 (-0.75) 0.75 0
                 , Vertex3 (-0.75) (-0.75) 0, Vertex3 0.75 (-0.75) 0])
