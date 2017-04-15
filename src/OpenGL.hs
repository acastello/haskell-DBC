{-# LANGUAGE FlexibleInstances, ScopedTypeVariables #-}

module OpenGL 
  ( vertex
  , Vertex2 (..), Vertex3 (..)
  , Bidimensional (..)
  , Draw (..)
  , draw
  ) where

import Control.Concurrent
import Control.Monad

import Data.Int
import Data.IORef

import Graphics.UI.GLUT
import Graphics.UI.GLUT.Objects

import Text.Printf

instance Vertex a => Vertex [a] where
    vertex = mapM_ vertex
    vertexv = undefined

class Bidimensional a where
    vert :: a -> IO ()
    bounds :: a -> (Double, Double, Double, Double)

center2 :: Bidimensional a => a -> (Double, Double)
center2 e = ((x+x')/2, (y+y')/2) where
    (x,y,x',y') = bounds e

instance Bidimensional (Double, Double) where
    vert (x,y) = vertex $ Vertex2 x y
    bounds (x,y) = (x-0.5, y-0.5, x+0.5, y+0.5)

instance Bidimensional a => Bidimensional [a] where
    vert = mapM_ vert
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

data Draw a = 
    P { drawEl :: a }
  | L { drawEl :: a }

renderDraw e = renderPrimitive pm (vert $ drawEl e) where
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

draw :: forall a. Bidimensional a => [Draw a] -> IO ()
draw xs = do
    refs <- newRefs
    center refs $= avg (center2 . drawEl <$> xs)
    getArgsAndInitialize
    initialDisplayMode $= [ RGBAMode, DoubleBuffered ]
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

        displayCB refs @ Refs { zoom = zoom, pos = pos } = do
            z <- get zoom
            (x,y) <- get pos
            (x', y') <- get (center refs)
            clear [ColorBuffer]
            loadIdentity
            matrixMode $= Modelview 0

            scale z z (1.0 :: Double)
            translate $ Vector3 x y 0

            forM_ xs renderDraw
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
