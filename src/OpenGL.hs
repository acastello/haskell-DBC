module OpenGL 
  ( Drawable (..)
  , draw
  ) where

import Graphics.UI.GLUT

class Drawable a where

draw :: Drawable 
draw = undefined
