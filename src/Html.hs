{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Html where

import Control.Monad

import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BL

import qualified Data.List as L

import qualified Data.IntMap as M

import GHC.Exts (IsString(..), fromString)

import Numeric

import Prelude hiding (div, span)

import Text.Blaze.Html.Renderer.Pretty
import Text.Blaze.Html5
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import Core
import Source
import Query

infixr 3 -=
(-=) :: ToMarkup a => (Html -> b) -> a -> b
x -= y = x (toHtml y)

instance ToMarkup B.ByteString where
    toMarkup = toHtml . B.unpack

instance ToMarkup BL.ByteString where
    toMarkup = toHtml . BL.unpack

instance ToMarkup [String] where
    toMarkup = toMarkup . mconcat 

instance ToMarkup [B.ByteString] where
    toMarkup = toMarkup . mconcat 

instance ToMarkup Item where
    toMarkup it = mconcat
      [ b -= it_name it
      , toHtml $ it_id it
      ]

header' = header $ div $ do
    a ! A.href "html.html" $ do
        "Home"

itemFull it = do
    img ! A.src (fromString $ mconcat ["Icons/", B.unpack (it_icon it), ".png"])
    div ! A.class_ "tooltip" $ do
      h1 ! A.style ("color: " `mappend` qualityColor (it_qual it))
        -= it_name it
      br
      h2 -= ["Item Level ", show (it_level it)]
      span -= show (it_slot it)
      forM_ (it_stats it) $ \(s,n) -> do
          span -= [printf "%+d " n, show s]
      let rlvl = it_rlevel it
      span -= ["Requires Level ", show $ if rlvl /= 0 then rlvl else 1 ]

w = do
    let it = maybe undefined Prelude.id $ M.lookup 42943 items
    writeFile "html.html" $ renderHtml $ main' $ itemFull $ it


main' src = docTypeHtml $ do
    H.head $ ""
    body $ do
        header'
        link ! A.rel "stylesheet" ! A.type_ "text/css" ! A.href "html.css"
        src

qualityColor :: Quality -> AttributeValue
qualityColor q = case q of 
    Poor      -> "#9d9d9d"
    Common    -> "#ffffff"
    Uncommon  -> "#1eff00"
    Rare      -> "#0070dd"
    Epic      -> "#a335ee"
    Legendary -> "#ff8000"
    Artifact  -> "#e6cc80"
    Heirloom  -> "#e6cc80"
