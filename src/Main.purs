module Main where

import Control.Monad.Eff (Eff)
import Data.Array (replicate, (!!))
import Data.Maybe (Maybe())
import Data.Maybe.Unsafe (fromJust)
import Data.Nullable (toMaybe)
import Prelude

import DOM (DOM)
import DOM.HTML (window)
import DOM.HTML.Document (body)
import DOM.HTML.Types (htmlElementToElement)
import DOM.HTML.Window (document)

import React (ReactElement, ReactClass, createFactory, spec, createClass)
import ReactDOM (render)
import React.DOM as D
import React.DOM.Props as P

data Token = X | O | E

instance showToken :: Show Token where
  show X = "X"
  show O = "O"
  show E = ""

classForToken :: Token -> String
classForToken X = "cell x"
classForToken O = "cell o"
classForToken E = "cell"

type Board = Array Token

get :: Int -> Int -> Board -> Maybe Token
get x y board = board !! (3 * x + y)

newBoard :: Board
newBoard = replicate 3 O ++ replicate 3 X ++ replicate 3 O

boardComponent :: Board -> ReactClass Unit
boardComponent board = createClass $ spec unit \_ -> return (grid board)

grid :: Board -> ReactElement
grid board = D.table' (map (row board) [0,1,2])

row :: Board -> Int -> ReactElement
row board x = D.tr' (map (cell board x) [0,1,2])

cell :: Board -> Int -> Int -> ReactElement
cell board x y =
  D.td [ P.className (classForToken token) ]
       [ D.text (show token) ]
  where
    token = fromJust (get x y board)

main :: forall eff. Eff (dom :: DOM | eff) Unit
main = do
  body' <- getBody
  void (render (ui newBoard) body')
  where
    ui board = D.div' [ createFactory (boardComponent board) unit ]

    getBody = do
      win <- window
      doc <- document win
      elm <- fromJust <$> toMaybe <$> body doc
      return $ htmlElementToElement elm
