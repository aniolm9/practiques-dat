{-# LANGUAGE OverloadedStrings #-}


import Life.Board
import Life.Draw

import Drawing

-----------------------------------------------------
-- The game state

data Game = Game
        { gmBoard :: Board      -- last board generation
        , gmGridMode :: GridMode
        }
    deriving (Show, Read)

setGmBoard x g       = g{ gmBoard = x }
setGmGridMode x g    = g{ gmGridMode = x }

data GridMode = NoGrid | LivesGrid | ViewGrid
    deriving (Show, Read)

-----------------------------------------------------
-- Initialization

viewWidth, viewHeight :: Double
viewWidth = 60.0
viewHeight = 30.0

main :: IO ()
main =
    activityOf viewWidth viewHeight initial handleEvent draw

board0Cells =
    [(-5, 0), (-4, 0), (-3, 0), (-2, 0), (-1, 0), (0, 0), (1, 0), (2, 0), (3, 0), (4, 0)]

initial = Game
    { gmBoard = foldr (setCell True) initBoard board0Cells
    , gmGridMode = NoGrid
    }

-----------------------------------------------------
-- Event processing

handleEvent :: Event -> Game -> Game
handleEvent (KeyDown "N") game =                -- Next generation
    setGmBoard (nextGeneration (gmBoard game)) game

handleEvent (MouseDown (x, y)) game =           -- Set live/dead cells
    let pos = (round x, round y)
        brd = gmBoard game
    in setGmBoard (setCell (not $ cellIsLive pos brd) pos brd) game

-- The compiler defines a function gmGridMode :: Game -> GridMode
handleEvent (KeyDown "G") game =                -- Change grid
    let nextGrid = case (gmGridMode game) of NoGrid    -> LivesGrid
                                             LivesGrid -> ViewGrid
                                             ViewGrid  -> NoGrid
    in setGmGridMode nextGrid game

handleEvent _ game =                            -- Ignore other events
    game

-----------------------------------------------------
-- Drawing

draw game =
    drawBoard (gmBoard game) <> gridDraw (gmGridMode game)
    where gridDraw NoGrid = blank
          gridDraw LivesGrid = drawGrid (minLiveCell (gmBoard game)) (maxLiveCell (gmBoard game))
          gridDraw ViewGrid = drawGrid (round (-viewWidth/2), round (-viewHeight/2)) (round (viewWidth/2), round (viewHeight/2))
