
{-# LANGUAGE OverloadedStrings #-}

module Main where
import Life.Board
import Life.Draw

import Drawing
import Drawing.Vector

-----------------------------------------------------
-- The game state

data Game = Game
        { gmBoard :: Board      -- last board generation
        , gmGridMode :: GridMode
        , gmZoom :: Double, gmShift :: Point
        }
    deriving (Show, Read)

setGmBoard x g       = g{ gmBoard = x }
setGmGridMode x g    = g{ gmGridMode = x }
setGmZoom x g        = g{ gmZoom = x }
setGmShift x g       = g{ gmShift = x }

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
    , gmZoom = 1.0, gmShift = (0.0, 0.0)
    }

-----------------------------------------------------
-- Event processing

pointToPos :: Point -> Game -> Pos
pointToPos p game =
    let (gx, gy) = (1.0 / gmZoom game) *^ p ^-^ gmShift game
    in (round gx, round gy)

handleEvent :: Event -> Game -> Game
handleEvent (KeyDown "N") game =                -- Next generation
    setGmBoard (nextGeneration (gmBoard game)) game

handleEvent (MouseDown (x, y)) game =           -- Set live/dead cells
    let pos = pointToPos (x, y) game
        brd = gmBoard game
    in setGmBoard (setCell (not $ cellIsLive pos brd) pos brd) game

-- The compiler defines a function gmGridMode :: Game -> GridMode
handleEvent (KeyDown "G") game =                -- Change grid
    let nextGrid = case (gmGridMode game) of NoGrid    -> LivesGrid
                                             LivesGrid -> ViewGrid
                                             ViewGrid  -> NoGrid
    in setGmGridMode nextGrid game

handleEvent (KeyDown "I") game =                -- Zoom in
    if gmZoom game < 2.0 then setGmZoom (gmZoom game * 2.0) game
    else game

handleEvent (KeyDown "O") game =                -- Zoom out
    if gmZoom game > 1/16 then setGmZoom (gmZoom game / 2.0) game
    else game

handleEvent (KeyDown "ARROWUP") game =           -- Down shift
    setGmShift (gmShift game ^-^  (1.0 / gmZoom game) *^  (0, 5)) game

handleEvent (KeyDown "ARROWDOWN") game =         -- Up shift
    setGmShift (gmShift game ^+^  (1.0 / gmZoom game) *^  (0, 5)) game

handleEvent (KeyDown "ARROWRIGHT") game =        -- Left shift
    setGmShift (gmShift game ^-^  (1.0 / gmZoom game) *^  (0, 5)) game

handleEvent (KeyDown "ARROWLEFT") game =         -- Right shift
    setGmShift (gmShift game ^+^  (1.0 / gmZoom game) *^  (0, 5)) game

handleEvent _ game =                            -- Ignore other events
    game

-----------------------------------------------------
-- Drawing

zoomAndMove :: Double -> (Double, Double) -> Drawing -> Drawing
zoomAndMove zoom (x,y) draw = scaled zoom zoom (translated x y draw)

draw game =
    zoomAndMove (gmZoom game) (gmShift game) (drawBoard (gmBoard game)) <> zoomAndMove (gmZoom game) (gmShift game) (gridDraw (gmGridMode game))
    where gridDraw NoGrid = blank
          gridDraw LivesGrid = drawGrid (minLiveCell (gmBoard game)) (maxLiveCell (gmBoard game))
          gridDraw ViewGrid = drawGrid (pointToPos ((-viewWidth/2), (-viewHeight/2)) game) (pointToPos (viewWidth/2, viewHeight/2) game)
