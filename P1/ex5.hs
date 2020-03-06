module Main where
import Drawing

-- Define the main function to use later.
main :: IO ()

-- Implement a function to draw lightbulbs.
lightBulb c y = colored c (translated 0 y (solidCircle 1))

-- Define the rectangle.
x = 2.5
y = 7.5
outline c = colored c (rectangle x y)
fill c = colored c (solidRectangle x y)

-- Join all the elements.
trafficLight = lightBulb red (2.5) <> lightBulb green (-2.5) <> lightBulb yellow 0 <> outline black <> fill gray

-- Start of ex. 5.
repeatDraw :: (Int -> Drawing) -> Int -> Drawing
repeatDraw thing 0 = blank
repeatDraw thing n = thing n <> repeatDraw thing (n-1)

-- Draw a traffic light in the coordinates r,c.
light :: Int -> Int -> Drawing
light r c = translated (3 * fromIntegral c - 6) (8 * fromIntegral r - 16) trafficLight

-- Draw a row of r lights.
lightRow :: Int -> Drawing
lightRow r = repeatDraw (light r) 3
{- How does it work? 
light r is a function that expects a second parameter c. When calling repeatDraw it becomes:
 light r n <> repeatDraw light r (n-1) so we can see that n is used as the c in light.
-}

-- Draw n light rows.
myDrawing n = repeatDraw lightRow n
main = svgOf (myDrawing 3)
-- End of ex. 5.
