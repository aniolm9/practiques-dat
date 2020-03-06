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

-- Start of ex. 6.
-- Draw a traffic light in the coordinates r,c.
light :: (Double, Double) -> Drawing
light (r,c) = translated (3 * c - 6) (8 * r - 16) trafficLight

-- Using foldMap draw traffic lights in the coordinates in the list.
trafficLights :: [(Double, Double)] -> Drawing
trafficLights [] = blank
trafficLights l = foldMap light l
{- How does it work?
foldMap runs the function light in every element of the list l (Foldable).
The elements of l are coordinates so in every iteration it returns a
Drawing (Monoid) and then it performs a composition of all the Drawings
with the operator <>.
-}

main = svgOf (trafficLights
    [(3.0,1.0),(3.0,2.0),(3.0,3.0),
     (2.0,1.0),(2.0,2.0),(2.0,3.0),
     (1.0,1.0),(1.0,2.0),(1.0,3.0)
    ])
-- End of ex. 6.
