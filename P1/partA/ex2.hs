module Main where
import Drawing

-- Define the main function to use later.
main :: IO ()

-- Start of ex. 2.
-- Implement a function to draw lightbulbs.
lightBulb c y = colored c (translated 0 y (solidCircle 1))

-- Define the rectangle.
x = 2.5
y = 7.5
outline c = colored c (rectangle x y)
fill c = colored c (solidRectangle x y)

-- Join all the elements.
trafficLight = lightBulb red (2.5) <> lightBulb green (-2.5) <> lightBulb yellow 0 <> outline black <> fill gray

main = svgOf trafficLight
-- End of ex. 2.
