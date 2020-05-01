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

-- Start of ex. 3.
lights 0 = blank
lights n = trafficLight <> translated 3 0 (lights (n-1))
arrayLights x 0 = blank
arrayLights x y = lights x <> translated 0 8 (arrayLights x (y-1))

myDrawing x y = translated ((-1.5)*(x-1)) ((-4)*(y-1)) (arrayLights x y)
main = svgOf (myDrawing 3 3)
-- End of ex. 3.
