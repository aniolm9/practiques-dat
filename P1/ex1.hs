module Main where
import Drawing

-- Define the main function to use later.
main :: IO ()

-- Start of ex. 1.
myDrawing :: Drawing
myDrawing = blank

main = svgOf myDrawing
-- End of ex. 1.
