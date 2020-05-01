module Main where
import Drawing

-- Define the main function to use later.
main :: IO ()

branca = polyline[(0,0),(0,1)]
-- Without flowers
--arbre 0 = blank
-- With flowers
arbre 0 = colored yellow (solidCircle 0.2)
arbre n = branca <> translated 0 1 (rotated (pi/10) (arbre(n-1))) <> translated 0 1 (rotated (-pi/10) (arbre(n-1)))

myDrawing = arbre 8
main = svgOf myDrawing
