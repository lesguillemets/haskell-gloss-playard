import Graphics.Gloss as G

main = do
    let window = G.InWindow "HI" (450,350) (10,10)
    G.display window G.blue (G.Circle 40)
