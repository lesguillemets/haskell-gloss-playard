import Graphics.Gloss as G

main = do
    let window = G.InWindow "Drawing!" (500,500) (100,100)
    -- note the order
    G.display window G.blue $ Pictures [downStairs,stairs,whiteCircle]

square :: Float -> Point -> Picture
square n (x,y) = polygon [(x,y), (x+n,y), (x+n,y+n), (x,y+n)]

stairs :: Picture
stairs = color black . Pictures $ map (\n -> square 10 (n*10,n*10)) [0..25]

downStairs :: Picture
downStairs = color green . Pictures $ map (\n -> square 10 (n*10,n*10)) [-25..0]

whiteCircle :: Picture
whiteCircle = color white . circleSolid $ 50
