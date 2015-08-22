import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort

data Model = Model {
    _ball :: Point,
    _v :: Vector,
    _trail :: Picture
}

initial :: Model
initial = Model (-300,0) (30,80) blank

g :: Float
g = 60

drawer :: Model -> Picture
drawer m = pictures [_trail m, ballAt (_ball m)]

step :: ViewPort -> Float -> Model -> Model
step _ dt (Model (x,y) (vx,vy) t) =
        Model
            (x+vx*dt, y+vy*dt)
            (vx, if y > 0 || vy > 0 then vy - g*dt else 0.8*negate vy)
            (Pictures [trailAt (x,y), t])

ballAt :: Point -> Picture
ballAt (x,y) = translate x y $ circleSolid 2

trailAt :: Point -> Picture
trailAt (x,y) = color (makeColorI 0 255 12 120) . translate x y $ circleSolid 2

window :: Display
window = InWindow "thr!" (750,350) (10,10)

main = simulate window white 30 initial drawer step
