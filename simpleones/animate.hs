import Graphics.Gloss as G

window :: Display
window = InWindow "animate!" (500,500) (30,30)


main = animate window black circles

d = 10

circles :: Float -> Picture
circles t = Pictures $ map (\n -> colorCircle n (6*t)) [-5..5]


colorCircle :: Int -> Float -> Picture
colorCircle n t = Pictures . map
    (\i -> color (rainbow i) . rotate i $ sq) $ [0,d..360-d]
    where
        rot = if n `mod` 2 == 0 then t else negate t
        ang = rot + fromIntegral n*d/2
        sq = rotate ang . translate 0 (200+20*fromIntegral n)
            $ rectangleSolid (20+fromIntegral n) 20

type HSV = (Float, Float, Float)
fromHSV :: Float -> HSV -> Color
fromHSV a (_,0,v) = makeColor v v v a
fromHSV a (_,_,0) = makeColor 0 0 0 a
fromHSV a (h,s,v) =
        case hi of
            0 -> makeColor v x m a
            1 -> makeColor x v m a
            2 -> makeColor m v x a
            3 -> makeColor m x v a
            4 -> makeColor x m v a
            _ -> makeColor v m x a
            where
                hi = floor (h/60) :: Int
                f = h/60 - fromIntegral hi
                m = v * (1-s)
                x = if hi `mod` 2 == 0
                        then v * (1- (1-f)*s)
                        else v * (1- f*s)

rainbow :: Float -> Color
rainbow i = fromHSV 1 (i,1,1)
