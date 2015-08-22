import Graphics.Gloss

main = do
    let window = InWindow "HI" (700,700) (10,10)
    display window black (
        Pictures [dimLine, addLine,
                 colorCircle, innerCircle, outerCircle]
        )

d :: Float
d = 10

-- use of makeColor
colorCircle :: Picture
colorCircle = Pictures . map
    (\i -> color (rainbow i) . rotate i $ sq) $ [0,d..360-d]
    where
        sq = translate 0 200 $ rectangleSolid 20 20

-- use of mixColors
innerCircle :: Picture
innerCircle = Pictures . map
    (\i -> color (mixColors 0.5 0.5 (rainbow i) (rainbow (i-d)))
        . rotate i $ sq) $ [0,d..360-d]
    where
        sq = rotate (d/2) . translate 0 180 $ rectangleSolid 20 20

-- use of addColors
outerCircle :: Picture
outerCircle = Pictures . map
    (\i -> color (addColors (rainbow i) (rainbow (i-d)))
        . rotate i $ sq) $ [0,d..360-d]
    where
        sq = rotate (d/2) . translate 0 220 $ rectangleSolid 20 20

-- use of dims
dimLine :: Picture
dimLine = Pictures . take 20 $ zipWith color
    (iterate dim red)
    (iterate (translate 30 0) . translate (-300) 0 $ rectangleSolid 20 20 )

addLine :: Picture
addLine = Pictures $ zipWith color
    (map (\i -> mixColors (1-i) i (rainbow 0) (rainbow 180)) [0,0.1..1])
    (iterate (translate 0 (-30)) . translate 0 150 $ rectangleSolid 20 20)


-- from: https://en.wikipedia.org/wiki/HSL_and_HSV#Converting_to_RGB
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
