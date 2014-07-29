import Graphics.Gloss as Gloss
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Data.Vector
import Graphics.Gloss.Geometry.Angle
import System.Random
import Data.Trees.KdTree as KdTree
import GHC.Float

data Boid = Boid { pos :: Gloss.Point,
                   vec :: Vector,
                   col :: Color } deriving Show

data World = World { windowDim   :: Int,
                     boidCount   :: Int,
                     momentum    :: Float,
                     maxVelocity :: Float,
                     boids       :: [Boid],
                     rules       :: [Rule],
                     txt         :: String,
                     mouseCoord  :: Gloss.Point}

data Rule = Rule { rule     :: (Boid -> [Boid] -> Vector),
                   weight   :: Float,
                   distance :: Double}

randPoints :: StdGen -> (Float, Float) -> [Gloss.Point]
randPoints g (min, max) =
    let p (a:b:rest) = (a, b) : p rest
    in (p (randomRs (min, max) g :: [Float]))

randColors :: StdGen -> [Color]
randColors g =
    let c (r:g:b:a:rest) = (r, b, b, 1.0) : c rest
        cs = (c (randomRs (0.0, 1.0) g :: [Float]))
    in map (\(r,g,b,a) -> makeColor r g b a) cs

randBoids :: StdGen -> (Float, Float) -> [Boid]
randBoids g (min, max) =
    map (\(p, c) -> Boid {pos=p,
                          vec=(0.1, 0.1),
                          col=c}) $ zip (randPoints g (min, max)) (randColors g)

avgV :: [Vector] -> Vector
avgV vs = normaliseV $ foldl (+) (0.0, 0.0) vs

avgP :: [Gloss.Point] -> Gloss.Point
avgP ps = let v = foldl (+) (0.0, 0.0) ps
           in mulSV (1.0 / fromIntegral (length ps)) v

nanCheckVOp :: (Vector -> Vector) -> Vector -> Vector
nanCheckVOp vOp v = let (x, y) = vOp v in
                    if isNaN x || isNaN y then v else (x, y)

divByMagV :: Vector -> Vector
divByMagV v = nanCheckVOp normaliseV v

boidPostions :: [Boid] -> [Gloss.Point]
boidPostions boids = map (\(Boid {pos=p}) -> p) boids

boidVecs :: [Boid] -> [Gloss.Point]
boidVecs boids = map (\(Boid {vec=v}) -> v) boids

boidAlign :: Boid -> [Boid] -> Vector
boidAlign _ [] = (0.0, 0.0)
boidAlign Boid {vec=v} neighbours = avgV $ (boidVecs neighbours) ++ [(v * (-1.0, -1.0))]

boidSeperate :: Boid -> [Boid] -> Vector
boidSeperate _ [] = (0.0, 0.0)
boidSeperate Boid {pos=p} neighbours = avgV $ map (divByMagV . (\x -> x - p))
                                       $ boidPostions neighbours

boidCohesion :: Boid -> [Boid] -> Vector
boidCohesion _ [] = (0.0, 0.0)
boidCohesion Boid {pos=p} neighbours = normaliseV $ (p - (avgP $ boidPostions neighbours))

boidAvoid :: Gloss.Point -> Float -> Boid -> Vector
boidAvoid target distance Boid {pos=p}
    = let targetV = target - p
      in if magV targetV < distance then divByMagV targetV else (0.0, 0.0)

wrapPoint :: Int -> Gloss.Point -> Gloss.Point
wrapPoint dim (x, y) =
    let (minx, miny) = (-20.0, -20.0)
        (maxx, maxy) = ((fromIntegral dim) + 20.0, (fromIntegral dim) + 20.0)
        w = maxx-minx
        h = maxy-miny
        x' = if x > maxx then x - w else (if x < minx then x + w else x)
        y' = if y > maxy then y - h else (if y < miny then y + h else y)
    in (x', y')

limitMagV :: Vector -> Float -> Vector
limitMagV v limit = if magV v > limit then mulSV limit (normaliseV v) else v

applyForce :: Float -> Float -> Boid -> Vector -> Boid
applyForce m maxV b f = let v  = (vec b)
                            v' = nanCheckVOp (\v -> v + (mulSV m v) + (mulSV (0.0 - m) f)) v
                        in b {vec = limitMagV v' maxV}

translateBoid :: Int -> Boid -> Boid
translateBoid dim b = b {pos = wrapPoint dim ((pos b) + (vec b))}

calcForce :: (Double -> Boid -> [Boid]) -> [Rule] -> Boid -> Vector
calcForce neighbours rules boid =
    foldl (+) (0.0, 0.0)
    $ map (\Rule {rule=r, weight=w, distance=d} ->
                 mulSV w $ r boid $ neighbours d boid) rules

instance KdTree.Point Boid where
    dimension _              = 2
    coord 0 Boid {pos=(x,_)} = (float2Double x)
    coord 1 Boid {pos=(_,y)} = (float2Double y)

updateVecs :: Float -> (Boid -> Vector) -> World -> World
updateVecs d forceFn world =
    let World {boids=boids, momentum=momentum, maxVelocity=maxVelocity} = world
    in world {boids = [applyForce momentum maxVelocity boid
                       $ mulSV d $ forceFn boid
                       | boid <- boids]}

translateBoids :: World -> World
translateBoids world = let World {windowDim=dim, boids=boids} = world
    in world {boids = map (translateBoid dim) boids}

updateFromRules :: Float -> World -> World
updateFromRules d world =
    let World {boids=boids, rules=rules} = world
        tree = KdTree.fromList boids
    in updateVecs d (calcForce (nearNeighbors tree) rules) world

updateTxt :: Float -> World -> World
updateTxt d world
    = world {txt = show (mouseCoord world)}

updateFromMouse :: Float -> World -> World
updateFromMouse d world =
    let World {windowDim=dim, boids=boids} = world
        halfDim = (int2Float dim) * 0.5
    in updateVecs (d * 200.0)
          (boidAvoid ((mouseCoord world) + (halfDim, halfDim)) 40.0) world

respondToEvent :: Event -> World -> World
respondToEvent (EventMotion coord) world
    = world {mouseCoord = coord}
respondToEvent _ world = world

boidPoly = Polygon [(0.0, (-0.5)), ((-0.3), 0.5), (0.3, 0.5)]
boidPicture color = scale 40 40 $ Color color boidPoly

boidPics :: [Boid] -> [Picture]
boidPics boids =
    map (\Boid {pos=(x,y), vec=v, col=c} ->
          translate x y
          $ rotate (radToDeg ((degToRad 270.0) - (argV v)))
          $ scale 0.6 0.6
          $ boidPicture c) boids

translateToOrigin :: Float -> Picture -> Picture
translateToOrigin dim pic =
    let halfDim = (- (dim / 2.0))
    in translate halfDim halfDim pic

txtPic :: Float -> String -> Picture
txtPic dim txt = translate (-dim*0.5+10.0) (dim*0.5-50.0)
                 $ scale 0.2 0.2
                 $ text txt

worldPic :: World -> Picture
worldPic World {windowDim=dim, boids=boids, txt=txt} =
    let fdim = (fromIntegral dim)
    in pictures [txtPic fdim txt,
                 translateToOrigin fdim (pictures (boidPics boids))]

main = let dim = 800
           nBoids = 50
           window = (InWindow "Flock" (dim, dim) (10, 10))
           in do
              g <- newStdGen
              play window white 60
                 World {windowDim=dim, boidCount=200, momentum=0.3, maxVelocity=3.0,
                        boids=(take nBoids $ randBoids g (0.0, (fromIntegral dim))),
                        rules=[ Rule {rule=boidAlign, weight=10.0, distance=180.0}
                              , Rule {rule=boidSeperate, weight=65.0, distance=40.0}
                              , Rule {rule=boidCohesion, weight=100.0, distance=180.0}
                               ],
                        txt="", mouseCoord=(0.0, 0.0)}
                 worldPic
                 --(\e  w -> w)
                 respondToEvent
                 (\d w -> translateBoids
                          . updateFromRules d
                          . updateTxt d
                          . updateFromMouse d
                          $ w)
