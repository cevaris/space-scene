module StarCluster (drawStarCluster) where 

import System.Random
import Control.Applicative
--import Control.Monad.State

 
import Graphics.UI.GLUT
import Graphics.UI.GLUT.Objects

import Star
import GLUtils


--randomStuff :: RandomGen g => g -> [[Float]]
--randomStuff g = work (randomRs (0.0, 1.0) g)

--work :: [Float] -> [[Float]]
--work (r:rs) =
--    let n = truncate (r * 7.0) + 1
--        (xs,ys) = splitAt n rs
--    in xs : work ys

--randomTupleR :: (Random a, Random b, Random c, RandomGen g) => (a, a) -> (b, b) -> (c, c) -> g -> ((a, b, c), g)
--randomTupleR xb yb zb = runState (liftA3 (,) (state $ randomR xb) (state $ randomR yb) (state $ randomR zb))

clusterPoints :: (RandomGen g) => Int -> g -> [(Float,Float,Float)]
clusterPoints n s = zip3 (take n (randomRs ((-1),1) s)) (take n (randomRs ((-1),1) s)) (take n (randomRs ((-1),1) s))
  
drawStarCluster :: (Float, Float, Float) -> GLdouble -> IO ()
drawStarCluster (xT, yT, zT) w = do
  

  seed  <- getStdGen
  --seed <- mkStdGen 42
  --print $ clusterPoints 10 seed
  --print $ take 1 (randomStuff seed)

  let translatef = translate :: Vector3 GLfloat -> IO ()

  
    
  --translate (Vector3 xT yT zT)
  mapM_ (\(x, y, z) -> do
    preservingMatrix $ do
      preservingAttrib [AllServerAttributes] $ do
        color3f 1 1 0
        scale 0.1 0.1 (0.1::GLfloat)
        --scale x x (x::GLfloat)
        --translate (Vector3 (xT+x) (yT+y) (zT+z))
        translatef $ vector3f (xT*x) (yT*y) (zT*z)
        renderObject Solid (Sphere' w 10 10)
      ) (clusterPoints 10 seed)
            
      