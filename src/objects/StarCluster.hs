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

clusterPoints :: (RandomGen a, RandomGen b, RandomGen c) => Int -> a -> b -> c-> [(Float,Float,Float)]
clusterPoints n a b c = zip3 (take n (randomRs ((-1),1) a)) (take n (randomRs ((-1),1) b)) (take n (randomRs ((-1),1) c))
  
drawStarCluster :: (Float, Float, Float) -> IO ()
drawStarCluster (xT, yT, zT) = do
  

  --seed  <- getStdGen
  --seed <- mkStdGen 42
  --print $ clusterPoints 10 seed
  --print $ take 1 (randomStuff seed)

  let translatef = translate :: Vector3 GLfloat -> IO ()

  
    
  --translate (Vector3 xT yT zT)
  mapM_ (\(x, y, z) -> do
    preservingMatrix $ do
      preservingAttrib [AllServerAttributes] $ do
        --print ((show x) ++ " " ++ (show y) ++ " " ++ (show z))
        color3f 1 1 0
        --let v = vector3f (xT*x) (yT*y) (zT*z)
        --print $ show v
        translatef $ vector3f (xT*x) (yT*y) (zT*z)
        scale3f (0.01*abs(x)) (0.01*abs(x)) (0.01*abs(x))
        --scale 0.01 0.1 (0.1::GLfloat)        
        renderObject Solid (Sphere' 1 5 5)
      ) (clusterPoints 100 (mkStdGen 1) (mkStdGen 10) (mkStdGen 30))
            
      