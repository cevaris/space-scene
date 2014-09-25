module StarCluster (drawStarCluster) where 

import System.Random
import Control.Applicative
--import Control.Monad.State

 
import Graphics.UI.GLUT
import Graphics.UI.GLUT.Objects

import Star
import GLUtils

clusterPoints :: (RandomGen a, RandomGen b, RandomGen c) => Int -> a -> b -> c-> [(Float,Float,Float)]
clusterPoints n a b c = zip3 (take n (randomRs ((-1),1) a)) (take n (randomRs ((-1),1) b)) (take n (randomRs ((-1),1) c))
  
drawStarCluster :: (Float, Float, Float) -> IO ()
drawStarCluster (xT, yT, zT) = do

  mapM_ (\(x, y, z) -> do
    preservingMatrix $ do
      preservingAttrib [AllServerAttributes] $ do
        --position (Light 0) $= Vertex4 5 5 15 0
        --cullFace $= Just Back
        ----lighting $= Enabled
        --light (Light 0) $= Enabled
        
        --color3f 1 1 0
        color3f 255 69 0
        translate $ vector3f (xT*x) (yT*y) (zT*z)
        scale3f (0.01*abs(x)) (0.01*abs(x)) (0.01*abs(x))
        renderObject Solid (Sphere' 1 5 5)
      ) (clusterPoints 100 (mkStdGen 1) (mkStdGen 10) (mkStdGen 30))
            
      