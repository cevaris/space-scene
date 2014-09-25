module Sphere (drawSphere,spherePoints) where 
 
import Graphics.UI.GLUT
import Data.Fixed

import GLUtils

spherePoints:: Float -> [(Float,Float)]
spherePoints d = [(x,y) | x <- [(-90.0)..90.0], y <- [0.0..360.0], ((mod' x d) == 0 && x < 90), (mod' y d) == 0]


drawSphere :: Float-> (Float, Float, Float) -> IO ()
drawSphere s (x, y, z) = do

  let d = 5.0

  preservingMatrix $ do
    preservingAttrib [AllServerAttributes] $ do

      translate $ vector3f x y z
      scale3f s s s
      color3f 0 0 1

      mapM_ (\(ph, th) -> do
        renderPrimitive Points $ do
          drawVertex3f th ph 0
          drawVertex3f th (ph+d) 0
        --print $ "[ph " ++ show ph ++ "] [th " ++ show th ++ "]" 
        ) (spherePoints d)

        

        
            
      