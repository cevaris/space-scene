module Sphere (drawSphere,spherePoints) where 
 
import Graphics.UI.GLUT
import Data.Fixed

import GLUtils

spherePoints:: Float -> [(Float,Float)]
spherePoints d = [(x,y) | x <- [(-90.0)..90.0], y <- [0.0..360.0], ((mod' x d) == 0 && x < 90), (mod' y d) == 0]



drawLatBand :: Float -> (Float,Float) -> IO ()
drawLatBand d (ph, th) =  do
    renderPrimitive Points $ do

      --glColor3f(Cos(th)*Cos(th) , Sin(ph)*Sin(ph) , Sin(th)*Sin(th));
      --glVertex3d(Sin(th)*Cos(ph) , Sin(ph) , Cos(th)*Cos(ph));
      --drawVertex3f th ph 0
      --drawVertex3f th (ph+d) 0 

      drawVertex3f ((sin th)*(cos ph)) (sin ph) ((cos th)*(cos ph))
      drawVertex3f ((sin th)*(cos (ph+d))) (sin (ph+d)) ((cos th)*(cos (ph+d)))
    --print $ "[ph " ++ show ph ++ "] [th " ++ show th ++ "]" 


drawSphere :: Float-> (Float, Float, Float) -> IO ()
drawSphere s (x, y, z) = do

  let d = 5.0

  preservingMatrix $ do
    preservingAttrib [AllServerAttributes] $ do

      translate $ vector3f x y z
      scale3f s s s
      color3f 0 0 1


      --let points = [ (x,y) | ph <- [(-90.0)..90.0], th <- [0.0..360.0], ((mod' x d) == 0 && x < 90), (mod' y d) == 0]
      
      mapM_ (\(ph, th) -> drawLatBand d (ph, th)) (spherePoints d)

  --postRedisplay Nothing  

      --mapM_ (\(ph, th) -> do
      --  renderPrimitive Points $ do
      --    drawVertex3f th ph 0
      --    drawVertex3f th (ph+d) 0
      --  --print $ "[ph " ++ show ph ++ "] [th " ++ show th ++ "]" 
      --  ) (spherePoints d)

        

        
            
      