module Station (drawStation) where 
 
import Graphics.UI.GLUT

import GLUtils
  
drawStation :: (Float, Float, Float) -> IO ()
drawStation (xT, yT, zT) = do

  mapM_ (\(x, y, z) -> do
    preservingMatrix $ do
      preservingAttrib [AllServerAttributes] $ do
        position (Light 0) $= Vertex4 5 5 15 0
        cullFace $= Just Back
        light (Light 0) $= Enabled
        color3f 255 69 0
        translate $ vector3f (xT*x) (yT*y) (zT*z)
        scale3f (0.01*abs(x)) (0.01*abs(x)) (0.01*abs(x))
        renderObject Solid (Sphere' 1 5 5)
      ) (clusterPoints 100 (mkStdGen 1) (mkStdGen 10) (mkStdGen 30))
            
      