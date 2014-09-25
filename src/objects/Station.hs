module Station (drawStation) where 
 
import Graphics.UI.GLUT

import GLUtils

import Cube
import Pyramid
  
drawStation :: (Float, Float, Float) ->
               (Float, Float, Float) -> IO ()
drawStation (x, y, z) (ux, uy, uz) = do
    
    preservingMatrix $ do
      preservingAttrib [AllServerAttributes] $ do
        translate $ vector3f x y z
        drawCube (1, 1, 1)
        drawPyramid 1 (0,(-1),0) (1,0,0) (0,1,0)
        drawPyramid 1 (0,1,0) (1,0,0) (0,(-1),0)
        
            
      