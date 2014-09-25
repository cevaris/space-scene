module Station (drawStation) where 
 
import Graphics.UI.GLUT

import GLUtils

import Cube
import Pyramid
  
drawStation :: (Float, Float, Float) ->
               (Float, Float, Float) -> IO ()
drawStation (x, y, z) (ux, uy, uz) = do

    let ws = 0.6
        wd = 0.5
    
    preservingMatrix $ do
      preservingAttrib [AllServerAttributes] $ do
        translate $ vector3f x y z
        drawCube 3 (0,0,0)
        -- Up
        drawPyramid 1 (0,(-1),0) (1,0,0) (0,1,0)
        -- Down
        drawPyramid 1 (0,1,0) (1,0,0) (0,(-1),0)
        
        -- Front
        drawPyramid ws (0,0,wd) (1,0,0) (0,0,(-1))
        -- Back
        drawPyramid ws (0,0,(-wd)) (1,0,0) (0,0,1)
        -- Right
        drawPyramid ws (wd,0,0) (0,1,0) ((-1),0,0)
        ---- Front
        drawPyramid ws ((-wd),0,0) (0,1,0) (1,0,0)
        
            
      