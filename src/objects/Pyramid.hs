module Pyramid (drawPyramid) where 
 
import Graphics.UI.GLUT

import GLUtils
  
drawPyramid :: (Float, Float, Float) -> IO ()
drawPyramid (x, y, z) = do

  preservingMatrix $ do
    preservingAttrib [AllServerAttributes] $ do

      translate $ vector3f x y z
      scale3f 1 1 1
      
      renderPrimitive Triangles $ do

        -- Front
        color3f 1 0 0
        drawVertex3f  0 1 0
        drawVertex3f (-1) (-1) 1
        drawVertex3f 1 (-1) 1
   
        -- Right
        color3f 0 0 1
        drawVertex3f 0 1 0
        drawVertex3f 1 (-1) 1
        drawVertex3f 1 (-1) (-1)
   
        -- Back
        color3f 0 1 0
        drawVertex3f 0 1 0
        drawVertex3f 1 (-1) (-1)
        drawVertex3f (-1) (-1) (-1)
   
        -- Left
        color3f 1 0 1
        drawVertex3f  0 1 0
        drawVertex3f (-1)(-1)(-1)
        drawVertex3f (-1) (-1) 1

      renderPrimitive Quads $ do

        color3f 1 1 1
        drawVertex3f 1 (-1) 1
        drawVertex3f 1 (-1) (-1)
        drawVertex3f (-1) (-1) (-1)
        drawVertex3f (-1) (-1) 1

        
      
            
      