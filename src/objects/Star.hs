module Star (drawStar) where 
 
import Graphics.UI.GLUT
import Graphics.UI.GLUT.Objects

import GLUtils

drawStar :: GLdouble -> IO ()
drawStar w = do
  let r = w
      s = 100
      t = 100

  preservingMatrix $ do
    preservingAttrib [AllServerAttributes] $ do    
      color3f 1 1 0
    
      scale 0.1 0.1 (0.1::GLfloat)
    
      renderObject Solid (Sphere' r s t)