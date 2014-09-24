module Fighter (drawFighter) where 
 
import Graphics.UI.GLUT

import GLUtils
  
drawFighter :: (Float, Float, Float) -> 
               (Float, Float, Float) ->
               (Float, Float, Float) -> IO ()
drawFighter (x, y, z) (dx, dy, dz) (ux, uy, uz) = do
  let wid  = 0.05
      nose = 0.50
      wing = 0.00
      strk = (-0.20)
      tail = (-0.50)
      d0 = sqrt(dx*dx+dy*dy+dz*dz)
      x0 = dx/d0
      y0 = dy/d0
      z0 = dz/d0;
      --  Unit vector in "up" direction
      d1 = sqrt(ux*ux+uy*uy+uz*uz);
      x1 = ux/d1;
      y1 = uy/d1;
      z1 = uz/d1;
      -- Cross product gives the third vector
      x2 = y0*z1-y1*z0;
      y2 = z0*x1-z1*x0;
      z2 = x0*y1-x1*y0;
      --mat = [x0, x1,  x2, 0,
      --      y0, y1,  y2, 0,
      --      z0, z1,  z2, 0,
      --       0,  0,   0,  1]
  mat <- newMatrix RowMajor $ listf [x0, x1,  x2, 0,
                                     y0, y1,  y2, 0,
                                     z0, z1,  z2, 0,
                                     0,  0,   0,  1]

  preservingMatrix $ do
    preservingAttrib [AllServerAttributes] $ do
      translate $ vector3f x y z
      multMatrix (mat :: GLmatrix GLfloat)

      color3f 0 0 0
        
      scale3f 0 0 0
