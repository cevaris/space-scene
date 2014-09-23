module Sun (drawSun) where 
 
import Graphics.UI.GLUT
import Graphics.UI.GLUT.Objects
--import Graphics.Rendering.OpenGL ( Height, Radius, Slices, Stacks, GLint, GLdouble )

import GLUtils


drawSun :: GLdouble -> IO ()
drawSun w = do
  let r = w
      s = 100
      t = 100

  --position (Light 0) $= Vertex4 5 5 15 0
  --cullFace $= Just Back
  --lighting $= Enabled
  --light (Light 0) $= Enabled

  color3f 1 1 0
  
  scale 0.1 0.1 (0.1::GLfloat)
  
  renderObject Solid (Sphere' r s t)

  color3f 1 1 1


  --(Sphere' w 100 100)

    --object <- (sun w)
    --preservingMatrix $ do
      --(Sphere' w 100 100)
        --callList object
 
--sun :: GLfloat -> IO DisplayList
--sun w = do
--  object <- defineNewList Compile $ do
--    (Sphere' w 100 100)

--  return object