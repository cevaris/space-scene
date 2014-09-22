module Cube (drawCube) where 
 
import Graphics.UI.GLUT

import GLUtils


drawCube :: GLfloat -> IO ()
drawCube w = do
    cubeDisplayList <- (cube w)
    preservingMatrix $ do
        callList cubeDisplayList
 
cube :: GLfloat -> IO DisplayList
cube w = do
  cubeDisplayList <- defineNewList Compile $ do
    renderPrimitive Quads $ do
      vertex $ Vertex3 w w w
      vertex $ Vertex3 w w (-w)
      vertex $ Vertex3 w (-w) (-w)
      vertex $ Vertex3 w (-w) w
      vertex $ Vertex3 w w w
      vertex $ Vertex3 w w (-w)
      vertex $ Vertex3 (-w) w (-w)
      vertex $ Vertex3 (-w) w w
      vertex $ Vertex3 w w w
      vertex $ Vertex3 w (-w) w
      vertex $ Vertex3 (-w) (-w) w
      vertex $ Vertex3 (-w) w w
      vertex $ Vertex3 (-w) w w
      vertex $ Vertex3 (-w) w (-w)
      vertex $ Vertex3 (-w) (-w) (-w)
      vertex $ Vertex3 (-w) (-w) w
      vertex $ Vertex3 w (-w) w
      vertex $ Vertex3 w (-w) (-w)
      vertex $ Vertex3 (-w) (-w) (-w)
      vertex $ Vertex3 (-w) (-w) w
      vertex $ Vertex3 w w (-w)
      vertex $ Vertex3 w (-w) (-w)
      vertex $ Vertex3 (-w) (-w) (-w)
      vertex $ Vertex3 (-w) w (-w)

  return cubeDisplayList