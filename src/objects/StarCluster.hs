module StarCluster (drawStar) where 

import System.Random
--import Data.List
 
import Graphics.UI.GLUT
import Graphics.UI.GLUT.Objects

import Star
import GLUtils

clusterPoints :: Float -> Float -> Float -> [(Float,Float,Float)]
clusterPoints x y z = do 
  seed  <- newStdGen
  let k = take 3 (randomRs (1,100) seed)

  [(2,1,4)]

drawStarCluster :: GLdouble -> IO ()
drawStarCluster w = do
  let r = w
      s = 100
      t = 100



  preservingMatrix $ do
    preservingAttrib [AllServerAttributes] $ do    
      color3f 1 1 0
    
      scale 0.1 0.1 (0.1::GLfloat)
    
      renderObject Solid (Sphere' r s t)