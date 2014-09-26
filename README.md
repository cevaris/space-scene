# Space Scene


A Haskell OpenGL/GLUT implementation Space 3D Scene

## Installation
    # Build/Compile project
    make
    # Execute application
    ./SpaceScene

  
### Key Bindings

| Command         | Key Binding   |
| --------------- |:-------------:|
| Close App       | ESC           |
| Rotate Right    | Right Arrow   |
| Rotate Left     | Left Arrow    |
| Rotate Down     | Down Arrow    |
| Rotate Up       | Up Arrow      |


### Objects Created
- Space Fighter
- Space Station
- Star (Spehre)
- Star Cluster (many randomly placed spheres)
- Pyramid
- Cube

### Sources

- Haskell OpenGL
  - https://www.opengl.org/discussion_boards/showthread.php/123078-best-way-to-draw-a-pyramid
  - https://github.com/haskell-opengl/GLUT 
- Objects
  - Space fighter is the Solid Plane object taken from class, ported to Haskell.
  - Star is the Sphere2 function taken form class, ported to Haskell.
