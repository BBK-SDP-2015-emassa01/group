# A Simple Ray Tracer ~ Group Programming

A simple ray tracer parallelised using AKKA actors.

A ray tracer works by casting rays from a virtual camera onto the scene, computing intersections with objects in the scene. 
The rays are cast through an invisible plane (the view window) in the scene that represents the generated image. The view window can
be thought of as a grid, where each square represents a pixel of the generated image. When a ray hits an object the ray is reflected 
and refracted, producing secondary rays which may intersect other objects. All of these rays contribute to the final colour
computed for the pixel.

#Running the Code

To run the code you will need an input file. A sample file is provided called input.dat.
This consists of a sequence of directives; sphere and light.

**Sphere** defines a sphere by the (x,y,z) position of its centre, its radius, its (r,g,b) colour,
and its reflectivity (all between 0 and 1).

**Light** defines a point light source by its (x,y,z) position and its (r,g,b) colour.

All numbers are floating point numbers.

To run the code;

  scala -cp . Trace input.dat output.png
  
The code writes a PNG image file, which you should be able to view using a web browser or other image viewer.
