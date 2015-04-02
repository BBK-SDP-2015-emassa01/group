

// TODO
//
// Make this an actor (by modifying the existing co-ordinator object) and write a message handler for at least the
// set method --> sets a pixel a particular color when going through the pixels row by row in Scene.traceImage eg set(x,y,color)
// --> set then reduces the number of pixels to be set by 1 (note var waiting = the no. of pixels to be set)
// --> once waiting = 0, then we can print, so ?call print from set method?
//
object Coordinator {
  def init(im: Image, of: String) = {
    image = im
    outfile = of
    waiting = im.width * im.height
  }

  // Number of pixels we're waiting for to be set.
  var waiting = 0
  var outfile: String = null
  var image: Image = null

  // TODO: make set a message
  def set(x: Int, y: Int, c: Colour) = {
    image(x, y) = c
    waiting -= 1
  }

  def print = {
    assert(waiting == 0)
    image.print(outfile)
  }
}
