import akka.actor.{Actor, ActorRef}

case class TraceImage(width: Int, height: Int, y: Int, scene: Scene, actor: ActorRef)
case class SetPixel(x: Int, y: Int, c: Colour)
case class SetParent()

// TODO
//
// Make this an actor and write a message handler for at least the
// set method.
//
object Coordinator {
  def init(im: Image, of: String) = {
    image = im
    outfile = of
    waiting = im.width * im.height
  }

  // Number of pixels we're waiting for to be set.
  var waiting: Int = 0
  var outfile: String = null
  var image: Image = null

  //TODO: make set a message
  def set(x: Int, y: Int, c: Colour) = {
    image(x, y) = c
    waiting -= 1
  }

  def print = {
    assert(waiting == 0)
    image.print(outfile)
  }
}

class Coordinator extends Actor {
  private var parent: Option[ActorRef] = None

  override def receive = {
    case SetPixel(x, y, colour) => {
      Coordinator.set(x, y, colour)
      if(Coordinator.waiting == 0) {
        Coordinator.print
        parent.map(_ ! 1) //return a future to Scene.traceImage so that the method can now continue, and the count values in Trace can be printed correctly.
      }
    }
    case SetParent() => parent = Some(sender)
    case _ => throw new RuntimeException("error - unknown message received by Coordinator")
  }
}

class Tracer extends Actor{

  val eye = Vector.origin
  val angle = 90f // viewing angle
  //val angle = 180f // fisheye
  val frustum = (.5 * angle * math.Pi / 180).toFloat
  val cosf = math.cos(frustum)
  val sinf = math.sin(frustum)
  // Anti-aliasing parameter -- divide each pixel into sub-pixels and
  // average the results to get smoother images.
  val ss = Trace.AntiAliasingFactor

  override def receive = {
    case TraceImage(width, height, y, scene, coordinator) => {

      for (x <- 0 until width) {
        // This loop body can be sequential.
        var colour = Colour.black

        for (dx <- 0 until ss) {
          for (dy <- 0 until ss) {

            // Create a vector to the pixel on the view plane formed when
            // the eye is at the origin and the normal is the Z-axis.
            val dir = Vector(
              (sinf * 2 * ((x + dx.toFloat / ss) / width - .5)).toFloat,
              (sinf * 2 * (height.toFloat / width) * (.5 - (y + dy.toFloat / ss) / height)).toFloat,
              cosf.toFloat).normalized

            val c = scene.trace(Ray(eye, dir)) / (ss * ss)
            colour += c
          }
        }

        if (Vector(colour.r, colour.g, colour.b).norm < 1)
          Trace.darkCount += 1
        if (Vector(colour.r, colour.g, colour.b).norm > 1)
          Trace.lightCount += 1

        coordinator ! SetPixel(x, y, colour)
      }
    }
    case _ => throw new RuntimeException("error - unknown message received by Tracer")
  }

}
