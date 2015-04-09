
// TODO
//
// Make this an actor (by modifying the existing co-ordinator object) and write a message handler for at least the
// set method --> sets a pixel a particular color when going through the pixels row by row in Scene.traceImage eg set(x,y,color)
// --> set then reduces the number of pixels to be set by 1 (note var waiting = the no. of pixels to be set)
// --> once waiting = 0, then we can print, so ?call print from set method?
//
case class StartRayTracer(scene: Scene)
case class ProcessedLine(x: Int, y: Int, c: Colour, darkCount: Int, lightCount: Int, rayCount: Int)
case class TraceImage(y: Int, scene: Scene)
case class SetHitCount()

object Coordinator{

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
  //def set(x: Int, y: Int, c: Colour) = {
  //  image(x, y) = c
  //  waiting -= 1
  //}

  def print = {
    assert(waiting == 0)
    image.print(outfile)
  }

}

import Coordinator._
import akka.actor.{Actor, ActorRef, Props}

class Coordinator extends Actor{

  private var parent: Option[ActorRef] = None //added

  override def receive = {
    case StartRayTracer(scene) =>
      parent = Some(sender)
      for (y <- 0 until image.height) {
        context.actorOf(Props[Tracer], "tracer" + y) ! TraceImage(y, scene)
      }
    case SetHitCount => Trace.hitCount += 1
    case ProcessedLine(x, y, colour, darkCount, lightCount, rayCount) =>
      Coordinator.image(x, y) = colour
      Coordinator.waiting -= 1
      Trace.darkCount += darkCount
      Trace.lightCount += lightCount
      Trace.rayCount += rayCount
      if(waiting == 0){
        parent.map(_ ! waiting)
      }
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
    case TraceImage(y, scene) => {

      for (x <- 0 until image.width) {
        // This loop body can be sequential.
        var colour = Colour.black
        var rayCount = 0

        for (dx <- 0 until ss) {
          for (dy <- 0 until ss) {

            // Create a vector to the pixel on the view plane formed when
            // the eye is at the origin and the normal is the Z-axis.
            val dir = Vector(
              (sinf * 2 * ((x + dx.toFloat / ss) / image.width - .5)).toFloat,
              (sinf * 2 * (image.height.toFloat / image.width) * (.5 - (y + dy.toFloat / ss) / image.height)).toFloat,
              cosf.toFloat).normalized

            val c = scene.trace(Ray(eye, dir)) / (ss * ss)
            rayCount += 1
            colour += c
          }
        }
        var darkCount = 0
        var lightCount = 0

        if (Vector(colour.r, colour.g, colour.b).norm < 1)
          //Trace.darkCount += 1
          darkCount += 1
        if (Vector(colour.r, colour.g, colour.b).norm > 1)
          //Trace.lightCount += 1
          lightCount += 1

        sender ! ProcessedLine(x, y, colour, darkCount, lightCount, rayCount)

      }
    }
    case _ => throw new RuntimeException("error - unknown message received by Tracer")
  }
}
