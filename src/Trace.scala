

object Trace {

  val AntiAliasingFactor = 4
  val Width = 800
  val Height = 600

  var rayCount = 0
  var hitCount = 0
  var lightCount = 0
  var darkCount = 0

  def main(args: Array[String]): Unit = {
    //if (args.length != 2) {
    //  println("usage: scala Trace input.dat output.png")
    //  System.exit(-1)
    //}

    //val (infile, outfile) = (args(0), args(1))
    val (infile, outfile) = ("input.dat", "output.png") // remove this line and uncomment all lines above for submission
    val scene = Scene.fromFile(infile)

    render(scene, outfile, Width, Height)

    println("rays cast " + rayCount)
    println("rays hit " + hitCount)
    println("light " + lightCount)
    println("dark " + darkCount)
  }

  def render(scene: Scene, outfile: String, width: Int, height: Int) = {
    val image = new Image(width, height)

    // Init the coordinator -- must be done before starting it.
    Coordinator.init(image, outfile)

    // TODO: Start the Coordinator actor.

    scene.traceImage(width, height)

    // TODO:
    // This one is tricky--we can't simply send a message here to print
    // the image, since the actors started by traceImage haven't necessarily
    // finished yet.  Maybe print should be called elsewhere? <--need to move to a more appropriate place since
    // render will spawn several actors (via traceImage), so print won't work here
    Coordinator.print
  }
}

//IN TRACE --> create ActorSystem --> start co-ordinator actor --> scene.traceImage(width, height)
// IN SCENE --> create "tracer" actors to compute the colour of each pixel in a row and send to co-ordinator actor
// IN CO-ORDINATOR --> co-ordinator actor receives messages from tracer actors to set the colour of each px
// --> when var waiting = 0, PRINT --> END!