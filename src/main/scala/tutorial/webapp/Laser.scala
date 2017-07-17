package tutorial.webapp

import scala.scalajs.js.{Array, Dictionary, Dynamic}


class Laser
{
    var laserCount = 0

    def getCount(): Int = laserCount

    val sound: Dynamic = 
        Dynamic.newInstance(Dynamic.global.Howl)(
            Dictionary("src" -> Array("laser.mp3"))
        )

    def incr(): Unit = {
        if (laserCount == 0)
        {
            sound.play()
        }
        laserCount += 1
    }

    def stop(): Unit = {
        if (laserCount != 0)
        {
            sound.stop()
            laserCount = 0
        }
    }
}
