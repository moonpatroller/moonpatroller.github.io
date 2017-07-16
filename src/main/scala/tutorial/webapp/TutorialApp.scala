package tutorial.webapp

import org.scalajs.jquery.jQuery
import org.scalajs.dom
import dom.document
import dom.html.Canvas
import dom.CanvasRenderingContext2D
import org.scalajs.dom.raw.{MouseEvent, KeyboardEvent, HTMLImageElement}


object TutorialApp {
  def main(args: Array[String]): Unit = {
    jQuery(() => setupUI())
  }

    // def appendPar(targetNode: dom.Node, text: String): Unit = {
    //   val parNode = document.createElement("p")
    //   val textNode = document.createTextNode(text)
    //   parNode.appendChild(textNode)
    //   targetNode.appendChild(parNode)
    // }

  def addClickedMessage(): Unit = {
    jQuery("body").append("<p>You clicked the button!</p>")
  }

  def setupUI(): Unit = {
    // jQuery("#click-me-button").click(() => addClickedMessage())
    // jQuery("body").append("<p>Hello World!</p>")

    // val $ = jQuery
    // val canvas = $("#tutorial")
    // canvas.css("cursor", "none")

    val canvas = document.getElementById("tutorial").asInstanceOf[Canvas]
    val world = new World(canvas)
    world.draw(0.0)
  }

}

class World(canvas: Canvas)
{
    var crystalsImg: HTMLImageElement = dom.document.createElement("img").asInstanceOf[HTMLImageElement]
    crystalsImg.src = "crystals_clear.png"

    def drawCrystal(n: Int, x: Int, y: Int): Unit =
    {
        val height = 66
        val width = 56
        ctx.drawImage(crystalsImg, 278, n * 68, width, height, x, y, width, height)
        // ctx.strokeRect(x, y, width, height);
    }

    case class Guy(xPos: Int, yPos: Int)
    {

        var guyImg: HTMLImageElement = dom.document.createElement("img").asInstanceOf[HTMLImageElement]
        guyImg.src = "keen_clear.png"

        var facing = "left"

        def draw(viewLeft: Int, viewTop: Int): Unit =
        {
            facing match {
                case "left" =>
                    val width = 176 - 163
                    val height = 31
                    ctx.drawImage(guyImg, 163, 34, width, height, xPos - viewLeft, yPos - viewTop, width, height)

                case "right" | "up" | "down" =>
                    var width = 11
                    var height = 31
                    ctx.drawImage(guyImg, 2, 34, width, height, xPos - viewLeft, yPos - viewTop, width, height)
            }
        }
    }

    case class World(widthPx: Int, heightPx: Int)
    {
        val rocks: Array[Array[Int]] = Array.fill(100, 100) { Math.floor(Math.random() * 20).toInt }
    }

    var world = World(100000, 100000)
    var guy = Guy(100, 100)

    var viewLeft = 0
    var viewTop = 0

    val ctx = canvas.getContext("2d").asInstanceOf[CanvasRenderingContext2D]
    val r = scala.util.Random
    val bgColor = s"rgb(${r.nextInt(256)},${r.nextInt(256)},${r.nextInt(256)})"

    case class Pointer(x: Int, y: Int)
    {
        def draw(): Unit =
        {
            ctx.beginPath()
            ctx.arc(this.x, this.y, 2, 0, 2 * Math.PI, true)
            ctx.stroke()
        }
    }
    var pointer = Pointer(0, 0)

    case class Keyboard(var left: Boolean, var right: Boolean, var up: Boolean, var down: Boolean)
    val keyboard = Keyboard(false, false, false, false)

    canvas.style.cursor = "none";
    canvas.addEventListener("mousemove", (event: MouseEvent) => {
        pointer = Pointer(event.clientX.toInt, event.clientY.toInt)
    })

    document.addEventListener("keydown", (event: KeyboardEvent) => {
        event.key match {
            case "ArrowLeft" =>
                keyboard.left  = true;
                keyboard.right = false;
            case "ArrowRight" =>
                keyboard.left  = false;
                keyboard.right = true;
            case "ArrowUp" =>
                keyboard.up   = true;
                keyboard.down = false;
            case "ArrowDown" =>
                keyboard.up   = false;
                keyboard.down = true;
        }
    });

    document.addEventListener("keyup", (event: KeyboardEvent) => {
        event.key match {
            case "ArrowLeft" =>  keyboard.left  = false
            case "ArrowRight" => keyboard.right = false
            case "ArrowUp" =>    keyboard.up    = false
            case "ArrowDown" =>  keyboard.down  = false
        }
    });

    def draw(idk: Double): Unit =
    {
        ctx.clearRect(0, 0, 500, 500)

        ctx.fillStyle = bgColor
        ctx.fillRect(0, 0, 500, 500)

        if (keyboard.left) {
            guy = guy.copy(xPos = guy.xPos - 1)
            if (guy.xPos < 0) {
                guy = guy.copy(xPos = world.widthPx - 1) 
            }
            guy.facing = "left"
        }
        else if (keyboard.right) {
            guy = guy.copy(xPos = (guy.xPos + 1) % world.widthPx) 
            guy.facing = "right"
        }
        else if (keyboard.up) {
            guy = guy.copy(yPos = guy.yPos - 1)
            if (guy.yPos < 0) {
                guy = guy.copy(yPos = world.heightPx - 1)
            }
        }
        else if (keyboard.down) {
            guy = guy.copy(yPos = (guy.yPos + 1) % world.heightPx)
        }

        if (guy.yPos > viewTop + canvas.height * 0.8) {
            viewTop = Math.floor(guy.yPos - canvas.height * 0.8).toInt
        }
        else if (guy.yPos < viewTop + canvas.height * 0.2) {
            viewTop = Math.floor(guy.yPos - canvas.height * 0.2).toInt
        }

        if (guy.xPos > viewLeft + canvas.width * 0.8) {
            viewLeft = Math.floor(guy.xPos - canvas.width * 0.8).toInt
        }
        else if (guy.xPos < viewLeft + canvas.width * 0.2) {
            viewLeft = Math.floor(guy.xPos - canvas.width * 0.2).toInt
        }

        // col and row here are for rocks, constrain to appearing every 100px.
        val firstCol = Math.min(world.widthPx  / 100 - 1, Math.max(0, Math.floor(viewLeft / 100))).toInt
        val firstRow = Math.min(world.heightPx / 100 - 1, Math.max(0, Math.floor(viewTop  / 100))).toInt

        for (x <- 0 until 6;
             y <- 0 until 6) 
        {
            val rockNum = world.rocks(firstCol + x)(firstRow + y)
            if (rockNum < 5) {
                drawCrystal(rockNum, 
                    x * 100 + 25 - (viewLeft % 100), 
                    y * 100 + 25 - (viewTop  % 100))
            }
        }

        guy.draw(viewLeft, viewTop);
        pointer.draw()

        // ctx.strokeStyle = "rgb(0,0,0)"
        // ctx.strokeRect(0, 0, 500, 500)
        document.defaultView.requestAnimationFrame(draw)

    }
}
