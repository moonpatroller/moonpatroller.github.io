package tutorial.webapp

import scala.scalajs.js.Dynamic.global

import org.scalajs.dom
import dom.document
import dom.html.Canvas
import dom.CanvasRenderingContext2D
import org.scalajs.dom.raw.{MouseEvent, KeyboardEvent, HTMLImageElement}


object TutorialApp
{
    def main(args: Array[String]): Unit = {
        setupUI()
    }

    def setupUI(): Unit = {
        val canvas = document.getElementById("tutorial").asInstanceOf[Canvas]
        val world = new World(canvas)
        world.draw(0.0)
    }
}

case class KeenSprite(var xPos: Int, var yPos: Int)
{
    case class Position(x: Int, y: Int, width: Int, height: Int)

    val standingRight = Position(2, 34, 11, 31)
    val walkingRight = Vector(
        Position(16, 34, 17, 31),
        Position(34, 34, 17, 31),
        Position(54, 34, 17, 31),
        Position(72, 34, 17, 31)
    )
    val standingLeft = Position(163, 34, 12, 31)
    val walkingLeft = Vector(
        Position(178, 34, 17, 31),
        Position(196, 34, 17, 31),
        Position(216, 34, 17, 31),
        Position(233, 34, 17, 31)
    )

    val guyImg: HTMLImageElement = dom.document.createElement("img").asInstanceOf[HTMLImageElement]
    guyImg.src = "keen_clear.png"

    sealed trait Direction
    case object Left extends Direction
    case object Right extends Direction

    sealed trait Action
    case object Walk extends Action
    case object Stand extends Action

    var dir: Direction = Right
    var action: Action = Stand
    var cycle = 0

    def rest(): Unit = {
        action = Stand
        cycle = 0
    }

    def walkLeft(maxXPos: Int, maxYPos: Int): Unit = {
        global.console.log("walk left")
        dir = Left
        action = Walk
        nextCycle()
        xPos = if (xPos < 1) maxXPos - 1 else xPos - 1
    }

    def walkRight(maxXPos: Int, maxYPos: Int): Unit = {
        global.console.log("walk right")
        dir = Right
        action = Walk
        nextCycle()
        xPos = (xPos + 1) % maxXPos
    }

    def walkUp(maxXPos: Int, maxYPos: Int): Unit = {
        action = Walk
        nextCycle()
        yPos = if (yPos < 1) maxYPos - 1 else yPos - 1
    }

    def walkDown(maxXPos: Int, maxYPos: Int): Unit = {
        action = Walk
        nextCycle()
        yPos = (yPos + 1) % maxYPos
    }

    def nextCycle(): Unit = cycle = (cycle + 1) % 36

    def draw(ctx: CanvasRenderingContext2D, viewLeft: Int, viewTop: Int): Unit =
    {
        val p = 
            (dir, action) match {
                case (Left,  Stand) => standingLeft
                case (Left,  Walk) => walkingLeft(cycle / 9 % walkingLeft.length)
                case (Right, Stand) => standingRight
                case (Right, Walk) => walkingRight(cycle / 9 % walkingRight.length)
            }
        ctx.drawImage(guyImg, p.x, p.y, p.width, p.height, viewLeft, viewTop, p.width, p.height)
    }
}

class Pointer(x: Int, y: Int)
{
    def draw(ctx: CanvasRenderingContext2D): Unit =
    {
        ctx.beginPath()
        ctx.arc(this.x, this.y, 2, 0, 2 * Math.PI, true)
        ctx.stroke()
    }
}

case class BoundedInt(min: Int, max: Int, value: Int)
{
    def incr(): BoundedInt = this.copy(value = Math.min(this.value + 1, this.max))
    def decr(): BoundedInt = this.copy(value = Math.max(this.value - 1, this.min))
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
    }

    case class GameWorld(widthPx: Int, heightPx: Int)
    {
        val rocks: Array[Array[Int]] = Array.fill(100, 100) { Math.floor(Math.random() * 20).toInt }
    }

    var world = GameWorld(100000, 100000)
    var guy = KeenSprite(100, 100)

    var viewLeft = BoundedInt(canvas.width  / 5, canvas.width  * 4 / 5, canvas.width / 2)
    var viewTop  = BoundedInt(canvas.height / 5, canvas.height * 4 / 5, canvas.height / 2)

    val ctx = canvas.getContext("2d").asInstanceOf[CanvasRenderingContext2D]
    val r = scala.util.Random
    val bgColor = s"rgb(${r.nextInt(256)},${r.nextInt(256)},${r.nextInt(256)})"

    var pointer = new Pointer(0, 0)

    case class Keyboard(var left: Boolean, var right: Boolean, var up: Boolean, var down: Boolean)
    val keyboard = Keyboard(false, false, false, false)

    canvas.style.cursor = "none"
    canvas.addEventListener("mousemove", (event: MouseEvent) => {
        pointer = new Pointer(event.clientX.toInt, event.clientY.toInt)
    })
    canvas.addEventListener("contextmenu", (e: MouseEvent) => { // Not compatible with IE < 9
        e.preventDefault()
    }, false)

    document.addEventListener("keydown", (event: KeyboardEvent) => {
        event.key match {
            case "ArrowLeft" =>
                keyboard.left  = true
                keyboard.right = false
            case "ArrowRight" =>
                keyboard.left  = false
                keyboard.right = true
            case "ArrowUp" =>
                keyboard.up   = true
                keyboard.down = false
            case "ArrowDown" =>
                keyboard.up   = false
                keyboard.down = true
            case _ =>
        }
    })

    document.addEventListener("keyup", (event: KeyboardEvent) => {

        event.key match {
            case "ArrowLeft" =>  keyboard.left  = false
            case "ArrowRight" => keyboard.right = false
            case "ArrowUp" =>    keyboard.up    = false
            case "ArrowDown" =>  keyboard.down  = false
            case _ =>
        }
    })

    def draw(idk: Double): Unit =
    {
        ctx.clearRect(0, 0, 500, 500)

        ctx.fillStyle = bgColor
        ctx.fillRect(0, 0, 500, 500)

        if (keyboard.left) {
            guy.walkLeft(world.widthPx, world.heightPx)
            viewLeft = viewLeft.decr()
        }
        else if (keyboard.right) {
            guy.walkRight(world.widthPx, world.heightPx)
            viewLeft = viewLeft.incr()
        }
        else if (keyboard.up) {
            guy.walkUp(world.widthPx, world.heightPx)
            viewTop = viewTop.decr()
        }
        else if (keyboard.down) {
            guy.walkDown(world.widthPx, world.heightPx)
            viewTop = viewTop.incr()
        }
        else {
            guy.rest()
        }

        val firstCol = guy.xPos / 100
        val firstRow = guy.yPos / 100
        println((firstRow, firstCol))

        for (x <- 0 until 6;
             y <- 0 until 6) 
        {
            val rockNum = world.rocks((firstRow + y) % 100)((firstCol + x) % 100)
            if (rockNum < 5)
            {
                drawCrystal(rockNum, 
                    // x * 100 + 25 - (viewLeft.value % 100), 
                    // y * 100 + 25 - (viewTop.value  % 100))
                    x * 100 + 25 - (guy.xPos % 100), 
                    y * 100 + 25 - (guy.yPos % 100))
            }
        }

        guy.draw(ctx, viewLeft.value, viewTop.value)
        pointer.draw(ctx)

        val info = document.getElementById("info")
        info.innerHTML = s"viewTop: ${viewTop.value}, guy.yPos: ${guy.yPos}, viewLeft: ${viewLeft.value}, guy.xPos: ${guy.xPos}, firstRow: $firstRow, firstCol: $firstCol"

        document.defaultView.requestAnimationFrame(draw)
    }
}
