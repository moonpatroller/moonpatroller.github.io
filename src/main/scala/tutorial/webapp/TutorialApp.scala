package tutorial.webapp

import scala.scalajs.js
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
    val walkingRight = Array(
        Position(16, 34, 17, 31),
        Position(34, 34, 17, 31),
        Position(54, 34, 17, 31),
        Position(72, 34, 17, 31)
    )
    val standingLeft = Position(163, 34, 12, 31)
    val walkingLeft = Array(
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
        dir = Left
        action = Walk
        nextCycle()
        xPos = if (xPos < 1) maxXPos - 1 else xPos - 1
    }

    def walkRight(maxXPos: Int, maxYPos: Int): Unit = {
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

    def getLaserOffset(): (Int, Int) = (7, 15)

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

case class Pointer(x: Int, y: Int)
{
    def draw(ctx: CanvasRenderingContext2D): Unit =
    {
        ctx.beginPath()
        ctx.arc(this.x, this.y, 2, 0, 2 * Math.PI, true)
        ctx.stroke()
    }
}

class Laser
{
    var laserCount = 0

    def getCount(): Int = laserCount

    val sound: js.Dynamic = 
        js.Dynamic.newInstance(js.Dynamic.global.Howl)(
            js.Dictionary("src" -> js.Array("laser.mp3"))
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

case class BoundedInt(min: Int, max: Int, value: Int)
{
    def incr(): BoundedInt = this.copy(value = Math.min(this.value + 1, this.max))
    def decr(): BoundedInt = this.copy(value = Math.max(this.value - 1, this.min))
}

class World(canvas: Canvas)
{
    var crystalsImg: HTMLImageElement = dom.document.createElement("img").asInstanceOf[HTMLImageElement]
    crystalsImg.src = "crystals_clear.png"

    def drawCrystal(n: Int, x: Int, y: Int, isHighlighted: Boolean, drawHP: Boolean, laserCount: Int): Unit =
    {
        val height = 66
        val width = 56
        ctx.drawImage(crystalsImg, 278, n * 68, width, height, x, y, width, height)
        if (isHighlighted)
        {
            ctx.beginPath()
            ctx.strokeStyle = "#0f0"
            ctx.arc(x + 28, y + 33, 33, 0, 2 * Math.PI, true)
            ctx.stroke()

            ctx.fillStyle = "#000"
            ctx.fillRect(x, y - 10, width, 10)

            ctx.fillStyle = "#99f"
            ctx.fillRect(x, y - 10, width * laserCount / 100, 10)

            ctx.beginPath()
            ctx.lineWidth = 2
            ctx.strokeStyle = "#fff"
            ctx.rect(x, y - 10, width, 10)
            ctx.stroke()
        }
    }

    case class GameWorld(widthPx: Int, heightPx: Int)
    {
        val rocks: Array[Array[Int]] = Array.fill(100, 100) { Math.floor(Math.random() * 30).toInt }
    }

    var world = GameWorld(100000, 100000)
    var guy = KeenSprite(100, 100)

    var viewLeft = BoundedInt(canvas.width  / 5, canvas.width  * 4 / 5, canvas.width / 2)
    var viewTop  = BoundedInt(canvas.height / 5, canvas.height * 4 / 5, canvas.height / 2)

    val ctx = canvas.getContext("2d").asInstanceOf[CanvasRenderingContext2D]
    val bgColor = {
        val r = scala.util.Random
        s"rgb(${r.nextInt(256)},${r.nextInt(256)},${r.nextInt(256)})"
    }

    var pointer = Pointer(0, 0)
    var rightButtonDown = false

    case class Keyboard(var left: Boolean, var right: Boolean, var up: Boolean, var down: Boolean)
    val keyboard = Keyboard(false, false, false, false)

    canvas.style.cursor = "none"

    canvas.addEventListener("mousemove", (event: MouseEvent) => {
        pointer = Pointer(event.clientX.toInt, event.clientY.toInt)
    })

    canvas.addEventListener("contextmenu", (e: MouseEvent) => { // Not compatible with IE < 9
        e.preventDefault()
    }, false)

    canvas.addEventListener("mousedown", (e: MouseEvent) => { // Not compatible with IE < 9
        if (e.button == 2) // 0, 1, 2 => left, middle, right buttons
        {
            rightButtonDown = true
        }
    })

    canvas.addEventListener("mouseup", (e: MouseEvent) => { // Not compatible with IE < 9
        if (e.button == 2) // 0, 1, 2 => left, middle, right buttons
        {
            rightButtonDown = false
        }
    })

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

    val laserColors = Array("#0f0", "#f00", "#0f0", "#f00", "#0f0")
    val laser = new Laser()

    val (guyWidthOffset, guyHeightOffset) = guy.getLaserOffset()

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

        var highlightedCoords = None: Option[(Int, Int)]

        for (x <- 0 until 6;
             y <- 0 until 6) 
        {
            val rockX = (firstRow + y) % 100
            val rockY = (firstCol + x) % 100
            val rockNum = world.rocks(rockX)(rockY)
            if (rockNum < 5)
            {
                val rockCanvasX = x * 100 + 25 - (guy.xPos % 100)
                val rockCanvasY = y * 100 + 25 - (guy.yPos % 100)
                val dx = pointer.x - (rockCanvasX + 28)
                val dy = pointer.y - (rockCanvasY + 33)
                val isHighlighted = dx * dx + dy * dy < 25 * 25
                if (isHighlighted)
                {
                    highlightedCoords = Some((rockX, rockY))
                }
                drawCrystal(rockNum, rockCanvasX, rockCanvasY, isHighlighted, rightButtonDown, laser.getCount())
            }
        }

        guy.draw(ctx, viewLeft.value, viewTop.value)

        // draw laser
        if (rightButtonDown)
        {
            highlightedCoords match { 
                case Some((rockX, rockY)) =>
                    laser.incr()
                    if (laser.getCount() >= 100)
                    {
                        laser.stop()
                        world.rocks(rockX)(rockY) = 100
                        rightButtonDown = false
                    }
                case None => 
                    laser.stop()
                    rightButtonDown = false
            }

            ctx.lineWidth = 2
            ctx.lineCap = "round"
    
            // ctx.strokeStyle = "#0f0"
            val grad = ctx.createLinearGradient(viewLeft.value + guyWidthOffset, viewTop.value + guyHeightOffset, pointer.x, pointer.y)
            for (i <- 0 until laserColors.length) {
                println(i + ": " + ((laser.getCount() + i) % laserColors.length))
                grad.addColorStop(0.25 * i, laserColors((laser.getCount() / 2 + i) % laserColors.length))
            }
            ctx.strokeStyle = grad

            ctx.beginPath()
            ctx.moveTo(viewLeft.value + guyWidthOffset, viewTop.value + guyHeightOffset)
            ctx.lineTo(pointer.x, pointer.y)
            ctx.stroke()
        }
        else
        {
            laser.stop()
        }

        pointer.draw(ctx)

        val info = document.getElementById("info")
        info.innerHTML = s"viewTop: ${viewTop.value}, guy.yPos: ${guy.yPos}, viewLeft: ${viewLeft.value}, guy.xPos: ${guy.xPos}, firstRow: $firstRow, firstCol: $firstCol"

        document.defaultView.requestAnimationFrame(draw)
    }
}
