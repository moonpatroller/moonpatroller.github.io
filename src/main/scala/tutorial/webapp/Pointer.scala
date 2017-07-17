package tutorial.webapp

import org.scalajs.dom.CanvasRenderingContext2D

case class Pointer(x: Int, y: Int)
{
    def draw(ctx: CanvasRenderingContext2D): Unit =
    {
        ctx.beginPath()
        ctx.arc(this.x, this.y, 2, 0, 2 * Math.PI, true)
        ctx.stroke()
    }
}
