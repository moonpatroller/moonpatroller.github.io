package tutorial.webapp

case class GameWorld(rows: Int, cols: Int)
{
    val rocks: Array[Array[Int]] = Array.fill(rows, cols) { Math.floor(Math.random() * 30).toInt }

    def intersects(xPos: Int, yPos: Int): Boolean = 
    {
        val rockRow = yPos / 100
        val rockCol = xPos / 100
        println(s"rockRow $rockRow, rockCol $rockCol")
        if (rocks(rockRow)(rockCol) >= 5)
        {
            false
        }
        else
        {
            val rockPosY = rockRow * 100 + 50
            val rockPosX = rockCol * 100 + 50
            val dX = xPos - rockPosX
            val dY = yPos - rockPosY
            println(s"rock ${rocks(rockRow)(rockCol)}, rockPosY $rockPosY, rockPosX $rockPosX, xPos $xPos, yPos $yPos.")
            dX * dX + dY * dY < 30 * 30
        }
    }
}
