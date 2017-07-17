package tutorial.webapp

case class GameWorld(rows: Int, cols: Int)
{
    val rocks: Array[Array[Int]] = Array.fill(rows, cols) { Math.floor(Math.random() * 30).toInt }

    def intersects(xPos: Int, yPos: Int): Boolean = 
    {
        val rockRow = yPos / 100
        val rockCol = xPos / 100
        rocks(rockRow)(rockCol) < 5
    }
}
