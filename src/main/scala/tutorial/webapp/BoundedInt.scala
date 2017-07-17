package tutorial.webapp

case class BoundedInt(min: Int, max: Int, value: Int)
{
    def incr(): BoundedInt = this.copy(value = Math.min(this.value + 1, this.max))
    def decr(): BoundedInt = this.copy(value = Math.max(this.value - 1, this.min))
}
