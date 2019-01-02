val x = Int.MinValue // Min - 1 wraps around to IntMax
val y = Int.MaxValue // max + 1 wraps around to IntMin

x - 1
y + 1

math.abs(x) // No abs value for IntMin

val z = y.toDouble + 1 // IntMax to double + 1
z.toInt // back to Int = IntMax
0.toDouble
y.toDouble / z
Double.MaxValue



