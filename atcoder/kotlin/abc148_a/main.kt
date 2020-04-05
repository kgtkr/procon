fun main(args: Array<String>): Unit {
    val a = Integer.parseInt(readLine()!!)
    val b = Integer.parseInt(readLine()!!)
    println((arrayOf(1, 2, 3).filter { it != a && it != b })[0])
}
