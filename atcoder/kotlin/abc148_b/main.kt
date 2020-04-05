fun main(args: Array<String>): Unit {
    readLine()!!;
    println(readLine()!!
        .split(" ")
        .map { it.toList() }
        .toList()
        .let { it[0].zip(it[1]).map { it.toList() } }
        .flatten()
        .joinToString(""));
}
