package io.github.aaronreidsmith

object Main {
  def main(args: Array[String]): Unit = {
    val inputFile      = getClass.getResource("/challenge.bin").getPath
    val virtualMachine = VirtualMachine.fromFile(inputFile)
    virtualMachine.run()
  }
}
