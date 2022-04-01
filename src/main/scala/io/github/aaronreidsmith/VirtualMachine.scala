package io.github.aaronreidsmith

import java.nio.file.{Files, Path}
import java.nio.{ByteBuffer, ByteOrder}
import scala.collection.mutable
import scala.io.StdIn

class VirtualMachine {
  private val MaxLiteral = 32768

  private val memory    = Array.fill(32768)(0)     // "memory with 15-bit address space storing 16-bit values"
  private val registers = Array.fill(8)(0)         // "eight registers"
  private val stack     = mutable.Stack.empty[Int] // "an unbounded stack which holds individual 16-bit values"

  // We register all of these as functions so they are not called until we want them to be
  // (since they mess with internal state)
  private def code4(): List[String] = List("take tablet", "use tablet")
  private def code5(): List[String] = List(
    "doorway",
    "north",
    "north",
    "bridge",
    "continue",
    "down",
    "east",
    "take empty lantern",
    "west",
    "west",
    "passage",
    "ladder",
    "west",
    "west",
    "south",
    "north"
  )
  private def code6(): List[String] = {
    val correctCoinOrder = {
      // These values were found by trial and error in the game
      // Solution ends up being 9 + 2 * 5^2 + 7^3 - 3 = 399
      val coins = List(
        ("red", 2),
        ("corroded", 3),
        ("shiny", 5),
        ("concave", 7),
        ("blue", 9)
      )
      coins.permutations
        .find { permutation =>
          val values = permutation.map(_._2)
          // _ + _ * _^2 + _^3 - _ = 399
          values.head + values(1) * values(2) * values(2) + values(3) * values(3) * values(3) - values(4) == 399
        }
        .get
        .map {
          case (color, _) => s"use $color coin"
        }
    }

    List(
      "take can",
      "use can",
      "use lantern",
      "west",
      "ladder",
      "darkness",
      "continue",
      "west",
      "west",
      "west",
      "west",
      "north",
      "take red coin",
      "north",
      "west",
      "take blue coin",
      "up",
      "take shiny coin",
      "down",
      "east",
      "east",
      "take concave coin",
      "down",
      "take corroded coin",
      "up",
      "west"
    ) ++ correctCoinOrder ++ List(
      "north",
      "take teleporter",
      "use teleporter"
    )
  }
  private def code7(): List[String] = {
    // I couldn't figure this out, so I just adapted this solution
    // https://github.com/zach2good/synacor-challenge/blob/04a3a8bec1ef76c25ff788b541d0cf35d3934035/vm.cpp#L623-L637
    registers(7) = 25734
    memory(521) = 1
    memory(522) = 32775
    memory(523) = 25734
    memory(5485) = 6
    memory(5486) = 21
    memory(5487) = 21
    memory(5488) = 21
    memory(5489) = 21
    memory(5490) = 21
    Nil
  }
  private def code8(): List[String] = List(
    "west",
    "north",
    "north",
    "north",
    "north",
    "north",
    "north",
    "north",
    "east",
    "take journal",
    "west",
    "north",
    "north",
    "take orb",
    "north",
    "east",
    "east",
    "north",
    "west",
    "south",
    "east",
    "east",
    "west",
    "north",
    "north",
    "east",
    "vault",
    "take mirror",
    "use mirror"
  )

  private var halted      = false
  private var pointer     = 0
  private var inputBuffer = mutable.Buffer.empty[Char]
  private val savedInputs = Map(
    // Code 1 is in instructions
    // Code 2 is shown on start-up
    // Code 3 is shown after the "self-test" phase of the VM
    "code4" -> code4 _,
    "code5" -> code5 _,
    "code6" -> code6 _,
    "code7" -> code7 _,
    "code8" -> code8 _
  )

  private def registerValueOrLiteral(int: Int): Int = {
    require(int <= 32775)
    if (int < MaxLiteral) int else registers(int % MaxLiteral)
  }

  private def writeToRegister(register: Int, value: Int): Unit = {
    require(32768 <= register && register <= 32775)
    registers(register % MaxLiteral) = value
  }

  def run(): Unit = {
    while (!halted) {
      val instruction = memory(pointer)
      instruction match {
        case 0 => // stop execution and terminate the program
          halted = true
        case 1 => // set register <a> to the value of <b>
          val a = memory(pointer + 1)
          val b = memory(pointer + 2)
          writeToRegister(a, registerValueOrLiteral(b))
          pointer += 3
        case 2 => // push <a> onto the stack
          val a = memory(pointer + 1)
          stack.push(registerValueOrLiteral(a))
          pointer += 2
        case 3 => // remove the top element from the stack and write it into <a>; empty stack = error
          val top = stack.pop()
          val a   = memory(pointer + 1)
          writeToRegister(a, top)
          pointer += 2
        case 4 => // set <a> to 1 if <b> is equal to <c>; set it to 0 otherwise
          val a     = memory(pointer + 1)
          val b     = memory(pointer + 2)
          val c     = memory(pointer + 3)
          val value = if (registerValueOrLiteral(b) == registerValueOrLiteral(c)) 1 else 0
          writeToRegister(a, value)
          pointer += 4
        case 5 => // set <a> to 1 if <b> is greater than <c>; set it to 0 otherwise
          val a     = memory(pointer + 1)
          val b     = memory(pointer + 2)
          val c     = memory(pointer + 3)
          val value = if (registerValueOrLiteral(b) > registerValueOrLiteral(c)) 1 else 0
          writeToRegister(a, value)
          pointer += 4
        case 6 => // jump to <a>
          val a = memory(pointer + 1)
          require(a < memory.length)
          pointer = a
        case 7 => // if <a> is nonzero, jump to <b>
          val a = memory(pointer + 1)
          val b = memory(pointer + 2)
          if (registerValueOrLiteral(a) != 0) {
            pointer = b
          } else {
            pointer += 3
          }
        case 8 => // if <a> is zero, jump to <b>
          val a = memory(pointer + 1)
          val b = memory(pointer + 2)
          if (registerValueOrLiteral(a) == 0) {
            pointer = b
          } else {
            pointer += 3
          }
        case 9 => // assign into <a> the sum of <b> and <c> (modulo 32768)
          val a     = memory(pointer + 1)
          val b     = memory(pointer + 2)
          val c     = memory(pointer + 3)
          val value = (registerValueOrLiteral(b) + registerValueOrLiteral(c)) % MaxLiteral
          writeToRegister(a, value)
          pointer += 4
        case 10 => // store into <a> the product of <b> and <c> (modulo 32768)
          val a     = memory(pointer + 1)
          val b     = memory(pointer + 2)
          val c     = memory(pointer + 3)
          val value = (registerValueOrLiteral(b) * registerValueOrLiteral(c)) % MaxLiteral
          writeToRegister(a, value)
          pointer += 4
        case 11 => // store into <a> the remainder of <b> divided by <c>
          val a     = memory(pointer + 1)
          val b     = memory(pointer + 2)
          val c     = memory(pointer + 3)
          val value = registerValueOrLiteral(b) % registerValueOrLiteral(c)
          writeToRegister(a, value)
          pointer += 4
        case 12 => // stores into <a> the bitwise and of <b> and <c>
          val a     = memory(pointer + 1)
          val b     = memory(pointer + 2)
          val c     = memory(pointer + 3)
          val value = registerValueOrLiteral(b) & registerValueOrLiteral(c)
          writeToRegister(a, value)
          pointer += 4
        case 13 => // stores into <a> the bitwise or of <b> and <c>
          val a     = memory(pointer + 1)
          val b     = memory(pointer + 2)
          val c     = memory(pointer + 3)
          val value = registerValueOrLiteral(b) | registerValueOrLiteral(c)
          writeToRegister(a, value)
          pointer += 4
        case 14 => // stores 15-bit bitwise inverse of <b> in <a>
          val a = memory(pointer + 1)
          val b = memory(pointer + 2)
          // https://github.com/tmrd993/synacor-challenge-java/blob/4facade7310627de2724581df703909105d909c1/src/main/java/com/timucinm/synacorchallenge/virtualmachine/instructions/Not.java#L46-L50
          val value = ~registerValueOrLiteral(b) & 0x7fff
          writeToRegister(a, value)
          pointer += 3
        case 15 => // read memory at address <b> and write it to <a>
          val a = memory(pointer + 1)
          val b = memory(pointer + 2)
          writeToRegister(a, memory(registerValueOrLiteral(b)))
          pointer += 3
        case 16 => // write the value from <b> into memory at address <a>
          val a = memory(pointer + 1)
          val b = memory(pointer + 2)
          memory(registerValueOrLiteral(a)) = registerValueOrLiteral(b)
          pointer += 3
        case 17 => // write the address of the next instruction to the stack and jump to <a>
          val a = memory(pointer + 1)
          stack.push(pointer + 2)
          pointer = registerValueOrLiteral(a)
        case 18 => // remove the top element from the stack and jump to it; empty stack = halt
          if (stack.nonEmpty) {
            pointer = stack.pop()
          } else {
            halted = true
          }
        case 19 => // write the character represented by ascii code <a> to the terminal
          val a    = memory(pointer + 1)
          val char = registerValueOrLiteral(a).toChar
          print(char)
          pointer += 2
        case 20 => // read a character from the terminal and write its ascii code to <a>; it can be assumed that once input starts, it will continue until a newline is encountered; this means that you can safely read whole lines from the keyboard and trust that they will be fully read
          val a = memory(pointer + 1)
          if (inputBuffer.isEmpty) {
            val input = StdIn.readLine()
            // If this is a saved input, then expand it to actual instructions, otherwise leave as is
            savedInputs.get(input) match {
              case Some(fn) =>
                inputBuffer = fn().mkString("\n").toBuffer :+ '\n'
              case None if input == "solve" =>
                inputBuffer = savedInputs.toSeq
                  .sortBy(_._1)
                  .flatMap(_._2.apply())
                  .mkString("\n")
                  .toBuffer :+
                  '\n'
              case None if input == "exit" =>
                halted = true
                inputBuffer = mutable.Buffer('\n')
              case None =>
                inputBuffer = input.toBuffer :+ '\n'
            }
          }
          val char = inputBuffer.head
          writeToRegister(a, char)
          inputBuffer = inputBuffer.tail
          pointer += 2
        case 21 => // no operation
          pointer += 1
        case other => throw new IllegalArgumentException(s"Invalid instruction: $other")
      }
    }
  }
}

object VirtualMachine {
  def fromFile(file: String): VirtualMachine = Files
    .readAllBytes(Path.of(file))
    .grouped(2)
    .zipWithIndex
    .foldLeft(new VirtualMachine) {
      case (virtualMachine, (pair, index)) =>
        val value = ByteBuffer.wrap(pair).order(ByteOrder.LITTLE_ENDIAN).getChar.toInt
        virtualMachine.memory(index) = value
        virtualMachine
    }
}
