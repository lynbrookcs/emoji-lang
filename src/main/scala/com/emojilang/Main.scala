package com.emojilang

object EmojiLang {
  def `😡`(msg: Any): Unit = println(msg.toString)

  def `👎`(boolean: Boolean): Boolean = !boolean

  class ResultOfIf[T](optionalIfResult: Option[T]) {
    def `🤷`(elseStatement: => T): T = {
      if (optionalIfResult.isEmpty) {
        elseStatement
      } else {
        optionalIfResult.get
      }
    }
  }

  def `🤔`[T](condition: Boolean)(expr: => T): ResultOfIf[T] = {
    if (condition) {
      new ResultOfIf(Some(expr))
    } else {
      new ResultOfIf(None)
    }
  }

  val ✅ = true
  val ❌ = false

  def `🐻`(condition: => Boolean)(expr: => Unit): Unit = {
    while (condition) {
      expr
    }
  }

  implicit class EmojiInt(value: Int) {
    def `😞`(other: Int): Boolean = {
      if (value < other) {
        ✅
      } else {
        ❌
      }
    }
  }
}

object Main extends App {
  import EmojiLang._
  `😡`(`👎`(❌))

  `😡`(`🤔`(`👎`(✅)) {
    1
  } `🤷` {
    2
  })

  var `🖩` = 0
  `🐻`(`🖩` `😞` 5) {
    `😡`("hi!")
    `🖩` += 1
  }
}
