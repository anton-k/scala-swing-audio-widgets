import scala.swing.audio.ui._
import java.awt.{Color}
import scala.swing.MainFrame

object App {
    def main(args: Array[String]) {
        val ui = new MainFrame {
            val dc = DoubleCheck((0, 0), List(3, 3, 2), Color.BLUE, Color.GREEN,
                List(("pad", List("dream", "whale", "ice")), ("lead", List("night", "organ", "fluty")), ("fx", List("bell", "scratch"))), 
                DoubleCheck.Orient(true, true, false), false)((a, b) => println((a, b)))
            contents = ver(List(
                TextInput(None, Color.RED)(println), 
                FileInput(None, Color.RED)(x => println(x.getName)) , 
                PushButton(Color.BLUE)(print("Hi")),
                dc
            ))
        }
        ui.visible = true
    }
}