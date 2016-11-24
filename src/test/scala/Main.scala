import scala.swing.audio.ui._
import java.awt.{Color}
import scala.swing.MainFrame

object App {
    def main(args: Array[String]) {
        val ui = new MainFrame {
            contents = ver(List(TextInput(None, Color.RED)(println), FileInput(None, Color.RED)(x => println(x.getName)) , PushButton(Color.BLUE)(print("Hi"))))
        }
        ui.visible = true
    }
}