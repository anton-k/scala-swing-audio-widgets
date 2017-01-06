import scala.swing._
import scala.swing.event._
import java.awt.{Color,Graphics2D,BasicStroke,Font,KeyboardFocusManager}
import java.awt.geom._
import javax.swing.{SwingUtilities,JFrame}
import javax.swing.{UIManager}
import java.io.File
import scala.util.Either

package scala.swing.audio {
    package object ui {
        private def genBox(orient: Orientation.Value)(items: List[Component])(implicit code: Unit = {}) = new BoxPanel(orient) {
            items.foreach( x => {contents += x; code} )
        }

        def hor(items: List[Component])(implicit code: Unit = {}) = genBox(Orientation.Horizontal)(items)(code)
        def ver(items: List[Component])(implicit code: Unit = {}) = genBox(Orientation.Vertical)(items)(code)
    }
}

package scala.swing.audio.ui {

trait SetWidget[A] {
    def set(value: A, fireCallback: Boolean): Unit    
}


trait GetWidget[A] {
    def get: A
}

trait GetWidgetAt[A,B] {
    def getAt(a: A): B
}

trait SetColor {
    def setColor(color: Color): Unit
}

trait SetText {
    def setText(text: String): Unit
}

trait SetTextList {
    def setTextAt(pos: Int, text: String): Unit
    def textListLength: Int
}

private object Utils {
    def aliasingOn(g: Graphics2D) {
        g.setRenderingHint(java.awt.RenderingHints.KEY_ANTIALIASING, 
            java.awt.RenderingHints.VALUE_ANTIALIAS_ON)
    }

    def getDecimal(x: Double) = x - Math.floor(x)

    def withinBounds(minVal: Float, maxVal: Float)(x: Float) = 
        if (x < minVal) minVal
        else if (x > maxVal) maxVal
        else x

    def withinBoundsInt(minVal: Int, maxVal: Int)(x: Int) = 
        if (x < minVal) minVal
        else if (x > maxVal) maxVal
        else x        

    def mod(a: Int, b: Int) = {
        val res = a % b
        if (res < 0) (res + b)
        else res
    }

    def linearValueWithoutOffset(value: Float, total: Float, offset: Float) = {
        val r = Utils.withinBounds(offset, total - offset)(value)
        (r - offset) / (total - 2 * offset)
    }

    def linearToAbsWithOffset(value: Float, total: Float, offset: Float) =
        value * (total - 2 * offset) + offset


    def distance(a: (Float, Float), b: (Float, Float)) = 
        Math.sqrt( Math.pow(a._1 - b._1, 2) + Math.pow(a._2 - b._2, 2) ).toFloat


    def drawCenteredString(g: Graphics2D, text: String, rect: Rectangle, font: Font = UIManager.getDefaults().getFont("TabbedPane.font")) {
        // Get the FontMetrics
        val metrics = g.getFontMetrics(font)
        // Determine the X coordinate for the text
        val x = rect.x + (rect.width - metrics.stringWidth(text)) / 2
        // Determine the Y coordinate for the text (note we add the ascent, as in java 2d 0 is top of the screen)
        val y = rect.y + ((rect.height - metrics.getHeight()) / 2) + metrics.getAscent();
        // Set the font
        g.setFont(font);
        // Draw the String
        g.drawString(text, x, y);        
    }


    def setTextInArray(maxListLength: Int, textArray: Array[String], pos: Int, text: String) = {
        if (pos < maxListLength) {
            var resTextArray = 
                if (pos >= textArray.length) {
                    textArray ++ Array.fill(pos - textArray.length + 1)("")                
                } else textArray
            resTextArray(pos) = text
            resTextArray
        } else {
            textArray
        }
        
    }

    def setFocus {
        setFocusOnNext
    }

    private def setFocusOnWindow {
        val manager = KeyboardFocusManager.getCurrentKeyboardFocusManager
        manager.getFocusedWindow.requestFocus
    }

    private def setFocusOnNext {
        val manager = KeyboardFocusManager.getCurrentKeyboardFocusManager()
        manager.focusNextComponent
    }

    def choosePlainFile(title: String = ""): Option[File] = {  
        val chooser = new FileChooser(new File("."))
        chooser.title = title
        val result = chooser.showOpenDialog(null)
        if (result == FileChooser.Result.Approve) {
          println("Approve -- " + chooser.selectedFile)
          Some(chooser.selectedFile)
        } else None
      }

}

object Timer {
  def apply(interval: Int, repeats: Boolean = true)(op: => Unit) {
    val timeOut = new javax.swing.AbstractAction() {
      def actionPerformed(e : java.awt.event.ActionEvent) = op
    }
    val t = new javax.swing.Timer(interval, timeOut)
    t.setRepeats(repeats)
    t.start()
  }
}


case class Text(var text: String, var color: Color) extends Component with SetText {      
    preferredSize = new Dimension(5 * text.length + 2 * offset, 50)    
    private val offset = 5

    override def paintComponent(g: Graphics2D) {
        Utils.aliasingOn(g)
        g.setColor(color)
        Utils.drawCenteredString(g, text, new Rectangle(0, 0, size.width, size.height))
    }

    def setText(t: String) {
        text = t
        repaint
    }
}

case class PushButton(var color: Color, var text: Option[String] = None)(onClick: => Unit) extends Component with SetWidget[Unit] with SetColor with SetText {    
    preferredSize = new Dimension(50, 50)

    listenTo(mouse.clicks)
    val clickColor = Color.BLACK
    var currentColor = color

    private def updateColor(col: Color) {
        currentColor = col
        repaint()
    }

    private def blink {
        updateColor(clickColor)
        Timer(140, repeats = false) {
           updateColor(color)
        }
    }

    reactions += {
        case MouseClicked(_, _, _, _, _) =>             
            onClick
            blink            
    }

    private val offset = 5

    override def paintComponent(g: Graphics2D) {
        Utils.aliasingOn(g)
        val d = size
        val x = offset
        val y = offset
        val w = d.width - 2 * offset
        val h = d.height - 2 * offset
        val arc = 2 * offset
        g.setColor(currentColor)
        g.fillRoundRect(x, y, w, h, arc, arc)

        g.setColor(Color.BLACK)            
        text.foreach { label =>
            Utils.drawCenteredString(g, label, new Rectangle(x, y, w, h))
        }
    }

    def set(value: Unit, fireCallback: Boolean = true) {
        if (fireCallback) {
            onClick
        }
        blink
    }


    def get = {}

    def setColor(c: Color) {
        currentColor = c
        repaint
    }

    def setText(t: String) {
        text = Some(t)
        repaint
    }
}

object ToggleButton {
    private def defaultOnClick(b: Boolean) = println(s"ToggleButton: ${b}")    
}

case class ToggleButton(init: Boolean, var color: Color, var text: Option[String] = None)
    (implicit onClick: Boolean => Unit = ToggleButton.defaultOnClick) 
    extends Component with SetWidget[Boolean] with GetWidget[Boolean] with SetColor with SetText {
        
    onClick(init)

    preferredSize = new Dimension(50, 50)

    listenTo(mouse.clicks)    
    var current = init

    reactions += {
        case MouseClicked(_, _, _, _, _) =>             
            current = !current
            onClick(current)            
            repaint
    }

    private val offset = 5

    override def paintComponent(g: Graphics2D) {
        Utils.aliasingOn(g)
        val d = size
        g.setColor(color)
        val x = offset
        val y = offset
        val w = d.width - 2 * offset
        val h = d.height - 2 * offset
        val arc = 2 * offset

        if (current)
            g.fillRoundRect(x, y, w, h, arc, arc)
        else
            g.setStroke(new BasicStroke(3f))
            g.drawRoundRect(x, y, w, h, arc, arc)

        g.setColor(Color.BLACK)            
        text.foreach { label =>
            Utils.drawCenteredString(g, label, new Rectangle(x, y, w, h))
        }            
    }

    def set(value: Boolean, fireCallback: Boolean) {
        current = value
        repaint
        if (fireCallback) {
            onClick(current)
        }
    }


    def get = current

    def setColor(c: Color) {
        color = c
        repaint
    }

    def setText(t: String) {
        text = Some(t)
        repaint
    }
}

case class MultiToggle(init: Set[(Int, Int)], val nx: Int, val ny: Int, var color: Color, textColor: Color = Color.BLACK, var texts: List[String] = List())(onClick: ((Int, Int), Boolean) => Unit) 
    extends Component 
    with SetWidget[((Int, Int), Boolean)] 
    with GetWidgetAt[(Int,Int),Boolean] 
    with SetColor
    with SetTextList {
    var current = init

    for (x <- 0 until nx) {
        for (y <- 0 until ny) {
            val p = (x, y) 
            onClick(p, isPressed(p))
        }
    }

    for (p <- init) {
        onClick(p, true)
    }

    preferredSize = new Dimension(30 * nx, 30 * ny)

    listenTo(mouse.clicks) 

    def getPoint(p: Point) = {
        val x = (nx * p.x / size.width).toInt
        val y = (ny * p.y / size.height).toInt
        (x, y)
    }

    reactions += {
        case MouseClicked(_, p, _, _, _) =>
            val np = getPoint(p)
            if (isPressed(np)) {
                current = current - np
                onClick(np, false)
            } else {
                current = current + np
                onClick(np, true)
            }
            repaint
    }

    private def isPressed(p: (Int, Int)) = current.contains(p)

    private val offset = 5

    private var textArray = texts.toArray

    private def getText(x: Int, y: Int) = {
        val ix = x + nx * y
        if (ix < textArray.length) Some(textArray(ix)) else None
    }

    override def paintComponent(g: Graphics2D) {
        Utils.aliasingOn(g)
        val d = size        
        val x0 = offset
        val y0 = offset
        val arc = 2 * offset

        val dw = (d.width - (1 + nx) * offset) / nx
        val dh = (d.height - (1 + ny) * offset) / ny        

        for (x <- 0 until nx) {
            for (y <- 0 until ny) {
                g.setColor(color)
                val px = x0 + x * offset + x * dw
                val py = y0 + y * offset + y * dh

                if (isPressed((x, y))) {
                    g.fillRoundRect(px, py, dw, dh, arc, arc)                    
                }
                else {
                    g.setStroke(new BasicStroke(2f))
                    g.drawRoundRect(px, py, dw, dh, arc, arc)
                } 

                getText(x, y).foreach { text =>
                    g.setColor(textColor)
                    Utils.drawCenteredString(g, text, new Rectangle(px, py, dw, dh))
                }
            }
        }
    }

    def set(value: ((Int, Int), Boolean), fireCallback: Boolean) {
        val isPressed = value._2
        val cell = value._1

        if (fireCallback) {
            onClick(cell, isPressed)
        }
        
        if (isPressed) {
            current = current + cell
        } else {
            current = current - cell
        }
        repaint        
    }

    def getAt(cell: (Int, Int)): Boolean = isPressed(cell)

    def textListLength = nx * ny

    def setTextAt(pos: Int, text: String) {
        textArray = Utils.setTextInArray(textListLength, textArray, pos, text)
        repaint
    }

    def setColor(c: Color) {
        color = c
        repaint
    }
}

case class HCheck(init: Int, len: Int, var color: Color, texts: List[String] = List(), allowDeselect: Boolean = false)(onSet: Int => Unit) 
    extends Component 
    with SetWidget[Int] 
    with GetWidget[Int] 
    with SetColor 
    with SetTextList {
    var current = init
    onSet(current)
    var textArray = texts.toArray

    preferredSize = new Dimension(40 * len, 30)
    private val bkgColor = Color.GRAY

    listenTo(mouse.clicks)

    private def getValue(p: Point) = 
        (Utils.linearValueWithoutOffset(p.x, size.width, offset) * len).toInt

    reactions += {
        case MouseClicked(_, p, _, _, _) => {
            val userValue = getValue(p)
            current = if (allowDeselect && current == userValue) (-1) else userValue
            onSet(current)
            repaint
        }
    }

    private val offset = 5    

    override def paintComponent(g: Graphics2D) {
        Utils.aliasingOn(g)
        val d = size        
        val x0 = offset
        val y0 = offset
        val arc = 2 * offset

        val dw = (d.width - (1 + len) * offset) / len
        val dh = (d.height - 2 * offset) 

        
        val textListLen = texts.length
        for (x <- 0 until len) {
            g.setColor(color)
            val px = x0 + x * offset + x * dw
            val py = y0

            if (x == current) {
                g.fillRoundRect(px, py, dw, dh, arc, arc)                    
            } else {
                g.setStroke(new BasicStroke(2f))
                g.drawRoundRect(px, py, dw, dh, arc, arc)                
            }

            g.setColor(Color.BLACK)
            if (x < textListLen) {
                val rect = new Rectangle(px, py, dw, dh)
                Utils.drawCenteredString(g, textArray(x), new Rectangle(px, py, dw, dh))                
            }
        }
    }  

    def set(value: Int, fireCallback: Boolean) {
        val boundedValue = Utils.mod(value, len)
        if (fireCallback) {
            onSet(boundedValue)
        }
        current = boundedValue
        repaint
    }


    def get = current

    def setColor(c: Color) {
        color = c
        repaint
    }

    def textListLength = len

    def setTextAt(pos: Int, text: String) {
        textArray = Utils.setTextInArray(len, textArray, pos, text)
        repaint
    }
}


case class VCheck(init: Int, len: Int, var color: Color, texts: List[String] = List(), allowDeselect: Boolean = false)(onSet: Int => Unit) extends Component with SetWidget[Int] with GetWidget[Int] with SetColor with SetTextList {
    var current = init
    onSet(current)
    var textArray = texts.toArray

    preferredSize = new Dimension(50, 30 * len)
    private val bkgColor = Color.GRAY

    listenTo(mouse.clicks)

    private def getValue(p: Point) = 
        (Utils.linearValueWithoutOffset(p.y, size.height, offset) * len).toInt

    reactions += {
        case MouseClicked(_, p, _, _, _) => {
            val userValue = getValue(p)
            current = if (allowDeselect && current == userValue) (-1) else userValue
            onSet(current)
            repaint
        }
    }

    private val offset = 5

    override def paintComponent(g: Graphics2D) {
        Utils.aliasingOn(g)
        val d = size        
        val x0 = offset
        val y0 = offset
        val arc = 2 * offset
        
        val dw = (d.width - 2 * offset) 
        val dh = (d.height - (1 + len) * offset) / len      

        val textListLen = texts.length
        for (y <- 0 until len) {   
            g.setColor(color)         
            val px = x0
            val py = y0 + y * offset + y * dh

            if (y == current) {
                g.fillRoundRect(px, py, dw, dh, arc, arc)                    
            } else {
                g.setStroke(new BasicStroke(2f))
                g.drawRoundRect(px, py, dw, dh, arc, arc)                
            }

            g.setColor(Color.BLACK)
            if (y < textListLen) {
                val rect = new Rectangle(px, py, dw, dh)
                Utils.drawCenteredString(g, textArray(y), new Rectangle(px, py, dw, dh))                
            }
        }
    } 

    def set(value: Int, fireCallback: Boolean) {
        val boundedValue = Utils.mod(value, len)
        if (fireCallback) {
            onSet(boundedValue)
        }
        current = boundedValue
        repaint
    }


    def get = current

    def setColor(c: Color) {
        color = c
        repaint
    }

    def textListLength = len

    def setTextAt(pos: Int, text: String) {
        textArray = Utils.setTextInArray(len, textArray, pos, text)
        repaint
    }
}


case class Dial(init: Float, var color: Color)(implicit onSet: Float => Unit = x => println(s"Dial: ${x}")) extends Component with SetWidget[Float] with GetWidget[Float] with SetColor {
    var current = init
    onSet(current)

    preferredSize = new Dimension(70, 70)
    private val bkgColor = Color.GRAY

    listenTo(mouse.clicks)
    listenTo(mouse.moves) 

    private val eps = 17   

    private def isNearValue(p: Point) = Utils.distance((p.x, p.y), toAbsCoord(current)) < eps  
    
    reactions += {
        case MouseDragged(_, p, _) => if (isNearValue(p)) {            
            val userValue = getCurrentValue((p.x.toFloat, p.y.toFloat))
            if (Math.abs(userValue - current) < 0.1) {               
                onSet(userValue)
                current = userValue
                repaint
            }                
        }        
    }

    private val offset = 12
    private val angleOffset = 0.08f

    private def toDegrees(x: Float) = (360 * x).toInt    

    private def toTau(t: Float) = {
        val r = (1 - 2 * angleOffset) * current
        0.75f - angleOffset - r
    }

    private def getCurrentValue(p: (Float, Float)) = {
        val (cx, cy, _) = getCenterAndRad()
        val r = (Utils.getDecimal(1 - (Math.atan2(-(p._2 - cy), p._1 - cx) / (2 * Math.PI) + 0.25))).toFloat
        (Utils.withinBounds(angleOffset, 1 - angleOffset)(r) - angleOffset) / (1 - 2 * angleOffset)
    }

    override def paintComponent(g: Graphics2D) {
        Utils.aliasingOn(g)
        val d = size
        val x = offset + (if (d.width < d.height)  0 else (0.5f * (d.width - d.height)).toInt)
        val y = offset + (if (d.width >= d.height) 0 else (0.5f * (d.height - d.width)).toInt)
        val diam = Math.min(d.width, d.height) - 2 * offset
       
        val rad = (diam * 0.5f).toInt
        //g.drawLine(x + rad, y, x + rad, y + diam)
        //g.drawLine(x, y + rad, x + diam, y + rad)
        
        g.setStroke(new BasicStroke(16f,              // Line width
                            BasicStroke.CAP_BUTT,    // End-cap style
                            BasicStroke.JOIN_ROUND)); // Vertex join style

        val r = (1 - 2 * angleOffset) * current
        g.setColor(bkgColor)     
        g.drawArc(x, y, diam, diam, 0, 360)   
        g.setColor(color)     
        g.drawArc(x, y, diam, diam, toDegrees(0.75f - angleOffset - r - 0.003f), toDegrees(r + 0.017f))                
    }

    private def getCenterAndRad() = {
        val d = size
        val x = offset + (if (d.width < d.height)  0 else (0.5f * (d.width - d.height)).toInt)
        val y = offset + (if (d.width >= d.height) 0 else (0.5f * (d.height - d.width)).toInt)
        val diam = Math.min(d.width, d.height) - 2 * offset
        val cx = x + diam / 2
        val cy = y + diam / 2
        val rad = diam * 0.5f
        (cx, cy, rad)
    }

    private def toAbsCoord(t: Float): (Float, Float) = {
        val (cx, cy, rad) = getCenterAndRad()
        val tau = toTau(t)
        ((cx + rad * Math.cos(2 * Math.PI * tau)).toFloat, (cy - rad * Math.sin(2 * Math.PI * tau)).toFloat)
    }

    def set(value: Float, fireCallback: Boolean) {
        val boundedValue = Utils.withinBounds(0, 1)(value)
        if (fireCallback) {
            onSet(boundedValue)
        }
        current = boundedValue
        repaint
    }

    def get = current

    def setColor(c: Color) {
        color = c
        repaint
    }
}


case class IntDial(init: Int, range: (Int, Int), var color: Color)(implicit onSet: Int => Unit = x => println(s"IntDial: ${x}")) extends Component with SetWidget[Int] with GetWidget[Int] with SetColor {
    val textColor = Color.BLACK
    var current = (init - range._1).toFloat / (range._2 - range._1)

    private def getIntCurrent = (range._1 + (range._2 - range._1) * current).toInt

    private def cbkCurrent {
        onSet(getIntCurrent)
    }

    cbkCurrent

    preferredSize = new Dimension(70, 70)
    private val bkgColor = Color.GRAY

    listenTo(mouse.clicks)
    listenTo(mouse.moves) 

    private val eps = 17   

    private def isNearValue(p: Point) = Utils.distance((p.x, p.y), toAbsCoord(current)) < eps  
    
    reactions += {
        case MouseDragged(_, p, _) => if (isNearValue(p)) {            
            val userValue = getCurrentValue((p.x.toFloat, p.y.toFloat))
            if (Math.abs(userValue - current) < 0.1) {                               
                current = userValue                
                repaint
            }                
        }        
        case MouseReleased(_,_,_,_,_) => {
            cbkCurrent
        }
    }

    private val offset = 12
    private val angleOffset = 0.08f

    private def toDegrees(x: Float) = (360 * x).toInt    

    private def toTau(t: Float) = {
        val r = (1 - 2 * angleOffset) * current
        0.75f - angleOffset - r
    }

    private def getCurrentValue(p: (Float, Float)) = {
        val (cx, cy, _) = getCenterAndRad()
        val r = (Utils.getDecimal(1 - (Math.atan2(-(p._2 - cy), p._1 - cx) / (2 * Math.PI) + 0.25))).toFloat
        (Utils.withinBounds(angleOffset, 1 - angleOffset)(r) - angleOffset) / (1 - 2 * angleOffset)
    }

    override def paintComponent(g: Graphics2D) {
        Utils.aliasingOn(g)
        val d = size
        val x = offset + (if (d.width < d.height)  0 else (0.5f * (d.width - d.height)).toInt)
        val y = offset + (if (d.width >= d.height) 0 else (0.5f * (d.height - d.width)).toInt)
        val diam = Math.min(d.width, d.height) - 2 * offset
       
        val rad = (diam * 0.5f).toInt
        //g.drawLine(x + rad, y, x + rad, y + diam)
        //g.drawLine(x, y + rad, x + diam, y + rad)
        
        g.setStroke(new BasicStroke(16f,              // Line width
                            BasicStroke.CAP_BUTT,    // End-cap style
                            BasicStroke.JOIN_ROUND)); // Vertex join style

        val r = (1 - 2 * angleOffset) * current
        g.setColor(bkgColor)     
        g.drawArc(x, y, diam, diam, 0, 360)   
        g.setColor(color)     
        g.drawArc(x, y, diam, diam, toDegrees(0.75f - angleOffset - r - 0.003f), toDegrees(r + 0.017f))                
        g.setColor(textColor)
        val msg = getIntCurrent.toString
        Utils.drawCenteredString(g, msg, new Rectangle(0, 0, size.width, size.height))
    }

    private def getCenterAndRad() = {
        val d = size
        val x = offset + (if (d.width < d.height)  0 else (0.5f * (d.width - d.height)).toInt)
        val y = offset + (if (d.width >= d.height) 0 else (0.5f * (d.height - d.width)).toInt)
        val diam = Math.min(d.width, d.height) - 2 * offset
        val cx = x + diam / 2
        val cy = y + diam / 2
        val rad = diam * 0.5f
        (cx, cy, rad)
    }

    private def toAbsCoord(t: Float): (Float, Float) = {
        val (cx, cy, rad) = getCenterAndRad()
        val tau = toTau(t)
        ((cx + rad * Math.cos(2 * Math.PI * tau)).toFloat, (cy - rad * Math.sin(2 * Math.PI * tau)).toFloat)
    }

    def set(value: Int, fireCallback: Boolean) {
        val boundedValue = Utils.withinBoundsInt(range._1, range._2)(value)
        current = (boundedValue - range._1).toFloat / (range._2 - range._1)

        if (fireCallback) {
            cbkCurrent
        }        
        repaint
    }

    def get = getIntCurrent

    def setColor(c: Color) {
        color = c
        repaint
    }
}


case class HFader(init: Float, var color: Color)(implicit onSet: Float => Unit = x => println(s"HFader: ${x}")) extends Component with SetWidget[Float] with GetWidget[Float] with SetColor {
    var current = init
    onSet(current)

    preferredSize = new Dimension(200, 40)
    val bkgColor = Color.GRAY

    listenTo(mouse.clicks)
    listenTo(mouse.moves) 

    private def eps = 0.1

    private def isNearValue(value: Float) = Math.abs(value - current) < eps
    

    reactions += {
        case MouseDragged(_, p, _) => {
            val userValue = getCurrentValue(p)
            if (isNearValue(userValue)) {
                onSet(userValue)
                current = userValue
                repaint
            }
        }            
    }

    private val offset = 5

    def getCurrentValue(p: Point) = 
        Utils.linearValueWithoutOffset(p.x, size.width, offset)    

    override def paintComponent(g: Graphics2D) {
        Utils.aliasingOn(g)
        val d = size;
        val px = offset
        val py = offset
        val w  = d.width - 2 * offset
        val h  = d.height - 2 * offset
        val arc= 2 * offset 
        
        g.setColor(bkgColor)
        g.fillRoundRect(px, py, w, h, arc, arc)

        g.setColor(color)
        g.fillRoundRect(px, py, (w * current).toInt, h, arc, arc)
        g.setStroke(new BasicStroke(2f))
        g.drawRoundRect(px, py, w, h, arc, arc)
    }

    def set(value: Float, fireCallback: Boolean) {
        val boundedValue = Utils.withinBounds(0, 1)(value)
        if (fireCallback) {
            onSet(boundedValue)
        }
        current = boundedValue
        repaint
    }

    def get = current

    def setColor(c: Color) {
        color = c
        repaint
    }
}


case class VFader(init: Float, var color: Color)(implicit onSet: Float => Unit = x => println(s"VFader: ${x}")) extends Component with SetWidget[Float] with GetWidget[Float] with SetColor {
    var current = init
    onSet(current)

    preferredSize = new Dimension(40, 200)
    val bkgColor = Color.GRAY

    listenTo(mouse.clicks)
    listenTo(mouse.moves) 

    private def eps = 0.1

    private def isNearValue(value: Float) = Math.abs(value - current) < eps
    

    reactions += {
        case MouseDragged(_, p, _) => {
            val userValue = getCurrentValue(p)
            if (isNearValue(userValue)) {
                onSet(userValue)
                current = userValue
                repaint
            }
        }            
    }

    private val offset = 5

    def getCurrentValue(p: Point) = 
        1 - Utils.linearValueWithoutOffset(p.y, size.height, offset)    

    override def paintComponent(g: Graphics2D) {
        Utils.aliasingOn(g)
        val d = size;
        val px = offset
        val py = offset
        val w  = d.width - 2 * offset
        val h  = d.height - 2 * offset
        val arc= 2 * offset 
        
        g.setColor(bkgColor)
        g.fillRoundRect(px, py, w, h, arc, arc)

        g.setColor(color)
        g.fillRoundRect(px, py + (h * (1 - current)).toInt, w, (h * current).toInt, arc, arc)
        g.setStroke(new BasicStroke(2f))
        g.drawRoundRect(px, py, w, h, arc, arc)
    }

    def set(value: Float, fireCallback: Boolean) {
        val boundedValue = Utils.withinBounds(0, 1)(value)
        if (fireCallback) {
            onSet(boundedValue)
        }
        current = boundedValue
        repaint
    }

    def get = current

    def setColor(c: Color) {
        color = c
        repaint
    }
}

case class XYPad(initX: Float, initY: Float, var color: Color)(onSet: (Float, Float) => Unit) extends Component with SetWidget[(Float, Float)] with GetWidget[(Float, Float)] with SetColor {
    var current = (initX, initY)
    onSet(current._1, current._2)

    preferredSize = new Dimension(200, 200)
    val bkgColor = Color.GRAY

    listenTo(mouse.clicks)
    listenTo(mouse.moves) 

    private def pointToValue(p: Point) =
        p.y.toFloat / size.height

    private val offset = 5       
    
    private def eps = 0.1

    private def isNearValue(value: (Float, Float)) = Utils.distance(current, value) < eps  

    reactions += {
        case MouseDragged(_, p, _) => {
            val userValue = getCurrentValue(p)
            if (isNearValue(userValue)) {
                onSet(userValue._1, userValue._2)
                current = userValue
                repaint
            }
        }            
    }

    private def getCurrentValue(p: Point) = 
        (Utils.linearValueWithoutOffset(p.x, size.width, offset), 
         1 - Utils.linearValueWithoutOffset(p.y, size.height, offset))   

    override def paintComponent(g: Graphics2D) {
        Utils.aliasingOn(g)  

        val d = size
        val px = offset
        val py = offset
        val w  = d.width - 2 * offset
        val h  = d.height - 2 * offset
        val arc= 2 * offset 

        g.setColor(bkgColor)
        g.fillRoundRect(px, py, w, h, arc, arc)      

        g.setColor(color)
        g.setStroke(new BasicStroke(2f))
        g.drawRoundRect(px, py, w, h, arc, arc)

        val cursor = valueToPoint(current)
        g.drawLine(cursor.x, py, cursor.x, py + h)
        g.drawLine(px, cursor.y, px + w, cursor.y)
        val rad = 8
        g.fillOval(cursor.x - rad, cursor.y - rad, 2 * rad, 2 * rad)        
    }

    private def valueToPoint(v: (Float, Float)) = {
        val x = Utils.linearToAbsWithOffset(v._1, size.width, offset)
        val y = Utils.linearToAbsWithOffset(1 - v._2, size.height, offset)
        new Point(x.toInt, y.toInt)
    }

    def set(value: (Float, Float), fireCallback: Boolean) {
        current = value
        repaint
        if (fireCallback) {
            onSet(value._1, value._2)
        }
        repaint
    }

    def get: (Float, Float) = current

    def setColor(c: Color) {
        color = c
        repaint
    }
}


case class HFaderRange(init: (Float, Float), var color: Color)(onSet: (Float, Float) => Unit) extends Component with SetWidget[(Float, Float)] with GetWidget[(Float, Float)] with SetColor {
    var current = init    

    def cbkCurrentValue {
        onSet(current._1, current._2)
    }    

    cbkCurrentValue

    preferredSize = new Dimension(200, 40)
    val bkgColor = Color.GRAY

    listenTo(mouse.clicks)
    listenTo(mouse.moves) 

    private def eps = 0.1
    private var isDrag = false
    private var initRad = getRad

    private def getCenter = 0.5f * (current._1 + current._2)
    private def getRad = current._2 - getCenter
    private def isNearValue(value: (Float, Float)) = Math.abs(value._1) < eps

    reactions += {
        case MouseClicked(_, p, _, _, _) => {
            val userValue = getCurrentValue(p)
            if (isNearValue(userValue)) {                
                initRad = getRad
            } 
        }

        case MouseDragged(_, p, _) => {
            val userValue = getCurrentValue(p)
            if (isNearValue(userValue)) {                  
                shiftCurrentValue(userValue)
                cbkCurrentValue                
                repaint
            }
        }

        case MouseReleased(_, _, _, _, _) => {            
            initRad = getRad
        }
    }

    def shiftCurrentValue(p: (Float, Float)) {
        val cx = getCenter + p._1
        val rad = Utils.withinBounds(0f, 1f)(initRad - p._2)
        current = (Utils.withinBounds(0, 1)(cx - rad), Utils.withinBounds(0, 1)(cx + rad))
    }

    private val offset = 5
    private val speedY = 0.2f

    def getCurrentValue(p: Point) = {
        val x = Utils.linearValueWithoutOffset(p.x, size.width, offset)            
        val cx = x - getCenter
        val cy = (p.y - size.height * 0.5f) / (3f * size.height)
        (cx, cy)
    }

    override def paintComponent(g: Graphics2D) {
        Utils.aliasingOn(g)
        val d = size;
        val px = offset
        val py = offset
        val w  = d.width - 2 * offset
        val h  = d.height - 2 * offset
        val arc= 2 * offset 
        val cx = getCenter
        
        val centerRectX1 = px + (w * cx - offset * 0.5f).toInt
        val centerRectWidth = offset


        g.setColor(bkgColor)
        g.fillRoundRect(px, py, w, h, arc, arc)

        g.setColor(color)
        g.fillRoundRect((px + (w * current._1)).toInt.min(centerRectX1 - (offset *0.5f).toInt), py, (w * (current._2 - current._1)).toInt.max(centerRectWidth + (offset).toInt), h, arc, arc)
        g.setStroke(new BasicStroke(2f))
        g.drawRoundRect(px, py, w, h, arc, arc)

        g.setColor(bkgColor)
        g.fillRoundRect(px + (w * cx - offset * 0.5f).toInt, py, offset, h, arc, arc)
    }

    def set(value: (Float, Float), fireCallback: Boolean) {
        current = value
        repaint
        if (fireCallback) {
            onSet(value._1, value._2)
        }
        repaint
    } 

    def get: (Float, Float) = current

    def setColor(c: Color) {
        color = c
        repaint
    }   
}


case class VFaderRange(init: (Float, Float), var color: Color)(onSet: (Float, Float) => Unit) extends Component with SetWidget[(Float,Float)] with GetWidget[(Float,Float)] with SetColor {
    var current = init    

    def cbkCurrentValue {
        onSet(current._1, current._2)
    }    

    cbkCurrentValue

    preferredSize = new Dimension(40, 200)
    val bkgColor = Color.GRAY

    listenTo(mouse.clicks)
    listenTo(mouse.moves) 

    private def eps = 0.1
    private var isDrag = false
    private var initRad = getRad

    private def getCenter = 0.5f * (current._1 + current._2)
    private def getRad = current._2 - getCenter
    private def isNearValue(value: (Float, Float)) = Math.abs(value._2) < eps

    reactions += {
        case MouseClicked(_, p, _, _, _) => {
            val userValue = getCurrentValue(p)
            if (isNearValue(userValue)) {                
                initRad = getRad
            } 
        }

        case MouseDragged(_, p, _) => {
            val userValue = getCurrentValue(p)
            if (isNearValue(userValue)) {                  
                shiftCurrentValue(userValue)
                cbkCurrentValue                
                repaint
            }
        }

        case MouseReleased(_, _, _, _, _) => {            
            initRad = getRad
        }
    }

    def shiftCurrentValue(p: (Float, Float)) {
        val cy = getCenter + p._2
        val rad = Utils.withinBounds(0f, 1f)(initRad + p._1)
        current = (Utils.withinBounds(0, 1)(cy - rad), Utils.withinBounds(0, 1)(cy + rad))
    }

    private val offset = 5
    private val speedY = 0.2f

    def getCurrentValue(p: Point) = {
        val y = Utils.linearValueWithoutOffset(p.y, size.height, offset)            
        val cy = y - getCenter
        val cx = (p.x - size.width * 0.5f) / (3f * size.width)
        (cx, cy)
    }

    override def paintComponent(g: Graphics2D) {
        Utils.aliasingOn(g)
        val d = size;
        val px = offset
        val py = offset
        val w  = d.width - 2 * offset
        val h  = d.height - 2 * offset
        val arc= 2 * offset 
        val cy = getCenter
        
        val centerRectY1 = py + (h * cy - offset * 0.5f).toInt
        val centerRectHeight = offset


        g.setColor(bkgColor)
        g.fillRoundRect(px, py, w, h, arc, arc)

        g.setColor(color)
        g.fillRoundRect(px, (py + (h * current._1)).toInt.min(centerRectY1 - (offset *0.5f).toInt), w, (h * (current._2 - current._1)).toInt.max(centerRectHeight + (offset).toInt), arc, arc)
        g.setStroke(new BasicStroke(2f))
        g.drawRoundRect(px, py, w, h, arc, arc)

        g.setColor(bkgColor)
        g.fillRoundRect(px, py + (h * cy - offset * 0.5f).toInt, w, offset, arc, arc)
    }

    def set(value: (Float, Float), fireCallback: Boolean) {
        current = value
        repaint
        if (fireCallback) {
            onSet(value._1, value._2)
        }
        repaint
    }

    def get: (Float, Float) = current

    def setColor(c: Color) {
        color = c
        repaint
    }
}

case class XYPadRange(initX: (Float, Float), initY: (Float, Float), var color: Color)(onSet: ((Float, Float), (Float, Float)) => Unit) extends Component with SetWidget[((Float, Float), (Float, Float))] with GetWidget[((Float, Float), (Float, Float))] with SetColor {
    private case class PointRange(x: Float, y: Float, isMinX: Boolean, isMinY: Boolean) {
        def point = (x, y)
    }

    private var current = (initX, initY)
    private var editPoint: Option[PointRange] = None
    private val bkColor = Color.GRAY

    def cbkCurrent {
        onSet(fixBounds(current._1), fixBounds(current._2))
    }
    
    preferredSize = new Dimension(200, 200)
    val bkgColor = Color.GRAY

    listenTo(mouse.clicks)
    listenTo(mouse.moves) 

    private val offset = 5       
    
    private def eps = 0.1

    private def getNearPoint(value: (Float, Float)) = {
        println(points)
        points.find(x => {val d = Utils.distance(x.point, value); println(d); d < eps})
    }

    reactions += {
        case MousePressed(_, p, _, _, _) => {
            val userEditPoint = getNearPoint(getCurrentValue(p))
            editPoint = userEditPoint
        }

        case MouseDragged(_, p, _) => {            
            editPoint.foreach{ edit =>
                val userValue = getCurrentValue(p)
                updateCurrentValue(edit, userValue)
                cbkCurrent
                repaint
            }
        } 

        case MouseReleased(_,_,_,_,_) => {
            editPoint = None
        }
    }

    private def fixBounds(p: (Float, Float)) =
        if (p._2 < p._1) (p._2, p._1) else p

    private def updateCurrentValue(editPoint: PointRange, value: (Float, Float)) = {
        val xRange = if (editPoint.isMinX) (value._1, current._1._2) else (current._1._1, value._1)
        val yRange = if (editPoint.isMinY) (value._2, current._2._2) else (current._2._1, value._2)        
        current = (xRange, yRange)
    }

    private def getCurrentValue(p: Point) = 
        (Utils.linearValueWithoutOffset(p.x, size.width, offset), 
         1 - Utils.linearValueWithoutOffset(p.y, size.height, offset))   

    override def paintComponent(g: Graphics2D) {
        Utils.aliasingOn(g)  

        val d = size
        val px = offset
        val py = offset
        val w  = d.width - 2 * offset
        val h  = d.height - 2 * offset
        val arc= 2 * offset 

        g.setColor(bkgColor)
        g.fillRoundRect(px, py, w, h, arc, arc)      

        g.setColor(color)
        g.setStroke(new BasicStroke(2f))
        g.drawRoundRect(px, py, w, h, arc, arc)

        val cursors = points.map(p => valueToPoint(p.point))
        def drawLine(n: Int, m: Int) {
            val p1 = cursors(n)
            val p2 = cursors(m)
            g.drawLine(p1.x, p1.y, p2.x, p2.y)
        }
        drawLine(0, 1)
        drawLine(1, 2)
        drawLine(2, 3)
        drawLine(3, 0)
               
        val rad = 6
        cursors.foreach { cursor =>
            g.fillOval(cursor.x - rad, cursor.y - rad, 2 * rad, 2 * rad)
        }        
    }

    private def valueToPoint(v: (Float, Float)) = {
        val x = Utils.linearToAbsWithOffset(v._1, size.width, offset)
        val y = Utils.linearToAbsWithOffset(1 - v._2, size.height, offset)
        new Point(x.toInt, y.toInt)        
    } 

    private def points = List(
        PointRange(current._1._1, current._2._1, true, true),         
        PointRange(current._1._1, current._2._2, true, false),
        PointRange(current._1._2, current._2._2, false, false), 
        PointRange(current._1._2, current._2._1, false, true)) 

    def set(value: ((Float, Float), (Float, Float)), fireCallback: Boolean) {
        current = value
        repaint
        if (fireCallback) {
            onSet(value._1, value._2)
        }
        repaint
    }

    def get = current

    def setColor(c: Color) {
        color = c
        repaint
    }
}


case class DropDownList(init: Int, items: List[String])(onSet: Int => Unit) extends FlowPanel with SetWidget[Int] with GetWidget[Int] with SetColor {
    val widget = new ComboBox(items)
    
    contents += widget
    listenTo(widget.selection)

    widget.selection.index = init
    onSet(init)        

    reactions += {
        case SelectionChanged(`widget`) => { onSet(widget.selection.index); Utils.setFocus }
    } 

    def set(value: Int, fireCallback: Boolean) {
        widget.selection.index = value
        widget.repaint
        if (fireCallback) {
            onSet(value)
        }
        repaint
    } 

    def get: Int = widget.selection.index

    def setColor(color: Color) {}
}

case class TextInput(init: Option[String], color: Color, textLength: Int = 7)(onSet: String => Unit) extends FlowPanel with SetWidget[String] with GetWidget[String] with SetColor {
    var clickColor = color
    val textColor  = Color.BLACK

    private def updateColor(color: Color) {
        textField.foreground = color
        textField.repaint            
    }

    private def setFont(fontType: Int) {
        val oldFont = textField.font
        val newFont = new Font(oldFont.getName(), fontType, oldFont.getSize())             
        textField.font = newFont
    }

    private def blink {
        setFont(Font.BOLD)
        updateColor(clickColor)            
        Timer(200, repeats = false) {
            setFont(Font.PLAIN)
            updateColor(textColor)               
        }
    }

    preferredSize = new Dimension(100, 20)
    init.foreach(onSet)

    val textField:TextField = new TextField(init.getOrElse(""))
    textField.columns = textLength

    contents += textField

    listenTo(textField.keys)

    reactions += {
        case KeyPressed(_, Key.Enter, _, _) => {
            onSet(textField.text)
            blink
            Utils.setFocus
        }
    }

    def set(value: String, fireCallback: Boolean) {
        textField.text = value
        textField.repaint
        if (fireCallback) {
            onSet(value)
        }
        repaint
    } 

    def get: String = textField.text

    def setColor(color: Color) {
        clickColor = color
        repaint
    }
}

case class FileInput(var file: Option[File], var color: Color, var defaultTitle: String = "Get File")(onSet: File => Unit) 
    extends Component 
    with SetWidget[File] 
    with GetWidget[Option[File]]
    with SetColor {    
    preferredSize = new Dimension(150, 50)

    var title = file.map(_.getName).getOrElse(defaultTitle)
    var current = file  

    def setTitle {
        title = current.map(_.getName).getOrElse(defaultTitle)
    }

    def getFile {
        Utils.choosePlainFile("").foreach { res =>
            current = Some(res)
            onSet(res)
            setTitle
            repaint
        }
    }

    listenTo(mouse.clicks)        

    reactions += {
        case MouseClicked(_, _, _, _, _) =>             
            getFile            
    }

    private val offset = 5

    override def paintComponent(g: Graphics2D) {
        Utils.aliasingOn(g)
        val d = size
        val x = offset
        val y = offset
        val w = d.width - 2 * offset
        val h = d.height - 2 * offset
        val arc = 2 * offset
        g.setColor(color)
        g.setStroke(new BasicStroke(3f))
        g.drawRoundRect(x, y, w, h, arc, arc)
        g.drawRoundRect(x + offset, y + offset, w - 2 * offset, h - 2 * offset, arc, arc)

        g.setColor(Color.BLACK)            
        Utils.drawCenteredString(g, title, new Rectangle(x, y, w, h))        
    }

    def set(value: File, fireCallback: Boolean = true) {
        if (fireCallback) {          
            onSet(value)
        }      
        current = Some(value)
        setTitle
        repaint
    }


    def get: Option[File] = current

    def setColor(c: Color) {
        color = c
        repaint
    }
}

object DoubleCheck {
    case class Orient(isFirst: Boolean, isFirstHor: Boolean, isSecondHor: Boolean) {
        val isFirstVer = !isFirstHor
        val isSecondVer = !isSecondHor
    }

}

case class DoubleCheck(init: (Int, Int), sizes: List[Int], var color1: Color, var color2: Color, texts: List[(String, List[String])], orient: DoubleCheck.Orient, allowDeselect: Boolean)(onSet: (Int, Int) => Unit) 
    extends Component 
    with SetWidget[(Int,Int)]
    with GetWidget[(Int,Int)] {

    val textColor = Color.BLACK

    val sizesArr = sizes.toArray
    var current = init
    val maxSize1 = sizes.length
    val maxSize2 = sizes.max
    var currentFirst = init._1

    val textHeight = 30
    val textWidth = 90

    preferredSize = (
        if (orient.isFirstHor && orient.isSecondHor) {
            (maxSize1.max(maxSize2), 2)
        } else if (orient.isFirstVer && orient.isSecondVer) {
            (2, maxSize1.max(maxSize2))
        } else if (orient.isFirstHor && orient.isSecondVer) {
            (maxSize1, (maxSize2 + 1))
        } else if (orient.isFirstVer && orient.isSecondHor) {
            (1 + maxSize2, maxSize1)
        } else (maxSize1, maxSize2) ) match {
            case (x, y) => new Dimension(x * textWidth, y * textHeight)
        }       

    def getBoundingRects = 
        ((if (orient.isFirstHor && orient.isSecondHor) {
            val mid = (size.height * 0.5f).toInt
            ( (0, 0, size.width, mid)
            , (0, mid, size.width, mid))
        } else if (orient.isFirstVer && orient.isSecondVer) {
            val mid = (size.width * 0.5f).toInt 
            ( (0, 0, mid, size.height)
            , (mid, 0, mid, size.height) )
        } else if (orient.isFirst && orient.isFirstHor && orient.isSecondVer) {
            val mid = (size.height * (1.0f / (maxSize2 + 1))).toInt
            ( (0, 0, size.width, mid)
            , (0, mid, size.width, size.height - mid) )
        } else if (!orient.isFirst && orient.isFirstHor && orient.isSecondVer) {
            val mid = (size.height * (maxSize2.toFloat / (maxSize2 + 1))).toInt
            ( (0, 0, size.width, mid)
            , (0, mid, size.width, size.height - mid) )            
        } else if (orient.isFirst && orient.isFirstVer && orient.isSecondHor) {
            val mid = (size.width * (1.0f / (maxSize2 + 1))).toInt
            ( (0, 0, mid, size.height)
            , (mid, 0, size.width - mid, size.height) )
        } else if (!orient.isFirst && orient.isFirstVer && orient.isSecondHor) {
            val mid = (size.width * (maxSize2.toFloat / (maxSize2 + 1))).toInt
            ( (0, 0, mid, size.height)
            , (mid, 0, size.width - mid, size.height) )       
        } else { 
            val mid = (size.height * 0.5f).toInt
            ( (0, 0, size.width, mid)
            , (0, mid, size.width, mid))
        }) match {
            case (r1, r2) => if (!orient.isFirst && (orient.isFirstHor != orient.isSecondHor)) (r2, r1) else (r1, r2)
        }) match {
            case ((x1, y1, w1, h1), (x2, y2, w2, h2)) => (new Rectangle(x1, y1, w1, h1), new Rectangle(x2 + offset, y2 + offset, w2 - 2 * offset, h2 - 2 * offset))
        }

    def getCurrentRects = {
        val (rects1, rects2) = getAllRects
        (rects1, rects2.take(sizesArr(currentFirst)))
    }

    def getAllRects = {
        val (boundRect1, boundRect2) = getBoundingRects 

        val rects1 = if (orient.isFirstHor)  getHorRects(boundRect1, maxSize1) else getVerRects(boundRect1, maxSize1)
        val rects2 = if (orient.isSecondHor) getHorRects(boundRect2, maxSize2) else getVerRects(boundRect2, maxSize2)

        (rects1, rects2)
    }
    

    def getHorRects(boundRect: Rectangle, maxSize: Int): List[Rectangle] = {
        val dw = boundRect.width.toFloat / maxSize
        def rect(ix: Int) = new Rectangle(
            (boundRect.x + offset * 0.5f + dw * ix).toInt, (boundRect.y + offset * 0.5f).toInt, 
            (dw - offset).toInt, (boundRect.height - offset).toInt)

        (0 until maxSize).map(rect).toList
    }

    def getVerRects(boundRect: Rectangle, maxSize: Int): List[Rectangle] = {
        val dh = boundRect.height.toFloat / maxSize

        def rect(ix: Int) = new Rectangle(
            (boundRect.x + 0.5f * offset).toInt, (boundRect.y +  offset * 0.5f + dh * ix).toInt, 
            (boundRect.width - offset).toInt, (dh - offset).toInt)

        (0 until maxSize).map(rect).toList
    }


    var names1 = texts.map(_._1).toArray
    var names2 = texts.map(_._2.toArray).toArray

    listenTo(mouse.clicks)        

    reactions += {
        case MouseClicked(_, p, _, _, _) => onClick(p)
    }

    def onClick(p: Point) { 
        getCell(p).foreach { 
            case Left(ix)  => onFirstSelect(ix)
            case Right(ix) => onSecondSelect(ix)            
        }
    } 

    def onFirstSelect(ix: Int) {  
        if (currentFirst != ix) {
            currentFirst = ix        
            repaint
        }
    }

    def onSecondSelect(ix: Int, fireCallback: Boolean = true) {
        current = (currentFirst, ix)
        if (fireCallback) {
            onSet(current._1, current._2)
        }        
        repaint  
    }

    def findCellInRect(r: List[Rectangle], p: Point) = 
        r.zipWithIndex.find({case (r, ix) => r.contains(p)}) match {
            case Some((r, ix)) => Some(ix)
            case None => None
        }

    def getCell(p: Point): Option[Either[Int,Int]] = {
        val (rects1, rects2) = getCurrentRects
        findCellInRect(rects1, p) match {
            case Some(ix) => Some(Left(ix))
            case None     => findCellInRect(rects2, p) match {
                    case Some(ix) => Some(Right(ix))
                    case None     => None
                }
        }
    }
        


    def setColor1(c: Color) {
        color1 = c
        repaint
    }

    def setColor2(c: Color) {
        color2 = c
        repaint
    }    

    def set(value: (Int, Int), fireCallback: Boolean) {
        current = within(value)
        if (fireCallback) {
            onSet(current._1, current._2)
        }        
        repaint
    }

    def get = current

    def within(a: (Int, Int)) = {
        val n1 = a._1
        val n2 = a._2

        val res1 = Utils.mod(n1, maxSize1)
        val res2 = Utils.mod(n2, sizesArr(res1))
        (res1, res2)
    }

    val offset = 5 
    val arc = 2 * offset   

    override def paintComponent(g: Graphics2D) {
        val (text1, text2) = getCurrentTexts
        val (rects1, rects2) = getCurrentRects
        Utils.aliasingOn(g)
        drawBorderRect(g)
        drawRects(g, color1, rects1, currentFirst)
        drawRects(g, color2, rects2, if (currentFirst == current._1) current._2 else -1)
        drawNames(g, text1, rects1)
        drawNames(g, text2, rects2)
    }

    def drawBorderRect(g: Graphics2D) {        
        val d = size        
        val x = (offset * 0.5f).toInt
        val y = (offset * 0.5f).toInt
        val w = d.width - offset
        val h = d.height - offset        
        g.setColor(color1)
        g.setStroke(new BasicStroke(2f))
        g.drawRoundRect(x, y, w, h, arc, arc)        
    }

    def drawNames(g: Graphics2D, texts: Seq[String], rects: Seq[Rectangle]) {
        g.setColor(textColor)
        rects.zip(texts).map({ case (rect, text) => Utils.drawCenteredString(g, text, rect) })
    }

    def drawRects(g: Graphics2D, color: Color, rects: Seq[Rectangle], choice: Int) {
        g.setColor(color)
        rects.zipWithIndex.map( { case (rect, ix) => 
            if (ix == choice) {
                g.fillRoundRect(rect.x, rect.y, rect.width, rect.height, arc, arc)        
            } else {
                g.setStroke(new BasicStroke(2f))
                g.drawRoundRect(rect.x, rect.y, rect.width, rect.height, arc, arc)        
            }

        } )
    }

    def getCurrentTexts: (Array[String], Array[String]) = (names1, names2(currentFirst))

    def setTextAt1(x: Int, name: String) {
        if (x < maxSize1) {
            names1(x) = name
        }
    }

    def setTextAt2(x: Int, y: Int, name: String) {
        if (x < maxSize1) {
            val size2 = sizesArr(x)
            if (y < size2) {
                names2(x)(y) = name

            }
        }
    }

    def textListLength1 = maxSize1
    def textListLength2(x: Int) = sizesArr(Utils.mod(x, maxSize1))
}


}

