package test
import ru.wordmetrix.webcrawler._
import ru.wordmetrix.webcrawler.TreeApproximator._
import scala.swing.Panel
import java.awt.Color
import java.awt.Graphics2D
import swing._
import event._
import scala.util.Random.nextGaussian
import java.awt.Paint
import java.awt.BasicStroke
import scala.swing.event.Key._

object Draw2DMap extends SimpleSwingApplication {

    def generate2tree1(d: Double, n: Int, dimension: Int) = (1 until n)
        .map(x => 2 * (x % 2) - 1)
        .map(x => List((x, 1, 1), (x, -1, 1))).flatten
        .map({
            case (x, y, z) => (
                (3 to dimension).map(
                    dim => Vector(dim -> (nextGaussian * d + x))
                ).fold(Vector(
                        1 -> (nextGaussian * d + x),
                        2 -> (nextGaussian * d + y)
                    ))(_ + _)
                .normal, x)
        }).foldLeft(TreeApproximator[Int, Int]((Vector[Int](1 -> 1d), 0)))({
            case (tree, (key, value)) =>
                tree + (key, value)
        }).asInstanceOf[TreeApproximatorNode[Int, Int]]

    def top = new MainFrame {
        title = "Convert Celsius / Fahrenheit"
        var n = 50
        var dispersy = 0.5
        var dimension = 3
        var tree = generate2tree1(dispersy, n, dimension)
        var showalign = false

        def regenerate() = {
            println(dimension)
            tree = generate2tree1(dispersy, n, dimension)
            this.repaint()
        }

        reactions += {
            case KeyPressed(_, Down, _, _) => {
                n = Math.max(n / 2, 4)
                regenerate()
            }

            case KeyPressed(_, Left, _, _) => {
                dispersy = Math.max(dispersy / 2d, 0.01)
                regenerate()
            }
            case KeyPressed(_, PageUp, _, _) => {
                dimension = Math.min(dimension + 1, 5)
                regenerate()
            }
            case KeyPressed(_, PageDown, _, _) => {
                dimension = Math.max(dimension - 1, 2)
                regenerate()
            }

            case KeyPressed(_, Right, _, _) => {
                dispersy = Math.min(dispersy * 2d, 1000.0)
                regenerate()
            }

            case KeyPressed(_, Enter, _, _) => {
                showalign = !showalign
                println(showalign)
                repaint()
            }

            case KeyReleased(_, Space, 128, _) => {
                tree = tree.align(Vector(1 -> 1.0))._1.asInstanceOf[Node[Int, Int]]
                println("align")
                repaint()
            }

            case KeyPressed(_, Up, _, _) => {
                n = Math.min(n * 2, 1000)
                regenerate()
            }

            case KeyPressed(_, Escape, _, _) => {
                regenerate()
            }
            case _: MouseClicked | KeyReleased(_, Space, _, _) =>
                tree = tree.rectify(n / 4)
                println("rectify")
                this.repaint()
            case k: KeyPressed  => println(k)
            case k: KeyTyped    => println(k)
            case k: KeyReleased => println(k)

        }
        val draw = new Panel {
            background = Color.white
            maximize()

            def xp(x: Double) = (x / 4 * size.width + size.width / 2).toInt

            def yp(y: Double) = (y / 4 * size.height + size.height / 2).toInt

            def point(tree: Tree[Int, Int]) = {
                (tree.average / tree.n).toMap.withDefaultValue(0.0) match {
                    case v => (xp(v(1)), yp(v(2)))
                }
            }

            def drawNode(tree: Tree[Int, Int])(implicit g: Graphics2D): (Int, Int, Int) = {
                val (x, y) = point(tree)

                val level = tree match {
                    case node: Node[Int, Int] => {
                        g.setStroke(new BasicStroke(1))
                        g.drawOval(x - 3, y - 3, 5, 5)

                        g.setStroke(new BasicStroke());
                        (drawNode(node.child1) match {
                            case (x1, y1, level) => g.setColor(Color.red); g.setStroke(new BasicStroke(level)); g.drawLine(x, y, x1, y1); level
                        }) + (
                            drawNode(node.child2) match {
                                case (x2, y2, level) => g.setColor(Color.blue); g.setStroke(new BasicStroke(level)); g.drawLine(x, y, x2, y2); level
                            })
                    }
                    case leaf: Leaf[Int, Int] =>
                        g.setStroke(new BasicStroke(1))
                        g.drawOval(x - 5, y - 5, 11, 11)
                        //                        g.drawString(leaf.value.toString, xp(x * 1.2), yp(y * 1.2))
                        1
                }

                (x, y, 1)
            }

            override protected def paintComponent(g: Graphics2D) {
                super.paintComponent(g)

                g.setColor(Color.black)
                g.drawString("Energy:   %4.3f".format(tree.energy2), 10, 10)
                g.drawString("Dispersy: %4.3f".format(dispersy), 10, 40)
                g.drawString("Amount:   %4d".format(n), 10, 70)
                g.drawString("Dimension:   %1d".format(dimension), 10, 100)
                g.drawString("""Press:""", 10, 140)
                g.drawString(""" - Space - to rectify""", 10, 170)
                g.drawString(""" - Control+Space - to align""", 10, 200)
                g.drawString(""" - Enter - to toggle aligned view""", 10, 230)
                g.drawString(""" - Up - to increase amount of points""", 10, 260)
                g.drawString(""" - Down - to decrease amount of points""", 10, 290)
                g.drawString(""" - Left - to increase dispersy""", 10, 310)
                g.drawString(""" - Right - to decrease dispersy""", 10, 340)
                g.drawString(""" - Escape - to restart""", 10, 370)
                showalign match {
                    case true => for (List((x1, y1), (x2, y2)) <- tree.map(point).sliding(2)) {
                        g.setStroke(new BasicStroke(1))
                        g.drawLine(x1, y1, x2, y2)
                        g.drawOval(x1 - 5, y1 - 5, 11, 11)
                        g.drawOval(x2 - 5, y2 - 5, 11, 11)
                    }
                    case false => drawNode(tree)(g)
                }
            }
        }
        listenTo(draw.mouse.clicks, draw.keys)
        draw.focusable = true
        contents = draw

    }
}