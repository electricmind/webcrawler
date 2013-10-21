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

    def generate2tree1(d: Double, n: Int) = (1 until n)
        .map(x => 2 * (x % 2) - 1)
        .map(x => List((x, 1, 1), (x, -1, 1))).flatten
        .map({
            case (x, y, z) => (Vector(1 -> (nextGaussian * d + x), 2 -> (nextGaussian * d + y),
                3 -> (nextGaussian * d + z) /*,
                4 -> (nextGaussian * d + x),
                5 -> (nextGaussian * d + x)*/

            ).normal, x)
        }).foldLeft(TreeApproximator[Int, Int]((Vector[Int](1 -> 1d), 0)))({
            case (tree, (key, value)) =>
                tree + (key, value)
        }).asInstanceOf[TreeApproximatorNode[Int, Int]]

    def top = new MainFrame {
        title = "Convert Celsius / Fahrenheit"
        var n = 50
        var dispersy = 0.5
        var tree = generate2tree1(dispersy, n)

        def regenerate() = {
            tree = generate2tree1(dispersy, n)
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

            case KeyPressed(_, Right, _, _) => {
                dispersy = Math.min(dispersy * 2d, 1000.0)
                regenerate()
            }

            case KeyPressed(_, Up, _, _) => {
                n = Math.min(n * 2, 1000)
                regenerate()
            }
            case KeyPressed(_, Escape, _, _) => {
                regenerate()
            }

            case _: MouseClicked | _: KeyPressed =>
                tree = tree.rectify(n / 4)
                this.repaint()
        }
        val draw = new Panel {
            background = Color.white
            maximize()

            def xp(x: Double) = (x / 4 * size.width + size.width / 2).toInt
            def yp(y: Double) = (y / 4 * size.height + size.height / 2).toInt

            def drawNode(tree: Tree[Int, Int])(implicit g: Graphics2D): (Double, Double, Int) = {
                val v = (tree.average / tree.n).toMap.withDefaultValue(0.0)
                val x = v(1)
                val y = v(2)

                val level = tree match {
                    case node: Node[Int, Int] => {
                        g.setStroke(new BasicStroke(1))
                        g.drawOval(xp(x) - 3, yp(y) - 3, 5, 5)

                        g.setStroke(new BasicStroke());
                        (drawNode(node.child1) match {
                            case (x1, y1, level) => g.setStroke(new BasicStroke(level)); g.drawLine(xp(x), yp(y), xp(x1), yp(y1)); level
                        }) + (
                            drawNode(node.child2) match {
                                case (x2, y2, level) => g.setStroke(new BasicStroke(level)); g.drawLine(xp(x), yp(y), xp(x2), yp(y2)); level
                            })
                    }
                    case leaf: Leaf[Int, Int] =>
                        g.setStroke(new BasicStroke(1))
                        g.drawOval(xp(x) - 5, yp(y) - 5, 11, 11)
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
                g.drawString("""Press:""", 10, 110)
                g.drawString(""" - Space - to rectify""", 10, 140)
                g.drawString(""" - Up - to increase amount of points""", 10, 170)
                g.drawString(""" - Down - to decrease amount of points""", 10, 200)
                g.drawString(""" - Left - to increase dispersy""", 10, 230)
                g.drawString(""" - Right - to decrease dispersy""", 10, 260)
                g.drawString(""" - Escape - to restart""", 10, 290)
                drawNode(tree)(g)
            }
        }
        listenTo(draw.mouse.clicks, draw.keys)
        draw.focusable = true
        contents = draw

    }
}