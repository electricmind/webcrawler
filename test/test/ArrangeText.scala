package test
import ru.wordmetrix.webcrawler.{ Vector, TreeApproximator }
import TreeApproximator._
import java.io._
//TODO: Try to use integers ids instead of words
object ArrangeText extends App {
    type Word = String
    implicit val accuracy: Double = 0.01
    def arrange(tree: Tree[Word, File], path: File): Unit = tree match {
        case node: Node[Word, File] => {
            arrange(node.child1, new File(path, "1"))
            arrange(node.child2, new File(path, "2"))
        }
        case leaf: Leaf[Word, File] => {
            path.mkdirs()
            println("%s -> %s".format(leaf.value, path))
            val fin = new FileInputStream(leaf.value)
            val buf = new Array[Byte](fin.available())
            fin.read(buf)
            val fout = new FileOutputStream(new File(path, leaf.value.getName()))
            fout.write(buf)
            fout.close()
            fin.close()
        }
    }

    override def main(args: Array[String]) {
        val delimiter = """\W+""".r

        val Array(target, files @ _*) = args

        def vectors = files.toIterator.map(x => {
            println(x); ((new Vector(
                io.Source.fromFile(x).getLines().map(delimiter.split).flatten
                    .toList.groupBy(x => x)
                    .map({ case (x, y) => (x, y.length.toDouble) }).toList),
                new File(x))
            )
        })
        val t = System.nanoTime()
        def tree = vectors.foldLeft(TreeApproximator(vectors.next))({
            case (tree, (vector, filename)) => {
                println("%s tree(%s).energy => %4.3f".format((System.nanoTime()-t)/1000000000, tree.n, 0d/*tree.energy2*/))
                tree + (vector, filename)

            }
        })

        def tree_opt = (1 to 10).foldLeft(tree.asInstanceOf[Node[Word, File]])({
            case (tree, n) =>
                println("#%3d = %4.3f".format(n, tree.energy2))
                tree.rectify(tree.n)
        })
        arrange(tree_opt, new File(target))
    }
}
