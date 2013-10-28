package test
import ru.wordmetrix.webcrawler.{ Vector, TreeApproximator, debug, CFG }
import TreeApproximator._
import java.io._
import ru.wordmetrix.webcrawler.Clusters
object ArrangeText extends App {
    implicit lazy val cfg = CFG(List("-d"))
    type Word = Int
    implicit lazy val accuracy: Double = 0.01

    object string2word {
        var map = Map[String, Word]()
        val words = Iterator.from(10)
        def apply(s: String) = {
            val word = map.getOrElse(s, words.next)
            map = map + (s -> word)
            word
        }
    }

    def arrange_tree(tree: Tree[Word, File], path: File): Unit = tree match {
        case node: Node[Word, File] => {
            arrange_tree(node.child1, new File(path, "1"))
            arrange_tree(node.child2, new File(path, "2"))
        }
        case leaf: Leaf[Word, File] => copy(leaf.value, new File(path, leaf.value.toString))
    }

    def copy(finname: File, foutname: File) = {

        println("%s -> %s".format(finname, foutname))
        val fin = new FileInputStream(finname)
        val buf = new Array[Byte](fin.available())
        fin.read(buf)
        foutname.getParentFile().mkdirs() //    path.mkdirs()
        val fout = new FileOutputStream(foutname)
        fout.write(buf)
        fout.close()
        fin.close()
    }

    class Use[A](a: A) {
        def use [B](f: A => B) = f(a)
    }
    implicit def aToUse[A](a: A) = new Use(a)

    // 1 use (x => x + 1)

    def arrange_cluster(map: Iterable[Iterable[Vector[Word]]], tree: Tree[Word, File], path : File) = {
        val v2f = tree.toMap
        map.zipWithIndex foreach {
            case (vs, i) => vs foreach {
                case (v) => v2f(v) use {
                    x => copy(x, new File(new File(path, i.toString), x.getName))
                }
            }
        }
    }

    override def main(args: Array[String]) {
        val delimiter = """\W+""".r

        val Array(target, files @ _*) = args

        def vectors = files.toIterator.map(x => {
            ((Vector(
                io.Source.fromFile(x).getLines().map(delimiter.split).flatten
                    .toList.groupBy(x => x.toLowerCase())
                    .map({ case (x, y) => (x, y.length.toDouble) })
                    .filter(_._2 > 5)
                    .map({ case (x, y) => string2word(x) -> y })
                    .toList),
                new File(x))
            )
        })

        val t = System.currentTimeMillis()
        def tree = vectors.foldLeft(TreeApproximator(vectors.next))({
            case (tree, (vector, filename)) => {
                debug.time("%s %d %s tree(%s).energy => %4.3f, length = %d / %d".format(
                    (System.currentTimeMillis() - t) / 10,
                    tree.n,
                    string2word.map.size,
                    filename,
                    tree.energy2,
                    vector.size,
                    tree.average.size)
                ) {
                    (tree + (vector, filename)).asInstanceOf[TreeApproximator.Node[Word, File]].rectify(2)
                }

            }
        })

        def tree_opt = (1 to 5).foldLeft(tree.asInstanceOf[Node[Word, File]])({
            case (tree, n) =>
                debug.time("Rectifying #%3d = %4.3f %d".format(
                    n, tree.energy2, tree.average.size)
                ) {
                    tree.rectify(tree.n)
                }
        })
        
        def tree_aligned = tree_opt.align()._1

        
       // arrange_tree(tree_opt.aligned, new File(target))
        tree_aligned use {
            tree => arrange_cluster(debug.time("clustering") { Clusters(tree) }, tree, new File(target))
        } 
        
    }
}
