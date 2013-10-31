package test
import ru.wordmetrix.webcrawler.{ Vector, TreeApproximator, debug, CFG, Use, SmartFile }
import Use._

import TreeApproximator._
import java.io._
import ru.wordmetrix.webcrawler.Clusters
import SmartFile._

object ArrangeText extends App {
    implicit lazy val cfg = CFG(List("-d"))
    type Word = Int
    implicit lazy val accuracy: Double = 0.01

    object string2word {
        var map = Map[String, Word]()
        val words = Iterator.from(10)
        var inverted = Map[Word, String]()
        def apply(s: String) = {
            val word = map.getOrElse(s, words.next)
            map += (s -> word)
            inverted += (word -> s)
            word
        }
    }
    implicit def string2File(s: String) = new File(s)
    implicit def vectors2Vectors(v: Vector[Word]): Vector[String] = Vector(v.map {
        case (x, y) => (string2word.inverted.getOrElse(x,"UNKNOWN") -> y)
    } toList)

    def arrange_tree(tree: Tree[Word, File], path: File): Unit = tree match {
        case node: Node[Word, File] => {
            path / "1" / "vocabulary.txt" write (node.child1.average.normal - node.child2.average.normal)
            arrange_tree(node.child1, new File(path, "1"))

            path / "2" / "vocabulary.txt" write (node.child2.average.normal - node.child1.average.normal)
            arrange_tree(node.child2, new File(path, "2"))
        }

        case leaf: Leaf[Word, File] =>
            leaf.value.copyTo(path / leaf.value.toString)
    }

    def arrange_cluster(map: Iterable[Iterable[Vector[Word]]], tree: Tree[Word, File], path: File) = {
        val v2f = tree.toMap
        val average = tree.average.normal
        println("!!! ==>", map.size)
        map.zipWithIndex foreach {
            case (vs, i) =>
                path / "%04d".format(i) / "vocabulary.txt" write (vs.reduce(_ + _).normal - average.normal)
                vs foreach {
                    // TODO: The vector is lost sometimes
                    case (v) => v2f.get(v) use {
                        case None =>
                            println("We met a problem with v: " + v)
                            v2f.keys.maxBy(x => v * x) use {
                                x =>
                                    {
                                        println("The best solution is x: " + x)
                                        println("that is as good as " + x * v + " " + x.normal * v.normal + " " + (x - v).norm)
                                    }
                            }
                        case Some(x) => x.copyTo(path / "%04d".format(i) / x.getName())
                    }
                }
        }

    }

    override def main(args: Array[String]) {
        val delimiter = """\W+""".r

        val (command, target, files) = args match {
            case Array(command, target, files @ _*) if command == "tree" ||
                command == "cluster" => (command, target, files)

            case Array(target, files @ _*) => ("both", target, files)

            case _ =>
                println("\nEnter: tree | cluster <PATH> [<FILE> [..]]\n")
                ("nothing", ".", Seq[String]())
        }

        def vectors = files.toIterator.map(x => new File(x)).map(x => {
            ((Vector(
                x.readLines().map(delimiter.split).flatten
                    .toList.groupBy(x => x.toLowerCase())
                    .map({ case (x, y) => (x, y.length.toDouble) })
                    .filter(_._2 > 5)
                    .map({ case (x, y) => string2word(x) -> y })
                    .toList), x)
            )
        })

        val t = System.currentTimeMillis()

        def tree = vectors.foldLeft(TreeApproximator[Word, File]())({
            case (tree, (vector, file)) => {
                debug.time("%s %d %s tree(%s).energy => %4.3f, length = %d / %d".format(
                    (System.currentTimeMillis() - t) / 10,
                    tree.n,
                    string2word.map.size,
                    file,
                    tree.energy2,
                    vector.size,
                    tree.average.size)
                ) {
                    System.gc()
                    (tree + (vector, file)).rectify(2)
                }

            }
        })

        def tree_opt = (1 to 5).foldLeft(tree /*.asInstanceOf[Node[Word, File]]*/ )({
            case (tree, n) =>
                debug.time("Rectifying #%3d = %4.3f %d".format(
                    n, tree.energy2, tree.average.size)
                ) {
                    tree.rectify(tree.n)
                }
        })

        def tree_aligned = "/tmp/tree.dat" cache {
            println("start align")
            val tree = tree_opt.align()._1
            tree
        }

        command match {
            case "tree" => arrange_tree(tree_aligned, target)
            case "cluster" => tree_aligned use {
                tree => arrange_cluster(debug.time("clustering") { Clusters(tree) }, tree, target)
            }
            case "both" =>

                tree_aligned use {
                    tree =>
                        {
                            arrange_cluster(debug.time("clustering") { Clusters(tree) }, tree, target / "cluster")
                            arrange_tree(tree, target / "tree")
                        }
                }

            case _ => println("Huh, boyz ...")
        }
    }
}
