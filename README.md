webcrawler
==========

Simple web crawler is intended to gather a text corpus to scrutinize doing an NLP-like research. It scans a web site retrieving those pages, that are similar with the initial one. Heirarchical clustering allows to fine-tune this sample later.


Targeted getting information from a site
-----------------------------------------
The idea is quite simple: get the pages that are linked to an initial one or its descendants. Unfortunately a lot of auxiliary pages exists on real site that refers to everywhere that leads a download process away from a subject after a second level of links. Unlike a typical based on a network analysis, I have implemented a different approach: Algorithm maintains a cluster of pages that are similar with initial one and follows links in order of a priority queue, where priority comes from comparison of page that contains a link against the cluster. It produces the intended result: web crawler chooses similar pages even though it already got a few wrong ones, but requires a few tricks including online hierarchical clustering..
 
Usage
-------
An outcome of clustering technique is a bunch of articles that are arranged 
into a few clusters, and, by product, aligned in a sequence of mutually similar items. Current implementation also uses a simple technique to elicit keywords that distinguish clusters from each other.

Current version is still incomplete and requires some redundant computation, but can be used as follows:

 * Issue a command `java -jar webcrawler-assembly-1.1.jar -path <Directory> <Initial URI>` to download 
 web-pages similar with initial one into the Directory. This command dumps pages
 in plain text.

 * Use [treeapproximator](https://github.com/electricmind/treeapproximator) from a separate package to arrange items in a tree.
   
An additional tool `java -cp webcrawler-assembly-1.1.jar ru.wordmetrix.treeapproximator.Draw2DMap [<File of Points>]` is a simple demo that 
generates a few clouds of gaussian-distributed points and builds a tree.

The outcome
-----------
I used this approach to arrange 
[3500 pages that are adjacent to the page "Algorithm" of Wikipedia](http://electricmind.github.io/demo/treeapproximator/2013-11-04-an-outcome-of-hierarchical-clustering-of-wikipedia-pages.html)

History
---------

Version 1.0, 20131101, Initial release

Version 1.1, 20140114, Use akka.actor, build with sbt and make a few enhancements

Version 1.2, 20140129, Move libraries out, implement ArrangeText as independent tool and factor out priority forecasting code, add initialization with multiply seeds and permission to cross intersite links

Version 1.3, 20150129,

 * Move ArrangeText into separate package;
 
 * Export matrix of links and vector representation of text for downloaded pages (export them as sparse matrix for Octave);
     
 * TuneVocabulary tries to weight words on the base of links;
 
 * Report a variety of statistics during crawling;
 
 * The "breadthsearch" key turns off estimation and uses breadth search instead;
  
 
 
 
 
