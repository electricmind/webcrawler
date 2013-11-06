webcrawler
==========

Simple web crawler is intended to gather a text corpus to scrutinize doing an NLP-like research. It scans a web site getting pages, that are similar with initial. Heirarchical clustering allows fine-tune this sample later.


Targeted getting information from a site
-----------------------------------------
The idea is quite simple: get the pages that are linked to an initial one or its descendants. Unfortunately a lot of auxiliary pages exists on real site that refers to everywhere that leads a download process off a subject after a second level of links. Usual solution uses a network analysis, but I implemented a different approach: Algorithm maintains a cluster of pages that are similar with initial one and follows links in order of a priority queue, where priority comes from comparison of page that contains a link against the cluster. It works fine: web crawler chooses similar pages even though it already got a few wrong ones, but requires a few tricks including online hierarchical clustering..
 
Hierarchical clustering
-------------------------
Hierarchical clustering arranges similar pages into a tree of groups, where each group defines a central vector. An algorithm starts with a group of two vectors, where central vector is their average. For each new vector it looks for the closest vector recursively choosing the closest central vector until a leaf, that is converted into a new pair of vectors. Algorithm updates central vector for all of the inner nodes on a path to the new vector.  For each new vector a learning step randomly chooses a couple of vectors and re-add them again.

This approach is similar with and really is an improved K-means. Theoretically,  it has n*log n complexity but on real data a tree becomes unbalanced and dimension grows a lot that turns complexity up to  n**1.5.

At the final stage algorithm alignes vectors and  joins clusters together on the base of an  accuracy coefficient that is more easy to choose than an amount of clusters. Later coefficient can be changed without repeat computation for all data corpus.

From the begining algorithm maintains an actual set of clusters, that makes on-line learning possible.

The use
-------
An outcome of clustering techniques is a bunch of articles that are arranged 
into a few clusters, and, by product, aligned in a sequence of mutually similar
items. Current implementation also uses a simple technique to elicit keywords
that distinguish the clusters.

Current version is still incomplete and requires some redundant computation
power, but can be used as follows:

 * Issue a command `webcrawler <Directory> <Initial URI>` to download 
 web-pages similar with initial one into the Directory. This command dumps pages
 in plain text.

 * Arrange pages with `arrangetext [links|clusters|tree] <Directory> [List of files]`, where:

  * "links" dumps a page of references to the Wikipedia (for lack of a preliminary wishes it's just a desperate attempt to present an outcome, but looks fine),

  * "clusters" stores clusters of pages in separate directories,  titled from discernible keywords and equipped with a positional vectors;

  * "tree" arranges pages into a tree of  folders that each contains two subfolders along with a positional vector of its subcluster.
   
An additional tool `Draw2DMap [<File of Points>]` is a simple demo that 
generates a few clouds of gaussian-distributed points and builds a tree.

An outcome
-----------
I used this approach to arrange 
[3500 pages that are adjacent to the page "Algorithm" of Wikipedia](http://electricmind.github.io/demo/treeapproximator/2013-11-04-an-outcome-of-hierarchical-clustering-of-wikipedia-pages.html)




