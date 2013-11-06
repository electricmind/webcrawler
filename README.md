webcrawler
==========

Simple web crawler is intended to gather a text corpus to scrutinize doing an NLP-like research. It scans a web site getting pages, that are similar with initial. Heirarchical clustering allows fine-tune this sample later.


Targeted getting information from a site
-----------------------------------------
The idea is quite simple: get the pages that are linked to an initial one or its descendants. Unfortunately a lot of auxiliary pages exists on real site that refers to everywhere that leads a download process off a subject after a second level of links. Usual solution uses a network analysis, but I implemented a different approach: Algorithm maintains a cluster of pages that are similar with initial one and follows links in order of a priority queue, where priority comes from comparison of page that contains a link against the cluster. It works fine: web crawler chooses similar pages even though it already got a few wrong ones, but requires a few tricks including online hierarchical clustering..
 
Hierarchical clustering
-------------------------
Hierarchical clustering arranges pages into hierarchy of groups of
similar pages. It approximates a a distribution of vectors by so-called 
central vectors. My approach is similar with and really is 
an improved K-means that builds a tree of clusters 
where each cluster has a few subclusters. 
 
Theoretically,  the algorithm has n*log n complexity but on real data it 
shows n**1.5 complexity since dimension of vectors drastically grows 
and tree becomes unbalanced.
 
The wonderful ability of this approach is lack of predefined amount of clusters 
and possibility of on-line learning. It makes a tree of clusters from a few of 
initial vectors and maintains while new vectors are coming.  On final stage 
algorithm requires some insight for a "likelihood coefficient" but it makes
 a sense of "accuracy" that is quite simply. Lately, coefficient can be changed 
 without repeat computation on all corpus of data as well.

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




