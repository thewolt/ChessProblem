Chess Problem
=============

The problem is to find all the unique configurations of a given set of chess pieces
so that none of them are in check against each other.

There's a well known problem involving N queens but in this one you can use King, Queen,
Rook, Bishop and Knight.

Installation
------------

You need:

* [scala](http://scala-lang.org)
* [xsbt](https://github.com/harrah/xsbt/wiki)


Usage
-----

To get the total number of configuration for 2 Queens, 2 Kings, 2 Bishops and one Knight on
a 7x7 board:

    $ sbt run

To run the tests:

    $ sbt test

Challenge
---------

### Memory

The first versions were very memory intensive because it kept in memory all the positions
to ensure the unicity of the configurations. Of course, the idea was to find the algorithm to make sure
that each configuration is tested once.

### Speed

The first versions ran for dozens of minutes as the final version ( without usage of parallel collections ) 
makes it about 45 seconds on my MacBook Air. I wonder if it's possible to take advantage of the parallel
collections.

