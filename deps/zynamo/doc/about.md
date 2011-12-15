Zynamo - Elastic Zotonic
========================

Zynamo is a distributed fault-tolerant service for Zotonic, the Erlang CMS. 
Elastic Zotonic is a CMS framework that is Not Only SQL.


Why Elastic Zotonic?
--------------------

Elastic Zotonic answers the need for high availability and storing large amounts of data.
It does so by introducing a fault tolerant distributed layer to Zotonic.



Zotonic, Dynamo and Riak
------------------------

Elastic Zotonic is shaped after Dynamo[1] and Riak[2].  It shards all data using a consistent hashing scheme to buckets.
These buckets are distributed over a group of servers. We make sure that all buckets are replicated to N (typically 3) servers.

We use a different distribution method than Riak where we can grow from 1 to many servers. An important aspect for Zotonic is that the performance is optimal with the single server solution and that data is not stored multiple times on a single physical server.

Zotonic's way of partitioning the ring helps in distributing range queries to less (physical) nodes. In general we only need to perform range queries on M/N servers, where M is the number of servers and N is the number of replicated copies.

Both Zotonic and Riak use Dynamo's techniques of consistent hashing, vector clocks[3] and gossiping the ring state.

Zotonic's approach differs from Riak's in that it is better suited for low number of servers with high query rates.  Where Riak is well suited for a high number of servers with a high write rate.



Which backing store does Zotonic use?
-------------------------------------

Zotonic uses a normal RDBMS as a node's backing store.  This enables easy indexing and querying of the data. 
It also reuses well known techniques that are well understood by the majority of system administrators.  The data scheme used in Zotonic also supports ad-hoc SQL queries and data manipulation via SQL queries, as it fully leverages triggers and foreign key constraints to distribute data changes to other nodes.

Currently we use PostgreSQL, support for sqlite3 and MySQL are being planned.









  [1] http://s3.amazonaws.com/AllThingsDistributed/sosp/amazon-dynamo-sosp2007.pdf
  [2] http://wiki.basho.com/
  [3] http://en.wikipedia.org/wiki/Vector_clock
