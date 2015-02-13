Just enough Erlang shell
========================

An indispensible tool for both learning and programming Erlang.

`Submitted by: LRP; July 30, 2011`

WHY
---

You don't need to know Erlang or use the Erlang shell to create simple
Zotonic websites. But for Zotonic application development and
debugging, Erlang programming skills are essential.

Erlang is not that hard to learn. Several excellent books and any
number of web tutorials will get you well underway.

The Erlang shell is an indispensible tool for both learning and
programming Erlang.

The easiest way to learn how to use the Erlang shell is to fire it up
and play.

This Cookbook recipe provides all you need to get started.

ASSUMPTIONS
-----------

You have a recent version of Erlang installed on your system.

HOW
---

`What is the Erlang shell?`

The Erlang shell lets you test Erlang expression sequences both locally and remotely. It also lets you work with Erlang records and manage Erlang jobs.

`How can I enter the Erlang shell?`

From your terminal command line::

  $ erl

(and press enter). You'll see something like::

  Erlang R13B03 (erts-5.7.4) [source] [64-bit] [smp:3:3] [rq:3] [async-threads:0] [hipe] [kernel-poll:false] Eshell V5.7.4 (abort with ^G) 
  1>

``1>`` is the Erlang shell command line. You are now able to execute Erlang expressions.

`How can I get help?`

help(). -- list of shell functions 
How can I execute an expression?

Type now ``now()``. and then hit ENTER.

NOTE: the period at the end of the expression is necessary. It
terminates the Erlang expression sequence and initiates evaluation.

`How can I edit a line?`

* Ctrl+A -- go to beginning of line
* Ctrl+E -- go to end of line
* ``li`` <Tab> -- tab completiion; for instance, li tab will return ``lists:``
  * ``li`` <Tab> <Tab> -- Erlang terms that contain the characters "li"
    
For more shell editing commands, refer to Section 1.3.1 in:
http://www.erlang.org/documentation/doc-5.1/doc/getting_started/getting_started.html

`How can I manage Erlang jobs?`

* Ctrl+G -- user switch command; job control options
* Ctrl+G h -- help user switch options

`How can I leave the shell?`

``q().`` -- quits Erlang

`Are there other ways to quit?`

* halt(). -- exits the Erlang runtime system
* Ctrl+C Ctrl+C -- Display break options, then quit

BEWARE: You don't want to bring a running Erlang system down just to
quit the shell. Use q() until you get the hang of stuff.

`When I enter Ctl+C, I get a bunch of choices. What do they mean?`

Refer to: http://www.erlang.org/doc/man/shell.html

REFERENCES
----------

http://www.erlang.org/documentation/doc-5.1/doc/getting_started/getting_started.html

http://www.erlang.org/doc/man/shell.html

http://www.erlang.org/doc/getting_started/seq_prog.html

Erlang: Starting Out:
http://learnyousomeerlang.com/starting-out

