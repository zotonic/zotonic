.. _cookbook-otp1:

Just enough Erlang/OTP and rebar, part 1
========================================

Zotonic source code have you scratching your head? Learn Rebar first.

`Created Aug 8, 2011 by Lloyd R. Prentice`

Why
---

Rebar is a relatively new set of Erlang/OTP development tools. Rebar
makes it easier to develop and maintain Erlang/OTP applications and
releases.

Zotonic is built on Erlang/OTP. An understanding of Erlang/OTP
conventions is essential if you wish to read Zotonic source code,
develop Zotonic modules, or contribute code or patches to Zotonic.

As yet, Zotonic is not maintained under Rebar (external dependencies in `deps` are). But developing a
skeletal Erlang/OTP application under Rebar is a great way to dip your
feet into Erlang/OTP development.

By following each step in this tutorial carefully, and referring back
to the many excellent on-line Erlang documentation resources, you will
accelerate your progress up the daunting Erlang/OTP learning
curve. And more, you'll learn how to read and understand Zotonic
source code while you’re at it.

In this tutorial we'll use rebar to create, compile, and test two
Erlang applications. One will include a simple gen_server.

In Part II, we'll generate documentation, run eunit tests, and create
a release that can be copied and run on a suitable host system.

Assumptions
-----------

You have Erlang R14B or later installed on your system. You have
Internet access and basic Bash command line skills. File editing tasks
refer to vim. But you can use your code editor of choice.

This tutorial was tested on Ubuntu 11.04.

How
---

How can I install and learn Rebar?
...................................
Create a root directory for experimentation. Let’s call it "learn."::

  $ mkdir learn 
  $ cd learn 

Download the rebar binary
.........................
In the shell::

  learn$ git clone https://github.com/basho/rebar.git rebar-src
  learn$ cd rebar-src/
  rebar-src$ ./bootstrap
  rebar$ cd ..
  learn$ cp rebar-src/rebar .
  learn$ chmod u+x rebar

How can I create an application
...............................
In the shell::

  learn$ ./rebar create-app appid=zzz
  learn$ ls
  >> rebar rebar-src src 

Note that Rebar has created a directory named `src`::

  learn $ ls src 
  >> zzz_app.erl zzz.app.src zzz_sup.erl 

In `src`, Rebar has created three Erlang modules. Open them up and
take a look in your favorite code editor:

Learn more about Erlang applications:
http://www.erlang.org/doc/design_principles/applications.html
http://www.erlang.org/doc/man/application.html

How can I add a gen_server template to my new application?
..........................................................
In the shell::

  learn$ ./rebar create template=simplesrv srvid=zzz_srv 
  learn$ ls src 
  >> zzz_app.erl zzz.app.src zzz_srv.erl zzz_sup.erl 

Open up ``zzz_svr.erl`` and look it over. For more info, study these ``gen_gerver`` resources:

http://www.erlang.org/doc/design_principles/gen_server_concepts.html
http://www.erlang.org/doc/man/gen_server.html

.. highlight: erlang
   
How can I make my new gen_server do something?
..............................................

In ``src/zzz_srv.erl``, add two functions to the API ``-export`` directive as follows::
  
  -export([start_link/0, say_hello/0, stop/0]).

Add the say_hello/0 function as follows::

  say_hello() -> 
    gen_server:call(?MODULE, hello).

Replace handle_call(_Request, _From, State)::
 
  handle_call(hello, _From, State) ->
    io:format("Hello from zzz_srv!~n", []), 
    {reply, ok, State}; 
  handle_call(_Request, _From, State) -> 
    Reply = ok, 
    {reply, Reply, State}. 

Add the stop/0 function::

  stop() ->
    gen_server:cast(?MODULE, stop).

Add before handle_cast(_Msg, State), put::

  handle_cast(stop, State) ->
    {stop, normal, State};

NOTE: If your gen_server is under supervision, there’s a better way to stop your server. See:

Section 2.6 of gen_server Concepts - Stopping:
http://www.erlang.org/doc/design_principles/gen_server_concepts.html

You could compile this code with Rebar now, but let’s defer.

To really get the hang, let’s create TWO applications. We'll put them under a new directory, `apps/`::

  learn$ mkdir apps 
  learn$ mkdir apps/zzz 
  learn$ mkdir apps/zzz_lib 
  learn$ ls apps
  >> zzz zzz_lib 
  learn$ mv src apps/zzz/
  learn$ ls apps/zzz
  >> src 

Now we'll create the zzz_lib application::
    
  learn$ ./rebar create-app appid=zzz_lib
  learn$ ls
  >> apps rebar rebar-src src

And let’s make it do something::

  learn$ cd src 

Create and save a module called ``hello.erl`` that does something::

  -module(hello).
  -export([hello/0]). 
  hello() ->
    io:format("Hello from zzz_lib!~n", []).

Back in the shell move the ``src`` directory to ``apps/zzz_lib``::

  src$ cd ..
  learn$ mv src apps/zzz_lib/
  
How can I compile these two applications?
.........................................

First, we need to create a ``rebar.config`` file in our project home
directory. Create the file, add the following directive and save::

  {sub_dirs, ["apps/zzz", "apps/zzz/src", "apps/zzz_lib", "apps/zzz_lib/src" ] }. 

Back in the shell::
    
  learn$ ls
  >> apps rebar rebar-src rebar.config

Now compile::
    
  learn$ ./rebar compile

If you see the following, pat yourself on the back::

  ==> zzz (compile)
  Compiled src/zzz_app.erl
  Compiled src/zzz_sup.erl
  Compiled src/zzz_srv.erl
  ==> src (compile)
  ==> zzz_lib (compile)
  Compiled src/hello.erl
  Compiled src/zzz_lib_app.erl
  Compiled src/zzz_lib_sup.erl
  ==> src (compile)
  ==> learn (compile)
  
Check out the ebin directories::

  learn$ ls apps/zzz/ebin 
  >> zzz.app zzz_app.beam zzz_srv.beam zzz_sup.beam
  learn$ ls apps/zzz_lib/ebin 
  >> hello.beam zzz_lib.app zzz_lib_app.beam zzz_lib_sup.beam 

you’re now ready to rock and roll!!
  
How can I test?
...............

Start the Erlang shell::

  learn$ erl -pa apps/*/ebin
  1> zzz_srv:start_link().
  {ok,<0.33.0>} 
  2> zzz_srv:say_hello().
  Hello from zzz_srv! 
  ok
  3> zzz_srv:stop().
  ok 
  4> hello:hello().
  Hello from zzz_lib!
  ok 

Troubleshooting
---------------

I got an error when I compiled. What now?

make sure your ``rebar.config`` directive, as shown above, is correct.

Make sure you have this directory structure::

  learn$ tree
  .
  apps
  │   ├── zzz
  │   │   ├── ebin
  │   │   └── src
  │   │       ├── zzz_app.erl
  │   │       ├── zzz.app.src
  │   │       ├── zzz_srv.erl
  │   │       └── zzz_sup.erl
  │   └── zzz_lib
  │   │   ├── ebin
  │       └── src
  │           ├── hello.erl
  │           ├── zzz_lib_app.erl
  │           ├── zzz_lib.app.src
  │           └── zzz_lib_sup.erl
  ├── rebar
  └── rebar.config

Fix any source code errors, and recompile::
    
  learn$ ./rebar compile

What you've learned
-------------------

You've now had a good soak in basic Erlang/OTP conventions and
Erlang. You can install Rebar, create Erlang/OTP applications, and
compile them. You've also created a simple gen_server.

Where to go from here
---------------------

Study the online and printed Erlang documentation upside and
sideways. Skim to see what’s there, then reread everytime you have a
problem. You'll be an Erlang/OTP wizard before you know it.

References on the web
---------------------

Getting Started:
https://github.com/basho/rebar/wiki/Getting-started

Damn Technology:
http://damntechnology.blogspot.com/

How to create, build, and run an Erlang OTP application using Rebar:
http://skeptomai.com/?p=56#sec-3

Commands:
https://github.com/basho/rebar/wiki/Rebar-commands

Erlang App. Management with Rebar:
http://erlang-as-is.blogspot.com/2011/04/erlang-app-management-with-rebar-alan.html

Dizzy Smith – Building Erlang Applications with Rebar:
http://ontwik.com/erlang/dizzy-smith-building-erlang-applications-with-rebar/

Rebar Demo using ibrowse:
http://vimeo.com/8311407

rebar / rebar.config.sample:
https://github.com/basho/rebar/blob/master/rebar.config.sample?source=cc

Books
-----

Programming Erlang: Software for a Concurrent World:
http://www.amazon.com/Programming-Erlang-Software-Concurrent-World/dp/193435600X

Erlang Programming:
http://www.amazon.com/ERLANG-Programming-Francesco-Cesarini/dp/0596518188/ref=pd_sim_b_1

Erlang and OTP in Action:
http://www.amazon.com/Erlang-OTP-Action-Martin-Logan/dp/1933988789/ref=pd_sim_b_1
