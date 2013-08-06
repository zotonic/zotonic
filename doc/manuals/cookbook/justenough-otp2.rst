Just enough Erlang/OTP and rebar, part 2
========================================

Building a gen_server to front the library and generating
documentation.

`Created Aug 17, 2011 by Lloyd R. Prentice`

WHY
---

If you worked through the Cookbook item :ref:`cookbook-otp1`, you have
created two Erlang applications under management of Rebar. You've used
Rebar to create a gen_server template in the first application and
added several functions. You've also created a library application
with one function.

In this tutorial we'll call our library function from our gen_server
and generate documentation.

ASSUMPTIONS
-----------

You have Erlang R14B or later installed on your system. You have basic
Bash command line skills. File editing tasks refer to vim. But you can
use your code editor of choice.

You've successfully compiled the zzz and zzz_lib applications from
Part I under Rebar.

This tutorial was tested on Ubuntu 11.04.

HOW
---

How can I call a library function from zzz_lib in a zzz module?

In the shell::

  learn$ cd apps/zzz/src

Edit handle_call/2 in zzz_srv.erl as follows::

  handle_call(hello, _From, State) ->
    hello:hello(),
    io:format("~nHello from zzz_srv!~n", []),
    {reply, ok, State};

While you're at it, check your handle_cast/2 code. It should look like this::

  stop() ->
    gen_server:cast(?MODULE, stop).

  %% callbacks
  handle_cast(stop, State) ->
    {stop, normal, State};

  handle_cast(_Msg, State) ->
    {noreply, State}.

Now edit and save init/1 in learn/apps/zzz/src/zzz_sup.erl::

  init([]) ->
     {ok, {{one_for_one, 1, 60}, [?CHILD(zzz_srv, worker)]}}.

Now recompile::

  learn$ ./rebar clean compile

And test::

  learn$ erl -pa apps/*/ebin
  ...
  1> zzz_sup:start_link().
  {ok,<0.33.0>}

Note that you may see a different PID on your system. Try some functions::

  2> zzz_srv:say_hello().
  Hello from zzz_lib!
  Hello from zzz_srv!
  ok

Both our library function and zzz_svr have performed as we'd hoped.

While developing this tutorial, I introduced a bug in
``zzz_svr:handle_cast/2``. Once it started, I couldn't stop. This was
a good excuse to introduce a valuable debugging tool.

How can I kill a running process?
.................................

In the Erlang shell, start `pman`, the process manager::

  3> pman:start().
  <0.37.0>

You should see a graphic window open with a list of running
processes. Find ``zzz_srv`` and ``zzz_sup`` under Name. Click on
zz_sup to highlight. Now pull down the Trace menu item and click on
`kill`. Note that you've not only killed zzz_sup, but zzz_srv as
well. Of course. zzz_sup supervises zzz_srv. When it dies, so does
zzz_srv.

You can confirm::

  4> zzz_srv:say_hello().
  ** exception exit: {noproc,{gen_server,call,[zzz_srv,hello]}}
     in function  exit/1
        called as exit({noproc,{gen_server,call,[zzz_srv,hello]}})
     in call from gen_server:call/2

`You assassin, you!`

But, no worry, you can start it back up again::

  5> zzz_srv:start_link().
  {ok,<0.48.0>}

Now let's test zzz_srv:stop().::

  6) zzz_srv:stop().
  ok

Seemed to worked. But...::

  7> zzz_srv:say_hello().
  Hello from zzz_lib!
  Hello from zzz_srv!
  ok

It turns out, zzz_sup started it up again. Try it again::

  8> zzz_srv:stop().
  ** exception exit: shutdown

Wait! If you look at pman, you'll see that zzz_sup also died. What's going on here?

If you look at zzz_sup.erl in learn/apps/zzz/src, you'll note that init/1 allows only one restart, the value following "one_for_one."::

  init([]) ->
     {ok, {{one_for_one, 1, 60}, [?CHILD(zzz_srv, worker)]}}.

For more details, checkout the Supervisor Behaviour: http://www.erlang.org/doc/design_principles/sup_princ.html

How can I generate documentation?
.................................

Add comments to learn/apps/zzz/src/zzz_srv.erl as follows::

  %% @spec say_hello() -> none
  %% @doc display greeting
  say_hello() ->

And to learn/apps/zzz_lib/src/hello.erl::

  %% @spec hello() -> none
  %% @doc Library test function 
  hello() ->

Now execute::

  learn$ ./rebar clean compile
  ...
  learn$ ./rebar doc
  ==> zzz (doc)
  ==> zzz_lib (doc)

Check out your new doc directories::

  learn$ ls apps/zzz
  doc  ebin  src

  learn$ ls apps/zzz_lib
  doc  ebin  src

Now bring up your browser and point to file::

  file:///home/learn/apps/zzz/doc/index.html file:///home/learn/apps/zzz_lib/doc/index.html

TROUBLESHOOTING
---------------

I got an error when I compiled. What now?

make sure rebar.config in ../learn looks like this::

  {sub_dirs,
          ["apps/zzz",
           "apps/zzz/src",
           "apps/zzz_lib",
           "apps/zzz_lib/src"
          ]
  }.

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

WHAT YOU'VE LEARNED
-------------------

You've now had a good soak in basic Erlang/OTP conventions and
Erlang. You can install Rebar, create an Erlang/OTP application,
compile it and create documentation.

WHERE TO GO FROM HERE
---------------------

Study the online and printed Erlang documentation upside and
sideways. Skim to see what's there, then reread everytime you have a
problem. You'll be an Erlang/OTP wizard before you know it.

Now, dive into Zotonic source code. It should be much easier to
follow.

REFERENCES
----------

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