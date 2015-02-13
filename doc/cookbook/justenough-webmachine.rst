Just enough Webmachine
======================

Getting a solid foundation for your Zotonic skillset.

WHY
---

Zotonic talks to the web through a wonderful Erlang-based http REST
toolkit called `Webmachine`. Webmachine was generously contibuted to
the open-source world by Basho of Cambridge, MA.

A basic understanding of Webmachine will provide a solid foundation
for your Zotonic skillset.

This Cookbook recipe will give you hands-on experience with
Webmachine, Webmachine resources, and how to modifify a Webmachine
resource.

ASSUMPTIONS
-----------

You have git, a recent version of Erlang, and Zotonic installed on
your system.

You have basic Linux shell and program editing competencies.

You have basic sequential Erlang skills and understand the structure
of Erlang modules.

HOW
---

We'll install and compile Webmachine, create a simple demo
application, and modify and execute a Webmachine resource. Finally,
we'll take a brief look at how Zotonic integrates Webmachine.

NOTE: Your Zotonic install already includes a webmachine instance. But
we'll install another instance so you can play.

What is Webmachine?
...................

Webmachine handles well-formed http REST requests, gracefully handles
all errors, and provides powerful debugging tools.

A Webmachine application is a set of `resources` written in Erlang,
each of which is a set of functions over the state of the resource.

Note: in Zotonic’s version of Webmachine, we renamed `resource` to
`controller`, as we think that’s a better name for those things. And
in Zotonic, resources are "pages".

These functions give you a place to define the representations and
other Web-relevant properties of your application's resources

How can I explore Webmachine?
.............................

Make sure that you have a working Erlang/OTP release, R12B5 or later.

Get the webmachine code::

  git clone git://github.com/basho/webmachine

Build webmachine::

  cd webmachine
  make

Create, build, and start the skeleton resource::

  ./scripts/new_webmachine.sh mywebdemo /tmp
  cd /tmp/mywebdemo
  make
  ./start.sh

You should see a series of progress reports. Point a web browser at http://localhost:8000/

You should now see Hello, new world

How can I stop the demo?
........................

Press ctl C, then press a

How can I examine the resource that produced the hello-world message?
.....................................................................
In the shell::

  $ cd /tmp/mywebdemo/src

Open mywebdemo_resource.erl in your favorite code editor.

How can I modify the resource to execute and display my own Erlang function?
............................................................................

Make the following changes mywebdemo_resource.erl just under the init/1 function::

  earthman() ->
      "Earth man!".

  to_html(ReqData, State) ->
      {"<html><body>Hello, " ++ earthman() ++ "<body></html>", ReqData, State}.

Save, and compile::

  mywebdemo$ make

Now, from your web browser, refresh http://localhost:8000. You should see your change.

You'll find more Webmachine documentation at:
http://webmachine.basho.com/docs.html. It'll take you pretty much
wherever you want to go.

You now know enough about Webmachine to understand how Zotonic
communicates through the web.

So how does Zotonic integrate Webmachine? Peek into your Zotonic `deps` directory::

  $ cd /home/zotonic/zotonic/deps

You'll see the``webzmachine`` directory, which is Zotonic’s version of
webmachine it’s contents will be quite familiar from your explorations
above, although some implementation details differ.

REFERENCES
----------

http://webmachine.basho.com/

