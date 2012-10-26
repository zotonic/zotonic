Erlang tab completion
=====================

Get quicker access to Zotonic code on the shell.

`Contributed by: Maas-Maarten Zeeman`

Why
---

When you are working on the shell chances are you are looking to be
fast.  A way to be faster is to leverage the EShell's tab completion.

Assumptions
-----------

Readers are assumed to be comfortable using the EShell to run Erlang
code interactively.  This feature this guide describes only works with
Zotonic modules only from ``zotonic debug``.  With the ``zotonic
shell`` command, the Zotonic modules need to be explicitly loaded to
work with tab-completion.

How
---

The Erlang shell has tab-completion. IIt is very handy for using in
Zotonic, especially because of Zotonic's naming scheme. Just type::

  1> z_<tab> 

And you get a list of zotonic library modules. Type::

  2> mod_<tab>, type m_<tab> a list of models. 

You get the idea. The really nice thing is that it works for function names too.

