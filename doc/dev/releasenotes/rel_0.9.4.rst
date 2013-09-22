Release 0.9.4
=============

Released on 2013-09-22 21:28 by arjan.

This release lets you run Zotonic 0.9 on Erlang R16. It is a patch
release to 0.9.3 which also claimed R16 compatibility but in fact
wasn't.

Arjan Scherpenisse (4):

* core: Add mochiweb as submodule
* core: Startup sequence now gets what applications need to be started from zotonic.app.src
* deps: New gen_smtp submodule

Marc Worrell (1):

* core: remove some more parametrized module references. Issue #513

