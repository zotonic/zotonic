About
=====

Erlang-iconv is a fork of the iconv in Jungerl. I pulled it out of jungerl
because I only wanted iconv, not all the other stuff in jungerl and because the
jungerl build system wasn't very helpful on anything but linux.

erlang-iconv has been built and tested on the following platforms:

* Linux
* FreeBSD
* OpenBSD
* OSX

It should also build on NetBSD and most other UNIX-like systems. I haven't
bothered to try on windows yet.

Usage
=====

Here's some sample usage converting some shift-jis text to utf-8:

<pre>
1&gt; iconv:start().
{ok,&lt;0.53.0&gt;}
2&gt; X = &lt;&lt;83,97,109,112,108,101,58,32,142,132,32,130,205,32,131,67,32,131,147,32,131,79,32,131,71,32,130,197,32,130,183,13,10&gt;&gt;.
&lt;&lt;83,97,109,112,108,101,58,32,142,132,32,130,205,32,131,
  67,32,131,147,32,131,79,32,131,71,32,130,197,32,...&gt;&gt;
3&gt; io:format("~s~n", [X]).
Sample:
         Í C  O G Å ·

ok
4&gt; io:format("~ts~n", [X]).
** exception exit: {badarg,[{io,format,
                                [&lt;0.25.0&gt;,"~ts~n",
                                 [&lt;&lt;83,97,109,112,108,101,58,32,142,132,32,130,205,32,
                                    131,67,32,131,147,...&gt;&gt;]]},
                            {erl_eval,do_apply,5},
                            {shell,exprs,6},
                            {shell,eval_exprs,6},
                            {shell,eval_loop,3}]}
     in function  io:o_request/3
5&gt; {ok, CD} = iconv:open("utf-8", "shift-jis").
{ok,&lt;&lt;176,86,91,1,0,0,0,0&gt;&gt;}
6&gt; {ok, Output} = iconv:conv(CD, X).
{ok,&lt;&lt;83,97,109,112,108,101,58,32,231,167,129,32,227,129,
      175,32,227,130,164,32,227,131,179,32,227,130,176,
      ...&gt;&gt;}
7&gt; io:format("~ts~n", [Output]).
Sample: 私 は イ ン グ エ で す
8&gt; iconv:close(CD).
ok
</pre>

As you can see, before we passed it through iconv it was an unprintable mess.

Installation
============

To install, use rake:

<pre>
rake
sudo rake install
</pre>

If the compilation of the C part fails with errors about iconv, you have to
specify the prefix where iconv is located, for example on FreeBSD:

<pre>
rake iconv=/usr/local
</pre>

And if you don't have iconv or it's development header installed (and iconv
isn't part of your C libary) install it.
