Just Enough Regular Expressions (in Erlang)
===========================================

Learn how to manipulate string data with the ``re`` module.

`Lloyd R. Prentice August 31, 2011`

WHY
---

The Erlang standard library re provides a powerful suite of functions
to excute regular expressions to find, replace, and manipulate
substrings within a string or Erlang binary.

`Re` functions are particularly useful for form validation,
decomposing and modifying urls, e-mail addresses, and other common web
elements represented as strings or binaries.

They are found throughout Zotonic source files.

See: http://www.erlang.org/doc/man/re.html

ASSUMPTIONS
-----------

You have a current version of Erlang installed on your system.

Examples have been tested on Ubuntu 11.04.

HOW
---

Bring up an Erlang terminal::

  $ erl
  Erlang R14B03 (erts-5.8.4) [source] [64-bit] [smp:3:3] [rq:3] [async-threads:0] [kernel-poll:false]

  Eshell V5.8.4  (abort with ^G)
  1> 

and follow along with the following examples. Modify and re-execute
each example until you feel comfortable with what’s going on.

What is a regular expression?
.............................

A regular expression is a pattern that is matched against a subject
string from left to right.

The power of regular expressions comes from the ability to include
alternatives and repetitions in the pattern. These are encoded in the
pattern by the use of metacharacters.

What is a pattern?
..................

Most characters stand for themselves in a pattern.

The pattern "ick", for instance, would match the first occurence of
"ick" in the string "The quick brown fox."

We'll use ``re:run/2`` to illustrate. This function checks a string agains a regular expression: ``run(Subject,RE) -> {match, Captured} | nomatch``.

Example::

  6> re:run("The quick brown fox.","ick").
  {match,[{6,3}]}

The atom "match" is self-explanatory. The tuple {6,3} in the list provides the start position and length of the pattern in the subject string.

.. code-block:: erlang

   7> re:run("The brown fox.","ick").          
   nomatch

re:run/2 will also work with a binary::

  8> re:run(<<"The quick brown fox.">>,"ick").
  {match,[{6,3}]}

If we wish to find all instances of "ick" in a string, we need to use
re:run/3, ``run(Subject,RE,Options) -> {match, Captured} | match | nomatch``.

Example::

  9> re:run("The sick quick brown fox.", "ick", [global]).
  {match,[[{5,3}],[{11,3}]]}

For documentation of re:run/3 options, see http://www.erlang.org/doc/man/re.html

How can I replace a substring in a string?
..........................................

Use ``re:replace/3``: ``replace(Subject, RE, Replacement) -> iodata() | unicode:charlist()``.

Example::

  10> re:replace("The quick brown fox.", "brown", "red").
  [<<"The quick ">>,<<"red">>|<<" fox.">>]

Hmmm... re:replace/3 returns an odd-looking binary, which is called an
`iolist`: an efficient data structure, used like this to prevent
copying of data in memory.

But, let’s use re:replace/4 to provide an option: ``replace(Subject, RE, Replacement, Options) -> iodata() | unicode:charlist()``::

  11> re:replace("The quick brown fox.", "brown", "red", [{return, list}]).
  "The quick red fox."

The ``{return, list}`` does the trick of returning a "regular"
string. Erlang documentation is generally thorough, but often not that
easy to follow. That’s why I created this Cookbook item. I wanted to
learn this stuff myself.

Regular expressions can deliver much much more, however, with shrewd
use of metacharacters.

What is a metacharacter?
........................

Metacharacters are interpreted in special ways. For instance, the
metacharacter . matches the first instance of any character in a
string except newline.

Example::

  13> re:run("The quick brown fox.", ".").
  {match,[{0,1}]}

You'd usually use . in a more elaborate pattern::

  14> re:run("The quick brown fox.", "qu.").                               
  {match,[{4,3}]}

  15> re:run("The quack brown fox.", "qu.").
  {match,[{4,3}]}

The metachacter ^ asserts start of string.

Examples::

  16> re:run("The quack brown fox.", "^The").
  {match,[{0,3}]}

  17> re:run("The quack brown fox.", "^qua").
  nomatch

Similarly, the metacharacter $ asserts the end of a line::

  18> re:run("The quick brown fox is sick.", "ick.$").
  {match,[{24,4}]}

The metacharacter * matches zero or more characters.

Examples::

  19> re:run("The quick brown fox.", "i*").
  {match,[{0,0}]}

  20> re:run("The quick brown fox.", "T*").   
  {match,[{0,1}]}

  21> re:run("TTTTThe quick brown fox.", "T*").
  {match,[{0,5}]}

The metacharacter + matches one or more characters::

  22> re:run("TTTTThe quick brown fox.", "z+").
  nomatch

  23> re:run("TTTTThe quick brown fox.", "T+").
  {match,[{0,5}]}

The metacharacter | alternate patterns. Think of it as "or"::

  24> re:run("The quick brown fox.", "fox|pig").
  {match,[{16,3}]}

  25> re:run("The quick brown pig.", "fox|pig").
  {match,[{16,3}]}

You can also match generic character types. \s, for instance matches
any whitespace character.

Examples::

  26> re:run("The quick brown fox","\s",[global]).
  {match,[[{3,1}],[{9,1}],[{15,1}]]}

How can I match non-printing characters?
........................................
See: Non-printing characters http://www.erlang.org/doc/man/re.html

Note that the metacharacters [ and ] have special meaning, they
enclose "character classes."  A character class is the set of
characters in a character class match, if any found, one character in
the subject string.

Examples::

  24> re:run("The quick brown fox.", "[qui]").
  {match,[{4,1}]}

  25> re:run("The quick brown fox.", "[ui]"). 
  {match,[{5,1}]}

  26> re:run("The quick brown fox.", "[qui]", [global]).
  {match,[[{4,1}],[{5,1}],[{6,1}]]}

You can combine characters, meta-characters, and other regular
expression elements into extended patterns that can search, match, and
replace nearly any substrings you can imagine.

Example::

  27> re:run("E-mail: xyz@pdq.com", "[a-zA-Z0-9]+@[a-zA-Z0-9]+\.[a-z]{2,3}").
  {match,[{8,11}]}

Note: DO NOT use this pattern in production. It needs more refinement and much more testing.

What other goodies does ``re`` offer?

``split(Subject, RE) -> SplitList`` and ``split(Subject, RE, Options) -> SplitList``.

Examples::

  28> re:split("this/is/my/path","/").
  [<<"this">>,<<"is">>,<<"my">>,<<"path">>]

If you wish to use a pattern multiple times and boost perfomance, you
can compile it with ``re:compile/1``.

Example::

  29>  {_, P} = re:compile("[a-zA-Z0-9]+@[a-zA-Z0-9]+\.[a-z]{2,3}").
  {ok,{re_pattern,0,0,
                <<69,82,67,80,164,0,0,0,0,0,0,0,5,0,0,0,0,0,0,0,0,0,64,
                  ...>>}}
  30> re:run("E-mail: xyz@pdq.com", P).
  {match,[{8,11}]}

How are regular expressions used in Zotonic source?

For one of many examples, look at ``zotonic/src/markdown/get_url/1``::

  get_url(String) ->
    HTTP_regex = "^(H|h)(T|t)(T|t)(P|p)(S|s)*://",
    case re:run(String, HTTP_regex) of
        nomatch    -> not_url;
        {match, _} -> get_url1(String, [])
    end.

Where can I go from here?

Study and experiment with all the metacharacters and other regular
expression constructs in:

http://www.erlang.org/doc/man/re.html

Do further research on the web. Everytime you see an interesting
regular expression, test it in re:run/2. You may well have to edit to
get it to run on re:run/2. But if you understand the basics, it won’t
be difficult.

TROUBLESHOOTING
---------------

CAUTION: Complex regular expression patterns are hard to read and
error prone. Break them down into short segments and test each
segment. Then build them back up.

The hard part is confirming that your pattern will match all possible
instances of the string segments you’re interested in.

RESOURCES
---------

http://www.erlang.org/doc/man/re.html
http://langref.org/erlang/pattern-matching
http://www.regular-expressions.info/examples.html

