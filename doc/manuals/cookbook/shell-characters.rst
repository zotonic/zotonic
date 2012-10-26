.. highlight:: erlang

Filter and convert characters
=============================

Applying Erlang Binary syntax to get fast character manipulation.

- `Author: Lloyd R. Prentice`
- `Co-Author: Andreas Stenius`

Why
---

Erlang bit syntax is extraordinarily powerful and well worth learning.

Review documenation here: http://www.erlang.org/doc/programming_examples/bit_syntax.html

Here’s an experiment. Follow along in your handy Erlang shell::

  1> A = "The cat on the mat". 
  "The cat on the mat" 
  2> B = <<"The cat on the mat">>. 
  <<"The cat on the mat">> 
  3> io:format(A,[]). 
  The cat on the matok 
  4> io:format(B,[]). 
  The cat on the matok 
  So, big deal. What’s the difference?

Memory consumption, that’s what. A is represented internally as a
list. Each character is a membory cell with a character and a pointer
to the next cell. The list format is a memory hog. List represention
of text strings is both bane and blessing for Erlang programmers. B,
on the other hand, is mapped in memory as a contiguous array of
characters, thus consuming much less memory.

For details on working with Erlang Binaries, look here:
http://www.erlang.org/doc/man/binary.html

When you want to filter and convert characters in an Erlang Binary,
however, things get dicey. It’s all there in the Erlang documentation,
but examples are few and far between.

The purpose of this particular recipe is to prepare text for
conversion from a \*.txt file to \*.tex, a file marked up for input to
the truly wonderful open source typesetting program Latex.

In our case, we don’t have control of what gets typed into the source
text file. So we need to both filter for unwanted characters and
sequences as well as convert characters and sequences into LateX
markeup.

What does this have to do with Zotonic?

You’ll just have to wait and see.

Assumptions
-----------

All you’ll need here is a text editor to copy and paste an Erlang
module and an Erlang shell.

A passing understanding of Erlang list comprehensions will also be
useful. Find more here:
http://www.erlang.org/doc/programming_examples/list_comprehensions.html

How
---
Study latexFilter/1 below::

  -module(filterz). 
  -export([latexFilter/1]). 

  %% Binary filters 
  %% with many thanks to Andreas Stenius 
  latexFilter(B) -> 
     %% Delete control characters and extended ASCII 
      B1 = << <<C>> || <<C>> <= B, (C == 10) or (C == 13) or (C >= 31), 
  (C =< 127) >>, 
     %% Filter binary for conversion to *.tex format 
     %% NOTE: Partially tested, but needs more 
     F = [{"\r","\n"},                         % Convert returns to new lines 
          {"\n *","\n"},                       % Delete spaces following newline 
          {"^\\s*", ""},                       % Delete all whitespace 
  characters at beginning of manuscript 
          {"^\n*", ""},                        % Delete new lines at 
  beginning of manuscript 
          {"\n+$","\n"},                       % Delete excess new lines 
  at end of manuscript 
          {"  +"," "},                         % Delete successive spaces 
          {"\n{3,}","\n\\\\bigskip\n"},        % Convert three or more 
  newlines to Latex bigskip 
          {"[&$#%~^{}]", "\\\\&"},             % Escape reserved Latex character 
          {"<i>", "\\\\emph{"},                % Convert HTML tag to Latex tag 
          {"</i>", "}"},                       % Convert HTML tag to Latex tag 
          {"\"(.*\")","``\\1"},                % Convert opening double quotes (") to Latex conventions (``) 
          {" '"," `\\1"}],                     % Convert opening single quotes (') to Latex conventions (`) 
     B2 = lists:foldl(fun({Pattern, Replacement}, Subject) -> 
                         re:replace(Subject, Pattern, Replacement, 
                                    [global, {return, binary}]) end,
                      B1, F), 
     {ok, B2}.


The first thing that happens in latexFiler/1 is an Erlang binary
comprehension to delete pesky control and extended ASCII characters::

  B1 = << <<C>> || <<C>> <= B, (C == 10) or (C == 13) or (C >= 31), (C =< 127) >>,
  
Note that the syntax is very similar to list comprehensions. As you
can see, a single binary comprehension can chug out a lot of
work. Define a few binaries in your Erlang shell and play around with
the conditionals. You’ll catch on pretty quick.

For our purposes, we need to also search and replace a bunch of
substrings. For this we’ve enlisted ``lists:foldl/3``, a powerful list
function that just happens to work with binaries. See:
http://www.erlang.org/doc/man/lists.html#append-1

Expect a headache when you study this function. It’s not that easy to
understand.

The ``fold`` function exists in two versions, ``foldl`` and ``foldr`` (for left
and right, described shortly). It takes a list, and calls a function
for each item in the list, along with a accumulator, or state. The
function can operate on the item and state, producing a new state for
the next round. The fold function also takes an initial state to use
for the first item. The left and right mentioned previously determines
in which order the list is traversed. So foldl moves through the list
from left to right, i.e. takes the head off of the list for each
iteration moving towards the tail; whereas the foldr starts with the
tail and moves towards the head, i.e. from right to left. Regular
expressions can also get hairy, but they’re invaluable if you’re
working with text strings. For more information:

- http://www.troubleshooters.com/codecorn/littperl/perlreg.htm
- http://www.erlang.org/doc/man/re.html

