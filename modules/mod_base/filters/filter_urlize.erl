%% @author Maas-Maarten Zeeman <mmzeeman@xs4all.nl>
%% @copyright 2010 Maas-Maarten Zeeman
%% @doc 'urlize' filter, find urls and make them clickable.

%% Copyright 2010 Maas-Maarten Zeeman
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%% 
%%     http://www.apache.org/licenses/LICENSE-2.0
%% 
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.

-module(filter_urlize).

-export([urlize/2]).

urlize(undefined, _Context) ->
    undefined;
urlize(Input, _Context) when is_list(Input) or is_binary(Input) ->
    do_urlize(Input);
urlize(Input, _Context) ->
    Input.

%% Maybe the do_urlize should go to z_html. It could be handy in other places too

%% @doc Given a text, convert any URL into clickable links
%% @spec do_urlize(string()) -> string()
do_urlize(Text) ->
    [urlize_word(Word) || Word <- re:split(Text, "(\s+)")].

%% @doc Given a word, convert it into a clickable link if it looks like one.
%% @spec urlize_word(String) -> String
urlize_word(Word) ->
    case smells_like_something_interesting(Word) of
        true -> urlize_word1(Word);
        false -> z_html:escape(Word)
    end.

%%
urlize_word1(Word) ->
    %% strip off leading and trailing braces, dots, etc
    Pat =
"^(?P<Head>(?:\\(|<|&lt)*)(?P<Middle>.*?)(?P<Tail>(?:\\)|\\.|\\,|\\>|\\n|&gt)*)$",
    case re:run(Word, Pat, [{capture, all_but_first, binary}]) of
        {match, [Lead, Middle, Tail]} ->
            case make_url(Middle) of
               undefined -> z_html:escape(Word); % do nothing, just return the word
               Url -> 
                   Link = z_tags:render_tag(<<"a">>, [{href, Url}],
z_html:escape(Middle)),
                   [z_html:escape(Lead), Link, z_html:escape(Tail)]
            end;
        nomatch -> z_html:escape(Word)
    end.

%% @doc Given a word, convert it to a url if it looks like one. When
%% it doesn't look like a url, return undefined 
%%
%% @spec(string()) ->string() | undefined
make_url(Word) ->
    case smells_like_a_uri(Word) of
        true -> safe_escape_url(Word);
        false -> make_url1(Word)
    end.
make_url1(Word) ->
    case smells_like_a_domain_name(Word) of
        true -> iolist_to_binary(["http://", safe_escape_url(Word)]); 
        false-> make_url2(Word)
    end.
make_url2(Word) ->
    case smells_like_an_email_address(Word) of
        true -> iolist_to_binary(["mailto://", safe_escape_url(Word)]); 
        false -> undefined
end.

safe_escape_url(Url) ->
    z_html:escape(z_html:noscript(Url)).

smells_like_something_interesting(Word) ->
    case re:run(Word, "@|:|\\.") of
        nomatch -> false;
        {match, _Match} -> true
    end.    

smells_like_a_uri(Word) ->
    z_string:starts_with(<<"http://">>, Word) orelse
(z_string:starts_with(<<"https://">>, Word)).

smells_like_a_domain_name(Word) ->
    Tlds = [<<".com">>, <<".gov">>, <<".org">>, <<".net">>, <<".nl">>, <<".me">>,
<<".io">>],
    z_string:starts_with(<<"www.">>, Word) orelse 
                                    (not z_string:contains(<<"@">>, Word) and (ends_with_one_of(Tlds, Word))).

smells_like_an_email_address(Word) ->
    SimpleEmailPat = "^\\S+@[a-zA-Z0-9._-]+\\.[a-zA-Z0-9._-]+$",
    case (z_string:contains(<<"@">>, Word) and not z_string:contains(<<":">>, Word)) of
        true ->
            case re:run(Word, SimpleEmailPat) of
                nomatch -> false;
                {match, _} -> true
            end;
        false ->false
    end.
                     
ends_with_one_of([], _String) ->
    false;
ends_with_one_of([H|T], String) ->
    case z_string:ends_with(H, String) of
        true -> true;
        false -> ends_with_one_of(T, String)
    end.
