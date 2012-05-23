%% @author Arjan Scherpenisse <arjan@scherpenisse.net>
%% @copyright 2010 Arjan Scherpenisse
%% @doc 'twitter' filter, make a tweet from twitter look nice

%% Copyright 2010 Arjan Scherpenisse
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

-module(filter_twitter).
-export([twitter/2]).


twitter(undefined, _Context) -> undefined;
twitter(<<"<p>", _/binary>> = Input, _Context) -> Input;
twitter(Input, Context) when is_binary(Input) -> iolist_to_binary(twitter1(Input, 0, Context));
twitter(Input, _Context) -> Input.

twitter1(Input, Index, Context) when is_binary(Input) ->
    case Input of
        <<Pre:Index/binary, "http://", Post/binary>> ->  [Pre, twitter1_url("http://", Post, 0, Context)];
        <<Pre:Index/binary, "https://", Post/binary>> -> [Pre, twitter1_url("https://", Post, 0, Context)];
        <<Pre:Index/binary, $&, $#, Post/binary>> ->     [Pre, $&, $#, twitter1(Post, 0, Context)];
        <<Pre:Index/binary, $@, Post/binary>> ->         [Pre, twitter1_at(Post, 0, Context)];
        <<Pre:Index/binary, $#, Post/binary>> ->         [Pre, twitter1_hash(Post, 0, Context)];
        <<_:Index/binary, _/binary>> -> twitter1(Input, Index + 1, Context);
        _ -> Input
    end.

twitter1_url(Pre, Input, Index, Context) ->
    case Input of
        <<_:Index/binary, "&#38;", _/binary>> ->
            twitter1_url(Pre, Input, Index + 5, Context);
        <<_:Index/binary, "&amp;", _/binary>> ->
            twitter1_url(Pre, Input, Index + 5, Context);
        <<Url:Index/binary, Char, Post/binary>> ->
            case Char /= $& andalso z_utils:url_valid_char(Char) of
                true ->
                    twitter1_url(Pre, Input, Index + 1, Context);
                false ->
                    Html = ["<a href=\"", Pre, Url, "\">", Url, "</a> "],
                    [Html, twitter1(<<Char, Post/binary>>, 0, Context)]
            end;
        <<Url:Index/binary>> ->
            ["<a href=\"", Pre, Url, "\">", Url, "</a>"];
        _ ->
            Input
    end.

twitter1_at(Input, Index, Context) ->
    case Input of
        <<Name:Index/binary, Char, Post/binary>> when not(Char >= $a andalso Char =< $z
                                                          orelse
                                                          Char >= $A andalso Char =< $Z
                                                          orelse
                                                          Char >= $0 andalso Char =< $9
                                                          orelse Char =:= $_ orelse Char =:= $.
                                                         ) ->
            Html = twitter_at_url(Name),
            [Html, twitter1(<<Char, Post/binary>>, 0, Context)];
        <<Name:Index/binary>> ->
            twitter_at_url(Name);
        <<_:Index/binary, _/binary>> ->
            twitter1_at(Input, Index + 1, Context);
        _ ->
            Input
    end.

twitter_at_url(Name) ->
    ["<a href=\"http://twitter.com/", Name, "\">@", Name, "</a>"].

twitter1_hash(Input, Index, Context) ->
    case Input of
        <<Name:Index/binary, Char, Post/binary>> when not(Char >= $a andalso Char =< $z
                                                          orelse
                                                          Char >= $A andalso Char =< $Z
                                                          orelse
                                                          Char >= $0 andalso Char =< $9
                                                          orelse Char =:= $_ orelse Char =:= $.
                                                         ) ->
            Html = twitter_hash_url(Name),
            [[Html, Char], twitter1(Post, 0, Context)];
        <<Name:Index/binary>> ->
            twitter_hash_url(Name);
        <<_:Index/binary, _/binary>>  ->
            twitter1_hash(Input, Index + 1, Context);
        _ ->
            Input
    end.

twitter_hash_url(Hash) ->
    ["<a href=\"http://twitter.com/#search?q=%23", Hash, "\">#", Hash, "</a>"].

