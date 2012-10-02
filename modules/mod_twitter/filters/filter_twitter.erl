%% @author Arjan Scherpenisse <arjan@scherpenisse.net>
%% @copyright 2010-2012 Arjan Scherpenisse
%% @doc 'twitter' filter, make a tweet from twitter look nice

%% Copyright 2010-2012 Arjan Scherpenisse
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
-export([
    twitter/2,
    twitter/3
]).

%% Number of characters for truncating an url
-define(URL_TRUNCATE, 50).


twitter(undefined, _Context) -> undefined;
twitter(<<"<p>", _/binary>> = Input, _Context) -> Input;
twitter(Input, Context) when is_list(Input) ->
    twitter(z_convert:to_binary(Input), Context);
twitter(Input, Context) when is_binary(Input) -> iolist_to_binary(twitter1(Input, 0, [], Context));
twitter(Input, _Context) -> Input.

%% @doc filter with options, only option for now: url_location (follows url shorteners)
twitter(Input, Options, Context) ->
    twitter1(Input, 0, Options, Context).
    

twitter1(Input, Index, Opts, Context) when is_binary(Input) ->
    case Input of
        <<Pre:Index/binary, "http://", Post/binary>> ->  [Pre, twitter1_url(<<"http://">>, Post, 0, Opts, Context)];
        <<Pre:Index/binary, "https://", Post/binary>> -> [Pre, twitter1_url(<<"https://">>, Post, 0, Opts, Context)];
        <<Pre:Index/binary, "ftp://", Post/binary>> ->   [Pre, twitter1_url(<<"ftp://">>, Post, 0, Opts, Context)];
        <<Pre:Index/binary, "ftps://", Post/binary>> ->  [Pre, twitter1_url(<<"ftps://">>, Post, 0, Opts, Context)];
        <<Pre:Index/binary, "mailto:", Post/binary>> ->  [Pre, twitter1_url(<<"mailto:">>, Post, 0, Opts, Context)];
        <<Pre:Index/binary, $&, $#, Post/binary>> ->     [Pre, $&, $#, twitter1(Post, 0, Opts, Context)];
        <<Pre:Index/binary, $@, Post/binary>> ->         [Pre, twitter1_at(Post, 0, Opts, Context)];
        <<Pre:Index/binary, $#, Post/binary>> ->         [Pre, twitter1_hash(Post, 0, Opts, Context)];
        <<_:Index/binary, _/binary>> -> twitter1(Input, Index + 1, Opts, Context);
        _ -> Input
    end.

twitter1_url(Pre, Input, Index, Opts, Context) ->
    case Input of
        <<_:Index/binary, "&#38;", _/binary>> ->
            twitter1_url(Pre, Input, Index + 5, Opts, Context);
        <<_:Index/binary, "&amp;", _/binary>> ->
            twitter1_url(Pre, Input, Index + 5, Opts, Context);
        <<Url:Index/binary, Char, Post/binary>> ->
            case Char /= $& andalso z_url:url_valid_char(Char) of
                true ->  twitter1_url(Pre, Input, Index + 1, Opts, Context);
                false -> twitter1_url_anchor(Pre, Url, <<Char, Post/binary>>, Opts, Context)
            end;
        <<Url:Index/binary>> ->
            twitter1_url_anchor(Pre, Url, <<>>, Opts, Context);
        _ ->
            Input
    end.

    twitter1_url_anchor(Pre, <<>>, Post, Opts, Context) ->
        [Pre, twitter1(Post, 0, Opts, Context)];
    twitter1_url_anchor(Pre, Url, Post, Opts, Context) ->
        Length1 = size(Url) - 1,
        <<Url1:Length1/binary,LastChar>> = Url,
        Html = case is_url_truncatable(LastChar) of
                  true -> [ twitter1_url_html(Pre, Url1, Opts), LastChar];
                  false -> twitter1_url_html(Pre, Url, Opts)
               end,
        [Html, twitter1(Post, 0, Opts, Context)].


    % Create the html link, follow the url to remove any url shortener.
    twitter1_url_html(Pre, Url, Opts) ->
        case proplists:get_value(url_location, Opts, false) of
            true -> 
                Url2 = z_url:location(<<Pre/binary,Url/binary>>),
                Text = z_string:truncate(z_url:remove_protocol(Url2), ?URL_TRUNCATE),
                ["<a href=\"", Url2, "\">", Text, "</a>"];
            false ->
                ["<a href=\"", Pre, Url, "\">", Url, "</a>"]
        end.


    is_url_truncatable($.) -> true;
    is_url_truncatable($;) -> true;
    is_url_truncatable($#) -> true;
    is_url_truncatable($,) -> true;
    is_url_truncatable($') -> true;
    is_url_truncatable($") -> true;
    is_url_truncatable($?) -> true;
    is_url_truncatable($!) -> true;
    is_url_truncatable($/) -> true;
    is_url_truncatable($+) -> true;
    is_url_truncatable($%) -> true;
    is_url_truncatable(_) -> false.


twitter1_at(Input, Index, Opts, Context) ->
    case Input of
        <<Name:Index/binary, Char, Post/binary>> when not(Char >= $a andalso Char =< $z
                                                          orelse
                                                          Char >= $A andalso Char =< $Z
                                                          orelse
                                                          Char >= $0 andalso Char =< $9
                                                          orelse Char =:= $_ orelse Char =:= $.
                                                         ) ->
            Html = twitter_at_url(Name),
            [Html, twitter1(<<Char, Post/binary>>, 0, Opts, Context)];
        <<Name:Index/binary>> ->
            twitter_at_url(Name);
        <<_:Index/binary, _/binary>> ->
            twitter1_at(Input, Index + 1, Opts, Context);
        _ ->
            Input
    end.

twitter_at_url(Name) ->
    ["<a href=\"http://twitter.com/", Name, "\">@", Name, "</a>"].

twitter1_hash(Input, Index, Opts, Context) ->
    case Input of
        <<Name:Index/binary, Char, Post/binary>> when not(Char >= $a andalso Char =< $z
                                                          orelse
                                                          Char >= $A andalso Char =< $Z
                                                          orelse
                                                          Char >= $0 andalso Char =< $9
                                                          orelse Char =:= $_ orelse Char =:= $.
                                                         ) ->
            Html = twitter_hash_url(Name),
            [[Html, Char], twitter1(Post, 0, Opts, Context)];
        <<Name:Index/binary>> ->
            twitter_hash_url(Name);
        <<_:Index/binary, _/binary>>  ->
            twitter1_hash(Input, Index + 1, Opts, Context);
        _ ->
            Input
    end.

twitter_hash_url(Hash) ->
    ["<a href=\"http://twitter.com/#search?q=%23", Hash, "\">#", Hash, "</a>"].

