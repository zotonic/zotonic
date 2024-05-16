%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009-2023 Marc Worrell
%% @doc Server supplying random strings and unique ids

%% Copyright 2009-2023 Marc Worrell
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

-module(z_ids).
-author("Marc Worrell <marc@worrell.nl>").
-include("zotonic.hrl").

-define(ID_LENGTH,20).
-define(OPTID_LENGTH,6).

-export([
    unique/0,
    id/0,
    id/1,
    identifier/0,
    identifier/1,
    password/0,
    random_id/2,
    optid/1,
    sign_key/1,
    sign_key_simple/1,
    number/0,
    number/1,
    rand_bytes/1
]).

-type charset() :: 'az' | 'az09' | 'azAZ09' | 'special' | '09'.

%%%--------------------------------------------------------------------------
%%% API
%%%--------------------------------------------------------------------------

-spec unique() -> binary().
%% @doc Return an unique id to be used in javascript or html.  No randomness,
%% just unique for the current runtime system instance.
%%
%% Returns a binary of unspecified length starting with the character $t followed
%% by a number of digits, example: `<<"t123456">>'.
%% Note that ids are not unique among connected nodes or after a restart.
unique() ->
    make_unique().

-spec id() -> binary().
%% @doc Equivalent to `id(?ID_LENGTH)'.
id() ->
    id(?ID_LENGTH).

-spec id(Length::integer()) -> binary().
%% @doc Generate a random key consisting of numbers and upper and lower case
%% characters.
%%
%% The version with the default Length can be used for session ids: the result
%% is sufficiently random and sufficiently unique.
id(Len) ->
    random_id('azAZ09', Len).

-spec identifier() -> binary().
%% @doc Equivalent to `identifier(?OPTID_LENGTH)'.
identifier() ->
    identifier(?OPTID_LENGTH).

-spec identifier(Length::integer()) -> binary().
%% @doc Get a random identifier of a certain length, consisting of
%% lower case characters only.
identifier(Len) ->
    random_id('az', Len).

-spec password() -> binary().
%% @doc Generate a password that matches most criteria of complexity and
%% is still (kind of) readable.
password() ->
    iolist_to_binary([
        id(5), $-, id(5), $-, id(5), $-, id(4), random_id('special', 2)
    ]).

-spec random_id(charset(), Length::integer()) -> binary().
%% @doc Get a random identifier of the specified length, consisting of
%% characters from the specified set:
%% - 'az'      ```[a-z]'''       : lower case characters only.
%% - 'az09'    ```[a-z0-9]'''    : lower case characters and numbers only.
%% - 'azAZ09'  ```[a-zA-Z0-9]''' : lower and upper case characters and numbers.
%% - '09'      ```[0-9]'''       : numbers only.
%% - 'special' ```[!#$%&()*+/;:<>=?@]''' : special characters for passwords.
random_id('az', Len) ->
    make_lower_id(Len);
random_id('az09', Len) ->
    make_no_upper_id(Len);
random_id('azAZ09', Len) ->
    make_any_char_id(Len);
random_id('special', Len) ->
    make_special_char_id(<<"!#$%&()*+/;:<>=?@">>, Len);
random_id('09', Len) ->
    make_special_char_id(<<"0123456789">>, Len).

-spec optid(undefined | false | binary()) -> binary().
%% @doc Generate an identifier if the identifier was not defined.
optid(undefined) ->
    identifier(?OPTID_LENGTH);
optid(false) ->
    identifier(?OPTID_LENGTH);
optid(Id) ->
    Id.

-spec sign_key(Context::term()) -> binary().
%% @doc Get the key for signing requests stored in the user agent.
sign_key(Context) ->
    case m_config:get_value(site, sign_key, Context) of
        undefined ->
            Key = random_id('azAZ09', 50),
            m_config:set_value(site, sign_key, Key, Context),
            Key;
        <<>> ->
            application_key(sign_key);
        SignKey ->
            SignKey
    end.

-spec sign_key_simple(Context::term()) -> binary().
%% @doc Get the key for less secure signing of data (without nonce).
sign_key_simple(Context) ->
    case m_config:get_value(site, sign_key_simple, Context) of
        undefined ->
            Key = random_id('azAZ09', 21),
            m_config:set_value(site, sign_key_simple, Key, Context),
            Key;
        <<>> ->
            application_key(sign_key_simple);
        SignKey ->
            SignKey
    end.

-spec application_key(atom()) -> binary().
%% @doc Set/get a default sign key for the zotonic core functions.
%% Returns a binary of 50 random numbers and upper and lower case
%% characters.
application_key(Name) when is_atom(Name) ->
    case application:get_env(zotonic_core, Name) of
        undefined ->
            Key = random_id('azAZ09', 50),
            application:set_env(zotonic_core, Name, Key),
            Key;
        {ok, Key} ->
            Key
    end.

-spec number() -> pos_integer().
%% @doc Equivalent to `number(1000000000)'.
number() ->
    number(1000000000).

-spec number(Max::pos_integer()) -> pos_integer().
%% @doc Return a random integer less than or equal to Max. Max defaults to a
%% large number smaller than MaxInt.
number(Max) ->
    make_number(Max).


%%%--------------------------------------------------------------------------
%%% Internal functions
%%%--------------------------------------------------------------------------

-spec make_unique() -> binary().
%% @doc Create an unique temporary id, safe to use in html and javascript.
%% Returns a binary that starts with t followed by a number of digits.
%% The ids will be reused after a restart of the Erlang VM.
make_unique() ->
    Unique = integer_to_binary(erlang:unique_integer([positive])),
    <<"t", Unique/binary>>.

make_number(Max) ->
    rand:uniform(Max).

-spec make_any_char_id(Length::integer()) -> binary().
%% @doc Generate a random key consisting of numbers and upper and lower case
%% characters.
make_any_char_id(Len) ->
    << << case N of
              C when C < 26 -> C  + $a;
              C when C < 52 -> C - 26 + $A;
              C -> C - 52 + $0
          end >>
      || N <- random_list(62, Len)
    >>.

-spec make_special_char_id(Chars::binary(), Length::integer()) -> binary().
%% @doc Generate a random key consisting of bytes from the given characters.
make_special_char_id(Chars, Len) ->
    << << (binary:at(Chars, N)) >> || N <- random_list(size(Chars), Len) >>.

-spec make_lower_id(Length::integer()) -> binary().
%% @doc Generate a random identifier, only lower case letters
make_lower_id(Len) ->
    << << (N + $a) >> || N <- random_list(26, Len) >>.

-spec make_no_upper_id(Length::integer()) -> binary().
%% @doc Generate a random identifier, only lower case letters and
%% numbers
make_no_upper_id(Len) ->
    << << case N of
              C when C < 26 -> C  + $a;
              C -> C - 26 + $0
          end >>
      || N <- random_list(32, Len)
    >>.

random_list(Radix, Length) ->
    N = (radix_bits(Radix) * Length + 7) div 8,
    Val = bin2int(rand_bytes(N)),
    int2list(Val, Radix, Length, []).

int2list(_, _, 0, Acc) ->
    Acc;
int2list(Val, Radix, Length, Acc) ->
    int2list(Val div Radix, Radix, Length-1, [ Val rem Radix | Acc]).

bin2int(Bin) ->
    lists:foldl(fun(N, Acc) -> Acc * 256 + N end, 0, binary_to_list(Bin)).

-spec radix_bits(1..64) -> 4 | 5 | 6.
% radix_bits(N) when N =< 16 -> 4;
radix_bits(N) when N =< 26 -> 5;
radix_bits(_N) -> 6.

%% @doc Return N random bytes. This falls back to the pseudo random version of rand_uniform
%% if strong_rand_bytes fails.
-spec rand_bytes(integer()) -> binary().
rand_bytes(N) when N > 0 ->
    try
        crypto:strong_rand_bytes(N)
    catch
        error:low_entropy ->
            ?LOG_NOTICE("Crypto is low on entropy"),
            list_to_binary([ rand:uniform(256) || _X <- lists:seq(1, N) ])
    end.
