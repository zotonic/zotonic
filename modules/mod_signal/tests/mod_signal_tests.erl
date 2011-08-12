%% @author Maas-Maarten Zeeman <mmzeeman@xs4all.nl>
%% @copyright 2010 Maas-Maarten Zeeman
%% Date: 2010-12-03
%% @doc Tests for mod_signal.

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

-module(mod_signal_tests).

-include_lib("eunit/include/eunit.hrl").

-include_lib("zotonic.hrl").

key_test() ->
    ?assertEqual({spam, []}, mod_signal:key({spam, []})),
    ?assertEqual({spam, [{a, "a"}]}, mod_signal:key({spam, [{a, "a"}]})),
    ?assertEqual({spam, [{a, "a"}, {b, "b"}]}, mod_signal:key({spam, [{b, "b"}, {a, "a"}]})),
    ?assertEqual({spam, [{a, "a"}, {b, "b"}]}, mod_signal:key({spam, [{b, "b"}, {b, "c"}, {a, "a"}]})),

    ?assertEqual({spam, []},
                 mod_signal:key({spam, []}, [])),

    % no matching properties, no key
    ?assertEqual(undefined,
                 mod_signal:key({spam, []}, [foo])),
    ?assertEqual(undefined,
                 mod_signal:key({spam, []}, [foo, bar])),

    ?assertEqual({eggs, [{spam, "eggs"}]},
                 mod_signal:key({eggs, [{spam, "eggs"}]}, [spam])),

    % elements are returned in the order they are given.
    ?assertEqual({eggs, [{a, "a"}, {b, "b"}]},
                 mod_signal:key({eggs, [{c, "c"}, {b, "b"}, {a, "a"}]}, [a, b])).


mod_signal_test_() ->
    {inorder, {foreach,
     fun() ->
             mod_signal:start_link([{host, testsandbox}]),
             #context{host=testsandbox}
     end,
     fun(Context) ->
             mod_signal:stop(Context)
     end,
     [fun(Context) -> [ ?_test(test_connect(Context)) ] end,
      fun(Context) -> [ ?_test(test_slots(Context)) ] end]
    }}.

test_connect(Context) ->
    ?assertEqual(0, mod_signal:item_count(Context)),
    mod_signal:connect({blaat, []}, slot1, Context),
    % counts the room the slot and its tagset takes..

    ?assertEqual(2, mod_signal:item_count(Context)),
    ?assertEqual(1, mod_signal:slot_count(Context)),

    mod_signal:connect({blaat, []}, slot2, Context),
    ?assertEqual(3, mod_signal:item_count(Context)),
    ?assertEqual(2, mod_signal:slot_count(Context)),

    mod_signal:connect({blaat, []}, slot3, Context),
    ?assertEqual(4, mod_signal:item_count(Context)),
    ?assertEqual(3, mod_signal:slot_count(Context)).

test_slots(Context) ->
    ?assertEqual(0, mod_signal:slot_count(Context)),
    ?assertEqual([], mod_signal:slots({boe, [{bla, "bla"}, {b, "boe"}]}, Context)),

    mod_signal:connect({blaat, []}, slot1, Context),
    ?assertEqual(1, mod_signal:slot_count(Context)),

    mod_signal:connect({blaat, []}, slot2, Context),
    ?assertEqual(2, mod_signal:slot_count(Context)),

    mod_signal:connect({blaat, []}, slot3, Context),
    ?assertEqual(3, mod_signal:slot_count(Context)),

    ?assertEqual([slot1, slot2, slot3], mod_signal:slots({blaat, []}, Context)),
    ?assertEqual([slot1, slot2, slot3], mod_signal:slots({blaat, [{bla, "bla"}, {b, "boe"}]}, Context)),
    ?assertEqual([], mod_signal:slots({boe, [{bla, "bla"}, {b, "boe"}]}, Context)),

    % Connect to a signal with props.
    mod_signal:connect({blaat, [{b, "b"}, {a, "a"}]}, slot4, Context),

    % don't match slot4... 
    ?assertEqual([slot1, slot2, slot3], mod_signal:slots({blaat, []}, Context)),

    % en hier alleen slot4
    ?assertEqual([slot1, slot2, slot3, slot4], lists:sort(mod_signal:slots({blaat, [{a, "a"}, {b, "b"} ]}, Context))).



    

