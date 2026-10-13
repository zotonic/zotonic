%% @copyright 2026 Marc Worrell

%% Copyright 2026 Marc Worrell
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

-module(m_media_identification_tests).
-include_lib("eunit/include/eunit.hrl").
-include_lib("zotonic_core/include/zotonic.hrl").
-export([identification_failure/0]).

%% Isolate the shared identification mock from the integration test site.
identification_failure_test_() ->
    {timeout, 30, fun() ->
        {ok, Peer, _} = peer:start_link(#{connection => standard_io,
            args => ["-pa" | code:get_path()]}),
        try
            ok = peer:call(Peer, ?MODULE, identification_failure, [], 30000)
        after
            peer:stop(Peer)
        end
    end}.

identification_failure() ->
    ok = meck:new(z_media_identify, [passthrough, no_link]),
    try
        lists:foreach(fun(Result) ->
            meck:expect(z_media_identify, identify_file, fun(_, _, _) -> Result end),
            %% No MIME reaches the ACL callbacks, for inserts or replacements.
            ?assertEqual({error, identify}, m_media:insert_file("unidentified.png", #context{})),
            ?assertEqual({error, identify}, m_media:replace_file(
                "unidentified.png", 123, #{}, #{}, [], #context{}))
        end, [{error, identify}, {ok, #{}}, {ok, #{<<"mime">> => undefined}}])
    after
        meck:unload(z_media_identify)
    end.
