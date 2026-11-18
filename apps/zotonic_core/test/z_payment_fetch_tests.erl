%% @copyright 2026 Marc Worrell
%% @doc Payment API dependencies must honor the no-redirect option on OTP 25.
%% @end

%% Copyright 2014-2025 Marc Worrell
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

-module(z_payment_fetch_tests).
-include_lib("eunit/include/eunit.hrl").

redirect_policy_test() ->
    {ok, _} = application:ensure_all_started(inets),
    {ok, _} = application:ensure_all_started(tls_certificate_check),
    lists:foreach(fun({Code, Follow}) ->
        with_listener(fun(Listen, Url) ->
            Parent = self(),
            Server = spawn_monitor(fun() ->
                {ok, Socket} = gen_tcp:accept(Listen, 2000),
                _ = headers(Socket, <<>>),
                ok = gen_tcp:send(Socket, ["HTTP/1.1 ", integer_to_list(Code), <<" Found\r\nLocation: /target\r\nContent-Length: 0\r\nConnection: close\r\n\r\n">>]),
                gen_tcp:close(Socket),
                case Follow of
                    true ->
                        {ok, Target} = gen_tcp:accept(Listen, 2000),
                        <<"GET /target ", _/binary>> = headers(Target, <<>>),
                        gen_tcp:send(Target, <<"HTTP/1.1 200 OK\r\nContent-Length: 2\r\nConnection: close\r\n\r\nok">>),
                        gen_tcp:close(Target);
                    false ->
                        ?assertEqual({error, timeout}, gen_tcp:accept(Listen, 200))
                end,
                Parent ! checked
            end),
            Options = case Follow of true -> []; false -> [{autoredirect, false}] end,
            Reply = z_url_fetch:fetch(Url, [{use_range, false}, {authorization, <<"Bearer test">>} | Options]),
            case Follow of
                true -> ?assertMatch({ok, {_, _, 2, <<"ok">>}}, Reply);
                false -> ?assertMatch({error, {Code, _, _, _, <<>>}}, Reply)
            end,
            await(Server)
        end)
    end, [{Code, Follow} || Code <- [301, 302, 303, 307, 308], Follow <- [true, false]]).

cancel_profile_test() ->
    {ok, _} = application:ensure_all_started(inets),
    {ok, _} = application:ensure_all_started(tls_certificate_check),
    with_listener(fun(Listen, Url) ->
        Parent = self(),
        Server = spawn_monitor(fun() ->
            {ok, Socket} = gen_tcp:accept(Listen, 2000),
            _ = headers(Socket, <<>>),
            gen_tcp:send(Socket, <<"HTTP/1.1 200 OK\r\nContent-Length: 20\r\n\r\nx">>),
            ?assertEqual({error, closed}, gen_tcp:recv(Socket, 0, 2000)),
            Parent ! checked
        end),
        ?assertEqual({error, timeout}, z_url_fetch:fetch(Url, [{timeout, 100}, {use_range, false}])),
        await(Server)
    end).

with_listener(Test) ->
    {ok, Listen} = gen_tcp:listen(0, [binary, {active, false}, {reuseaddr, true}]),
    {ok, {_, Port}} = inet:sockname(Listen),
    try Test(Listen, "http://localhost:" ++ integer_to_list(Port) ++ "/start")
    after gen_tcp:close(Listen) end.

headers(Socket, Buffer) ->
    case binary:match(Buffer, <<"\r\n\r\n">>) of
        nomatch ->
            {ok, Data} = gen_tcp:recv(Socket, 0, 2000),
            headers(Socket, <<Buffer/binary, Data/binary>>);
        _ -> Buffer
    end.

await({Pid, Ref}) ->
    receive
        {'DOWN', Ref, process, Pid, normal} ->
            receive checked -> ok after 0 -> error(missing_server_check) end;
        {'DOWN', Ref, process, Pid, Reason} -> error({server_failed, Reason})
    after 3000 -> exit(Pid, kill), error(server_timeout)
    end.
