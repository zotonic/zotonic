%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2026 Marc Worrell
%% @doc Translate resource trees in unique sidejobs and report their progress.
%% @end

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

-module(translation_tree).
-author("Marc Worrell <marc@worrell.nl>").
-moduledoc("
Unique, site-local tree translation sidejobs. A process-owned registration prevents
duplicate jobs for the same tree. Editing is blocked only in the user interface.
Progress is published on `model/rsc/event/<root>/translation_tree` and can also be
polled after reconnecting. Completed results are retained for one hour.
").

-export([start/3, run/5, status/2, languages/2]).

-include_lib("zotonic_core/include/zotonic.hrl").

%% @doc Start a sidejob and wait only for its startup acknowledgement or failure.
%% Root is a resource ID or a user-bound bulk selection token. The model must
%% validate the operation and authorize the tree or selection first.
-spec start(Root, Operation, Context) -> {ok, map()} | {error, term()}
    when
        Root :: m_rsc:resource_id() | binary(),
        Operation :: tuple(),
        Context :: z:context().
start(Root, Operation, Context) ->
    Ref = make_ref(),
    case z_sidejob:start(?MODULE, run, [self(), Ref, Root, Operation], Context) of
        {ok, Pid} ->
            Monitor = monitor(process, Pid),
            receive
                {Ref, Result} -> demonitor(Monitor, [flush]), Result;
                {'DOWN', Monitor, process, Pid, _} -> {error, failed}
            end;
        Error -> Error
    end.

%% @doc Register the unique tree job, acknowledge startup, and process pages with progress reports.
-spec run(Caller, Ref, Root, Operation, Context) -> ok
    when
        Caller :: pid(),
        Ref :: reference(),
        Root :: m_rsc:resource_id() | binary(),
        Operation :: tuple(),
        Context :: z:context().
run(Caller, Ref, Root, Operation, Context) ->
    case z_proc:register({?MODULE, Root}, self(), Context) of
        ok ->
            State = #{
                root => Root,
                job => z_ids:id(),
                state => running,
                total => 0,
                done => 0,
                skipped => 0,
                skipped_ids => [],
                failed => 0,
                operation => operation_name(Operation)
            },
            %% Acknowledge startup before traversing or translating the tree. The caller
            %% never waits for translation results, and does not impose a job deadline.
            gproc:set_value(root_key(Root, Context), State),
            Caller ! {Ref, {ok, State}},
            Final = try
                report(State, Context),
                case m_translation_tree:ids(Root, Context) of
                    {ok, Ids} ->
                        State1 = State#{ total := length(Ids) },
                        report(State1, Context),
                        process(Ids, Operation, State1, Context);
                    {error, Reason} ->
                        ?LOG_WARNING(#{
                            in => zotonic_mod_translation,
                            text => <<"Could not read translation tree">>,
                            root => Root,
                            result => error,
                            reason => Reason
                        }),
                        State#{ state => failed }
                end
            catch Class:Error:Stack ->
                ?LOG_ERROR(#{
                    in => zotonic_mod_translation,
                    text => <<"Tree translation failed">>,
                    result => error,
                    reason => Error,
                    class => Class,
                    stack => Stack,
                    root => Root
                }),
                State#{ state => failed }
            end,
            report(Final, Context),
            z_proc:unregister({?MODULE, Root}, Context);
        {error, duplicate} ->
            Caller ! {Ref, {error, busy}}
    end,
    ok.

%% @doc Return live or cached progress, marking an interrupted worker as failed.
-spec status(Id, Context) -> map() when Id :: m_rsc:resource_id() | binary(), Context :: z:context().
status(Root, Context) ->
    Live = try gproc:lookup_value(root_key(Root, Context))
        catch error:badarg -> undefined end,
    State = case Live of
        S when is_map(S) ->
            S;
        _ ->
            case z_depcache:get({?MODULE, Root}, Context) of
                {ok, Cached} -> Cached;
                undefined -> #{ state => idle, root => Root }
            end
    end,
    case State of
        #{state := running} ->
            %% Registry cleanup is asynchronous after a killed worker.
            Pid = z_proc:whereis({?MODULE, Root}, Context),
            case is_pid(Pid) andalso is_process_alive(Pid) of
                true -> State;
                false -> State#{ state => failed }
            end;
        _ -> State
    end.

%% @doc Build the site-local process registry key for a tree job and its live progress.
root_key(Id, Context) -> {n, l, {{?MODULE, Id}, z_context:site(Context)}}.

%% @doc Store live progress, cache it for one hour, and publish it to the tree edit topic.
report(#{root := Root} = State, Context) ->
    gproc:set_value(root_key(Root, Context), State),
    z_depcache:set({?MODULE, Root}, State, 3600, Context),
    case Root of
        <<"bulk-", _/binary>> -> ok; % Bulk selections use owner-authorized status polling.
        _ -> z_mqtt:publish([<<"model">>, <<"rsc">>, <<"event">>, integer_to_binary(Root), <<"translation_tree">>], State, Context)
    end.

%% @doc Apply the operation to editable pages and report completed, skipped, and failed counts.
process([], _Operation, State, _Context) -> State#{state => complete};
process([Id | Rest], Operation, #{done := Done} = State, Context) ->
    Result = try
        %% Permissions may have changed since the dialog was opened or the job started.
        %% Check before collecting source text or calling a translation service.
        case z_acl:rsc_editable(Id, Context) of
            true -> apply_operation(Id, Operation, Context);
            false -> skipped
        end
        catch Class:Reason ->
            ?LOG_ERROR(#{
                in => zotonic_mod_translation,
                text => <<"Tree page translation failed">>,
                rsc_id => Id,
                class => Class,
                reason => Reason
            }),
            {error, Reason}
        end,
    State1 = case Result of
        ok -> State;
        skipped -> State#{
            skipped := maps:get(skipped, State) + 1,
            skipped_ids := [Id | maps:get(skipped_ids, State)]
        };
        {error, Error} ->
            ?LOG_WARNING(#{
                in => zotonic_mod_translation,
                text => <<"Could not translate tree page">>,
                result => error,
                reason => Error,
                rsc_id => Id
            }),
            State#{failed := maps:get(failed, State) + 1}
    end,
    State2 = State1#{done := Done + 1},
    report(State2, Context),
    process(Rest, Operation, State2, Context).

%% @doc Apply one page operation, preserving its last language and skipping missing source languages.
apply_operation(Id, {remove, Lang}, Context) ->
    case languages(Id, Context) of
        [_] -> skipped;
        [] -> skipped;
        Langs ->
            case lists:member(Lang, Langs) of
                true -> m_translation:remove_translation(Id, Lang, Context);
                false -> skipped
            end
    end;
apply_operation(Id, {<<"empty">>, _From, To, _Overwrite}, Context) ->
    ensure_language(Id, To, Context);
apply_operation(Id, {Method, From, To, Overwrite}, Context) ->
    SourceLanguages = m_rsc:p(Id, language, Context),
    case is_list(SourceLanguages) andalso lists:member(From, SourceLanguages) of
        false -> skipped;
        true ->
            case Method of
                <<"translate">> ->
                    case translation_translate_rsc:add_translation(Id, From, To, Overwrite, true, Context) of
                        ok -> ensure_language(Id, To, Context);
                        Error -> Error
                    end;
                <<"copy">> ->
                    translation_translate_rsc:copy_translation(Id, From, To, Overwrite, Context)
            end
    end.

%% @doc Enable the destination language if it is not already present on the resource.
ensure_language(Id, To, Context) ->
    Langs = languages(Id, Context),
    case lists:member(To, Langs) of
        true -> ok;
        false ->
            case m_rsc:update(Id, #{<<"language">> => lists:usort([To | Langs])}, Context) of
                {ok, _} -> ok;
                Error -> Error
            end
    end.

%% @doc Return unique resource languages, falling back to the site default for an empty list.
-spec languages(Id, Context) -> [atom()] when Id :: m_rsc:resource_id(), Context :: z:context().
languages(Id, Context) ->
    case m_rsc:p(Id, language, Context) of
        L when is_list(L), L =/= [] -> lists:usort(L);
        _ -> [z_language:default_language(Context)]
    end.

%% @doc Extract the operation name used in progress messages.
operation_name({remove, _}) -> remove;
operation_name({Method, _, _, _}) -> Method.
