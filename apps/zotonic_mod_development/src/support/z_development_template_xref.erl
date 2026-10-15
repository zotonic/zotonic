%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2023 Marc Worrell
%% @doc Check syntax and xref templates. Parses all templates and checks
%% if extends and include references available templates.
%% @end

%% Copyright 2023 Marc Worrell
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

-module(z_development_template_xref).

-export([
    check/1
    ]).

-include_lib("zotonic_core/include/zotonic.hrl").

%% @doc Collect template errors and reference inventories with source positions and target choices.
-spec check(Context) -> {ok, XRef} when
    Context :: z:context(),
    XRef :: #{
        includes := map(),
        inheritance := map(),
        missing := map(),
        optional := map(),
        errors := map(),
        extend_errors := map()
    }.
check(Context) ->
    All = z_module_indexer:all(template, Context),
    {Includes, Errors} = lists:foldl(
        fun(Template, {Acc, ErrAcc}) ->
            #module_index{ filepath = Filename } = Template,
            case z_template:includes(Template, #{}, Context) of
                {ok, Includes} ->
                    Acc1 = Acc#{
                        Filename => Includes
                    },
                    {Acc1, ErrAcc};
                {error, Reason} ->
                    ErrAcc1 = ErrAcc#{
                        Filename => reason(Reason)
                    },
                    {Acc, ErrAcc1}
            end
        end,
        {#{}, #{}},
        All),
    % Now check if all included files exist
    {Miss, Opts} = maps:fold(
        fun(Filename, Tpls, {MissAcc, OptAcc} = Acc) ->
            case missing_includes(Tpls, Context) of
                {ok, {[], []}} ->
                    Acc;
                {ok, {Missing, Opt}} ->
                    MissAcc1 = MissAcc#{
                        Filename => Missing
                    },
                    OptAcc1 = OptAcc#{
                        Filename => Opt
                    },
                    {MissAcc1, OptAcc1}
            end
        end,
        {#{}, #{}},
        Includes),
    Inheritance = lists:foldl(
        fun(Template, Acc) ->
            #module_index{ filepath = Filename } = Template,
            case z_template:extends(Template, #{}, Context) of
                {ok, undefined} ->
                    Acc;
                {ok, Extends} ->
                    #module_index{key = #module_index_key{name = Name}} = Template,
                    Option = case Extends of
                        overrules -> {overrules, Name, Filename};
                        _ -> Extends
                    end,
                    Matches = z_development_template:matching_templates(Option, Context),
                    Info = (extends_info(Filename, Extends))#{
                        option => Option, is_missing => Matches =:= []
                    },
                    Acc#{Filename => Info};
                {error, _} ->
                    Acc
            end
        end,
        #{},
        All),
    {ok, #{
        includes => maps:map(fun(_File, Refs) -> [include_info(R, Context) || R <- Refs] end, drop_empty(Includes)),
        inheritance => Inheritance,
        missing => maps:map(fun(_File, Refs) -> [include_info(R, Context) || R <- Refs] end, drop_empty(Miss)),
        optional => maps:map(fun(_File, Refs) -> [include_info(R, Context) || R <- Refs] end, drop_empty(Opts)),
        errors => drop_empty(Errors),
        extend_errors => maps:filter(fun(_, Info) -> maps:get(is_missing, Info) end, Inheritance)
    }}.

reason(Reason) when is_map(Reason) ->
    Reason;
reason(Reason) when is_atom(Reason) ->
    #{
        text => atom_to_binary(Reason, utf8)
    };
reason(Reason) when is_binary(Reason) ->
    #{
        text => Reason
    };
reason(Reason) when is_list(Reason) ->
    #{
        text => unicode:characters_to_binary(Reason, utf8)
    };
reason(Reason) ->
    S = io_lib:format("~tp", [ Reason ]),
    #{
        text => unicode:characters_to_binary(S, utf8)
    }.

%% @doc Preserve category-aware lookup semantics for include choice dialogs.
include_info(#{template := Name} = Info, Context) ->
    Option = case maps:get(is_catinclude, Info, false) of
        true -> {cat, Name};
        false -> Name
    end,
    Info#{option => Option, has_matches => z_development_template:matching_templates(Option, Context) =/= []}.

%% @doc Preserve indexed source paths for editor launches while dropping empty results.
drop_empty(Errs) ->
    maps:filter(fun(_File, Entries) -> Entries =/= [] end, Errs).

%% @doc Locate an inheritance reference using scanner positions, including leading comments.
extends_info(Filename, Template) ->
    Position = case file:read_file(Filename) of
        {ok, Data} ->
            case template_compiler_scanner:scan(Filename, Data) of
                {ok, Tokens} ->
                    case [{Line, Column} || {Tag, {_, Line, Column}, _} <- Tokens,
                            Tag =:= extends_keyword orelse Tag =:= overrules_keyword] of
                        [First | _] -> First;
                        [] -> {1, 1}
                    end;
                _ -> {1, 1}
            end;
        _ -> {1, 1}
    end,
    {Line, Column} = Position,
    #{template => Template, line => Line, column => Column}.

missing_includes(Includes, Context) ->
    Errs = lists:filter(
        fun
            (#{ template := Template, method := normal }) ->
                case z_module_indexer:find(template, Template, Context) of
                    {ok, _} -> false;
                    {error, enoent} -> true
                end;
            (_) ->
                false
        end,
        Includes),
    Warns = lists:filter(
        fun
            (#{ template := Template, method := optional }) ->
                case z_module_indexer:find(template, Template, Context) of
                    {ok, _} -> false;
                    {error, enoent} -> true
                end;
            (_) ->
                false
        end,
        Includes),
    {ok, {Errs, Warns}}.
