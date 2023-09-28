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

-spec check(Context) -> {ok, XRef} when
    Context :: z:context(),
    XRef :: #{
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
    ExtendErrors = lists:foldl(
        fun(Template, Acc) ->
            #module_index{ filepath = Filename } = Template,
            case z_template:extends(Template, #{}, Context) of
                {ok, undefined} ->
                    Acc;
                {ok, Extends} when is_binary(Extends) ->
                    case z_module_indexer:find(template, Extends, Context) of
                        {ok, _} ->
                            Acc;
                        {error, enoent} ->
                            Acc#{
                                Filename => Extends
                            }
                    end;
                {ok, overrules} ->
                    case find_overrules(Template, Context) of
                        ok ->
                            Acc;
                        error ->
                            Acc#{
                                Filename => overrules
                            }
                    end;
                {error, _} ->
                    Acc
            end
        end,
        #{},
        All),
    {ok, #{
        missing => drop_empty(Miss),
        optional => drop_empty(Opts),
        errors => drop_empty(Errors),
        extend_errors => drop_empty(ExtendErrors)
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

find_overrules(#module_index{ key = Key, filepath = Filename }, Context) ->
    #module_index_key{ name = TplName } = Key,
    Tpl = {overrules, TplName, Filename},
    case z_template_compiler_runtime:map_template(Tpl, #{}, Context) of
        {ok, _File} ->
            ok;
        {error, _} ->
            error
    end.

drop_empty(Errs) ->
    BuildDir = <<(build_dir())/binary, "/">>,
    maps:fold(
        fun
            (_K, [], Acc) ->
                Acc;
            (K, Vs, Acc) ->
                K1 = binary:replace(K, BuildDir, <<>>),
                Acc#{ K1 => Vs }
        end,
        #{},
        Errs).

build_dir() ->
    unicode:characters_to_binary(z_path:build_lib_dir()).

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
