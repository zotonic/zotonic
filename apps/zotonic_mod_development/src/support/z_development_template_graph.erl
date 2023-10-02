%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2023 Marc Worrell
%% @doc Calculate the dependency graph between all templates.
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

-module(z_development_template_graph).

-export([
    dot/1,
    graph/1
    ]).

-include_lib("zotonic_core/include/zotonic.hrl").

-type template_node() :: #{
        id := binary(),
        template := binary(),
        module := binary()
    }.

-type template_edge() :: #{
        from := binary(),
        to := binary(),
        type => extends | overrules | include
    }.

-export_type([
    template_node/0,
    template_edge/0
    ]).

-spec dot(Context) -> {ok, DotFile} when
    Context :: z:context(),
    DotFile :: binary().
dot(Context) ->
    {ok, G} = graph(Context),
    Nodes = lists:map(
        fun(#{ module := Mod, id := Id, template := Tpl }) ->
            [
                "    ", Id,
                " [",
                    "label=\"", Tpl, "\\n", Mod, "\"",
                    " color=", color(Mod),
                    case Mod of
                        <<"mod_", _/binary>> -> <<>>;
                        _ -> " fillcolor=powderblue style=filled "
                    end,
                "];\n"
            ]
        end,
        maps:get(nodes, G, [])),
    Edges = lists:map(
        fun(#{ from := From, to := To, module := Mod } = E) ->
            [
                "    ", From, " -> ", To,
                case maps:get(type, E, false) of
                    extends ->
                        [ " [style=dashed dir=back color=", color(Mod),"] " ];
                    overrules ->
                        [ " [style=dotted dir=\"back\" color=", color(Mod),"] " ];
                    _ ->
                        [ " [color=", color(Mod), "] " ]
                end,
                ";\n"
            ]
        end,
        maps:get(edges, G, [])),
    Dot = iolist_to_binary([
        <<"digraph G {\n">>,
            "    rankdir=LR;\n",
            "    node [ fontsize=11 shape=box color=cadetblue ];\n",
            "\n",
            Nodes,
            "\n",
            lists:usort(Edges),
        <<"}\n">>
    ]),
    {ok, Dot}.

-spec graph(Context) -> {ok, Graph} when
    Context :: z:context(),
    Graph :: #{ nodes => Nodes, edges => Edges },
    Nodes :: [ template_node() ],
    Edges :: [ template_edge() ].
graph(Context) ->
    % Build an index of all "top" level templates reachable by
    % lookup, skip templates hidden by like-named templates in
    % modules of higher priority.
    All = z_module_indexer:all(template, Context),
    Ts = collect_nodes(All, Context),
    TsWithHidden = collect_hidden(Ts, Context),
    TsWithHiddenList = maps:values(TsWithHidden),
    TsWithHiddenN = lists:zip(TsWithHiddenList, lists:seq(1, length(TsWithHiddenList))),
    BuildDir = <<(build_dir())/binary, "/">>,
    NodeMap = lists:foldl(
        fun({Template, N}, Acc) ->
            #module_index{ key = Key, filepath = Path } = Template,
            #module_index_key{ name = TplName } = Key,
            Path1 = binary:replace(Path, BuildDir, <<>>),
            {Mod, Tpl} = case binary:split(Path1, <<"/priv/templates/">>) of
                [M,T] -> {M, T};
                T -> {<<>>, T}
            end,
            Mod1 = binary:replace(Mod, <<"zotonic_mod_">>, <<"mod_">>),
            Node = #{
                id => <<"n", (integer_to_binary(N))/binary>>,
                module => Mod1,
                template => Tpl,
                index => Template,
                basename => basename(Tpl)
            },
            case maps:is_key(Path, Ts) of
                true ->
                    % "Top" level templates use their short-hand template
                    % name for the index. This is used for finding includes
                    % and extends.
                    Acc#{
                        TplName => Node
                    };
                false ->
                   % Templates that are hidden by other (higher prio) templates
                    % but are used for overrules or all-include are indexed by
                    % their full path.
                    Acc#{
                        Path => Node
                    }
            end
        end,
        #{},
        TsWithHiddenN),
    Nodes = maps:values(NodeMap),
    Edges = edges(Nodes, NodeMap, Context),
    Extends = extend_edges(Nodes, NodeMap, Context),
    G = #{
        nodes => Nodes,
        edges => Extends ++ Edges
    },
    {ok, G}.

collect_nodes(All, Context) ->
    {Ns, _} = lists:foldl(
        fun(Template, {Acc, Dedup}) ->
            #module_index{ key = Key } = Template,
            #module_index_key{ name = TplName } = Key,
            case maps:get(TplName, Dedup, undefined) of
                undefined ->
                    {ok, T} = z_module_indexer:find(template, TplName, Context),
                    #module_index{ filepath = Path } = T,
                    Dedup1 = Dedup#{
                        TplName => true
                    },
                    Acc1 = Acc#{
                        Path => T
                    },
                    {Acc1, Dedup1};
                _ ->
                    {Acc, Dedup}
            end
        end,
        {#{}, #{}},
        All),
    Ns.

%% @doc Collect overrules and 'all include' templates that are normally hidden
%% by the like-named template in the module with highest priority.
collect_hidden(TplMap, Context) ->
    Tpls = maps:values(TplMap),
    TplMap1 = collect_overrules(Tpls, TplMap, Context),
    Tpls1 = maps:values(TplMap1),
    collect_all_includes(Tpls1, TplMap1, Context).


collect_all_includes([], Map, _Context) ->
    Map;
collect_all_includes([Template|Ts], Map, Context) ->
    case z_template:includes(Template, #{}, Context) of
        {ok, Includes} ->
            {Extra, Map1} = lists:foldl(
                fun
                    (#{
                        template := IncFile,
                        method := all   % optional | all | normal
                    }, Acc) ->
                        All = z_module_indexer:find_all(template, IncFile, Context),
                        lists:foldl(
                            fun(Tpl, {ExtraAcc, MapAcc}) ->
                                #module_index{ key = Key, filepath = Path } = Tpl,
                                #module_index_key{ name = TplName } = Key,
                                case maps:get(TplName, Map, undefined) of
                                    Tpl ->
                                        {ExtraAcc, MapAcc};
                                    _ ->
                                        case maps:get(Path, Map, undefined) of
                                            undefined ->
                                                MapAcc1 = MapAcc#{
                                                    Path => Tpl
                                                },
                                                {[Tpl|ExtraAcc], MapAcc1};
                                            _ ->
                                                {ExtraAcc, MapAcc}
                                        end
                                end
                            end,
                            Acc,
                            All);
                    (_, Acc) ->
                        Acc
                end,
                {[], Map},
                Includes),
            collect_all_includes(Extra ++ Ts, Map1, Context);
        {error, _} ->
            collect_all_includes(Ts, Map, Context)
    end.



collect_overrules([], Map, _Context) ->
    Map;
collect_overrules([Template|Ts], Map, Context) ->
    case z_template:extends(Template, #{}, Context) of
        {ok, overrules} ->
            case find_overrules(Template, Context) of
                {ok, Next} ->
                    #module_index{ key = Key, filepath = Path } = Next,
                    #module_index_key{ name = TplName } = Key,
                    case maps:get(TplName, Map, undefined) of
                        Next ->
                            collect_overrules(Ts, Map, Context);
                        _ ->
                            case maps:get(Path, Map, undefined) of
                                undefined ->
                                    Map1 = Map#{
                                        Path => Next
                                    },
                                    collect_overrules([Next|Ts], Map1, Context);
                                _ ->
                                    collect_overrules(Ts, Map, Context)
                            end
                    end;
                {error, _} ->
                    collect_overrules(Ts, Map, Context)
            end;
        _ ->
            collect_overrules(Ts, Map, Context)
    end.

find_overrules(#module_index{ key = Key, filepath = Filename }, Context) ->
    #module_index_key{ name = TplName } = Key,
    Templates = z_module_indexer:find_all(template, TplName, Context),
    case find_next_template(Filename, Templates) of
        {ok, _} = Ok ->
            Ok;
        {error, enoent} ->
            {error, enoent}
    end.

find_next_template(_Filename, []) ->
    {error, enoent};
find_next_template(Filename, [#module_index{filepath=Filename},Next|_]) ->
    {ok, Next};
find_next_template(Filename, [_|Rest]) ->
    find_next_template(Filename, Rest).


extend_edges(Nodes, NodeMap, Context) ->
    lists:filtermap(
        fun(#{ index := Template, id := FromId, module := Mod  }) ->
            case z_template:extends(Template, #{}, Context) of
                {ok, Extends} when is_binary(Extends) ->
                    case maps:get(Extends, NodeMap, undefined) of
                        #{
                            id := ToId
                        } ->
                            {true, #{
                                from => ToId,
                                to => FromId,
                                module => Mod,
                                type => extends
                            }};
                        undefined ->
                            false
                    end;
                {ok, overrules} ->
                    case find_overrules(Template, Context) of
                        {ok, Next} ->
                            #module_index{ filepath = Path } = Next,
                            case maps:get(Path, NodeMap, undefined) of
                                undefined ->
                                    false;
                                #{ id := ToId } ->
                                    {true, #{
                                        from => ToId,
                                        to => FromId,
                                        module => Mod,
                                        type => overrules
                                    }}
                            end;
                        {error, _} ->
                            false
                    end;
                {ok, undefined} ->
                    false;
                {error, _} ->
                    false
            end
        end,
        Nodes).

edges(Nodes, NodeMap, Context) ->
    lists:flatten(
        lists:map(
            fun(#{ index := Template, id := FromId, module := Mod  }) ->
                case z_template:includes(Template, #{}, Context) of
                    {ok, Includes} when is_list(Includes) ->
                        lists:filtermap(
                            fun
                                (#{
                                    template := IncFile,
                                    method := all,       % optional | all | normal
                                    is_catinclude := _
                                }) ->
                                    All = z_module_indexer:find_all(template, IncFile, Context),
                                    L = lists:filtermap(
                                        fun(Tpl) ->
                                            #module_index{ filepath = Path } = Tpl,
                                            case maps:get(IncFile, NodeMap, undefined) of
                                                #{ index := #module_index{ filepath = Path }, id := ToId } ->
                                                    {true, #{
                                                        from => FromId,
                                                        to => ToId,
                                                        module => Mod,
                                                        type => include
                                                    }};
                                                _ ->
                                                    case maps:get(Path, NodeMap, undefined) of
                                                        #{ id := ToId } ->
                                                            {true, #{
                                                                from => FromId,
                                                                to => ToId,
                                                                module => Mod,
                                                                type => include
                                                            }};
                                                        _ ->
                                                            false
                                                    end
                                            end
                                        end,
                                        All),
                                    {true, L};
                                (#{
                                    template := IncFile,
                                    is_catinclude := true
                                }) ->
                                    Included = find_all_catinclude(IncFile, Nodes),
                                    Es = lists:map(
                                        fun
                                            (#{ id := ToId }) ->
                                                #{
                                                    from => FromId,
                                                    to => ToId,
                                                    module => Mod,
                                                    type => include
                                                }
                                        end,
                                        Included),
                                    {true, Es};
                                (#{
                                    template := IncFile,
                                    is_catinclude := false
                                }) ->
                                    case maps:get(IncFile, NodeMap, undefined) of
                                        #{
                                            id := ToId
                                        } ->
                                            {true, #{
                                                from => FromId,
                                                to => ToId,
                                                module => Mod,
                                                type => include
                                            }};
                                        undefined ->
                                            false
                                    end
                            end,
                            Includes);
                    {error, _} ->
                        []
                end
            end,
            Nodes)
        ).



find_all_catinclude(Template, Nodes) ->
    lists:filter(
        fun(#{ basename := B }) -> B =:= Template end,
        Nodes).

% Determine the base name of a template, as used with catinclude.
basename(Tpl) ->
    Parts = binary:split(Tpl, <<"/">>, [ global ]),
    [Filename|RevDir] = lists:reverse(Parts),
    Filename1 = case binary:split(Filename, <<".">>, [ global ]) of
        [ F, <<"name">>, _, <<"tpl">> ] ->
            <<F/binary, ".tpl">>;
        [ F, _, <<"tpl">> ] ->
            <<F/binary, ".tpl">>;
        _ ->
            Filename
    end,
    iolist_to_binary(lists:join($/, lists:reverse([Filename1 | RevDir]))).

build_dir() ->
    unicode:characters_to_binary(z_path:build_lib_dir()).

color(Name) ->
    Nr = erlang:phash2(Name, length(colors())),
    lists:nth(Nr+1, colors()).

% https://graphviz.org/doc/info/colors.html#svg
% commented out colors too light for a white background
colors() ->
    [
        % <<"aliceblue">>,
        % <<"antiquewhite">>,
        <<"aqua">>,
        <<"aquamarine">>,
        % <<"azure">>,
        <<"beige">>,
        <<"bisque">>,
        <<"black">>,
        <<"blanchedalmond">>,
        <<"blue">>,
        <<"blueviolet">>,
        <<"brown">>,
        <<"burlywood">>,
        <<"cadetblue">>,
        <<"chartreuse">>,
        <<"chocolate">>,
        <<"coral">>,
        <<"cornflowerblue">>,
        % <<"cornsilk">>,
        <<"crimson">>,
        <<"cyan">>,
        <<"darkblue">>,
        <<"darkcyan">>,
        <<"darkgoldenrod">>,
        <<"darkgray">>,
        <<"darkgreen">>,
        <<"darkgrey">>,
        <<"darkkhaki">>,
        <<"darkmagenta">>,
        <<"darkolivegreen">>,
        <<"darkorange">>,
        <<"darkorchid">>,
        <<"darkred">>,
        <<"darksalmon">>,
        <<"darkseagreen">>,
        <<"darkslateblue">>,
        <<"darkslategray">>,
        <<"darkslategrey">>,
        <<"darkturquoise">>,
        <<"darkviolet">>,
        <<"deeppink">>,
        <<"deepskyblue">>,
        <<"dimgray">>,
        <<"dimgrey">>,
        <<"dodgerblue">>,
        <<"firebrick">>,
        % <<"floralwhite">>,
        <<"forestgreen">>,
        <<"fuchsia">>,
        <<"gainsboro">>,
        % <<"ghostwhite">>,
        <<"gold">>,
        <<"goldenrod">>,
        <<"gray">>,
        <<"grey">>,
        <<"green">>,
        % <<"greenyellow">>,
        % <<"honeydew">>,
        <<"hotpink">>,
        <<"indianred">>,
        <<"indigo">>,
        % <<"ivory">>,
        <<"khaki">>,
        <<"lavender">>,
        % <<"lavenderblush">>,
        <<"lawngreen">>,
        % <<"lemonchiffon">>,
        <<"lightblue">>,
        <<"lightcoral">>,
        % <<"lightcyan">>,
        <<"lightgoldenrodyellow">>,
        <<"lightgray">>,
        <<"lightgreen">>,
        <<"lightgrey">>,
        <<"lightpink">>,
        <<"lightsalmon">>,
        <<"lightseagreen">>,
        <<"lightskyblue">>,
        <<"lightslategray">>,
        <<"lightslategrey">>,
        <<"lightsteelblue">>,
        <<"lightyellow">>,
        <<"lime">>,
        <<"limegreen">>,
        % <<"linen">>,
        <<"magenta">>,
        <<"maroon">>,
        <<"mediumaquamarine">>,
        <<"mediumblue">>,
        <<"mediumorchid">>,
        <<"mediumpurple">>,
        <<"mediumseagreen">>,
        <<"mediumslateblue">>,
        <<"mediumspringgreen">>,
        <<"mediumturquoise">>,
        <<"mediumvioletred">>,
        <<"midnightblue">>,
        % <<"mintcream">>,
        <<"mistyrose">>,
        <<"moccasin">>,
        <<"navajowhite">>,
        <<"navy">>,
        % <<"oldlace">>,
        <<"olive">>,
        <<"olivedrab">>,
        <<"orange">>,
        <<"orangered">>,
        <<"orchid">>,
        <<"palegoldenrod">>,
        <<"palegreen">>,
        <<"paleturquoise">>,
        <<"palevioletred">>,
        <<"papayawhip">>,
        <<"peachpuff">>,
        <<"peru">>,
        <<"pink">>,
        <<"plum">>,
        <<"powderblue">>,
        <<"purple">>,
        <<"red">>,
        <<"rosybrown">>,
        <<"royalblue">>,
        <<"saddlebrown">>,
        <<"salmon">>,
        <<"sandybrown">>,
        <<"seagreen">>,
        % <<"seashell">>,
        <<"sienna">>,
        <<"silver">>,
        <<"skyblue">>,
        <<"slateblue">>,
        <<"slategray">>,
        <<"slategrey">>,
        % <<"snow">>,
        <<"springgreen">>,
        <<"steelblue">>,
        <<"tan">>,
        <<"teal">>,
        <<"thistle">>,
        <<"tomato">>,
        <<"turquoise">>,
        <<"violet">>,
        <<"wheat">>
        % <<"white">>,
        % <<"whitesmoke">>,
        % <<"yellow">>,
        % <<"yellowgreen">>
    ].

