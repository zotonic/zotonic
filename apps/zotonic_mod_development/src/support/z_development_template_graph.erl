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
        id := binary(),
        template := binary(),
        module := binary()
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
    % G = #{
    %     nodes => [
    %         #{ id => <<"a">>, template => <<"A">>, module => <<"mod_foo">> },
    %         #{ id => <<"b">>, template => <<"A1">>, module => <<"mod_foo">> },
    %         #{ id => <<"c">>, template => <<"Ac2">>, module => <<"mod_foo">> },
    %         #{ id => <<"d">>, template => <<"Ad3">>, module => <<"mod_foo">> }
    %     ],
    %     edges => [
    %         #{ from => <<"a">>, to => <<"b">>, color => <<"red">> },
    %         #{ from => <<"b">>, to => <<"a">>, color => <<"blue">> },
    %         #{ from => <<"a">>, to => <<"c">>, color => <<"black">> },
    %         #{ from => <<"c">>, to => <<"d">>, color => <<"black">> }
    %     ]
    % },
    Nodes = lists:map(
        fun(N) ->
            Mod = maps:get(module, N),
            [
                maps:get(id, N),
                " [",
                    "label=\"", maps:get(template, N), "\\n", Mod, "\"",
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
        fun(E) ->
            [
                maps:get(from, E),
                " -> ",
                maps:get(to, E),

                case maps:get(is_extend, E, false) of
                    true ->
                        " [style=dotted,dir=\"back\"]";
                    false ->
                        ""
                end,
                ";"
            ]
        end,
        maps:get(edges, G, [])),
    Dot = iolist_to_binary([
        <<"digraph G {\n">>,
            "rankdir=LR;\n",
            "node [ fontsize=11 shape=box color=cadetblue ];\n",
            "\n",
            Nodes,
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
    Ts = lists:foldl(
        fun(Template, Acc) ->
            #module_index{ key = Key } = Template,
            #module_index_key{ name = TplName } = Key,
            case maps:get(TplName, Acc, undefined) of
                undefined ->
                    {ok, T} = z_module_indexer:find(template, TplName, Context),
                    Acc#{
                        TplName => T
                    };
                _ ->
                    Acc
            end
        end,
        #{},
        All),
    Ts1 = maps:values(Ts),
    Ts2 = lists:zip(Ts1, lists:seq(1, length(Ts1))),
    BuildDir = <<(build_dir())/binary, "/">>,
    Nodes = lists:foldl(
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
                index => Template
            },
            Acc#{
                TplName => Node
            }
        end,
        #{},
        Ts2),
    Nodes1 = maps:values(Nodes),
    Edges = lists:flatten(
        lists:map(
            fun(#{ index := Template, id := FromId  }) ->
                case z_template:includes(Template, #{}, Context) of
                    {ok, Includes} ->
                        % TODO: expand catinclude and all include
                        lists:filtermap(
                            fun(#{
                                    template := IncFile,
                                    method := Method,   % optional | all | normal
                                    is_catinclude := IsCatinclude
                                }) ->
                                case maps:get(IncFile, Nodes, undefined) of
                                    #{
                                        id := ToId
                                    } ->
                                        {true, #{
                                            from => FromId,
                                            to => ToId,
                                            is_extend => false
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
            Nodes1)
        ),
    Extends = lists:filtermap(
        fun(#{ index := Template, id := FromId  }) ->
            case z_template:extends(Template, #{}, Context) of
                {ok, Extends} when is_binary(Extends) ->
                    case maps:get(Extends, Nodes, undefined) of
                        #{
                            id := ToId
                        } ->
                            {true, #{
                                from => ToId,
                                to => FromId,
                                is_extend => true
                            }};
                        undefined ->
                            false
                    end;
                {ok, overrules} ->
                    % TODO
                    false;
                {ok, undefined} ->
                    false;
                {error, _} ->
                    false
            end
        end,
        Nodes1),
    G = #{
        nodes => Nodes1,
        edges => Extends ++ Edges
    },
    {ok, G}.
    % G = #{

    % },
    % {ok, G}.

build_dir() ->
    unicode:characters_to_binary(z_path:build_lib_dir()).

color(Name) ->
    Nr = erlang:phash2(Name, length(colors())),
    lists:nth(Nr+1, colors()).

% https://graphviz.org/doc/info/colors.html#svg
colors() ->
    [
        <<"aliceblue">>,
        <<"antiquewhite">>,
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
        <<"greenyellow">>,
        <<"honeydew">>,
        <<"hotpink">>,
        <<"indianred">>,
        <<"indigo">>,
        % <<"ivory">>,
        <<"khaki">>,
        <<"lavender">>,
        <<"lavenderblush">>,
        <<"lawngreen">>,
        <<"lemonchiffon">>,
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
        <<"oldlace">>,
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

