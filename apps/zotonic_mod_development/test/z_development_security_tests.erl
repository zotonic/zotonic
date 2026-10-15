%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2026 Marc Worrell
%% @doc Tests for development editor discovery and launching.
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

-module(z_development_security_tests).
-include_lib("eunit/include/eunit.hrl").
-include_lib("zotonic_core/include/zotonic.hrl").
-export([tests/1]).

%% @doc Run against a running development site with eunit:test(tests(z:c(Site))).
%% Only rejected launches are exercised; no editor is opened or preference saved.
tests(Context) ->
    Anonymous = z_acl:anondo(Context),
    Admin = z_acl:sudo(Context),
    {ok, #module_index{filepath = File}} = z_module_indexer:find(
        template, <<"admin_development.tpl">>, Admin),
    Base = #{<<"file">> => File, <<"editor">> => <<"auto">>, <<"line">> => 1, <<"column">> => 1},
    [
        {"model reads require development permission", fun() ->
            lists:foreach(fun(Path) ->
                ?assertEqual({error, eacces}, m_development:m_get([Path], undefined, Anonymous))
            end, [<<"editor">>, <<"dispatch_info">>, <<"list_observers">>])
        end},
        {"launch and preference writes require permission", fun() ->
            ?assertEqual({error, eacces}, post(<<"open">>, Base, Anonymous)),
            ?assertEqual({error, eacces}, post(<<"preference">>, #{<<"editor">> => <<"auto">>}, Anonymous))
        end},
        {"arbitrary and traversing files cannot be opened", fun() ->
            lists:foreach(fun(Path) ->
                ?assertEqual({error, enoent}, post(<<"open">>, Base#{<<"file">> => Path}, Admin))
            end, [<<"/etc/passwd">>, <<"/tmp/priv/templates/../../etc/passwd">>,
                  <<"/tmp/priv/templates/admin_development.tpl">>])
        end},
        {"malformed launch fields are rejected", fun() ->
            lists:foreach(fun(Override) ->
                ?assertEqual({error, badarg}, post(<<"open">>, maps:merge(Base, Override), Admin))
            end, [#{<<"file">> => []}, #{<<"line">> => 0}, #{<<"column">> => <<"1">>},
                  #{<<"editor">> => binary:copy(<<"x">>, 128)}, #{<<"editor">> => <<>>}])
        end},
        {"source tokens fail closed and are purpose checked", fun() ->
            Valid = z_crypto:pickle({development_editor_source, File, 1}, Admin),
            <<First, Rest/binary>> = Valid,
            Changed = case First of $A -> $B; _ -> $A end,
            Tampered = <<Changed, Rest/binary>>,
            ?assertEqual({error, eacces}, post(<<"open">>, Base#{<<"source">> => Valid}, Anonymous)),
            WrongPurpose = z_crypto:pickle({unrelated, File, 1}, Admin),
            BadPosition = z_crypto:pickle({development_editor_source, File, 0}, Admin),
            lists:foreach(fun(Token) ->
                ?assertEqual({error, eacces}, post(<<"open">>, Base#{<<"source">> => Token}, Admin))
            end, [WrongPurpose, BadPosition, Tampered]),
            ?assertEqual({error, badarg}, post(<<"open">>, Base#{<<"source">> => []}, Admin)),
            ?assertEqual({error, badarg}, post(<<"open">>, Base#{<<"source">> => <<>>}, Admin))
        end},
        {"filter source links resolve functions and require permission", fun() ->
            Source = unicode:characters_to_binary(
                "😀 {{ x|escape|no_such_filter_zz }}\n"
                "{% filter lower %}X{% endfilter %}\n"
                "{# {{ x|escape }} #} {{ \"x|escape\" }}"),
            ?assertEqual([], z_development_template:source_links(Source, Anonymous)),
            Links = z_development_template:source_links(Source, Admin),
            ?assertEqual([{<<"escape">>, 1, 8}, {<<"lower">>, 2, 11}],
                [{maps:get(name, L), maps:get(row, L), maps:get(column, L)} || L <- Links]),
            lists:foreach(fun(#{file := SourceFile, line := Line, token := Token}) ->
                ?assertEqual({development_editor_source, SourceFile, Line}, z_crypto:depickle(Token, Admin)),
                ?assert(filelib:is_regular(SourceFile)),
                ?assert(Line > 1)
            end, Links)
        end},
        {"scomp and model source links use active site resolution", fun() ->
            Source = <<"{% pager result=m.search.query %} {{ m.rsc[1].title|escape }} "
                "{{ m.no_such_model_zz.value }} {% no_such_scomp_zz %} "
                "{# {% pager %} {{ m.rsc }} #} {{ \"m.rsc\" }}">>,
            ?assertEqual([], z_development_template:source_links(Source, Anonymous)),
            Links = z_development_template:source_links(Source, Admin),
            ?assertEqual([{scomp, <<"pager">>}, {model, <<"search">>},
                         {model, <<"rsc">>}, {filter, <<"escape">>}],
                [{maps:get(kind, L), maps:get(name, L)} || L <- Links]),
            [{ok, PagerFile, PagerLine}, {ok, SearchFile, SearchLine}, {ok, RscFile, RscLine}] =
                [z_development_editor:source_location(M, F) || {M, F} <-
                    [{scomp_base_pager, render}, {m_search, m_get}, {m_rsc, m_get}]],
            ?assertEqual([{unicode:characters_to_binary(F), N} || {F, N} <-
                    [{PagerFile, PagerLine}, {SearchFile, SearchLine}, {RscFile, RscLine}]],
                [{maps:get(file, L), maps:get(line, L)} || L <- lists:sublist(Links, 3)])
        end},
        {"literal delegates link to exported event/2 handlers", fun() ->
            Source = <<"{% wire delegate=`mod_development` %}\n"
                "{% wire delegate=\"mod_development\" %}\n"
                "{% wire action={postback delegate='mod_development'} %}\n"
                "{% wire delegate=mod_development %} "
                "{% wire delegate=`no_such_delegate_zz` %} "
                "{% wire delegate=`filter_escape` %} "
                "{# {% wire delegate=`mod_development` %} #}">>,
            ?assertEqual([], z_development_template:source_links(Source, Anonymous)),
            Links = [L || #{kind := delegate} = L <- z_development_template:source_links(Source, Admin)],
            ?assertEqual([1, 2, 3], [maps:get(row, L) || L <- Links]),
            {ok, EventFile, EventLine} = z_development_editor:source_location(mod_development, {event, 2}),
            lists:foreach(fun(#{file := F, line := N, token := Token, name := Name}) ->
                ?assertEqual(<<"mod_development">>, Name),
                ?assertEqual({unicode:characters_to_binary(EventFile), EventLine}, {F, N}),
                ?assertEqual({development_editor_source, F, N}, z_crypto:depickle(Token, Admin))
            end, Links)
        end},
        {"actions link to active render_action/4 callbacks", fun() ->
            Source = <<"{% wire action={alert text=\"Hello\" action={hide target=\"item\"}} "
                "action={no_such_action_zz} action=dynamic_action "
                "other={alert} %} {# action={alert} #} {{ \"action={alert}\" }}">>,
            ?assertEqual([], z_development_template:source_links(Source, Anonymous)),
            Links = [L || #{kind := action} = L <- z_development_template:source_links(Source, Admin)],
            ?assertEqual([<<"alert">>, <<"hide">>], [maps:get(name, L) || L <- Links]),
            lists:foreach(fun(#{file := F, line := N, token := Token, name := Name}) ->
                {ok, #module_index{erlang_module = Module}} =
                    z_module_indexer:find(action, binary_to_existing_atom(Name, utf8), Admin),
                {ok, ExpectedFile, ExpectedLine} =
                    z_development_editor:source_location(Module, {render_action, 4}),
                ?assertEqual({unicode:characters_to_binary(ExpectedFile), ExpectedLine}, {F, N}),
                ?assertEqual({development_editor_source, F, N}, z_crypto:depickle(Token, Admin))
            end, Links)
        end},
        {"literal template options expose matching-template navigation", fun() ->
            Source = <<"{% wire action={dialog_open template=\"admin_development.tpl\"} "
                "template='missing-template.tpl' template=dynamic_template %} "
                "{# template=\"ignored.tpl\" #} {{ \"template='ignored.tpl'\" }}">>,
            ?assertEqual([], z_development_template:source_links(Source, Anonymous)),
            Links = [L || #{kind := template} = L <- z_development_template:source_links(Source, Admin)],
            ?assertEqual([<<"admin_development.tpl">>, <<"missing-template.tpl">>],
                [maps:get(name, L) || L <- Links]),
            ?assertEqual([], z_development_template:matching_templates(<<"missing-template.tpl">>, Admin)),
            lists:foreach(fun(Name) ->
                #context{} = z_development_template:event(
                    #postback{message = template_matches_source},
                    z_context:set_q(<<"template">>, Name, Anonymous))
            end, [<<"admin_development.tpl">>, <<"/etc/passwd">>, <<>>, binary:copy(<<"x">>, 4096)])
        end},
        {"fixed include names retain category matching semantics", fun() ->
            Source = <<"{% include \"base.tpl\" %}\n"
                "{% optional include 'optional.tpl' %}\n"
                "{% all include \"all.tpl\" %}\n"
                "{% catinclude \"category.tpl\" id %}\n"
                "{% all catinclude 'all-category.tpl' id %}\n"
                "{% include dynamic_template %} {% catinclude dynamic_template id %} "
                "{# {% include \"ignored.tpl\" %} #}">>,
            ?assertEqual([], z_development_template:source_links(Source, Anonymous)),
            Links = z_development_template:source_links(Source, Admin),
            ?assertEqual([
                {<<"base.tpl">>, false, 1}, {<<"optional.tpl">>, false, 2},
                {<<"all.tpl">>, false, 3}, {<<"category.tpl">>, true, 4},
                {<<"all-category.tpl">>, true, 5}
            ], [{maps:get(name, L), maps:get(is_category, L), maps:get(row, L)} || L <- Links]),
            #context{} = z_development_template:event(
                #postback{message = template_matches_source},
                z_context:set_q([{<<"template">>, <<"base.tpl">>}, {<<"is_category">>, <<"true">>}], Anonymous))
        end},
        {"unhandled tpl strings use template navigation without duplicate links", fun() ->
            Source = <<"{% wire foo=\"a.tpl\" other='folder/b.tpl' ignored=\"a.tpl.bak\" %} "
                "{{ \"standalone.tpl\" }} {% catinclude \"category.tpl\" id %} "
                "{% include \"included.tpl\" %} {% wire template=\"explicit.tpl\" %} "
                "{# foo=\"comment.tpl\" #} <div foo=\"text.tpl\"></div>">>,
            ?assertEqual([], z_development_template:source_links(Source, Anonymous)),
            Links = [L || #{kind := template} = L <- z_development_template:source_links(Source, Admin)],
            ?assertEqual([
                {<<"a.tpl">>, false}, {<<"folder/b.tpl">>, false},
                {<<"standalone.tpl">>, false}, {<<"category.tpl">>, true},
                {<<"included.tpl">>, false}, {<<"explicit.tpl">>, false}
            ], [{maps:get(name, L), maps:get(is_category, L, false)} || L <- Links]),
            lists:foreach(fun(#{name := Name, column := Column}) ->
                ?assertEqual(Name, binary:part(Source, Column - 1, byte_size(Name)))
            end, Links)
        end},
        {"template viewer handles malformed paths without reading files", fun() ->
            lists:foreach(fun(Path) ->
                #context{} = mod_development:event(
                    #postback{message = {template_view, [{template, Path}]}}, Admin)
            end, [[], #{}, 42, binary:copy(<<"x">>, 4096), <<"/etc/passwd">>])
        end},
        {"callbacks reject anonymous callers before processing input", fun() ->
            #context{} = mod_development:event(
                #postback{message = {template_view, [{template, #{}}]}}, Anonymous),
            #context{} = z_development_template:event(
                #postback{message = {template_matches, [{template, #{}}]}}, Anonymous)
        end}
    ].

post(Action, Payload, Context) ->
    m_development:m_post([<<"editor">>, Action], #{payload => Payload}, Context).
