%% Copyright 2026 Marc Worrell
%% SPDX-License-Identifier: Apache-2.0
-module(agent_content_markdown).
-export([compose/4]).

%% Render editorial HTML through category-aware templates, then use the existing
%% faithful converter. All calls run inside the model's bounded anonymous worker.
-spec compose(Id, Props, Language, Context) -> Result when
    Id :: integer(), Props :: map(), Language :: atom(), Context :: z:context(),
    Result :: #{markdown := binary(), content_complete := boolean()}.
compose(Id, Props, Language, Context) ->
    C = z_context:set_language(Language, Context),
    Body = text(maps:get(<<"body">>, Props, <<>>), Language, C),
    Blocks = [B || B <- maps:get(<<"blocks">>, Props, []), not survey(B)],
    case byte_size(Body) =< 524288 of
        false -> #{markdown => <<"Content omitted: follow the source page.">>, content_complete => false};
        true ->
            Parts = [block(Id, B, Language, C) || B <- lists:sublist(Blocks, 100)],
            Complete = length(Blocks) =< 100 andalso lists:all(fun({Ok, _}) -> Ok end, Parts),
            {Html, _} = z_template:render_to_iolist({cat, "_agent_content.tpl"},
                [{id, Id}, {body_html, Body}, {blocks_html, [H || {_, H} <- Parts]}], C),
            case iolist_size(Html) =< 1048576 of
                true -> #{markdown => z_markdown:to_markdown(iolist_to_binary(Html), faithful), content_complete => Complete};
                false -> throw(too_large)
            end
    end.

%% Exclude forms before template selection, including any future survey type.
survey(#{<<"type">> := <<"survey_", _/binary>>}) -> true;
survey(_) -> false.

block(Id, #{<<"type">> := Type} = Block, Language, C) when is_binary(Type), byte_size(Type) =< 64 ->
    case re:run(Type, <<"^[a-z][a-z0-9_]*$">>, [{capture, none}]) of
        nomatch -> omitted();
        match ->
            Template = {cat, <<"_agent_block_", Type/binary, ".tpl">>, Id},
            case z_template_compiler_runtime:map_template(Template, #{id => Id}, C) of
                {error, _} -> omitted();
                {ok, File} ->
                    %% Localize fields used by the built-in renderers. Keep other
                    %% ACL-filtered block fields available to trusted site templates.
                    B = Block#{<<"body">> => text(maps:get(<<"body">>, Block, <<>>), Language, C),
                        <<"header">> => plain(maps:get(<<"header">>, Block, <<>>), Language, C)},
                    case byte_size(maps:get(<<"body">>, B)) + byte_size(maps:get(<<"header">>, B)) =< 32768 of
                        false -> omitted();
                        true ->
                            {Complete, Reference} = reference(Type, B, Language, C),
                            {Html, _} = z_template:render_to_iolist(File, [{id, Id}, {blk, B}, {reference, Reference}], C),
                            {Complete, Html}
                    end
            end
    end;
block(_, _, _, _) -> omitted().

omitted() -> {false, <<"<p>[Unsupported block omitted; follow the source page.]</p>">>}.

reference(<<"page">>, #{<<"rsc_id">> := Ref}, Language, C) when is_integer(Ref); is_binary(Ref) ->
    Id = m_rsc:rid(Ref, C),
    case Id =/= undefined andalso z_acl:rsc_visible(Id, C) andalso m_rsc:is_published_date(Id, C) of
        false -> {true, undefined};
        true ->
            Props = m_rsc:get(Id, C),
            Languages = maps:get(<<"language">>, Props, []),
            Default = z_language:default_language(C),
            Actual = case lists:member(Language, Languages) of
                true -> Language;
                false ->
                    case Languages =:= [] orelse lists:member(Default, Languages) of
                        true -> Default;
                        false -> hd(Languages)
                    end
            end,
            LC = z_context:set_language(Actual, C),
            Body = text(maps:get(<<"body">>, Props, <<>>), Actual, LC),
            Summary = text(maps:get(<<"summary">>, Props, <<>>), Actual, LC),
            Title = plain(maps:get(<<"title">>, Props, <<>>), Actual, LC),
            Small = byte_size(Body) + byte_size(Summary) + byte_size(Title) =< 32768,
            %% One level only: cycles and huge nested graphs cannot expand here.
            Complete = Small andalso lists:all(fun survey/1, maps:get(<<"blocks">>, Props, [])),
            {Complete, #{title => Title, summary => Summary, language => Actual,
                license => plain(maps:get(<<"license">>, Props, <<>>), Actual, LC),
                attribution => plain(maps:get(<<"attribution">>, Props, <<>>), Actual, LC),
                body => case Small of true -> Body; false -> <<>> end,
                page_url => m_rsc:p(Id, <<"page_url_abs">>, LC), complete => Complete}}
    end;
reference(_, _, _, _) -> {true, undefined}.

text(Value, _, _) when is_binary(Value) -> Value;
text(Value, _, _) when is_list(Value) -> unicode:characters_to_binary(Value);
text(Value, Language, C) -> z_convert:to_binary(z_trans:lookup(Value, Language, C)).

plain(Value, Language, C) -> z_html:unescape(z_html:strip(text(Value, Language, C))).
