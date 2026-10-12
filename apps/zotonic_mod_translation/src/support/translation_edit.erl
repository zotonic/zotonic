%% Copyright 2026 Zotonic. Apache-2.0.
-module(translation_edit).
-moduledoc("Single-field translation editing for the admin overview. Each request
checks resource edit permission, configured resource languages and the current
field. Resource fields and uniquely named block fields are supported; structured
survey answer lists and unsafe properties are deliberately not writable here.
The dialog uses an exact translation, never a fallback from another language.").
-export([event/2, field/4, patch/3, editor_type/1]).
-include_lib("zotonic_core/include/zotonic.hrl").

event(#postback{message={translation_edit, Args}}, Context) ->
    case load(Args, Context) of
        {ok, Id, Language, Raw, Field} ->
            Vars = [
                {id, Id}, {language, Language}, {field, Field},
                {edit_version, maps:get(<<"version">>, Raw)},
                {args, Args}, {level, 1}, {width, <<"large">>}
            ],
            z_render:dialog(?__("Edit translation", Context),
                "_dialog_translation_edit.tpl", Vars, Context);
        {error, _} -> denied(Context)
    end;
event(#submit{message={translation_save, Args}, form = Form}, Context) ->
    case load(Args, Context) of
        {ok, Id, Language, _Raw, _Field} ->
            Value = z_context:get_q(<<"translation_text">>, Context),
            Result = case is_binary(Value) of
                true ->
                    Props = patch(proplists:get_value(block, Args), proplists:get_value(property, Args), Value),
                    admin_edit_version:save(Id, z_context:get_q(<<"z_edit_version">>, Context),
                        fun(Ctx) -> m_rsc:update_translation(Id, Language, Props, Ctx) end, Context);
                false -> {error, badarg}
            end,
            Context1 = admin_edit_version:reply(z_convert:to_binary(Form), Id, Result, Context),
            case Result of
                {ok, Version} ->
                    % Re-render only this field. Other cells may have changed elsewhere;
                    % their dialogs always fetch the current value on opening.
                    Context2 = case load(Args, Context1) of
                        {ok, _, _, _, Saved} ->
                            Vars = [{id, Id}, {code, Language},
                                {p, proplists:get_value(property, Args)},
                                {block_name, proplists:get_value(block, Args)},
                                {text, maps:get(translation, Saved)}],
                            Render = #render{template = "_translation_edit_text.tpl", vars = Vars},
                            UpdatedContext = z_render:replace(proplists:get_value(target, Args), Render, Context1),
                            z_render:add_script([
                                "window.z_admin_edit_version?.savedOverview(", integer_to_binary(Id), ",",
                                z_json:encode(z_context:get_q(<<"z_edit_version">>, Context)), ",",
                                integer_to_binary(Version), ");"
                            ], UpdatedContext);
                        {error, _} -> Context1
                    end,
                    z_render:wire({dialog_close, [{level, 1}]}, Context2);
                {error, stale} ->
                    z_render:growl_error(?__("This page has changed. Your translation was not saved. Copy your changes before reloading.", Context1), Context1);
                {error, _} -> denied(Context1)
            end;
        {error, _} -> denied(Context)
    end.

denied(Context) ->
    z_render:growl_error(?__("This text cannot be edited, or you no longer have permission to edit it.", Context), Context).

load(Args, Context) ->
    Id = m_rsc:rid(proplists:get_value(id, Args), Context),
    case z_acl:rsc_editable(Id, Context) of
        false -> {error, eacces};
        true ->
            case z_language:to_language_atom(proplists:get_value(language, Args)) of
                {ok, Language} ->
                    % Raw access is authorized above; exact values avoid language fallback.
                    case m_rsc:get_raw(Id, Context) of
                        {ok, Raw} ->
                            Languages = maps:get(<<"language">>, Raw, [z_context:language(Context)]),
                            case lists:member(Language, z_language:editable_language_codes(Context))
                                andalso lists:member(Language, Languages)
                            of
                                true ->
                                    case field(Raw, proplists:get_value(block, Args),
                                            proplists:get_value(property, Args), Language) of
                                        {ok, Field} -> {ok, Id, Language, Raw, Field};
                                        Error -> Error
                                    end;
                                false -> {error, language}
                            end;
                        Error -> Error
                    end;
                Error -> Error
            end
    end.

-spec field(Raw, Block, Property, Language) -> {ok, map()} | {error, term()} when
    Raw :: map(), Block :: undefined | binary(), Property :: binary(), Language :: atom().
field(Raw, Block, Property, Language) when is_binary(Property) ->
    case container(Raw, Block) of
        {ok, Props} ->
            Value = maps:get(Property, Props, undefined),
            case is_text(Property, Value) andalso is_editable(Property, Raw) of
                true ->
                    Translation = case Value of
                        #trans{tr = Tr} -> proplists:get_value(Language, Tr, <<>>);
                        V when is_binary(V) -> V;
                        _ -> <<>>
                    end,
                    Type = editor_type(Property),
                    Text = case {Type, Property} of
                        {html, _} -> Translation;
                        {_, <<"summary">>} -> z_html:unescape(z_html:br2nl(Translation));
                        _ -> z_html:unescape(Translation)
                    end,
                    {ok, #{property => Property, type => Type, text => Text,
                        translation => #trans{tr = [{Language, Translation}]}}};
                false -> {error, field}
            end;
        Error -> Error
    end;
field(_Raw, _Block, _Property, _Language) -> {error, field}.

container(Raw, undefined) -> {ok, Raw};
container(_Raw, <<>>) -> {error, block};
container(Raw, Name) when is_binary(Name) ->
    case [B || #{<<"name">> := N} = B <- maps:get(<<"blocks">>, Raw, []), N =:= Name] of
        [Block] -> {ok, Block};
        _ -> {error, block}
    end;
container(_Raw, _Block) -> {error, block}.

is_editable(<<"title_slug">>, Raw) ->
    z_convert:to_bool(maps:get(<<"custom_slug">>, Raw, false));
is_editable(_Property, _Raw) ->
    true.

is_text(Property, Value) ->
    Safe = not lists:any(fun(Suffix) -> binary:longest_common_suffix([Property, Suffix]) =:= byte_size(Suffix) end,
        [<<"_unsafe">>, <<"_id">>, <<"_url">>, <<"_uri">>, <<"_json">>]),
    Safe andalso (is_record(Value, trans) orelse
        ((is_binary(Value) orelse Value =:= undefined) andalso lists:member(Property, [
            <<"title">>, <<"title_slug">>, <<"short_title">>, <<"chapeau">>, <<"subtitle">>, <<"header">>,
            <<"summary">>, <<"body">>, <<"body_extra">>, <<"prompt">>, <<"explanation">>,
            <<"matching">>, <<"narrative">>, <<"choices">>, <<"feedback">>,
            <<"agree">>, <<"disagree">>, <<"yes">>, <<"no">>,
            <<"seo_title">>, <<"seo_desc">>, <<"seo_keywords">>
        ]))).

-spec editor_type(binary()) -> html | input | textarea.
editor_type(<<"body", _/binary>>) -> html;
editor_type(<<"title_slug">>) -> input;
editor_type(Property) ->
    case binary:longest_common_suffix([Property, <<"_html">>]) of
        5 -> html;
        _ ->
            case lists:member(Property, [<<"title">>, <<"short_title">>, <<"chapeau">>,
                <<"subtitle">>, <<"header">>, <<"prompt">>, <<"agree">>, <<"disagree">>,
                <<"yes">>, <<"no">>, <<"seo_title">>, <<"seo_keywords">>]) of
                true -> input;
                false -> textarea
            end
    end.

-spec patch(undefined | binary(), binary(), binary()) -> map().
patch(undefined, Property, Value) -> #{Property => Value};
patch(<<>>, Property, Value) -> #{Property => Value};
patch(Block, Property, Value) ->
    #{<<"blocks">> => [#{<<"name">> => Block, Property => Value}]}.
