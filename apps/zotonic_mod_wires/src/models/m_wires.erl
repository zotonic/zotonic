%% @doc Resolve convention-based widget dependencies for the widget manager.
-module(m_wires).
-behaviour(zotonic_model).
-export([m_get/3]).

-spec m_get(Path, Msg, Context) -> Result when
    Path :: list(),
    Msg :: zotonic_model:opt_msg(),
    Context :: z:context(),
    Result :: zotonic_model:return().
m_get([<<"widget">>, Name], _Msg, Context) when is_binary(Name) ->
    case m_config:get_boolean(mod_wires, widget_autoload, true, Context) of
        true -> widget(Name, Context);
        false -> {error, disabled}
    end;
m_get(_Path, _Msg, _Context) ->
    {error, unknown_path}.

%% Only simple widget names are accepted; never turn a class into an arbitrary
%% URL or repository path. Resolve through the indexer and normal lib URL code.
widget(Name, Context) ->
    case re:run(Name, <<"^[a-zA-Z0-9_-]+$">>, [{capture, none}]) of
        match ->
            File = <<"js/modules/z.", Name/binary, ".js">>,
            case z_module_indexer:find(lib, File, Context) of
                {ok, _} -> {ok, {z_lib_include:assets([File], [], Context), []}};
                {error, _} -> {error, enoent}
            end;
        nomatch ->
            {error, badarg}
    end.
