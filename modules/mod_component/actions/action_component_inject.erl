%% @author Maas-Maarten <mmzeeman@xs4all.nl>
%% @copyright 2014 Maas-Maarten Zeeman
%% Date: 2014-12-15
%% @doc Inject a component on a page

-module(action_component_inject).

-export([render_action/4]).

-include("zotonic.hrl").

render_action(_TriggerId, _TargetId, Args, Context) ->
    InitScript = iolist_to_binary(proplists:get_value(init_script, Args, [])),
    Name = proplists:get_value(name, Args),
    Files = proplists:get_value(files, Args, []), 

    Urls = [begin
                [Url] = z_lib_include:url([U], Context),
                {U, Url}
        end ||  U <- Files],
    WaitFor = iolist_to_binary(z_utils:js_array(Files)),

    %% Install a script which wait for the resources to become available.
    PreScript = <<"z_load(\"", Name/binary, "\",", InitScript/binary, ", ", WaitFor/binary, ");">>,

    %% Generate scripts for loading the required files
    LoadScript = [<<"z_lazy_load(\"", Href/binary, "\",\"", N/binary, "\");">> || {N, Href} <- Urls],

    Script = iolist_to_binary([PreScript, LoadScript]),

    {Script, Context}.

