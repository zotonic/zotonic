%% @author Arjan Scherpenisse <arjan@scherpenisse.net>
%% @copyright 2009 Arjan Scherpenisse
%% Date: 2009-10-03
%%
%% @doc Set a value in the zotonic visitor record.

-module(service_base_persistent_set).
-author("Arjan Scherpenisse <arjan@scherpenisse.net>").

-svc_title("Set a value in the Zotonic persistent record.").
-svc_needauth(false).

-export([process_get/2]).

-include_lib("zotonic.hrl").


process_get(_ReqData, Context) ->
    case z_context:get_q("key", Context) of
        X when X =:= undefined orelse X =:= [] ->
            {error, missing_arg, "key"};
        Key ->
            case z_context:get_q("value", Context) of
                X when X =:= undefined orelse X =:= [] ->
                    {error, missing_arg, "value"};
                Value ->
                    z_context:set_persistent(list_to_atom(Key), Value, Context),
                    z_convert:to_json([{key, Key}, {value, Value}])
            end
    end.


