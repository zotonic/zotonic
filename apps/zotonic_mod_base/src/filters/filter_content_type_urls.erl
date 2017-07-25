%% @author David de Boer <david@ddeboer.nl>
%% @doc Get a list of content type URLs for a resource
-module(filter_content_type_urls).
-export([
    content_type_urls/2
]).

-include_lib("zotonic_core/include/zotonic.hrl").

-spec content_type_urls(m_rsc:resource(), #context{}) -> list().
content_type_urls(Id, Context) ->
    lists:map(
        fun({ContentType, DispatchRule}) ->
            {ContentType, url(DispatchRule, Id, Context)}
        end,
        controller_id:get_rsc_content_types(Id, Context)
    ).

url({DispatchRule, Args}, Id, Context) ->
    z_dispatcher:url_for(DispatchRule, Args ++ [{id, Id}], Context);
url(DispatchRule, Id, Context) ->
    url({DispatchRule, []}, Id, Context).
