-module(m_sitemodule).
-author("Driebit <tech@driebit.nl>").

-behaviour(gen_model).

%% interface functions
-export([
    m_find_value/3,
    m_to_list/2,
    m_value/2
]).


-include_lib("zotonic.hrl").


m_find_value(Site, #m{value=undefined} = M, _Context) ->
    M#m{value=[site, Site]};
m_find_value(running, #m{value=[site, Site]} = M, _Context) ->
    M#m{value=[site, Site, running]}.

m_to_list(_, _Context) ->
    undefined.

m_value(#m{value=[site, Site, running]}, _Context) ->
    case z_sites_manager:get_site_status(Site) of
        {ok, running} -> z_module_manager:active(Site, z:c(Site));
        _ -> false
    end;
m_value(_, _Context) ->
    undefined.
