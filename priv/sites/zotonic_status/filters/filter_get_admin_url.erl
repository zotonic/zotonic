-module(filter_get_admin_url).

-export([get_admin_url/2]).

get_admin_url(Site, _Context) ->
    SiteContext = z:c(Site),
    case z_dispatcher:url_for(admin, SiteContext) of
        undefined ->
            undefined;
        U -> 
            z_dispatcher:abs_url(U, SiteContext)
    end.
