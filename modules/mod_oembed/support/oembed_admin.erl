-module(oembed_admin).

-export([fix_missing/1, count_missing/1]).

%% @doc Scan all 'video' resources for OEmbedded videos which do not
%% have an 'oembed' component or which have an 'oembed' component with
%% an 'error' field. It then tries to re-embed all of these video URLs.
fix_missing(Context) ->
    lists:foreach(fun(Medium) ->
                          {id, Id} = proplists:lookup(id, Medium),
                          {oembed_url, Url} = proplists:lookup(oembed_url, Medium),
                          {ok, Id} = m_rsc:update(Id, [{oembed_url, Url}], Context)
                  end,
                  get_missing(Context)),
    ok.

count_missing(Context) ->
    length(get_missing(Context)).


get_missing(Context) ->
    All = z_search:query_([{cat, video}], Context),
    Media = [m_rsc:p(Id, medium, Context) || Id <- All],
    lists:filter(
      fun(M) ->
              OEmbed = proplists:get_value(oembed, M),
              proplists:lookup(oembed_url, M) =/= none
                  andalso (OEmbed =:= undefined orelse proplists:get_value(error, OEmbed) =/= undefined)
      end,
      Media).    
