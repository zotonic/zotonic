-module(m_copyright).

-export([
    m_get/3,

    lookup/1,

    rights/1,
    year/1,
    attribution/1,

    list_other/0,
    list_creative_commons/0,
    list_rights_statements/0
]).

-define(DEFAULT_RIGHTS, <<"CR">>).

m_get([ <<"list">>, <<"creative_commons">> | Rest ], _Msg, _Context) ->
    {ok, {list_creative_commons(), Rest}};
m_get([ <<"list">>, <<"rights_statements">> | Rest ], _Msg, _Context) ->
    {ok, {list_rights_statements(), Rest}};
m_get([ <<"lookup">>, Name | Rest ], _Msg, _Context) ->
    Desc = lookup(z_convert:to_binary(Name)),
    {ok, {Desc, Rest}};
m_get([ <<"rights">> | Rest ], _Mgs, Context) ->
    {ok, {rights(Context), Rest}};
m_get([ <<"year">> | Rest ], _Mgs, Context) ->
    {ok, {year(Context), Rest}};
m_get([ <<"attribution">> | Rest ], _Mgs, Context) ->
    {ok, {attribution(Context), Rest}}.

%% @doc Lookup the descriptive record about the given rights.
-spec lookup(Name) -> Description when
    Name :: binary(),
    Description :: #{ binary() => binary() }.
lookup(Name) ->
    Lists = [
        list_other(),
        list_creative_commons(),
        list_rights_statements()
    ],
    lookup_1(z_convert:to_binary(Name), Lists).

lookup_1(Name, []) ->
    #{
        <<"name">> => Name,
        <<"url">> => <<"#", Name/binary>>,
        <<"title">> => Name,
        <<"short">> => Name,
        <<"icons">> => []
    };
lookup_1(Name, [ H | T ]) ->
    case lookup_2(Name, H) of
        undefined -> lookup_1(Name, T);
        Desc -> Desc
    end.

lookup_2(_Name, []) ->
    undefined;
lookup_2(Name, [ #{ <<"name">> := N } = H | _T ]) when Name =:= N ->
    H;
lookup_2(<<"http:", _/binary>> = Name, [ #{ <<"url">> := Url } = H | _T ]) when Name =:= Url ->
    H;
lookup_2(<<"https:", _/binary>> = Name, [ #{ <<"url">> := Url } = H | _T ]) when Name =:= Url ->
    H;
lookup_2(Name, [ _ | T ]) ->
    lookup_2(Name, T).


%% @doc Return the default copyrights for this site. Defaults to CR (All Rights Reserved).
-spec rights(Context) -> binary() when
    Context :: z:context().
rights(Context) ->
    case z_convert:to_binary(m_config:get_value(mod_copyright, rights, Context)) of
        <<>> -> ?DEFAULT_RIGHTS;
        Rights -> Rights
    end.

%% @doc Return the default copyright attribution for this site. Defaults to the
%% title of the site.
-spec attribution(Context) -> binary() when
    Context :: z:context().
attribution(Context) ->
    case z_convert:to_binary(m_config:get_value(mod_copyright, attribution, Context)) of
        <<>> -> z_convert:to_binary(m_site:get(title, Context));
        Rights -> Rights
    end.

%% @doc Return the default year for the copyright for this site, defaults to the current year.
-spec year(Context) -> binary() when
    Context :: z:context().
year(Context) ->
    case z_convert:to_binary(m_config:get_value(mod_copyright, year, Context)) of
        <<>> ->
            {{Y, _, _}, _} = calendar:universal_time(),
            integer_to_binary(Y);
        Year ->
            Year
    end.


list_other() ->
    [
        #{
            <<"name">> => <<"CR">>,
            <<"url">> => <<"">>,
            <<"title">> => <<"All Rights Reserved">>,
            <<"short">> => <<"All Rights Reserved">>,
            <<"prefix">> => <<"©"/utf8>>,
            <<"icons">> => []
        },
        #{
            <<"name">> => <<"PD">>,
            <<"url">> => <<"">>,
            <<"title">> => <<"Public Domain">>,
            <<"short">> => <<"Public Domain">>,
            <<"icons">> => []
        }
    ].


list_creative_commons() ->
    [
        #{
            <<"name">> => <<"BY">>,
            <<"url">> => <<"https://creativecommons.org/licenses/by/4.0/">>,
            <<"title">> => <<"CC BY Attribution">>,
            <<"short">> => <<"CC BY 4.0">>,
            <<"icons">> => [ icon(<<"cc-by.svg">>) ]
        },
        #{
            <<"name">> => <<"BY,SA">>,
            <<"url">> => <<"https://creativecommons.org/licenses/by-sa/4.0/">>,
            <<"title">> => <<"CC BY-SA Attribution-ShareAlike">>,
            <<"short">> => <<"CC BY-SA 4.0">>,
            <<"icons">> => [ icon(<<"cc-by.svg">>), icon(<<"cc-sa.svg">>) ]
        },
        #{
            <<"name">> => <<"BY,NC">>,
            <<"url">> => <<"https://creativecommons.org/licenses/by-nc/4.0/">>,
            <<"title">> => <<"CC BY-NC Attribution-NonCommercial">>,
            <<"short">> => <<"CC BY-NC 4.0">>,
            <<"icons">> => [ icon(<<"cc-by.svg">>), icon(<<"cc-nc.svg">>) ]
        },
        #{
            <<"name">> => <<"BY,NC,SA">>,
            <<"url">> => <<"https://creativecommons.org/licenses/by-nc-sa/4.0/">>,
            <<"title">> => <<"CC BY-NC-SA Attribution-NonCommercial-ShareAlike">>,
            <<"short">> => <<"CC BY-NC-SA 4.0">>,
            <<"icons">> => [ icon(<<"cc-by.svg">>), icon(<<"cc-nc.svg">>), icon(<<"cc-sa.svg">>) ]
        },
        #{
            <<"name">> => <<"BY,ND">>,
            <<"url">> => <<"https://creativecommons.org/licenses/by-nd/4.0/">>,
            <<"title">> => <<"CC BY-ND Attribution-NoDerivs">>,
            <<"short">> => <<"CC BY-ND 4.0">>,
            <<"icons">> => [ icon(<<"cc-by.svg">>), icon(<<"cc-nd.svg">>) ]
        },
        #{
            <<"name">> => <<"BY,NC,ND">>,
            <<"url">> => <<"https://creativecommons.org/licenses/by-nc-nd/4.0/">>,
            <<"title">> => <<"CC BY-NC-ND Attribution-NonCommercial-NoDerivs">>,
            <<"short">> => <<"CC BY-NC-ND 4.0">>,
            <<"icons">> => [ icon(<<"cc-by.svg">>), icon(<<"cc-nc.svg">>), icon(<<"cc-nd.svg">>) ]
        },
        #{
            <<"name">> => <<"CC0">>,
            <<"url">> => <<"https://creativecommons.org/publicdomain/zero/1.0/">>,
            <<"title">> => <<"CC0 Public Domain">>,
            <<"short">> => <<"CC0 4.0">>,
            <<"icons">> => [ icon(<<"cc-zero.svg">>) ]
        }
    ].

list_rights_statements() ->
    [
        % Rights statements for in copyright objects
        #{
            <<"name">> => <<"InC">>,
            <<"url">> => <<"http://rightsstatements.org/vocab/InC/1.0/">>,
            <<"title">> => <<"In Copyright">>,
            <<"short">> => <<"Copyright"/utf8>>,
            <<"icons">> => [ icon(<<"rs-inc.svg">>) ]
        },
        #{
            <<"name">> => <<"InC-OW-EU">>,
            <<"url">> => <<"http://rightsstatements.org/vocab/InC-OW-EU/1.0/">>,
            <<"title">> => <<"In Copyright - EU Orphan work">>,
            <<"short">> => <<"Copyright - EU Orphan work"/utf8>>,
            <<"icons">> => [ icon(<<"rs-inc.svg">>) ]
        },
        #{
            <<"name">> => <<"InC-EDU">>,
            <<"url">> => <<"http://rightsstatements.org/vocab/InC-EDU/1.0/">>,
            <<"title">> => <<"In Copyright - Education use permitted">>,
            <<"short">> => <<"Copyright - Education use permitted"/utf8>>,
            <<"icons">> => [ icon(<<"rs-inc.svg">>) ]
        },
        #{
            <<"name">> => <<"InC-NC">>,
            <<"url">> => <<"http://rightsstatements.org/vocab/InC-NC/1.0/">>,
            <<"title">> => <<"In Copyright - Non-commercial use permitted">>,
            <<"short">> => <<"Copyright - Non-commercial use permitted"/utf8>>,
            <<"icons">> => [ icon(<<"rs-inc.svg">>) ]
        },
        #{
            <<"name">> => <<"InC-RUU">>,
            <<"url">> => <<"http://rightsstatements.org/vocab/InC-RUU/1.0/">>,
            <<"title">> => <<"In Copyright - Rights-holder(s) unlocatable or unidentifiable">>,
            <<"short">> => <<"Copyright - unknown rights-holder(s)"/utf8>>,
            <<"icons">> => [ icon(<<"rs-inc.svg">>) ]
        },

        % Rights statements for objects that are not in copyright
        #{
            <<"name">> => <<"NoC-CR">>,
            <<"url">> => <<"http://rightsstatements.org/vocab/NoC-CR/1.0/">>,
            <<"title">> => <<"No Copyright - Contractual restrictions">>,
            <<"short">> => <<"No Copyright - Contractual restrictions"/utf8>>,
            <<"icons">> => [ icon(<<"rs-noc.svg">>) ]
        },
        #{
            <<"name">> => <<"NoC-NC">>,
            <<"url">> => <<"http://rightsstatements.org/vocab/NoC-NC/1.0/">>,
            <<"title">> => <<"No Copyright - Non-commercial use only">>,
            <<"short">> => <<"No Copyright - Non-commercial use only"/utf8>>,
            <<"icons">> => [ icon(<<"rs-noc.svg">>) ]
        },
        #{
            <<"name">> => <<"NoC-OKLR">>,
            <<"url">> => <<"http://rightsstatements.org/vocab/NoC-OKLR/1.0/">>,
            <<"title">> => <<"No Copyright - Other known legal restrictions">>,
            <<"short">> => <<"No Copyright - Legal restrictions"/utf8>>,
            <<"icons">> => [ icon(<<"rs-noc.svg">>) ]
        },
        #{
            <<"name">> => <<"NoC-US">>,
            <<"url">> => <<"http://rightsstatements.org/vocab/NoC-US/1.0/">>,
            <<"title">> => <<"No Copyright - United States">>,
            <<"short">> => <<"No Copyright - USA"/utf8>>,
            <<"icons">> => [ icon(<<"rs-noc.svg">>) ]
        },

        % Other rights statements
        #{
            <<"name">> => <<"CNE">>,
            <<"url">> => <<"http://rightsstatements.org/vocab/CNE/1.0/">>,
            <<"title">> => <<"Copyright not evaluated">>,
            <<"short">> => <<"Copyright not evaluated"/utf8>>,
            <<"icons">> => [ icon(<<"rs-other.svg">>) ]
        },
        #{
            <<"name">> => <<"UND">>,
            <<"url">> => <<"http://rightsstatements.org/vocab/UND/1.0/">>,
            <<"title">> => <<"Copyright undetermined">>,
            <<"short">> => <<"Copyright undetermined"/utf8>>,
            <<"icons">> => [ icon(<<"rs-other.svg">>) ]
        },
        #{
            <<"name">> => <<"NKC">>,
            <<"url">> => <<"http://rightsstatements.org/vocab/NKC/1.0/">>,
            <<"title">> => <<"No known copyright">>,
            <<"short">> => <<"No known copyright"/utf8>>,
            <<"icons">> => [ icon(<<"rs-other.svg">>) ]
        }
    ].



icon(Filename) ->
    <<"/lib/images/copyright/", Filename/binary>>.
