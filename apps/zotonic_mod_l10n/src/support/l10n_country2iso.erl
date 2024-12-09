% -*- coding: utf-8; Mode: erlang; tab-width: 4; c-basic-offset: 4; indent-tabs-mode: nil -*-
% ex: set softtabstop=4 tabstop=4 shiftwidth=4 expandtab fileencoding=utf-8:
%% coding: utf-8

%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2012-2024 Marc Worrell
%% @doc Map a country name to an iso code. Uses the list of country names from l10n_iso2country
%% in combination with the country-name translations in zotonic_core. If a direct match is not
%% found then the system will try to find a match by using the Levensthein distance between the
%% given name and all known country names (in all supported languages).
%% @end

-module(l10n_country2iso).

-author("Marc Worrell <marc@worrell.nl>").

-export([
    country2iso/1,
    reindex/0,
    make_index/0
]).

%% @doc Map a country name to an iso code. Uses the list of country names from l10n_iso2country
%% in combination with the country-name translations in zotonic_core. If a direct match is not
%% found then the system will try to find a match by using the Levensthein distance between the
%% given name and all known country names (in all supported languages).
-spec country2iso(Country | undefined) -> Iso | undefined when
    Country :: binary(),
    Iso :: binary().
country2iso(undefined) -> undefined;
country2iso(<<>>) -> undefined;
country2iso(<<_, _>> = Iso) ->
    Iso1 = z_string:to_lower(Iso),
    case l10n_iso2country:iso2country(Iso1) of
        undefined -> undefined;
        _ -> Iso1
    end;
country2iso(Country) ->
    country2iso_1(z_string:to_lower(Country)).

country2iso_1(<<"antigua">>) -> <<"ag">>;
country2iso_1(<<"bosnia and herzegovina"/utf8>>) -> <<"ba">>;
country2iso_1(<<"british virgin islands"/utf8>>) -> <<"vg">>;
country2iso_1(<<"burma"/utf8>>) -> <<"mm">>;
country2iso_1(<<"brunei">>) -> <<"bn">>;
country2iso_1(<<"cote d'ivoire"/utf8>>) -> <<"ci">>;
country2iso_1(<<"côte d'ivoire"/utf8>>) -> <<"ci">>;
country2iso_1(<<"curacao"/utf8>>) -> <<"cw">>;
country2iso_1(<<"democratic republic of the congo"/utf8>>) -> <<"cd">>;
country2iso_1(<<"falkland islands (malvinas)"/utf8>>) -> <<"fk">>;
country2iso_1(<<"french guiana"/utf8>>) -> <<"gf">>;
country2iso_1(<<"french guyana"/utf8>>) -> <<"gf">>;
country2iso_1(<<"french polynesia"/utf8>>) -> <<"pf">>;
country2iso_1(<<"french southern and antarctic lands"/utf8>>) -> <<"tf">>;
country2iso_1(<<"guadeloupe"/utf8>>) -> <<"gp">>;
country2iso_1(<<"guadeloupe (french)"/utf8>>) -> <<"gp">>;
country2iso_1(<<"guam"/utf8>>) -> <<"gu">>;
country2iso_1(<<"guam (usa)"/utf8>>) -> <<"gu">>;
country2iso_1(<<"guernsey"/utf8>>) -> <<"gg">>;
country2iso_1(<<"guinea-bissau"/utf8>>) -> <<"gw">>;
country2iso_1(<<"heard island and mcdonald islands"/utf8>>) -> <<"hm">>;
country2iso_1(<<"holy see (vatican city)"/utf8>>) -> <<"va">>;
country2iso_1(<<"iran (islamic republic of)"/utf8>>) -> <<"ir">>;
country2iso_1(<<"korea, democratic people's republic of"/utf8>>) -> <<"kp">>;
country2iso_1(<<"korea, republic of"/utf8>>) -> <<"kr">>;
country2iso_1(<<"korea, south">>) -> <<"kr">>;
country2iso_1(<<"korea, north">>) -> <<"kp">>;
country2iso_1(<<"kyrgyzstan"/utf8>>) -> <<"kg">>;
country2iso_1(<<"lao people's democratic republic"/utf8>>) -> <<"la">>;
country2iso_1(<<"libyan arab jamahiriya"/utf8>>) -> <<"ly">>;
country2iso_1(<<"macau"/utf8>>) -> <<"mo">>;
country2iso_1(<<"martinique"/utf8>>) -> <<"mq">>;
country2iso_1(<<"martinique (french)"/utf8>>) -> <<"mq">>;
country2iso_1(<<"micronesia, federated states of"/utf8>>) -> <<"fm">>;
country2iso_1(<<"new caledonia"/utf8>>) -> <<"nc">>;
country2iso_1(<<"netherlands antilles"/utf8>>) ->  <<"bq">>;
country2iso_1(<<"palestine"/utf8>>) -> <<"ps">>;
country2iso_1(<<"pitcairn islands"/utf8>>) -> <<"pn">>;
country2iso_1(<<"republic of moldova"/utf8>>) -> <<"md">>;
country2iso_1(<<"reunion (french)"/utf8>>) -> <<"re">>;
country2iso_1(<<"reunion"/utf8>>) -> <<"re">>;
country2iso_1(<<"russia"/utf8>>) -> <<"ru">>;
country2iso_1(<<"saint barthelemy"/utf8>>) -> <<"bl">>;
country2iso_1(<<"saint kitts and nevis"/utf8>>) -> <<"kn">>;
country2iso_1(<<"saint martin"/utf8>>) -> <<"mf">>;
country2iso_1(<<"saint vincent and the grenadines"/utf8>>) -> <<"vc">>;
country2iso_1(<<"sao tome and principe"/utf8>>) -> <<"st">>;
country2iso_1(<<"sint maarten"/utf8>>) -> <<"sx">>;
country2iso_1(<<"slovakia"/utf8>>) -> <<"sk">>;
country2iso_1(<<"s. georgia and s. sandwich isls."/utf8>>) -> <<"gs">>;
country2iso_1(<<"south georgia south sandwich islands"/utf8>>) -> <<"gs">>;
country2iso_1(<<"svalbard"/utf8>>) -> <<"sj">>;
country2iso_1(<<"syrian arab republic"/utf8>>) -> <<"sy">>;
country2iso_1(<<"tajikistan"/utf8>>) -> <<"tj">>;
country2iso_1(<<"the former yugoslav republic of macedonia"/utf8>>) -> <<"mk">>;
country2iso_1(<<"timor-leste"/utf8>>) -> <<"tp">>;
country2iso_1(<<"turkey"/utf8>>) -> <<"tr">>;
country2iso_1(<<"great britain"/utf8>>) -> <<"gb">>;
country2iso_1(<<"united republic of tanzania"/utf8>>) -> <<"tz">>;
country2iso_1(<<"untied arab emirates"/utf8>>) -> <<"ae">>;
country2iso_1(<<"united states virgin islands"/utf8>>) -> <<"vi">>;
country2iso_1(<<"vatican city"/utf8>>) -> <<"va">>;
country2iso_1(<<"viet nam"/utf8>>) -> <<"vn">>;
country2iso_1(A) ->
    Index = index(),
    case maps:get(A, Index, undefined) of
        undefined ->
            A1 = normalize(A),
            case maps:get(A1, Index, undefined) of
                undefined -> nearest(A1);
                Iso -> Iso
            end;
        Iso ->
            Iso
    end.

nearest(A) ->
    Index = index(),
    {_Dist, Found} = maps:fold(
        fun
            (K, Iso, undefined) ->
                Dist = z_string:distance(A, K),
                {Dist, Iso};
            (K, Iso, {Dist, _Best} = Acc) ->
                Dist1 = z_string:distance(A, K),
                if
                    Dist1 < Dist -> {Dist1, Iso};
                    true -> Acc
                end
        end,
        undefined,
        Index),
    Found.

index() ->
    case persistent_term:get(?MODULE, undefined) of
        undefined ->
            Index = make_index(),
            persistent_term:put(?MODULE, Index),
            Index;
        Index ->
            Index
    end.

reindex() ->
    persistent_term:erase(?MODULE),
    index().

make_index() ->
    Dir = filename:join(code:priv_dir(zotonic_core), "translations"),
    Files = filelib:wildcard( filename:join( Dir, "*.zotonic-country.po" ) ),
    Pos = lists:map(
        fun(F) ->
            z_gettext:parse_po(F)
        end,
        Files),
    Acc1 = lists:foldl(
        fun(Po, Acc) ->
            add_po(Po, Acc)
        end,
        #{},
        Pos),
    Acc2 = add(<<"Palestine">>, <<"ps">>, Acc1),
    add(<<"West Bank">>, <<"we">>, Acc2).

add_po([], Acc) ->
    Acc;
add_po([ {English, Translation} | Ks ], Acc) when is_binary(English), is_binary(Translation) ->
    Acc1 = case find_iso(English) of
        {Iso, English} ->
            add(Translation, Iso, add(English, Iso, Acc));
        {Iso, English1} ->
            add(Translation, Iso, add(English1, Iso, add(English, Iso, Acc)));
        false ->
            % Not a country
            Acc
    end,
    add_po(Ks, Acc1);
add_po([ _ | Ks ], Acc) ->
    add_po(Ks, Acc).

add(T, Iso, Acc) ->
    T1 = z_string:to_lower(T),
    T2 = normalize(T),
    Acc#{
        T1 => Iso,
        T2 => Iso
    }.

find_iso(K) ->
    lists:keyfind(alt(K), 2, l10n_iso2country:iso2country()).

alt(<<"Zaire">>) -> <<"Congo, The Democratic Republic of the"/utf8>>;
alt(<<"Great Britain">>) -> <<"United Kingdom"/utf8>>;
alt(<<"England">>) -> <<"United Kingdom"/utf8>>;
alt(<<"Wales">>) -> <<"United Kingdom"/utf8>>;
alt(<<"Sint Martin">>) -> <<"Saint Martin (French)"/utf8>>;
alt(<<"Sint Maarten">>) -> <<"Sint Maarten (Dutch)"/utf8>>;
alt(K) -> K.

normalize(A) ->
    A1 = z_string:normalize(A),
    Words = binary:split(A1, <<" ">>, [ global ]),
    A2 = iolist_to_binary(lists:sort(Words)),
    binary:replace(A2, [
            <<" ">>, <<",">>, <<"-">>, <<".">>, <<";">>,
            <<"'">>, <<"’"/utf8>>,
            <<"_">>, <<"(">>, <<")">>
        ], <<>>, [ global ]).
