% -*- coding: utf-8; Mode: erlang; tab-width: 4; c-basic-offset: 4; indent-tabs-mode: nil -*-
% ex: set softtabstop=4 tabstop=4 shiftwidth=4 expandtab fileencoding=utf-8:
%% coding: utf-8

%% @author Marc Worrell <marc@worrell.nl>
%% @doc Mapping English country name to iso code
%% @copyright 2012-2022 Marc Worrell

-module(l10n_country2iso).

-author("Marc Worrell <marc@worrell.nl>").

-export([
    country2iso/1
]).

country2iso(undefined) -> undefined;
country2iso(<<>>) -> undefined;
country2iso(<<"Antigua">>) -> <<"ag">>;
country2iso(<<"Bosnia and Herzegovina"/utf8>>) -> <<"ba">>;
country2iso(<<"British Virgin Islands"/utf8>>) -> <<"vg">>;
country2iso(<<"Burma"/utf8>>) -> <<"mm">>;
country2iso(<<"Brunei">>) -> <<"bn">>;
country2iso(<<"Cote d'Ivoire"/utf8>>) -> <<"ci">>;
country2iso(<<"Côte d'Ivoire"/utf8>>) -> <<"ci">>;
country2iso(<<"Curacao"/utf8>>) -> <<"cw">>;
country2iso(<<"Democratic Republic of the Congo"/utf8>>) -> <<"cd">>;
country2iso(<<"Falkland Islands (Malvinas)"/utf8>>) -> <<"fk">>;
country2iso(<<"French Guiana"/utf8>>) -> <<"gf">>;
country2iso(<<"French Guyana"/utf8>>) -> <<"gf">>;
country2iso(<<"French Polynesia"/utf8>>) -> <<"pf">>;
country2iso(<<"French Southern and Antarctic Lands"/utf8>>) -> <<"tf">>;
country2iso(<<"Guadeloupe"/utf8>>) -> <<"gp">>;
country2iso(<<"Guadeloupe (French)"/utf8>>) -> <<"gp">>;
country2iso(<<"Guam"/utf8>>) -> <<"gu">>;
country2iso(<<"Guam (USA)"/utf8>>) -> <<"gu">>;
country2iso(<<"Guernsey"/utf8>>) -> <<"gg">>;
country2iso(<<"Guinea-Bissau"/utf8>>) -> <<"gw">>;
country2iso(<<"Heard Island and McDonald Islands"/utf8>>) -> <<"hm">>;
country2iso(<<"Holy See (Vatican City)"/utf8>>) -> <<"va">>;
country2iso(<<"Iran (Islamic Republic of)"/utf8>>) -> <<"ir">>;
country2iso(<<"Korea, Democratic People's Republic of"/utf8>>) -> <<"kp">>;
country2iso(<<"Korea, Republic of"/utf8>>) -> <<"kr">>;
country2iso(<<"Korea, South">>) -> <<"kr">>;
country2iso(<<"Korea, North">>) -> <<"kp">>;
country2iso(<<"Kyrgyzstan"/utf8>>) -> <<"kg">>;
country2iso(<<"Lao People's Democratic Republic"/utf8>>) -> <<"la">>;
country2iso(<<"Libyan Arab Jamahiriya"/utf8>>) -> <<"ly">>;
country2iso(<<"Macau"/utf8>>) -> <<"mo">>;
country2iso(<<"Martinique"/utf8>>) -> <<"mq">>;
country2iso(<<"Martinique (French)"/utf8>>) -> <<"mq">>;
country2iso(<<"Micronesia, Federated States of"/utf8>>) -> <<"fm">>;
country2iso(<<"New Caledonia"/utf8>>) -> <<"nc">>;
country2iso(<<"Netherlands Antilles"/utf8>>) ->  <<"bq">>;
country2iso(<<"Palestine"/utf8>>) -> <<"ps">>;
country2iso(<<"Pitcairn Islands"/utf8>>) -> <<"pn">>;
country2iso(<<"Republic of Moldova"/utf8>>) -> <<"md">>;
country2iso(<<"Reunion (French)"/utf8>>) -> <<"re">>;
country2iso(<<"Reunion"/utf8>>) -> <<"re">>;
country2iso(<<"Russia"/utf8>>) -> <<"ru">>;
country2iso(<<"Saint Barthelemy"/utf8>>) -> <<"bl">>;
country2iso(<<"Saint Kitts and Nevis"/utf8>>) -> <<"kn">>;
country2iso(<<"Saint Martin"/utf8>>) -> <<"mf">>;
country2iso(<<"Saint Vincent and The Grenadines"/utf8>>) -> <<"vc">>;
country2iso(<<"Saint Vincent and the Grenadines"/utf8>>) -> <<"vc">>;
country2iso(<<"Sao Tome and Principe"/utf8>>) -> <<"st">>;
country2iso(<<"Sint Maarten"/utf8>>) -> <<"sx">>;
country2iso(<<"Slovakia"/utf8>>) -> <<"sk">>;
country2iso(<<"S. Georgia and S. Sandwich Isls."/utf8>>) -> <<"gs">>;
country2iso(<<"South Georgia South Sandwich Islands"/utf8>>) -> <<"gs">>;
country2iso(<<"Svalbard"/utf8>>) -> <<"sj">>;
country2iso(<<"Syrian Arab Republic"/utf8>>) -> <<"sy">>;
country2iso(<<"Tajikistan"/utf8>>) -> <<"tj">>;
country2iso(<<"The former Yugoslav Republic of Macedonia"/utf8>>) -> <<"mk">>;
country2iso(<<"Timor-Leste"/utf8>>) -> <<"tp">>;
country2iso(<<"Turkey"/utf8>>) -> <<"tr">>;
country2iso(<<"Great Britain"/utf8>>) -> <<"gb">>;
country2iso(<<"United Republic of Tanzania"/utf8>>) -> <<"tz">>;
country2iso(<<"Untied Arab Emirates"/utf8>>) -> <<"ae">>;
country2iso(<<"United States Virgin Islands"/utf8>>) -> <<"vi">>;
country2iso(<<"Vatican City"/utf8>>) -> <<"va">>;
country2iso(<<"Viet Nam"/utf8>>) -> <<"vn">>;
country2iso(A) ->
    case lists:keyfind(A, 2, l10n_iso2country:iso2country()) of
        {Iso, _} -> Iso;
        false -> undefined
    end.
