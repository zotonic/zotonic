%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2012-2016 Marc Worrell
%% @doc Check if a user agent is a crawler
%%
%% Copyright 2012-2016 Marc Worrell
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.

-module(z_user_agent).
-author("Marc Worrell <marc@worrell.nl>").

-export([
    is_crawler/1
]).

-include_lib("zotonic.hrl").

%% @doc Check if the user agent is probably a bot.
-spec is_crawler(string()|binary()|#context{}|cowboy_req:req()|undefined) -> boolean().
is_crawler(undefined) ->
    false;
is_crawler(#context{} = Context) ->
    is_crawler(z_context:get_reqdata(Context));
is_crawler(Req) when is_map(Req) ->
    case cowmachine_req:get_req_header(<<"user-agent">>, Req) of
        undefined -> false;
        UserAgent -> is_crawler_ua(UserAgent)
    end;
is_crawler(UA) when is_list(UA)  ->
    is_crawler_ua(z_convert:to_binary(UA));
is_crawler(UA) when is_binary(UA)  ->
    is_crawler_ua(UA).

%% @doc List of hardcoded crawlers
is_crawler_ua(UA) ->
    UAlower = z_string:to_lower(UA),
    is_bot_prefix(UAlower)
    orelse lists:any(fun(Bot) ->
                        binary:match(UA, Bot) =/= nomatch
                     end,
                     bots()).

%% @doc Quick check on prefix of the user-agent string
is_bot_prefix(<<"mozilla/", _/binary>>) -> false;
% Usual bots
is_bot_prefix(<<"curl/", _/binary>>) -> true;
is_bot_prefix(<<"facebookexternalhit", _/binary>>) -> true;
is_bot_prefix(<<"facebot", _/binary>>) -> true;
is_bot_prefix(<<"feedfetcher-google", _/binary>>) -> true;
is_bot_prefix(<<"googlebot", _/binary>>) -> true;
is_bot_prefix(<<"iaskspider", _/binary>>) -> true;
is_bot_prefix(<<"ia_archiver", _/binary>>) -> true;
is_bot_prefix(<<"linkedinbot", _/binary>>) -> true;
is_bot_prefix(<<"msnbot", _/binary>>) -> true;
is_bot_prefix(<<"msrbot", _/binary>>) -> true;
is_bot_prefix(<<"htdig", _/binary>>) -> true;
is_bot_prefix(<<"surveybot", _/binary>>) -> true;
is_bot_prefix(<<"twitter", _/binary>>) -> true;
% Less usual bots
is_bot_prefix(<<"abachobot", _/binary>>) -> true;
is_bot_prefix(<<"accoona-ai-agent", _/binary>>) -> true;
is_bot_prefix(<<"adidxbot", _/binary>>) -> true;
is_bot_prefix(<<"automattic/", _/binary>>) -> true;
is_bot_prefix(<<"amsu", _/binary>>) -> true;
is_bot_prefix(<<"siteuptime", _/binary>>) -> true;
is_bot_prefix(<<"boitho.com", _/binary>>) -> true;
is_bot_prefix(<<"btbot", _/binary>>) -> true;
is_bot_prefix(<<"catchbot", _/binary>>) -> true;
is_bot_prefix(<<"converacrawler", _/binary>>) -> true;
is_bot_prefix(<<"cosmos", _/binary>>) -> true;
is_bot_prefix(<<"covario", _/binary>>) -> true;
is_bot_prefix(<<"dataparksearch", _/binary>>) -> true;
is_bot_prefix(<<"diamondbot", _/binary>>) -> true;
is_bot_prefix(<<"docomo", _/binary>>) -> true;
is_bot_prefix(<<"emeraldshield", _/binary>>) -> true;
is_bot_prefix(<<"envolk", _/binary>>) -> true;
is_bot_prefix(<<"esperanzabot", _/binary>>) -> true;
is_bot_prefix(<<"exabot", _/binary>>) -> true;
is_bot_prefix(<<"exb ", _/binary>>) -> true;
is_bot_prefix(<<"fast ", _/binary>>) -> true;
is_bot_prefix(<<"fast-", _/binary>>) -> true;
is_bot_prefix(<<"findlinks", _/binary>>) -> true;
is_bot_prefix(<<"fyberspyder", _/binary>>) -> true;
is_bot_prefix(<<"g2crawler", _/binary>>) -> true;
is_bot_prefix(<<"gaisbot", _/binary>>) -> true;
is_bot_prefix(<<"galaxybot", _/binary>>) -> true;
is_bot_prefix(<<"geniebot", _/binary>>) -> true;
is_bot_prefix(<<"gigabot", _/binary>>) -> true;
is_bot_prefix(<<"happyfunbot", _/binary>>) -> true;
is_bot_prefix(<<"hl_ftien_spider", _/binary>>) -> true;
is_bot_prefix(<<"holmes", _/binary>>) -> true;
is_bot_prefix(<<"iccrawler", _/binary>>) -> true;
is_bot_prefix(<<"ichiro", _/binary>>) -> true;
is_bot_prefix(<<"igdespyder", _/binary>>) -> true;
is_bot_prefix(<<"irlbot", _/binary>>) -> true;
is_bot_prefix(<<"issuecrawler", _/binary>>) -> true;
is_bot_prefix(<<"java/", _/binary>>) -> true;
is_bot_prefix(<<"jaxified", _/binary>>) -> true;
is_bot_prefix(<<"jyxobot", _/binary>>) -> true;
is_bot_prefix(<<"l.webis", _/binary>>) -> true;
is_bot_prefix(<<"larbin", _/binary>>) -> true;
is_bot_prefix(<<"lapozzbot", _/binary>>) -> true;
is_bot_prefix(<<"ldspider", _/binary>>) -> true;
is_bot_prefix(<<"lexxebot", _/binary>>) -> true;
is_bot_prefix(<<"linguee", _/binary>>) -> true;
is_bot_prefix(<<"linkwalker", _/binary>>) -> true;
is_bot_prefix(<<"lmspider", _/binary>>) -> true;
is_bot_prefix(<<"lwp-trivialWomlpeFactory", _/binary>>) -> true;
is_bot_prefix(<<"http://", _/binary>>) -> true;
is_bot_prefix(<<"magpie-", _/binary>>) -> true;
is_bot_prefix(<<"mnogosearch", _/binary>>) -> true;
is_bot_prefix(<<"mogimogi", _/binary>>) -> true;
is_bot_prefix(<<"moreoverbot", _/binary>>) -> true;
is_bot_prefix(<<"mvaclient", _/binary>>) -> true;
is_bot_prefix(<<"nessus:", _/binary>>) -> true;
is_bot_prefix(<<"newsgator", _/binary>>) -> true;
is_bot_prefix(<<"ng-search", _/binary>>) -> true;
is_bot_prefix(<<"nicebot", _/binary>>) -> true;
is_bot_prefix(<<"noxtrumbot", _/binary>>) -> true;
is_bot_prefix(<<"nusearch", _/binary>>) -> true;
is_bot_prefix(<<"nutch", _/binary>>) -> true;
is_bot_prefix(<<"nymesis", _/binary>>) -> true;
is_bot_prefix(<<"oegpomgilibot", _/binary>>) -> true;
is_bot_prefix(<<"omgilibot", _/binary>>) -> true;
is_bot_prefix(<<"omniexplorer", _/binary>>) -> true;
is_bot_prefix(<<"oozbot", _/binary>>) -> true;
is_bot_prefix(<<"orbiter", _/binary>>) -> true;
is_bot_prefix(<<"pagebites", _/binary>>) -> true;
is_bot_prefix(<<"pixray", _/binary>>) -> true;
is_bot_prefix(<<"polybot", _/binary>>) -> true;
is_bot_prefix(<<"pompos", _/binary>>) -> true;
is_bot_prefix(<<"postpost", _/binary>>) -> true;
is_bot_prefix(<<"psbot", _/binary>>) -> true;
is_bot_prefix(<<"pycurl", _/binary>>) -> true;
is_bot_prefix(<<"qseero", _/binary>>) -> true;
is_bot_prefix(<<"radian", _/binary>>) -> true;
is_bot_prefix(<<"rampy", _/binary>>) -> true;
is_bot_prefix(<<"rufus", _/binary>>) -> true;
is_bot_prefix(<<"sandcrawler", _/binary>>) -> true;
is_bot_prefix(<<"sbider", _/binary>>) -> true;
is_bot_prefix(<<"scrubby", _/binary>>) -> true;
is_bot_prefix(<<"searchsight", _/binary>>) -> true;
is_bot_prefix(<<"seekbot", _/binary>>) -> true;
is_bot_prefix(<<"semanticdiscovery", _/binary>>) -> true;
is_bot_prefix(<<"sensis", _/binary>>) -> true;
is_bot_prefix(<<"seznambot", _/binary>>) -> true;
is_bot_prefix(<<"shim-", _/binary>>) -> true;
is_bot_prefix(<<"shopwiki", _/binary>>) -> true;
is_bot_prefix(<<"silk", _/binary>>) -> true;
is_bot_prefix(<<"snappy", _/binary>>) -> true;
is_bot_prefix(<<"sogue", _/binary>>) -> true;
is_bot_prefix(<<"soso", _/binary>>) -> true;
is_bot_prefix(<<"sqworm", _/binary>>) -> true;
is_bot_prefix(<<"synoobot", _/binary>>) -> true;
is_bot_prefix(<<"terrawiz", _/binary>>) -> true;
is_bot_prefix(<<"thesubot", _/binary>>) -> true;
is_bot_prefix(<<"thumbnail.cz", _/binary>>) -> true;
is_bot_prefix(<<"tineye", _/binary>>) -> true;
is_bot_prefix(<<"tra.cx.pider", _/binary>>) -> true;
is_bot_prefix(<<"truwogps", _/binary>>) -> true;
is_bot_prefix(<<"turnitin", _/binary>>) -> true;
is_bot_prefix(<<"twengabot", _/binary>>) -> true;
is_bot_prefix(<<"unwindfetchor", _/binary>>) -> true;
is_bot_prefix(<<"updated", _/binary>>) -> true;
is_bot_prefix(<<"updowner", _/binary>>) -> true;
is_bot_prefix(<<"vortex", _/binary>>) -> true;
is_bot_prefix(<<"voyager", _/binary>>) -> true;
is_bot_prefix(<<"vyu2", _/binary>>) -> true;
is_bot_prefix(<<"webcollage", _/binary>>) -> true;
is_bot_prefix(<<"websquash", _/binary>>) -> true;
is_bot_prefix(<<"womlpefactory", _/binary>>) -> true;
is_bot_prefix(<<"xaldon", _/binary>>) -> true;
is_bot_prefix(<<"yacybot", _/binary>>) -> true;
is_bot_prefix(<<"yahooseeker", _/binary>>) -> true;
is_bot_prefix(<<"yasakli", _/binary>>) -> true;
is_bot_prefix(<<"yeti", _/binary>>) -> true;
is_bot_prefix(<<"yooglifetchagent", _/binary>>) -> true;
is_bot_prefix(<<"zao", _/binary>>) -> true;
is_bot_prefix(<<"zspider", _/binary>>) -> true;
is_bot_prefix(_) -> false.

bots() -> [
    % The usual suspects
    <<"pingdom">>,
    <<"googlebot">>,
    <<"msnbot">>,
    <<"bingbot">>,
    <<"baiduspider">>,
    <<"ask jeeves">>,
    <<"curl">>,
    <<"wget">>,
    <<"archive.org_bot">>,

    % Catch alls on some substrings for the lesser known bots
    <<"crawl">>,
    <<"bot">>,
    <<"spider">>,

    % Bots we have seen crawling and don't have bot/spider
    % in their name
    <<"arachmo">>,
    <<"aspseek">>,
    <<"altavista">>,  % The past is calling...
    <<"b-l-i-t-z-b-o-t">>,
    <<"charlotte">>,
    <<"codeguard">>,
    <<"dloader">>,
    <<"grub-client">>,
    <<"heritrix">>,
    <<"havij">>,
    <<"holmes">>,
    <<"htdig">>,
    <<"httrack">>,
    <<"imageshereimagesthereimageseverywhere">>,
    <<"linklink">>,
    <<"; mail.ru/">>,
    <<"netcraft web server survey">>,
    <<"outbrain">>,
    <<"search.ch">>,
    <<"slurp">>,
    <<"scooter">>,
    <<"scoutjet">>,
    <<"scrubby">>,
    <<"sleuth">>,
    <<"sucuri">>,
    <<"t-h-u-n-d-e-r-s-t-o-n-e">>,
    <<"ultraseek">>,
    <<"vagabondo">>,
    <<"yahoo! slurp">>,  % Still alive?

    % Quite often these are malicious
    % <<"w3crobot">>,
    % <<"dumbot">>,
    % <<"faxobot">>,
    <<"nutscape">>,
    <<"psycheclone">>
    ].
