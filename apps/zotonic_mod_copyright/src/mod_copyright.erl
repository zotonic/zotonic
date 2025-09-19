%% @copyright 2024 Driebit BV
%% @doc Add a panel to the admin to manage the copyright and
%% attribution of resources.
%% @end

%% Copyright 2024 Driebit BV
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

-module(mod_copyright).
-moduledoc("
Todo

Not yet documented.
").

-mod_title("Copyright").
-mod_description("Add copyrights and attribution to resources").
-mod_prio(500).
-mod_config([
        #{
            key => rights,
            type => string,
            default => "CR",
            description => "The default copyrights for this site. Defaults to CR (All Rights Reserved)."
        },
        #{
            key => attribution,
            type => string,
            default => "",
            description => "The default copyright attribution for this site. Defaults to the title of the site."
        },
        #{
            key => year,
            type => integer,
            default => "",
            description => "The default copyright year for this site. Defaults to the current year."
        }
    ]).

-export([
]).

