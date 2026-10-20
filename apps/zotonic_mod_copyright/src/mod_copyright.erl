%% @copyright 2024-2026 Driebit BV
%% @doc Add a panel to the admin to manage the copyright and
%% attribution and provenance of resources.
%% @end

%% Copyright 2024-2026 Driebit BV
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
-moduledoc(#{
    zotonic_keywords => [
        "reference", "content_editor", "module", "copyright_and_licensing", "media_resource", "metadata"
    ]
}).
-moduledoc("
Module for copyright metadata, rights statements, attribution, and source provenance.

The admin panel manages `rights`, `rights_attribution`, `rights_year`,
`provenance_html`, and `provenance_url` on each resource. Attribution is the
credit line; provenance describes where the material came from, such as a
collection, archive reference, or publication.

`provenance_html` is optional rich text and supports the resource's languages.
Templates use the normal resource language fallback when a translation is missing.
`provenance_url` is an optional, language-independent link to the original source
record. The standard resource property handling sanitizes HTML and URLs.

Include `{% include \"_provenance.tpl\" id=id %}` to display source provenance.
It is separate from `_copyright.tpl` and has no site-wide default values.

").

-mod_title("Copyright").
-mod_description("Add copyrights, attribution and provenance to resources").
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
