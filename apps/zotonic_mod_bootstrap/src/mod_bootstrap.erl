%% @author Andreas Stenius <git@astekk.se>
%% @copyright 2012-2026 Andreas Stenius
%% @doc Bootstrap provides simple and flexible HTML, CSS, and Javascript for popular user interface
%% components and interactions.
%% @end

%% Copyright 2012-2026 Andreas Stenius
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

-module(mod_bootstrap).
-moduledoc("
Adds support for the [Bootstrap](https://getbootstrap.com/) CSS / JavaScript framework.

The bundled `bootstrap.css` includes Bootstrap 5 and a Bootstrap 3 compatibility
layer for older templates. Sites that include their own Bootstrap 5 build can
include `/lib/bootstrap/css/bootstrap3-compat.css` after their Bootstrap 5 CSS
to add the same compatibility layer separately. For production, either include
`/lib/bootstrap/css/bootstrap3-compat.min.css` directly or use the `{% lib ...
minify %}` option with `/lib/bootstrap/css/bootstrap3-compat.css`.

Also provides helper templates/components for Bootstrap integration.
").
-author("Andreas Stenius <git@astekk.se>").

-mod_title("Bootstrap framework").
-mod_description("Bootstrap provides simple and flexible HTML, CSS, and Javascript for popular user interface components and interactions.").
-mod_prio(900).
-mod_depends([base]).
-mod_provides([bootstrap]).
