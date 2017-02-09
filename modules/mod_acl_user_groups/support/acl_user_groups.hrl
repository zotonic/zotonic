%% @copyright 2017 Marc Worrell
%% @doc Internal definitions for mod_acl_user_groups

%% Copyright 2017 Marc Worrell
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

%% @doc Privacy pre-defined values

-define(ACL_PRIVACY_PUBLIC, 0).
-define(ACL_PRIVACY_MEMBER, 10).
-define(ACL_PRIVACY_USER_GROUP, 20).
-define(ACL_PRIVACY_COLLAB_MEMBER, 30).
-define(ACL_PRIVACY_COLLAB_MANAGER, 40).
-define(ACL_PRIVACY_PRIVATE, 50).
