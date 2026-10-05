%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2026 Marc Worrell
%% @doc Model for tree translation dialogs, language counts, and background jobs.
%% @end

%% Copyright 2026 Marc Worrell
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

-module(m_translation_tree).
-author("Marc Worrell <marc@worrell.nl>").
-moduledoc("
Tree translation API. `get/<id>` returns language counts, `get/status/<id>` returns
job progress and `post/<id>` starts a job. Reads require edit access to the root;
tree traversal requires visibility of every member. Each page must still be editable
when the worker reaches it; otherwise it is skipped. Trees include their root and
unique menu/haspart descendants. No caller privileges are elevated.

The `bulk_dialog` postback accepts an `ids` list for bulk-update selections. It checks
edit access to every selected page, deduplicates the IDs, and stores a selection for
one day. Its `bulk-` token can be used instead of a resource ID on the same model
paths. Only the authenticated user who created the selection can access it. Bulk
jobs process exactly the selected pages, without traversing their descendants, and
check each page's edit permission again before changing it.

For example, `m.translation_tree[123]` returns the following data for a tree with
three unique pages, two available in English and all three available in Dutch:

```erlang
#{total => 3, languages => [{en, 2}, {nl, 3}]}
```

The total includes the root. Language counts can overlap because a page can have
multiple languages. The model callback wraps this data as `{ok, {Data, Rest}}`,
where `Rest` is the unused model path.

`m.translation_tree.status[123]` can return this progress map:

```erlang
#{root => 123,
  job => <<\"example-job-id\">>,
  state => running,
  operation => <<\"translate\">>,
  total => 3,
  done => 2,
  skipped => 1,
  failed => 0}
```

`done` counts all processed pages, including skipped and failed pages. `total` is
zero until traversal finishes. The state becomes `complete` when every page has
been processed, even if individual pages failed; `failed` as a state indicates a
job-level failure. With no known job the response is `#{root => 123, state => idle}`.
A successful `post/<id>` returns `{ok, Progress}` as soon as the job starts.

The `event/2` handler opens the translation dialog using a signed `dialog` postback
with an `id` argument identifying the tree root. For example:

```django
{% button text=_\"Translate all pages\"
    postback={dialog id=tree_id} delegate=\"m_translation_tree\" %}
```

The handler checks edit access to the root and visibility of the tree, then renders
`_dialog_translation_tree.tpl` with the resolved `id` and the language-count map as
`tree`. If these checks fail, it displays an error growl. Opening the dialog does
not start a job; the dialog submits a confirmed operation through `post/<id>`.
").

-behaviour(zotonic_model).

-export([
    m_get/3,
    m_post/3,
    event/2,
    details/2,
    ids/2,
    selection/2
]).

-include_lib("zotonic_core/include/zotonic.hrl").

%% @doc Read tree language counts or job progress after checking edit access to the root.
m_get([<<"status">>, Id | Rest], _Msg, Context) ->
    with_editable(Id, fun(Root) ->
        {ok, {translation_tree:status(Root, Context), Rest}}
    end, Context);
m_get([Id | Rest], _Msg, Context) ->
    case details(Id, Context) of
        {ok, Details} -> {ok, {Details, Rest}};
        Error -> Error
    end;
m_get(_, _, _) -> {error, unknown_path}.

%% @doc Validate a confirmed tree operation and return as soon as its sidejob starts.
m_post([Id], #{ payload := Options }, Context) when is_map(Options) ->
    with_editable(Id, fun(Root) ->
        case operation(Options, Context) of
            {ok, Operation} -> translation_tree:start(Root, Operation, Context);
            Error -> Error
        end
    end, Context);
m_post(_, _, _) ->
    {error, badarg}.

%% @doc Open the tree translation dialog for a signed <tt>{dialog, Args}</tt> postback.
%% Args is a proplist with an <tt>id</tt> resource reference for the tree root. Check root
%% edit access and tree visibility before rendering <tt>_dialog_translation_tree.tpl</tt>
%% with the resolved <tt>id</tt> and language counts as <tt>tree</tt>. On failure, show an error
%% growl. <tt>{bulk_dialog, Args}</tt> takes an <tt>ids</tt> list and opens the same dialog for
%% exactly those pages, using a user-bound selection token. Return the updated
%% render context; neither handler starts a job.
event(#postback{message = {bulk_dialog, Args}}, Context) ->
    case selection(proplists:get_value(ids, Args), Context) of
        {ok, Token} ->
            case details(Token, Context) of
                {ok, Details} ->
                    z_render:dialog(?__("Manage translations", Context), "_dialog_translation_tree.tpl",
                        [{id, Token}, {is_bulk, true}, {tree, Details}], Context);
                {error, _} ->
                    z_render:growl_error(?__("You are not allowed to edit these pages.", Context), Context)
            end;
        {error, _} ->
            z_render:growl_error(?__("You are not allowed to edit these pages.", Context), Context)
    end;
event(#postback{message = {dialog, Args}}, Context) ->
    Id = proplists:get_value(id, Args),
    case details(Id, Context) of
        {ok, Details} ->
            z_render:dialog(
                ?__("Translate all pages", Context),
                "_dialog_translation_tree.tpl",
                [
                    {id, m_rsc:rid(Id, Context)},
                    {tree, Details}
                ],
                Context);
        {error, _} ->
            z_render:growl_error(?__("You are not allowed to edit this tree.", Context), Context)
    end.

%% @doc Return the unique page count and per-language page counts for an editable tree.
-spec details(Id, Context) -> {ok, map()} | {error, term()}
    when
        Id :: m_rsc:resource(),
        Context :: z:context().
details(Id, Context) ->
    with_editable(Id, fun(Root) ->
        case ids(Root, Context) of
            {ok, Ids} ->
                Counts = lists:foldl(fun(RscId, Acc) ->
                    lists:foldl(fun(Lang, A) ->
                        maps:update_with(Lang, fun(N) -> N + 1 end, 1, A)
                    end, Acc, translation_tree:languages(RscId, Context))
                end, #{}, Ids),
                {ok, #{
                    total => length(Ids),
                    languages => lists:sort(maps:to_list(Counts))
                }};
            Error -> Error
        end
    end, Context).

%% @doc Traverse visible pages, including read-only pages. The worker checks edit access
%% separately before processing each page. Bulk tokens return exactly the selected
%% visible pages, without traversing their descendants.
-spec ids(Id, Context) -> {ok, [m_rsc:resource_id()]} | {error, term()}
    when
        Id :: m_rsc:resource_id() | binary(),
        Context :: z:context().
ids(<<"bulk-", _/binary>> = Token, Context) ->
    case selection_ids(Token, Context) of
        {ok, Ids} ->
            case lists:all(fun(Id) -> z_acl:rsc_visible(Id, Context) end, Ids) of
                true -> {ok, Ids};
                false -> {error, eacces}
            end;
        Error -> Error
    end;
ids(Id, Context) ->
    walk([Id], #{}, Context).

%% @doc Traverse menu and haspart descendants once, failing if a page is missing or invisible.
walk([], Seen, _Context) ->
    {ok, lists:sort(maps:keys(Seen))};
walk([Id | Rest], Seen, Context) when is_map_key(Id, Seen) ->
    walk(Rest, Seen, Context);
walk([Id | Rest], Seen, Context) ->
    case m_rsc:exists(Id, Context) andalso z_acl:rsc_visible(Id, Context) of
        true ->
            MenuIds = menu_ids(m_rsc:p(Id, <<"menu">>, Context)),
            Parts = m_edge:objects(Id, haspart, Context),
            Children = lists:filtermap(fun(R) ->
                case m_rsc:rid(R, Context) of
                    undefined -> false;
                    Rid -> {true, Rid}
                end
            end, MenuIds ++ Parts),
            walk(Children ++ Rest, Seen#{Id => true}, Context);
        false ->
            {error, eacces}
    end.

%% @doc Flatten supported menu representations into resource references.
menu_ids([#rsc_tree{id = Id, tree = Tree} | Rest]) ->
    [Id | menu_ids(Tree) ++ menu_ids(Rest)];
menu_ids([{Id, Tree} | Rest]) ->
    [Id | menu_ids(Tree) ++ menu_ids(Rest)];
menu_ids([Id | Rest]) when is_integer(Id); is_atom(Id); is_binary(Id) ->
    [Id | menu_ids(Rest)];
menu_ids(_) ->
    [].

%% @doc Authorize an editable root or an authenticated owner's bulk selection.
with_editable(<<"bulk-", _/binary>> = Token, Fun, Context) ->
    case selection_ids(Token, Context) of
        {ok, _} -> Fun(Token);
        Error -> Error
    end;
with_editable(Id, Fun, Context) when is_integer(Id); is_binary(Id); is_atom(Id) ->
    case m_rsc:rid(Id, Context) of
        undefined ->
            {error, enoent};
        Root ->
            case z_acl:rsc_editable(Root, Context) of
                true -> Fun(Root);
                false -> {error, eacces}
            end
    end;
with_editable(_, _, _) ->
    {error, badarg}.

%% @doc Create a user-bound selection of editable pages, without traversing descendants.
%% Reopening the same selection uses the same job key. Selections expire after one day.
-spec selection(Ids, Context) -> {ok, binary()} | {error, term()} when
    Ids :: [m_rsc:resource()], Context :: z:context().
selection([_ | _] = Refs, Context) ->
    User = z_acl:user(Context),
    Ids = lists:usort([case Id of
        _ when is_integer(Id); is_binary(Id); is_atom(Id) -> m_rsc:rid(Id, Context);
        _ -> undefined
    end || Id <- Refs]),
    case is_integer(User) andalso lists:all(fun(Id) ->
        is_integer(Id) andalso z_acl:rsc_editable(Id, Context)
    end, Ids) of
        true ->
            Hash = binary:encode_hex(crypto:hash(sha256, term_to_binary({User, Ids}))),
            Token = <<"bulk-", Hash/binary>>,
            z_depcache:set({?MODULE, Token}, {User, Ids}, 86400, Context),
            {ok, Token};
        false -> {error, eacces}
    end;
selection(_, _) -> {error, badarg}.

%% @doc Resolve a cached selection only for its authenticated owner.
selection_ids(Token, Context) ->
    User = z_acl:user(Context),
    case z_depcache:get({?MODULE, Token}, Context) of
        {ok, {User, Ids}} when is_integer(User) -> {ok, Ids};
        _ -> {error, eacces}
    end.

%% @doc Validate confirmed request options and normalize them into a worker operation.
operation(#{<<"method">> := <<"remove">>, <<"language">> := Lang, <<"confirmed">> := true}, _Context) when is_binary(Lang) ->
    case z_language:to_language_atom(Lang) of
        {ok, Code} -> {ok, {remove, Code}};
        _ -> {error, language}
    end;
operation(#{<<"method">> := Method, <<"src">> := Src, <<"dst">> := Dst, <<"confirmed">> := true} = Options, Context)
    when is_binary(Src), is_binary(Dst),
        (Method =:= <<"translate">> orelse Method =:= <<"copy">> orelse Method =:= <<"empty">>) ->
    case {z_language:to_language_atom(Src), z_language:to_language_atom(Dst)} of
        {{ok, From}, {ok, To}} when From =/= To ->
            case z_language:is_language_editable(From, Context)
                andalso z_language:is_language_editable(To, Context)
                andalso (Method =/= <<"translate">> orelse m_translation:has_translation_service(Context))
            of
                true -> {ok, {Method, From, To, z_convert:to_bool(maps:get(<<"overwrite">>, Options, false))}};
                false -> {error, language}
            end;
        _ ->
            {error, language}
    end;
operation(_, _) ->
    {error, badarg}.
