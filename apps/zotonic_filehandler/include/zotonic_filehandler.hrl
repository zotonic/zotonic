

%% Called with zotonic_notifier:first. Maps a file to a category {Application, Name}
-record(zotonic_filehandler_categorize, {
    filename :: binary()
}).

%% Called with zotonic_notifier:first. Maps a categorized file to a list of actions.
-record(zotonic_filehandler_map, {
    verb :: zotonic_filewatcher:verb(),
    application :: module(),
    what :: term(),
    extension :: binary(),
    root :: binary(),
    split :: list(binary()),
    filename :: binary()
}).

%% @doc File changed, picked up by zotonic core for livereload by mod_development
-record(zotonic_filehandler_filechange, {
    verb :: modify | create | delete,
    file :: binary(),
    basename :: binary(),
    extension :: binary()
}).

