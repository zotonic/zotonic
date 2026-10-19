%% @copyright 2026 Marc Worrell
%% @doc Readable labels for module topic slugs, without a taxonomy file.
-module(filter_module_topic).
-moduledoc("Convert a topic slug such as `media_management` to `Media management`.").
-export([module_topic/2]).

-spec module_topic(Topic, Context) -> Label when
    Topic :: atom() | binary() | string() | undefined,
    Context :: z:context(),
    Label :: binary().
module_topic(undefined, _Context) ->
    <<>>;
module_topic(Topic, _Context) ->
    Text = binary:replace(z_convert:to_binary(Topic), <<"_">>, <<" ">>, [global]),
    case Text of
        <<First/utf8, Rest/binary>> ->
            <<(z_string:to_upper(<<First/utf8>>))/binary, Rest/binary>>;
        <<>> ->
            <<>>
    end.
