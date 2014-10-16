-module(z_sanitize_test).

-include_lib("eunit/include/eunit.hrl").

youtube_test() ->
	Context = z_context:new(zotonic_status),
	In  = <<"<iframe width=\"560\" height=\"315\" src=\"//www.youtube.com/embed/2RXp3r2gb3A\" frameborder=\"0\" allowfullscreen></iframe>">>,
	Out = <<"<iframe src=\"//www.youtube.com/embed/2RXp3r2gb3A\" width=\"560\" height=\"315\" frameborder=\"0\" allowfullscreen=\"allowfullscreen\"></iframe>">>,
	?assertEqual(Out, z_sanitize:html(In, Context)).

youtube_object_test() ->
	Context = z_context:new(zotonic_status),
	In  = <<"<object data=\"http://www.youtube.com/embed/dQw4w9WgXcQ\" width=\"560\" height=\"315\"></object>">>,
	Out = <<"<iframe width=\"560\" height=\"315\" allowfullscreen=\"1\" frameborder=\"0\" src=\"https://www.youtube.com/embed/dQw4w9WgXcQ\"></iframe>">>,
	?assertEqual(Out, z_sanitize:html(In, Context)).

