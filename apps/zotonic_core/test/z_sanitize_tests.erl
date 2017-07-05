-module(z_sanitize_tests).

-include_lib("eunit/include/eunit.hrl").

youtube_test() ->
	Context = z_context:new(zotonic_site_testsandbox),
	In  = <<"<iframe width=\"560\" height=\"315\" src=\"//www.youtube.com/embed/2RXp3r2gb3A\" frameborder=\"0\" allowfullscreen></iframe>">>,
	Out = <<"<iframe src=\"//www.youtube.com/embed/2RXp3r2gb3A\" width=\"560\" height=\"315\" frameborder=\"0\" allowfullscreen=\"allowfullscreen\"></iframe>">>,
	?assertEqual(Out, z_sanitize:html(In, Context)).

youtube_object_test() ->
	Context = z_context:new(zotonic_site_testsandbox),
	In  = <<"<object data=\"http://www.youtube.com/embed/dQw4w9WgXcQ\" width=\"560\" height=\"315\"></object>">>,
	Out = <<"<iframe width=\"560\" height=\"315\" allowfullscreen=\"1\" frameborder=\"0\" src=\"https://www.youtube.com/embed/dQw4w9WgXcQ\"></iframe>">>,
	?assertEqual(Out, z_sanitize:html(In, Context)).

mso1_test() ->
    Context = z_context:new(zotonic_site_testsandbox),
    In = <<"Hello <!-- [if foo]...[endif]--> World">>,
    Out = <<"Hello  World">>,
    ?assertEqual(Out, z_sanitize:html(In, Context)).

mso2_test() ->
    Context = z_context:new(zotonic_site_testsandbox),
    In = <<"Hello <!--StartFragment--> <!--EndFragment--> World">>,
    Out = <<"Hello   World">>,
    ?assertEqual(Out, z_sanitize:html(In, Context)).

mso3_test() ->
    Context = z_context:new(zotonic_site_testsandbox),
    In = <<"<p class=\"MsoNormal\"><span style=\"mso-ansi-language: EN-US;\">Hello</span></p>">>,
    Out = <<"<p><span>Hello</span></p>">>,
    ?assertEqual(Out, z_sanitize:html(In, Context)).

mso4_test() ->
    Context = z_context:new(zotonic_site_testsandbox),
    In = <<"<p class=\"MsoNormal other-class\"><span style=\"mso-ansi-language: EN-US;\">Hello</span></p>">>,
    Out = <<"<p class=\"other-class\"><span>Hello</span></p>">>,
    ?assertEqual(Out, z_sanitize:html(In, Context)).

zmedia_test() ->
    Context = z_context:new(zotonic_site_testsandbox),
    In = <<"<!-- z-media 123 { \"align\":\"leftx\", \"caption\":\"&--\\u003e\" } -->">>,
    Out = <<"<!-- z-media 123 {\"align\":\"block\",\"caption\":\"&\\u2192\"} -->">>,
    ?assertEqual(Out, z_sanitize:html(In, Context)).

svg_imagetragick_test() ->
    A = z_svg:sanitize(<<"
<?xml version=\"1.0\" standalone=\"no\"?>
<!DOCTYPE svg PUBLIC \"-//W3C//DTD SVG 1.1//EN\" \"http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd\">
<svg width=\"800\" height=\"600\"
    xmlns:svg=\"http://www.w3.org/2000/svg\" version=\"1.2\"
    xmlns:xlink=\"http://www.w3.org/1999/xlink\">
    <image width=\"800\" height=\"600\" xlink:href=\"https://upload.wikimedia.org/wikipedia/commons/c/ca/Triple-Spiral-4turns_green_transparent.png\"></image>
</svg>
">>),
    ?assertEqual(nomatch, binary:match(A, <<"upload.wikimedia">>)),

    B = z_svg:sanitize(<<"
<?xml version=\"1.0\" standalone=\"no\"?>
<!DOCTYPE svg PUBLIC \"-//W3C//DTD SVG 1.1//EN\" \"http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd\">
<svg width=\"800\" height=\"600\"
    xmlns:svg=\"http://www.w3.org/2000/svg\" version=\"1.2\"
    xmlns:xlink=\"http://www.w3.org/1999/xlink\">
    <image width=\"800\" height=\"600\" clip-path=\"url(http://example.com)\"</image>
</svg>
">>),
    ?assertEqual(nomatch, binary:match(B, <<"example.com">>)),

    C = z_svg:sanitize(<<"
<?xml version=\"1.0\" standalone=\"no\"?>
<!DOCTYPE svg PUBLIC \"-//W3C//DTD SVG 1.1//EN\" \"http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd\">
<svg width=\"800\" height=\"600\"
    xmlns:svg=\"http://www.w3.org/2000/svg\" version=\"1.2\"
    xmlns:xlink=\"http://www.w3.org/1999/xlink\">
    <image width=\"800\" height=\"600\" clip-path=\"url(#foobar)\"</image>
</svg>
">>),
    ?assertMatch({_,_}, binary:match(C, <<"#foobar">>)),
    ok.
