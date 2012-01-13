%% @author Arjan Scherpenisse <arjan@scherpenisse.net>
%% @hidden

-module(z_lib_include_tests).

-include_lib("zotonic.hrl").

-include_lib("eunit/include/eunit.hrl").


uncollapse_test() ->
    ?assertEqual([],
                 z_lib_include:uncollapse("")),

    ?assertEqual(["/a/b1.js","/a/b2.js","/a/b/c.js","/a/b3.js"],
                 z_lib_include:uncollapse("/a/b1~b2~b/c~/a/b3~63415422477.js")),
    ?assertEqual(["/a/b1.js"],
                 z_lib_include:uncollapse("/a/b1~63415422477.js")),
    ?assertEqual(["/a1.js","/a2.js"],
                 z_lib_include:uncollapse("/a1~a2~63415422477.js")),
    ok.


tag_test() ->
    C = z_context:new(testsandbox),
    ?assertEqual([_LinkElem = [], _ScriptElem = []],
                 z_lib_include:tag([], C)),
    ?assertEqual([_LinkElem = [], _ScriptElem = []],
                 z_lib_include:tag(["/images/test.jpg"], C)),

    ?assertEqual([[], <<"<script src=\"/lib/js/a~62167258800.js\" type=\"text/javascript\"></script>">>],
                 z_lib_include:tag(["/js/a.js"], C)),
    ?assertEqual([[], <<"<script src=\"/lib/js/a~b~62167258800.js\" type=\"text/javascript\"></script>">>],
                 z_lib_include:tag(["/js/a.js", "/js/b.js"], C)),

    ?assertEqual([<<"<link href=\"/lib/css/a~62167258800.css\" type=\"text/css\" media=\"all\" rel=\"stylesheet\" />">>, 
                  <<"<script src=\"/lib/js/b~62167258800.js\" type=\"text/javascript\"></script>">>],
                 z_lib_include:tag(["/css/a.css", "/js/b.js"], C)).





