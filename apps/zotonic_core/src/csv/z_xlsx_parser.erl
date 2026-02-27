%% @author Marc Worrell <marc@worrell.nl>
%% @copyright Marc Worrell 2020-2025
%% @doc Parse a XLSX spreadsheet file. As unpacking unchecked zip files
%% can consume lots of memory, it is desirable to run this in a
%% separate process and limit the amount of memory that can be used
%% with: erlang:process_flag(max_heap_size, ...).
%% @end

%% Copyright 2020-2025 Marc Worrell
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

-module(z_xlsx_parser).

-export([
    parse_file/2,
    parse_file/1,
    fetch_worksheet/1
    ]).

-include_lib("zotonic_core/include/zotonic.hrl").

-define(DEFAULT_MAX_MEMORY, 160 * 1024 * 1024). % 160MB

%% @doc Parse a XLSX file with safety on the maximum amount of memory used. The parsing
%% is done in a separate process. Same as `parse_file/2' with no options.
-spec parse_file(file:filename_all()) -> {ok, Rows} | {error, term()} when
    Rows :: [ Row ],
    Row :: [ Cell ],
    Cell :: binary() | integer() | float() | undefined.
parse_file(File) ->
    parse_file(File, []).

%% @doc Parse a XLSX file with safety on the maximum amount of memory used. The parsing
%% is done in a separate process. Per default the memory used is limited to 160MB.
-spec parse_file(Filename, Options) -> {ok, Rows} | {error, term()} when
    Filename :: file:filename_all(),
    Options :: [ {max_memory, non_neg_integer()} ],
    Rows :: [ Row ],
    Row :: [ Cell ],
    Cell :: binary() | integer() | float() | undefined.
parse_file(Filename, Options) ->
    SpawnOpts = [
        {max_heap_size, proplists:get_value(max_memory, Options, ?DEFAULT_MAX_MEMORY)},
        monitor
    ],
    Self = self(),
    SelfRef = make_ref(),
    {_Pid, Ref} = z_proc:spawn_md_opt(
        fun() ->
            Result = do_parse_file(Filename),
            Self ! {SelfRef, Result}
        end,
        SpawnOpts),
    receive
        {SelfRef, Result} ->
            receive
                {'DOWN', Ref, process, _, _} ->
                    Result
            end;
        {'DOWN', Ref, process, _, Reason} ->
            {error, Reason}
    end.

%% @doc Parse the XLSX spreadsheet file. Note that the parsing is done in
%% memory, so the complete unzipped spreadsheet must fit in memory.
-spec do_parse_file(file:filename_all()) -> {ok, Rows} | {error, term()} when
    Rows :: [ Row ],
    Row :: [ Cell ],
    Cell :: binary() | integer() | float() | undefined.
do_parse_file(File) ->
    case fetch_worksheet(File) of
        {ok, #{ worksheet := notfound }} ->
            {error, noworksheet};
        {ok, #{ worksheet := WorksheetBin } = Parsed} ->
            Shared = parse_shared(Parsed),
            WorksheetBin1 = remove_empty_rows(WorksheetBin),
            case z_html_parse:parse(WorksheetBin1, #{ mode => xml, lowercase => true }) of
                {ok, WorksheetXML} ->
                    {MaxRows, MaxCols, Idx} = make_rowlist( parse_worksheet(WorksheetXML, Shared) ),
                    RowData = lists:map(
                        fun(NR) ->
                            lists:map(
                                fun(NC) ->
                                    maps:get({NR, NC}, Idx, <<>>)
                                end,
                                lists:seq(1, MaxCols))
                        end,
                        lists:seq(1, MaxRows)),
                    {ok, RowData};
                {error, _} = Error ->
                    Error
            end;
        {error, _} = Error ->
            Error
    end.

remove_empty_rows(Bin) ->
    iolist_to_binary(re:replace(Bin, <<"<row[^>]*/>">>, <<>>, [ global ])).

make_rowlist(Rows) ->
    lists:foldl(
        fun
            (Cells, Acc) ->
                lists:foldl(
                    fun({Loc, Data}, {MaxRow, MaxCol, Idx}) ->
                        {NR, NC} = Lnum = loc_to_num(Loc),
                        MaxRow1 = max(MaxRow, NR),
                        MaxCol1 = max(MaxCol, NC),
                        Idx1 = Idx#{ Lnum => Data },
                        {MaxRow1, MaxCol1, Idx1}
                    end,
                    Acc,
                    Cells)
        end,
        {0, 0, #{}},
        Rows).

loc_to_num(Loc) ->
    {Letters, Numbers} = lists:partition(
        fun(C) -> C >= $A andalso C =< $Z end,
        binary_to_list(Loc)),
    {list_to_integer(Numbers), letters_to_num(Letters)}.

letters_to_num(Letters) ->
    lists:foldl(
        fun(C, Acc) ->
            Acc * 26 + (C-$A+1)
        end,
        0,
        Letters).

parse_shared(#{ shared := SharedBin }) ->
    case z_html_parse:parse(SharedBin, #{ mode => xml, lowercase => true }) of
        {ok, {<<"sst">>, _, Elts}} ->
            {_, Shared} = lists:foldl(
                fun
                    ({<<"si">>, _, Ts}, {N, Acc}) ->
                        V = case lists:keyfind(<<"t">>, 1, Ts) of
                            {<<"t">>, _, T} -> text(T);
                            false -> <<>>
                        end,
                        {N+1, Acc#{ N => V}};
                    (_, NAcc) ->
                        NAcc
                end,
                {0, #{}},
                Elts),
            Shared;
        {error, _} ->
            #{}
    end;
parse_shared(_) ->
    #{}.

parse_worksheet({<<"worksheet">>, _Args, Elts}, Shared) ->
    case lists:filtermap(
        fun
            ({<<"sheetdata">>, _, SheetRows}) ->
                {true, extract_rows(SheetRows, Shared)};
            (_) ->
                false
        end,
        Elts)
    of
        [ Data | _ ] ->
            Data;
        [] ->
            ?LOG_WARNING(#{
                in => zotonic_core,
                text => <<"No sheetdata in XLSX worksheet">>,
                result => error,
                reason => sheetdata_missing
            }),
            []
    end.

extract_rows(Rs, Shared) ->
    lists:filtermap(
        fun
            ({<<"row">>, _, _} = R) -> {true, extract_row(R, Shared)};
            (_) -> false
        end,
        Rs).

extract_row({<<"row">>, _RowArgs, Cells}, Shared) ->
    lists:filtermap(
        fun
            ({<<"c">>, Args, CellData}) ->
                Data = map_cell(proplists:get_value(<<"t">>, Args), Args, CellData, Shared),
                {true, {proplists:get_value(<<"r">>, Args), Data}};
            (_) ->
                false
        end,
        Cells).

map_cell(<<"inlineStr">>, _CellArgs, CellData, _Shared) ->
    % Text
    case lists:keyfind(<<"is">>, 1, CellData) of
        {<<"is">>, _, IsData} when is_list(IsData) ->
            text(IsData);
        false ->
            <<>>
    end;
map_cell(<<"s">>, _CellArgs, CellData, Shared) ->
    % String from the shared string data
    case lists:keyfind(<<"v">>, 1, CellData) of
        {<<"v">>, _, VData} ->
            maps:get(binary_to_integer(text(VData)), Shared, <<>>);
        false ->
            <<>>
    end;
map_cell(<<"str">>, _CellArgs, CellData, _Shared) ->
    % Formula with string result
    case lists:keyfind(<<"v">>, 1, CellData) of
        {<<"v">>, _, VData} ->
            text(VData);
        false ->
            <<>>
    end;
map_cell(undefined, _CellArgs, CellData, _Shared) ->
    case lists:keyfind(<<"v">>, 1, CellData) of
        {<<"v">>, _, VData} -> text(VData);
        false -> <<>>
    end.

text(B) when is_binary(B) ->
    B;
text(Rs) ->
    text(Rs, <<>>).

text([], Acc) ->
    Acc;
text([{_, _, Rs} | Rest], Acc) ->
    Acc1 = text(Rs, Acc),
    text(Rest, Acc1);
text([ B | Rest ], Acc) when is_binary(B) ->
    text(Rest, <<Acc/binary, B/binary>>).

fetch_worksheet(File) ->
    zip:foldl(
        fun
            ("xl/worksheets/sheet1.xml", _GetInfo, GetBin, Acc) ->
                Acc#{ worksheet => GetBin() };
            ("xl/sharedStrings.xml", _GetInfo, GetBin, Acc) ->
                Acc#{ shared => GetBin() };
            (_FileInArchive, _GetInfo, _GetBin, AccIn) ->
                AccIn
        end,
        #{},
        unicode:characters_to_list(File)).
