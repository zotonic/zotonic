%% @doc Track imported data, used to check on duplicate imports and track what is imported.
%% @author Marc Worrell <marc@worrell.nl>

%% Copyright 2015 Marc Worrell
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

-module(m_import_csv_data).
-author("Marc Worrell <marc@worrell.nl>").

-export([
    get/2,
    update/5,
    install/2
    ]).

-include_lib("zotonic.hrl").

-spec get(integer(), #context{}) -> undefined | list().
get(Id, Context) ->
    z_db:assoc_row("select * from import_csv_data where id = $1", [Id], Context).


update(Id, Checksum, RowData, RscData, Context) ->
    z_db:transaction(fun(Ctx) ->
                    case z_db:q1("select id from import_csv_data where id = $1", [Id], Ctx) of
                        Id ->
                            z_db:q("update import_csv_data
                                    set checksum = $1,
                                        row_data = $2,
                                        rsc_data = $3,
                                        modified = now()
                                    where id = $4",
                                    [Checksum, ?DB_PROPS(RowData), ?DB_PROPS(RscData), Id],
                                    Ctx);
                        undefined ->
                            z_db:q("insert into import_csv_data
                                      (checksum, row_data, rsc_data, id)
                                    values
                                      ($1, $2, $3, $4)",
                                    [Checksum, ?DB_PROPS(RowData), ?DB_PROPS(RscData), Id],
                                    Ctx)
                    end
               end,
               Context).


install(_Vsn, Context) ->
    case z_db:table_exists(import_csv_data, Context) of
        false ->
            [] = z_db:q("
                create table import_csv_data (
                    id integer not null,
                    checksum bytea,
                    row_data bytea,
                    rsc_data bytea,
                    created timestamp with time zone NOT NULL DEFAULT now(),
                    modified timestamp with time zone NOT NULL DEFAULT now(),

                    primary key (id),
                    constraint fk_import_data_id foreign key (id) references rsc(id)
                        on update cascade
                        on delete cascade
                )
                ", Context),
            z_db:flush(Context),
            ok;
        true ->
            ok
    end.
