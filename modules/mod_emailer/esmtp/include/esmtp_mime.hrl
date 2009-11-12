
%% @copyright Geoff Cant
%% @author Geoff Cant <nem@erlang.geek.nz>
%% @version {@vsn}, {@date} {@time}
%% @doc Mail Mime library headers and record definitions.
%% @end

-ifndef(esmtp_mime).
-define(esmtp_mime, true).

-record(mime_msg, {headers = [], boundary, parts = []}).
-record(mime_part, {type,
                    encoding = {"8bit", "text/plain","utf-8"},
                    name,
                    data}).

-endif.
