%% @hidden

-module(z_email_server_tests).

-include_lib("eunit/include/eunit.hrl").
-include("../include/zotonic.hrl").

%% @doc Test that reply_to_message_id/2 returns a properly formatted RFC822
%% mailbox address containing "reply+<MessageId>@<domain>".
reply_to_message_id_test_() ->
    {timeout, 20,
     {setup,
      fun() ->
          Context = z_context:new(zotonic_site_testsandbox, en),
          OrigTitle = m_config:get_value(site, title, Context),
          m_config:set_value(site, title, <<"Test Site">>, Context),
          {Context, OrigTitle}
      end,
      fun({Context, OrigTitle}) ->
          % Restore the original site title after the test.
          case OrigTitle of
              undefined -> m_config:delete(site, title, Context);
              _ -> m_config:set_value(site, title, OrigTitle, Context)
          end
      end,
      fun({Context, _OrigTitle}) ->
          fun() ->
              MessageId = <<"abc123def">>,
              Domain = z_email:email_domain(Context),
              ReplyLocal = <<"reply+", MessageId/binary, "@", Domain/binary>>,
              ReplyMailbox = <<"<", ReplyLocal/binary, ">">>,
              Result = z_email_server:reply_to_message_id(MessageId, Context),
              % The result must be a binary.
              ?assertMatch(<<_/binary>>, Result),
              % The reply address must contain the message-id and the email domain.
              ?assertMatch({_, _}, binary:match(Result, ReplyLocal)),
              % The result must be RFC822-formatted (angle-bracket notation).
              ?assertMatch({_, _}, binary:match(Result, ReplyMailbox)),
              % The name part must appear in the result.
              ?assertMatch({_, _}, binary:match(Result, <<"Test Site">>))
          end
      end}}.
