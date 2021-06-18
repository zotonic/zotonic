%% @hidden

-module(z_email_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("zotonic.hrl").


receive_email_test() ->
    {timeout, 20, fun() ->
        Context = z_context:new(zotonic_site_testsandbox, en),
        set_relay_config(Context),
        z_notifier:observe(email_test, self(), Context),
        {ok, Recipient} = z_notifier:first(
            #email_add_handler{
                notification = email_test,
                user_id = 1,
                resource_id = 1
            },
            Context),
        To = <<(z_convert:to_binary(Recipient))/binary, "@localhost">>,
        OutMail = #email{
            to = To,
            subject = <<"Test">>, 
            text = <<"Hello World">>,
            attachments = [
                #upload{
                    filename = <<"test.dat">>,
                    data = <<"testdata">>
                }
            ]
        },
        {ok, _} = z_email:send(OutMail, Context),
        receive
             {'$gen_cast',{{email_test, received, 1, 1, Received}, _ContextEmail}} ->
                Email = Received#email_received.email,
                <<"Test">> = Email#email.subject,
                <<"Hello World">> = Email#email.text,
                Upload = hd(Email#email.attachments),
                <<"test.dat">> = Upload#upload.filename,
                <<"testdata">> = Upload#upload.data,
                ok;
            X ->
                ?DEBUG(X),
                throw({unexpected, X})
        end
    end}.

set_relay_config(Context) ->
    application:set_env(zotonic, smtp_relay, false),
    application:set_env(zotonic, email_override, ""),
    m_config:set_value(site, smtp_relay, <<"1">>, Context),
    m_config:set_value(site, smtp_relay_host, <<"127.0.0.1">>, Context),
    m_config:set_value(site, smtp_relay_port, <<"2525">>, Context),
    m_config:set_value(site, smtp_relay_ssl, <<>>, Context).
