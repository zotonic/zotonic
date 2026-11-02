{#
 # Shown on the mod_admin_config email configuration page
 #}
<div class="widget">
    <div class="widget-header">
        {_ Zotonic Relay _}
        <span class="text-muted pull-right">mod_email_relay</span>
    </div>
    <div class="widget-content">
        <p>
            {_ Use another Zotonic server to deliver outgoing email, or let this server deliver email on behalf of other Zotonic servers. This allows multiple sites or installations to share one server configured for email delivery. _}
        </p>
        <p>
            {_ To send through another server, enter its relay API URL and a shared secret below. The secret must match the receiving secret configured on that server. To accept email from other servers, set a receiving secret here and configure those servers to use this server’s relay API URL and the same secret. _}
        </p>

        {% wire type="submit"
                id="relay_settings"
                postback={config_save module=`mod_email_relay`}
                delegate=`mod_admin_config`
        %}
        <form id="relay_settings" class="form" action="postback">
            <fieldset>
                <legend>{_ Send emails via another Zotonic server _}</legend>
                <div class="form-group">
                    <label class="control-label">
                        <input type="checkbox" name="is_email_relay" value="1"
                            {% if m.config.mod_email_relay.is_email_relay.value %}checked{% endif %}>
                        {_ Relay email via another Zotonic server _}
                    </label>
                </div>
                <div class="form-group label-floating">
                    <input type="text" class="form-control" name="email_relay_url" value="{{ m.config.mod_email_relay.email_relay_url.value|escape }}" placeholder="{_ URL for the email relay API _}">
                    <label class="control-label">{_ URL for the email relay API _}</label>
                    <p class="help-block">{_ Complete URL of the API on the relaying Zotonic server. _}</p>
                </div>
                <div class="form-group">
                    <label class="control-label" for="email_relay_send_secret">{_ Secret for sending relayed emails _}</label>
                    <div class="input-group">
                        <input type="password" id="email_relay_send_secret" class="form-control" name="email_relay_send_secret" value="{{ m.config.mod_email_relay.email_relay_send_secret.value|escape }}" placeholder="{_ Secret for sending relayed emails _}">
                        {% include "_password_visibility.tpl" target_id="email_relay_send_secret" %}
                        <span class="input-group-btn">
                            <button type="button" class="btn btn-default" data-relay-copy="email_relay_send_secret"
                                    title="{_ Copy secret to clipboard _}" aria-label="{_ Copy secret to clipboard _}">
                                <span class="glyphicon glyphicon-copy" aria-hidden="true"></span>
                            </button>
                        </span>
                    </div>
                    <p class="help-block">{_ Shared secret between this server and the relaying Zotonic server. _}</p>
                </div>
            </fieldset>

            <div class="form-actions">
                <button type="submit" class="btn btn-primary">{_ Save _}</button>
            </div>
        </form>

        {% wire type="submit"
                id="relay_settings2"
                postback={config_save module=`mod_email_relay`}
                delegate=`mod_admin_config`
        %}
        <form id="relay_settings2" class="form" action="postback">
            <fieldset>
                <legend>{_ Relay emails for another Zotonic server _}</legend>
                <p class="help-block">{_ Settings for accepting emails and then relaying them to the intended recipients. _}</p>

                <div class="form-group">
                    <label class="control-label" for="email_relay_receive_secret">{_ Secret for receiving relayed emails _}</label>
                    <div class="input-group">
                        <input type="password" id="email_relay_receive_secret" class="form-control" name="email_relay_receive_secret" value="{{ m.config.mod_email_relay.email_relay_receive_secret.value|escape }}" placeholder="{_ Secret for receiving relayed emails _}">
                        {% include "_password_visibility.tpl" target_id="email_relay_receive_secret" %}
                        <span class="input-group-btn">
                            <button type="button" class="btn btn-default" data-relay-copy="email_relay_receive_secret"
                                    title="{_ Copy secret to clipboard _}" aria-label="{_ Copy secret to clipboard _}">
                                <span class="glyphicon glyphicon-copy" aria-hidden="true"></span>
                            </button>
                        </span>
                    </div>
                    <p class="help-block">{_ Shared secret between this server and the Zotonic server wanting to send emails. Leave empty to not relay email. _}
                    {_ Example secret: _} <tt>{{ m.identity.generate_password }}</tt></p>
                </div>
            </fieldset>

            <div class="form-actions">
                <button type="submit" class="btn btn-primary">{_ Save _}</button>
            </div>
        </form>

        <p>
            <br>
            <span class="glyphicon glyphicon-info-sign"></span>
            {_ The URL for the email relay API on this server is: _}<br>
            <tt>{% url api star="model/email_relay/post/relay" absolute_url %}</tt>
            <button type="button" class="btn btn-default btn-xs"
                    data-onclick-topic="model/clipboard/post/copy"
                    data-text="{% url api star="model/email_relay/post/relay" absolute_url %}"
                    title="{_ Copy relay API URL to clipboard _}" aria-label="{_ Copy relay API URL to clipboard _}">
                <span class="glyphicon glyphicon-copy" aria-hidden="true"></span>
                {_ Copy _}
            </button>
        </p>
    </div>
</div>

{% javascript %}
    document.querySelectorAll("[data-relay-copy]").forEach(button => {
        button.addEventListener("click", () => {
            const input = button.form.querySelector(`[name="${button.dataset.relayCopy}"]`);
            cotonic.broker.publish("model/clipboard/post/copy", { text: input.value });
        });
    });
{% endjavascript %}
