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
            {_ Relay emails via another Zotonic server. _}
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
                <div class="form-group label-floating">
                    <input type="text" class="form-control" name="email_relay_send_secret" value="{{ m.config.mod_email_relay.email_relay_send_secret.value|escape }}" placeholder="{_ Secret for sending relayed emails _}">
                    <label class="control-label">{_ Secret for sending relayed emails _}</label>
                    <p class="help-block">{_ Shared secret between this server and the relaying Zotonic server. _}</p>
                </div>
            </fieldset>

            <div class="form-actions">
                <button type="submit" class="btn btn-primary">{_ Save _}</button>
            </div>
        </form>

        <hr>

        {% wire type="submit"
                id="relay_settings2"
                postback={config_save module=`mod_email_relay`}
                delegate=`mod_admin_config`
        %}
        <form id="relay_settings2" class="form" action="postback">
            <fieldset>
                <legend>{_ Relay emails for another Zotonic server _}</legend>
                <p class="help-block">{_ Settings for accepting emails and then relaying them to the intended recipients. _}</p>

                <div class="form-group label-floating">
                    <input type="text" class="form-control" name="email_relay_receive_secret" value="{{ m.config.mod_email_relay.email_relay_receive_secret.value|escape }}" placeholder="{_ Secret for receiving relayed emails _}">
                    <label class="control-label">{_ Secret for receiving relayed emails _}</label>
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
        </p>
    </div>
</div>
