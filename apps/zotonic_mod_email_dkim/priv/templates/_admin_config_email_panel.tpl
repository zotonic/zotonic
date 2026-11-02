{#
 # Shown on the mod_admin_config email configuration page
 #}
{% with m.email_dkim.admin_info as info %}
<div class="widget">
    <div class="widget-header">
        {_ DKIM e-mail signing setup _}
        <span class="text-muted pull-right">mod_email_dkim</span>
    </div>
    <div class="widget-content">
        <p>
            {_ An RSA key pair has been generated to sign outgoing email. _}
        </p>

        <p>
            {_ To complete the DKIM setup, add a TXT record to your domain’s DNS settings using the name and value below. _}
        </p>

        <table class="table">
            <tr>
                <th class="text-nowrap">DNS Domain</th>
                <td>
                    <tt>{{ info.dns_entry_domain|escape }}</tt>
                    <button type="button" class="btn btn-default btn-xs"
                            data-onclick-topic="model/clipboard/post/copy"
                            data-text="{{ info.dns_entry_domain|escape }}"
                            title="{_ Copy the DNS name to the clipboard. _}">
                        <span class="glyphicon glyphicon-copy" aria-hidden="true"></span>
                        {_ Copy _}
                    </button>
                </td>
            </tr>
            <tr>
                <th class="text-nowrap">DNS Entry</th>
                <td>
                    <tt class="admin-url-break">{{ info.dns_entry|escape }}</tt>
                    <button type="button" class="btn btn-default btn-xs"
                            data-onclick-topic="model/clipboard/post/copy"
                            data-text="{{ info.dns_entry|escape }}"
                            title="{_ Copy the DNS TXT record value to the clipboard. _}">
                        <span class="glyphicon glyphicon-copy" aria-hidden="true"></span>
                        {_ Copy _}
                    </button>
                </td>
            </tr>
            <tr>
                <th class="text-nowrap">Private key</th>
                <td><tt>{{ info.privkey_filepath|escape }}</tt></td>
            </tr>
            <tr>
                <th class="text-nowrap">Public key</th>
                <td><tt>{{ info.pubkey_filepath|escape }}</tt></td>
            </tr>
        </table>

        <p class="help-block">
            <span class="glyphicon glyphicon-info-sign"></span>
            {_ When all is setup, you can use the <a target="_blank" rel="noreferrer noopener" href="https://dkimcore.org/c/keycheck">DKIM keycheck</a> website to verify your domain’s DKIM record. _}
        </p>
    </div>
</div>
{% endwith %}
