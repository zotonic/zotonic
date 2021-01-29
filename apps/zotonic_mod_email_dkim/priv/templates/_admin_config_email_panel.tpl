{#
 # Shown on the mod_admin_config email configuration page
 #}
<div class="panel panel-default">
    <div class="panel-heading">
        <h3 class="panel-title">
            {_ DKIM e-mail signing setup _}
            <span class="text-muted pull-right">mod_email_dkim</span>
        </h3>
    </div>
    <div class="panel-body">
    {% with m.email_dkim.admin_info as info %}
        <p>
            {_ The module has generated an RSA keypair for you which will be used when signing outgoing emails. _}
            {_ The location of these key files are the following: _}
            <li>Private key: <b>{{ info.privkey_filepath }}</b></li>
            <li>Public key: <b>{{ info.pubkey_filepath }}</b></li>
        </p>

        <p>
            {_ To finalize the DKIM configuration, you need to add an DNS entry TXT record to the following domain: _} <tt>{{ info.dns_entry_domain }}</tt>.
            {_ The DNS entry should contain the following information: _}
        </p>

        <pre>{{ info.dns_entry }}</pre>

        <p>{_ When all is setup, you can use the <a href="http://dkimcore.org/c/keycheck">DKIM keycheck</a> website to verify your domain's DKIM record. _}</p>

    {% endwith %}
</div>
