{% extends "admin_base.tpl" %}

{% block content %}
    {% with m.notifier.first.dkim_admin_info as info %}

    <h1>{_ DKIM e-mail signing setup _}</h1>

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

{% endblock %}
