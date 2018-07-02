<a id="{{#email}}" href="#" title="{_ E-mail this page _}">{% image "lib/images/social/square/email.png" mediaclass="base-share-page" %}</a>
{% wire id=#email action={dialog_close} action={dialog_mail_page id=id} %}
