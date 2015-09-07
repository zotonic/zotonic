{# TODO: make a text only version #}

<a id="{{#email}}" href="#" class="{{ class|default:"btn" }}">{{ text|default:_"E-mail page" }}</a>
{% wire id=#email action={dialog_mail_page id=id} %}
