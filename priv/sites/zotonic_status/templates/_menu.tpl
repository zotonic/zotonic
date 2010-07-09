{#
	<li><a href="/">Home</a></li>
	<li><a href="http://zotonic.com/">Zotonic</a></li>
#}
{% if has_user %}
	<li><a id="{{ #logoff }}" href="#logoff">{_ Log Off _}</a></li>
	{% wire id=#logoff postback={logoff} %}
{% endif %}
