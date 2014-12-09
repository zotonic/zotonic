{% if m.config.mod_twitter.consumer_key.value and m.config.mod_twitter.useauth.value %}
<li id="logon_twitter">
	<a href="{% url logon_service service='twitter' %}" class="btn social-login do_popupwindow" style="color: white; background-color: #55acee"><span class="icon-twitter-sign"></span> {_ Log on with Twitter _}</a>
</li>
{% endif %}
