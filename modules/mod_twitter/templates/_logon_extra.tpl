{% if m.config.mod_twitter.consumer_key.value %}
<li id="logon_twitter">
    <a id="{{ #twitter_logon }}" href="#twitter"><img src="/lib/images/sign-in-with-twitter.png" width="151" height="24" alt="{_ Sign in with Twitter _}" /></a>
    {% wire id=#twitter_logon 
	action={mask target=mask_target|default:"logon_outer" message=_"Waiting for Twitter…"}
	action={redirect dispatch="twitter_authorize" p=page}
    %}
</li>
{% endif %}
