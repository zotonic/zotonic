{% wire 
	action={alert title=_"One moment please"
                  text=_"Redirecting to Twitter"
                  only_text}
    action={redirect dispatch=`twitter_authorize`}
%}

{#
{% wire
	action={mask target=mask_target|default:"logon_outer" message=_"Waiting for Twitter…"}
	action={redirect dispatch="twitter_authorize" p=page}
%}
#}
