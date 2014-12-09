{% wire 
	action={alert title=_"One moment please"
                  text=_"Redirecting to Facebook"
                  only_text}
    action={redirect dispatch=`facebook_authorize`}
%}
