
<p>
	{_ Every session will be interrupted and you will be logged out. _}<br>
	{_ Afterwards, you'll be asked to log in again. _}
</p>

{% button class="btn btn-danger" text=_"Force-close all active sessions"
    postback={close_all_sessions}
    delegate=`mod_authentication`
%}

{% button class="btn" text=_"Cancel"
    action={dialog_close}
%}
