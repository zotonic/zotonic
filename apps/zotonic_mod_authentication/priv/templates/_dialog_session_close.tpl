
<p>
	{_ Every session will be interrupted and you will be logged out. _}<br>
	{_ Afterwards, you'll be asked to log in again. _}
</p>

<p class="help-block">
    <span class="glyphicon glyphicon-info-sign"></span> {_ Interrupted sessions might still be visible in the sessions overview, but will not be updated anymore. _}
</p>

<div class="modal-footer">
    {% button class="btn btn-danger" text=_"Force-close all active sessions"
        postback={close_all_sessions}
        delegate=`mod_authentication`
    %}
    {% button class="btn btn-default" text=_"Cancel"
        action={dialog_close}
    %}
</div>
