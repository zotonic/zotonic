<div id="{{ #preview }}">
    <div class="modal-body">
        <p>{% trans "Review the latest version for {name}." name=name|escape %}</p>
        <p role="status" aria-live="polite">
            <img src="/lib/images/spinner.gif" width="16" height="16" alt="">
            {_ Fetching remote changes… _}
        </p>
    </div>
    <div class="modal-footer">
        {% button class="btn btn-default" text=_"Cancel" action={dialog_close} %}
        <button type="button" class="btn btn-primary" disabled>{_ Deploy _}</button>
    </div>
</div>
{% wire action={postback postback={vcs_preview target=target element_id=#preview} delegate=`mod_site_update`} %}
