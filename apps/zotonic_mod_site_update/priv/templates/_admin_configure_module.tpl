<div class="modal-body">

    <div class="form-group">
        <label class="control-label" for="apikey">{_ Site update API token _}</label>
        <div>
            <input type="text" id="apikey" name="api_key"
                   value="{{ m.config.mod_site_update.webhook_token.value|escape }}"
                   class="do_autofocus col-lg-4 col-md-4 form-control"
            >
            {% wire id="apikey" type="blur" action={config_toggle module="mod_site_update" key="webhook_token" } %}
        </div>
        <p class="info-block">
            {_ This token is used to secure the webhook API call for updating this website on a code push to Github (or similar services like BitBucket). _}
        </p>
        <p>
            {_ To run the webhook update, configure your service to POST to the following URL: _}
        </p>
        <code>
            {% url api star="model/site_update/post/webhook/" absolute_url %}{{ m.config.mod_site_update.webhook_token.value|escape|default:"..." }}
        </code>
    </div>

</div>

<div class="modal-footer">
    {% button class="btn btn-default" text=_"Close" action={dialog_close} tag="a" %}
</div>