{% if m.oauth2_consumer.consumers[app_id] as app %}
    {% wire id=#new
            type="submit"
            postback={oauth2_consumer_update app_id=app.id}
            delegate=`mod_oauth2`
    %}
    <form id="{{ #new }}" action="postback">
        <p>
            {_ Update consumer for OAuth2 authorization and content import from another website. _}
        </p>

        {% include "_oauth2_consumer_fields.tpl" app=app %}

        <div class="modal-footer">
            {% button class="btn btn-default" text=_"Cancel" action={dialog_close} tag="a" %}
            {% button class="btn btn-primary" type="submit" text=_"Update" %}

            {% if app.token_count > 0 %}
                {% button class="btn btn-danger pull-left" type="submit" text=_"Delete"
                    action={confirm
                        text=[
                            _"Are you sure you want to delete this Consumer App?",
                            "<br>",
                            "<br>",
                            "<b>", _"This will disconnect all tokens and users.", "</b>"
                        ]
                        is_danger
                        ok=_"Delete Consumer App"
                        postback={oauth2_consumer_delete app_id=app.id}
                        delegate=`mod_oauth2`
                    }
                %}
            {% else %}
                {% button class="btn btn-danger pull-left" type="submit" text=_"Delete"
                    action={confirm
                        text=_"Are you sure you want to delete this Consumer App?"
                        is_danger
                        ok=_"Delete Consumer App"
                        postback={oauth2_consumer_delete app_id=app.id}
                        delegate=`mod_oauth2`
                    }
                %}
            {% endif %}
        </div>
    </form>
{% else %}
    <p class="alert alert-error">
        {_ Consumer App not found, or no view permission. _}
    </p>
    <div class="modal-footer">
        {% button class="btn btn-default" text=_"Cancel" action={dialog_close} tag="a" %}
    </div>
{% endif %}
