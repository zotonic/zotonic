{% wire id=#new
        type="submit"
        postback={oauth2_app_token_new app_id=app_id}
        delegate=`mod_oauth2`
%}
<form id="{{ #new }}" action="postback">
    <p>
        {% trans "Create a new token to allow access to {title}." title=m.site.title %}
    </p>

    <div class="form-group">
        <p class="help-block">{_ Select the user for this. _} {_ It is good practice to have users with limited access permissions for access tokens. _}</p>
        <div class="block-page">
            <div class="well" id="{{ #wrap }}">
            </div>
            <button class="btn btn-default page-connect" id="{{ #connect }}">
                {_ Select a user _}
            </button>
            {% wire id=#connect
                    action={dialog_open
                        title=_"Find user"
                        template="_action_dialog_connect.tpl"
                        intent="select"
                        category=`person`
                        content_group="any"
                        tabs_enabled=[ "find", "new" ]
                        center=false
                        autoclose
                        width="large"
                        level=2
                        actions=[
                            {update
                                target=#wrap
                                template="_rsc_item_select.tpl"
                            },
                            {set_value
                                target=#user_id
                                value_arg=`select_id`
                            }
                        ]

                    }
            %}
            <input type="hidden" name="user_id" id="{{ #user_id }}" value="" required>
            {% validate id=#user_id name="user_id" type={presence} %}
        </div>
    </div>

    <div class="form-group">
        <label class="control-label">{_ Access permission _}</label>
        <p class="help-block">{_ <b>Important:</b> write access is only disabled if the used ACL module and models support this. _}</p>

        <label class="radio">
            <input type="radio" name="is_read_only" value="1" required> {_ Read only access _}
        </label>
        <label class="radio">
            <input type="radio" name="is_read_only" value="0" required checked> {_ Read &amp; write access _}
        </label>
    </div>

    <div class="form-group">
        <div class="label-floating">
            <input type="text" value="" class="form-control" name="label"  placeholder="{_ Optional label _}">
            <label class="control-label">{_ Optional label _}</label>
            <p class="help-block">{_ For every combination of a user and a label there can only be a single token. Existing tokens with this label will be replaced. _}</p>
        </div>
    </div>

    <div class="form-group">
        <div class="label-floating">
            <input id="{{ #note }}" type="text" value="" class="form-control" name="note"  placeholder="{_ Optional note _}">
            <label class="control-label">{_ Optional note _}</label>
        </div>
    </div>

    <div class="modal-footer">
        {% button class="btn btn-default" text=_"Cancel" action={dialog_close} tag="a" %}
        {% button class="btn btn-primary" type="submit" text=_"Make Token" %}
    </div>
</form>
