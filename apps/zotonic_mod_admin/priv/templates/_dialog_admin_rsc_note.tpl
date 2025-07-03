{% if id.is_editable and m.acl.is_allowed.use.mod_admin %}
    {% wire id=#form
            type="submit"
            postback={admin_note_update_rsc id=id on_success={dialog_close}}
            delegate=`mod_admin`
    %}
    <form id="{{ #form }}" method="POST" action="postback" class="form">
        <div class="form-group">
            <textarea autofocus rows="6" class="form-control" name="note">{{ m.admin_note.rsc[id].note|escape }}</textarea>
        </div>

        <p class="help-block">
            <span class="glyphicon glyphicon-info-sign"></span> {_ This note is only visible for people who can edit the page. _}
        </p>

        <div class="modal-footer">
            {% button class="btn btn-danger pull-left"
                      action={confirm
                            text=_"Are you sure you want to delete this note?"
                            is_danger
                            ok=_"Delete"
                            postback={admin_note_delete_rsc id=id}
                            delegate=`mod_admin`
                      }
                      text=_"Delete"
                      tag="a"
            %}
            {% button class="btn btn-default" action={dialog_close} text=_"Cancel" tag="a" %}
            {% button class="btn btn-primary" type="submit" text=_"Save" %}
        </div>
    </form>
{% endif %}