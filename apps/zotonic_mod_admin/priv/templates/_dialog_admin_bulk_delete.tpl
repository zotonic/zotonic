<p>
    {% trans "Are you sure you want to delete these {n} pages?" n=q.ids|length %}
</p>

{% wire id=#delete_all
        type="submit"
        postback={delete_all
                ids=q.ids
                on_success=on_success
                on_success={dialog_close}
        }
        delegate=`mod_admin`
%}
<form id="{{ #delete_all }}" action="postback">

    <div class="modal-footer">
        {% button class="btn btn-default" action={dialog_close} text=_"Cancel" %}
        {% button class="btn btn-danger" type="submit" text=_"Delete" %}
    </div>

</form>
