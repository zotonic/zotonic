{% with edge.id as id %}
{% wire id=#unlink.id
        action={confirm text=[_"Are you sure you want to disconnect:", " ", edge.object_id.title, "?" ]
                        ok=_"Disconnect"
                        action={unlink edge_id=id subject_id=edge.subject_id hide=tr_id}
               }
%}
<button id="{{ #unlink.id }}" class="btn btn-default">{_ Disconnect _}</button>
{% endwith %}
