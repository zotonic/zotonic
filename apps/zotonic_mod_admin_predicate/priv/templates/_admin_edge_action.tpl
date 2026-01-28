{% with edge.id as id %}
{% with _"Are you sure you want to remove the <strong>{{ predicate }}</strong>
          connection between <strong>{{ subject }}</strong>
          and <strong>{{ object }}</strong>?"
        | merge_tags:%{ subject: edge.subject_id.title,
                        predicate: edge.predicate_id.title,
                        object: edge.object_id.title } as confirm_remove_text %}
{% wire id=#unlink.id
        action={confirm text=confirm_remove_text
                        ok=_"Remove connection"
                        action={unlink edge_id=id subject_id=edge.subject_id hide=tr_id}
                        is_danger
               }
%}
<button id="{{ #unlink.id }}" class="btn btn-default">{_ Remove connection _}</button>
{% endwith %}
{% endwith %}
