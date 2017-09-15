{% wire id="dialog-merge-find"
        type="submit"
        action={script script=""}
%}
<form id="dialog-merge-find" class="form form-horizontal">
    <input type="hidden" name="id" value="{{ id }}" />

    <input name="find_text" type="text" value="{{ text|default:'' }}" placeholder="{_ Type text to search _}" class="do_autofocus form-control" />
    {% block category_select %}
        <select class="form-control" name="find_category">
            <option value="">{_ Any category _}</option>
            <option value="" disabled></option>
            {% for c_id,_depth,indent,name in m.category.all_flat %}
                <option value="{{ c_id }}" {% if c_id == cat %}selected="selected" {% endif %}>
                    {{ indent }}{{ c_id.title|default:name }}
                </option>
            {% endfor %}
        </select>
    {% endblock %}
</form>

<p><br/></p>

<div id="dialog-merge-found"
     class="do_feedback"
     data-feedback="trigger: 'dialog-merge-find', delegate: 'mod_admin_merge'">
</div>

{% wire name="dialog_merge_find"
    action={postback
        delegate=delegate|default:"mod_admin_merge"
        postback={merge_select id=id}
    }
%}
{% javascript %}
    $('#dialog-merge-find').submit(function() { return false; });
    $('#dialog-merge-find').change();
    $("#dialog-merge-found").on('click', '.thumbnail', function(e) {
        e.preventDefault();
        z_event('dialog_merge_find', {
            select_id: $(this).data('id')
        });
    });
{% endjavascript %}
