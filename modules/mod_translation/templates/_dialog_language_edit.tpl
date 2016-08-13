{#
Params:
code
#}
{% with
    code
    as
    initial_lang_code
%}
<div class="form-horizontal">
    <div class="row">
        <div class="col-sm-12">
            {% if code %}
                {% include "_dialog_language_edit_detail.tpl" initial_lang_code=initial_lang_code %}
            {% else %}
                {% include "_dialog_language_edit_list.tpl" initial_lang_code=initial_lang_code %}
            {% endif %}
        </div>
    </div>
</div>
{% endwith %}
