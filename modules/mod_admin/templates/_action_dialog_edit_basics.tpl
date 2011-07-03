{% wire id=#form type="submit" 
	postback={rsc_edit_basics edge_id=edge_id update_element=update_element edge_template=edge_template}
	delegate=delegate 
%}
<form id="{{ #form }}" method="POST" action="postback">

    {% if m.modules.info.mod_translation.enabled %}
    {% catinclude "_admin_edit_basics_form.tpl" id in_dialog lang_code=z_language lang_code_with_dollar=["$", z_language]|join lang_code_with_brackets=["(", z_language, ")"]|join %}
    {% else %}
    {% catinclude "_admin_edit_basics_form.tpl" id in_dialog %}
    {% endif %}

	<div class="form-item clearfix">
        {% button text=_"Save" %}
        {% button text=_"Visit full editpage" action={redirect dispatch="admin_edit_rsc" id=id} %}
        {% button action={dialog_close} text=_"Cancel" %}
    </div>
</form>
