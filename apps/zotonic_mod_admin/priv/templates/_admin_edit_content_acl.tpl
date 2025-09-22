{% extends "admin_edit_widget_std.tpl" %}

{# Widget for editing access control to rsc  #}

{% block widget_title %}{_ Page settings _}{% endblock %}

{% block widget_show_minimized %}false{% endblock %}
{% block widget_id %}sidebar-settings{% endblock %}

{% block widget_content %}
    {% block page_settings %}
        <div class="form-group">
            <h4>{_ Page settings _}</h4>

            <div class="row">
                <div class="col-md-6">

                    <label for="is_protected" class="checkbox-inline" title="{_ Protect from deletion _}">
                        <input type="checkbox" id="is_protected" name="is_protected" value="1" {% if id.is_protected %}checked="checked"{% endif %} {% if id == 1 %}disabled="disabled"{% endif %} />
                        {_ Protect _}
                    </label>

                    <label for="is_dependent" class="checkbox-inline" title="{_ Delete if no other page is connected to this page. _}">
                        <input type="checkbox" id="is_dependent" name="is_dependent" value="1" {% if id.is_dependent %}checked="checked"{% endif %} {% if id == 1 or id.is_protected %}disabled="disabled"{% endif %} />
                        {_ Dependent _}
                    </label>
                </div>
                <div class="col-md-6 text-right buttons">
                    {% if id.is_editable %}
                        {% button type="submit"
                                  id="save_duplicate"
                                  class="btn btn-default"
                                  text=[_"Duplicate", "…"]
                                  title=_"Duplicate this page."
                        %}
                    {% else %}
                        {% button class="btn btn-default"
                                  text=[_"Duplicate", "…"]
                                  action={dialog_duplicate_rsc id=id}
                                  title=_"Duplicate this page."
                                  disabled=(not m.acl.insert[id.category.name])
                        %}
                    {% endif %}

                    {% if id /= 1 %}
                        {% button class="btn btn-danger"
                                  disabled=(id.is_protected or not id.is_deletable)
                                  id="delete-button"
                                  text=[_"Delete", "…"]
                                  action={dialog_delete_rsc id=id on_success={redirect back}}
                                  title=_"Delete this page."
                        %}
                    {% endif %}
                </div>
            </div>
        </div>
    {% endblock %}

    <hr>

    {% block access_control %}
        <h4>{_ Access control _}</h4>

        {% optional include "_admin_edit_visible_for.tpl" id=id %}
    {% endblock %}

{% endblock %}
