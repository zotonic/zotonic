{% if rule %}
    {% wire id=#ruleform type="submit" postback={update_rule id=rule.id kind=kind} delegate=`admin_acl_rules` %}
{% else %}
    {% wire id=#ruleform type="submit" postback={add_rule kind=kind} delegate=`admin_acl_rules` %}
{% endif %}
<form id="{{ #ruleform }}" class="row acl-rule-row acl-rule-{{ rule.id }}" method="post" action="postback">

    <div class="col-md-2">
	    <select class="form-control" id="{{ #user_group_id }}" name="acl_user_group_id" {% if rule %}onchange="$(this.form).submit()"{% endif %}>
		    {% for cg in m.hierarchy.acl_user_group.tree_flat %}
			    <option value="{{ cg.id }}" {% if cg.id == rule.acl_user_group_id %}selected{% endif %}>
				    {{ cg.indent }} {{ cg.id.title }}
			    </option>
		    {% endfor %}
	    </select>
    </div>

    {% if kind == "rsc" %}
        <div class="col-md-2">
	        <select class="form-control" id="{{ #content_group_id }}" name="content_group_id" {% if rule %}onchange="$(this.form).submit()"{% endif %}>
		        <option value="">{_ All _}</option>
		        {% for cg in m.hierarchy.content_group.tree_flat %}
			        <option value="{{ cg.id }}" {% if cg.id == rule.content_group_id %}selected{% endif %}>
				        {{ cg.indent }} {{ cg.id.title }}
			        </option>
		        {% endfor %}
	        </select>
        </div>
        
        <div class="col-md-2">
	        <select class="form-control" id="{{ #category_id }}" name="category_id" {% if rule %}onchange="$(this.form).submit()"{% endif %}>
		        <option value="">{_ All _}</option>
		        {% for cg in m.hierarchy[`$category`].tree_flat %}
			        <option value="{{ cg.id }}" {% if cg.id == rule.category_id %}selected{% endif %}>
				        {{ cg.indent }} {{ cg.id.title }}
			        </option>
		        {% endfor %}
	        </select>
        </div>

        <div class="col-md-1">
            <label class="checkbox-inline">
                <input type="checkbox" id="{{ #is_owner }}" name="is_owner" value="on"
                    {% if rule.is_owner %}checked="checked"{% endif %}
                    {% if rule %}onchange="$(this.form).submit()"{% endif %}
                />
                {_ owner _}
            </label>
        </div>
    {% elseif kind == "module" %}
        <div class="col-md-5">
	        <select class="form-control" id="{{ #module }}" name="module" {% if rule %}onchange="$(this.form).submit()"{% endif %}>
		        <option value="">{_ All _}</option>
                {% for mod, name in m.modules.all %}
                    <option value="{{ mod }}" {% if mod == rule.module %}selected{% endif %}>{{ mod }} ({{name }})</option>
                {% endfor %}
            </select>            
        </div>
    {% endif %}

    <div class="col-md-5">
        {% if is_new %}
            {% button
                text=_"Add"
                class="btn btn-primary pull-right"
                icon="glyphicon glyphicon-plus"
            %}
        {% else %}
            {% button
                text=_"Remove"
                class="btn btn-danger pull-right"
                icon="glyphicon glyphicon-minus"
                postback={remove_rule id=rule.id kind=kind} delegate=`admin_acl_rules`
            %}
        {% endif %} 

        <div>
            {% for action, label in m.acl_rule[kind].actions %}
                <label class="checkbox-inline">
                    <input type="checkbox" name="action${{ action }}" value="on"
                        {% if rule.actions[action] %}checked="checked"{% endif %}
                        {% if rule %}onchange="$(this.form).submit()"{% endif %}
                    /> {{ label }}
                </label>
            {% endfor %}
        </div>
    </div>
</form>

