{% extends "admin_acl_rules_base.tpl" %}

{% block content_acl %}
	<div class="alert alert-info"
		<p>
			{_ Maximum file upload size per user group. This restricts file uploads for users. _}<br/>
			{_ The default upload size is: _} <strong>{{ m.acl_rule.default_upload_size|filesizeformat }}</strong>
		</p>
		<p>
			{_ File types follow the group hierarchy. Derived groups inherit the mime settings from their ancestor. _}<br>
            {_ Enter extensions (like <tt>.png</tt>), mime types (like <tt>image/png</tt>) or one of: _}
            <tt>msoffice</tt>, <tt>openoffice</tt>, <tt>embed</tt>
		</p>
		<p>
			<strong>{_ These settings come into effect immediately, no publish of rules is needed, _}</strong>
		</p>
	</div>

    <div class="admin-padding">
        <div class="row header">
        	<div class="col-md-3">
        		<label>{_ ACL user group _}</label>
        	</div>
        	<div class="col-md-2">
        		<label>{_ Maximum upload size _}</label>
        	</div>
        	<div class="col-md-7">
        		<label>{_ Allowed file and mime types, separate with ',' — type 'none' if no extra types allowed, leave empty for the default _}</label>
        	</div>
        </div>

        {% if is_editable %}
        	{% wire id=#fperms
        			type="submit"
        			postback={set_upload_permissions}
        			delegate=`admin_acl_rules`
        	%}
        	<form class="form" id="{{ #fperms }}" action="postback">
    		    {% for cg in m.hierarchy.acl_user_group.tree_flat %}
    		    	{% with cg.id as id %}
    				    <div class="row">
    				    	<div class="col-md-3">
    				    		<p>{{ cg.indent }}{{ id.title }}</p>
    				    		<input type="hidden" name="id" value="{{ id }}">
    				    	</div>
    				    	<div class="col-md-2">
                                {% with id.acl_upload_size|default:m.acl_rule.default_upload_size as sz %}
    				    		<select "form-control" name="size-{{ id }}" id="{{ #size.id }}">
    				    			<option value="0">0</option>
    				    			<option value="1" {% if sz == 1 %}selected{% endif %}>1 MB</option>
    				    			<option value="10" {% if sz == 10 %}selected{% endif %}>10 MB</option>
    				    			<option value="20" {% if sz == 20 %}selected{% endif %}>20 MB</option>
    				    			<option value="50" {% if sz == 50 %}selected{% endif %}>50 MB</option>
    				    			<option value="100" {% if sz == 100 %}selected{% endif %}>100 MB</option>
    				    			<option value="200" {% if sz == 200 %}selected{% endif %}>200 MB</option>
    				    			<option value="500" {% if sz == 500 %}selected{% endif %}>500 MB</option>
    				    			<option value="1024" {% if sz == 1024 %}selected{% endif %}>1 GB</option>
                                    <option value="2048" {% if sz == 2048 %}selected{% endif %}>2 GB</option>
    				    		</select>
                                {% endwith %}
    				    	</div>
    				    	<div class="col-md-7">
    				    		<textarea class="form-control"
    				    				  name="mime-{{ id }}"
    				    				  placeholder="{{ m.acl_rule.default_mime_allowed }}"
    				    		>{{ id.acl_mime_allowed }}</textarea>
    				    		<br>
    				    	</div>
    				    </div>
    			    {% endwith %}
    		    {% endfor %}

    		    <div class="row">
    		    	<div class="col-md-5">
    		    	</div>

    		    	<div class="col-md-7">
    		    		<button class="btn btn-default" id="{{ #cancel }}">{_ Cancel _}</button>
                        <button type="submit" class="btn btn-primary">{_ Save Upload Permissions _}</button>
    		    		{% wire id=#cancel action={reload} %}
    		    	</div>
    		    </div>
    		</form>
    	{% else %}
    	    {% for cg in m.hierarchy.acl_user_group.tree_flat %}
    		    <div class="row">
    		    	<div class="col-md-3">
    		    		<p>{{ cg.indent }}{{ cg.id.title }}</p>
    		    	</div>
    		    	<div class="col-md-2">
    		    		{{ (cg.id.acl_upload_size*1024*1024)|filesizeformat }}
    		    	</div>
    		    	<div class="col-md-7">
                        {% if cg.id.acl_mime_allowed %}
                            {{ cg.id.acl_mime_allowed }}
                        {% else %}
                            <span class="text-muted">{{ m.acl_rule.default_mime_allowed }}</span>
                        {% endif %}
    		    	</div>
    		    </div>
    	    {% endfor %}
    	{% endif %}
    </div>
{% endblock %}

