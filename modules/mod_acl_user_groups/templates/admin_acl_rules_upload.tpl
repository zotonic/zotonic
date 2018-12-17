{% extends "admin_acl_rules_base.tpl" %}

{% block content_acl %}
	<div class="alert alert-info"
		<p>
			{_ Maximum file upload size per user group. This restricts file uploads for users. _}<br/>
			{_ The default upload size is: _} <strong>{{ m.acl_rule.default_upload_size|filesizeformat }}</strong>
		</p>
		<p>
			{_ Mime types follow the group hierarchy. Derived groups inherit the mime settings from their ancestor. _}
		</p>
		<p>
			<strong>{_ These settings come into effect immediately, no publish of rules is needed, _}</strong>
		</p>
	</div>

    <div class="row header">
    	<div class="col-md-2">
    		<label>{_ ACL user group _}</label>
    	</div>
    	<div class="col-md-3">
    		<label>{_ Maximum upload size _}</label>
    	</div>
    	<div class="col-md-7">
    		<label>{_ Mime types allowed, separate with ',' — type 'none' if no extra mime types allowed _}</label>
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
				    	<div class="col-md-2">
				    		<p>{{ cg.indent }}{{ id.title }}</p>
				    		<input type="hidden" name="id" value="{{ id }}">
				    	</div>
				    	<div class="col-md-3">
				    		<select "form-control" name="size-{{ id }}" id="{{ #size.id }}">
				    			<option value="0"></option>
				    			<option value="1" {% if id.acl_upload_size == 1 %}selected{% endif %}>1 MB</option>
				    			<option value="10" {% if id.acl_upload_size == 10 %}selected{% endif %}>10 MB</option>
				    			<option value="20" {% if id.acl_upload_size == 20 %}selected{% endif %}>20 MB</option>
				    			<option value="50" {% if id.acl_upload_size == 50 %}selected{% endif %}>50 MB</option>
				    			<option value="100" {% if id.acl_upload_size == 100 %}selected{% endif %}>100 MB</option>
				    			<option value="200" {% if id.acl_upload_size == 200 %}selected{% endif %}>200 MB</option>
				    			<option value="500" {% if id.acl_upload_size == 500 %}selected{% endif %}>500 MB</option>
				    			<option value="1024" {% if id.acl_upload_size == 1024 %}selected{% endif %}>1 GB</option>
				    		</select>
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
		    		<button type="submit" class="btn btn-primary">{_ Save Upload Permissions _}</button>
		    		<button class="btn btn-default" id="{{ #cancel }}">{_ Cancel _}</button>
		    		{% wire id=#cancel action={reload} %}
		    	</div>
		    </div>
		</form>
	{% else %}
	    {% for cg in m.hierarchy.acl_user_group.tree_flat %}
		    <div class="row">
		    	<div class="col-md-2">
		    		<p>{{ cg.indent }}{{ cg.id.title }}</p>
		    	</div>
		    	<div class="col-md-4">
		    		{{ (cg.id.acl_upload_size*1024*1024)|filesizeformat }}
		    	</div>
		    	<div class="col-md-4">
		    		{{ cg.id.mime_allowed|escape }}
		    	</div>
		    </div>
	    {% endfor %}
	{% endif %}
{% endblock %}
