{% extends "admin_acl_rules_base.tpl" %}

{% block content_acl %}
	<div class="alert alert-info"
		<p>
			{_ Maximum file upload size per user group. This restricts file uploads for users. _}<br/>
			{_ The default upload size is: _} <strong>{{ m.acl_rule.default_upload_size|filesizeformat }}</strong>
		</p>
	</div>

    <div class="row header">
    	<div class="col-md-2">
    		<label>{_ ACL user group _}</label>
    	</div>
    	<div class="col-md-4">
    		<label>{_ Maximum upload size _}</label>
    	</div>
    </div>

    {% if is_editable %}
	    {% for cg in m.hierarchy.acl_user_group.tree_flat %}
	    	{% with cg.id as id %}
			    <div class="row">
			    	<div class="col-md-2">
			    		<p>{{ cg.indent }}{{ id.title }}</p>
			    	</div>
			    	<div class="col-md-4">
			    		<select "form-control" id="{{ #size.id }}">
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

			    	{% wire id=#size.id
			    			type="change" 
			    			postback={set_upload_size id=id}
			    			delegate=`admin_acl_rules`
			    	%}
			    	{% wire id=#size.id
			    			type="click" 
			    			postback={set_upload_size id=id}
			    			delegate=`admin_acl_rules`
			    	%}
			    </div>
		    {% endwith %}
	    {% endfor %}
	{% else %}
	    {% for cg in m.hierarchy.acl_user_group.tree_flat %}
		    <div class="row">
		    	<div class="col-md-2">
		    		<p>{{ cg.indent }}{{ cg.id.title }}</p>
		    	</div>
		    	<div class="col-md-4">
		    		{{ (cg.id.acl_upload_size*1024*1024)|filesizeformat }}
		    	</div>
		    </div>
	    {% endfor %}
	{% endif %}
{% endblock %}
