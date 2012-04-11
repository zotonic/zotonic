{% with m.rsc[id].acl as acl %}

<div class="widget do_adminwidget" id="content-acl-role">
    <h3 class="widget-header">{_ Permissions _}</h3>
    <div class="widget-content">

        <div class="pull-right">
            <a href="javascript:void(0)" class="btn btn-mini btn-primary do_dialog" data-dialog="title: '{_ Help about ACL roles. _}', text: '{_ Users can be member of multiple roles. Depending on the roles an user is allowed to create and edit certain categories or is able to manage certain modules. _}'" title="{_ Need more help? _}"><i class="icon-question-sign icon-white"></i></a>
        </div>
        
	<p>
	    {_ Check below what people that are member of this role are allowed to do. _}
        </p>
        
	<div class="control-group">
            <label class="checkbox inline" for="field-view-all">
		<input id="field-view-all" type="checkbox" class="do_fieldreplace" name="acl_view_all" {% if acl.view_all %}checked="checked"{% endif %} value="1" />
		{_ All members are allowed to view all content.  (Check this for supervisors.) _}
            </label>
	</div>

	<div class="control-group">
            <label class="checkbox inline" for="field-update-own">
		<input id="field-update-own" type="checkbox" class="do_fieldreplace" name="acl_only_update_own" {% if acl.only_update_own %}checked="checked"{% endif %} value="1" />
		{_ Only allow to update content created by the user himself. (Check for user generated content sites.) _}
            </label>
	</div>

	<div class="control-group">
	    <label class="control-label" for="id_acl_visible_for">{_ Maximum visibility _}</label>
            <div class="controls">
		<select id="acl_visible_for" name="acl_visible_for">
		    <option value="0"
			    {% ifequal 0 acl.visible_for %}selected="selected"
			    {% endifequal %}>{_ The whole world _}</option>
		    <option value="1"
			    {% ifequal 1 acl.visible_for %}selected="selected"
			    {% endifequal %}>{_ Community members _}</option>
		    <option value="2" {% ifequal 2 acl.visible_for %}selected="selected"{% endifequal %}>{_ Group members _}</option>
		</select>
                <span class="help-inline">{_ (users can’t change visibility to higher level than this) _}</span>
            </div>
	</div>
	
	<hr/>
	
        <div class="row">
            <div class="span4">
		<h4>{_ Allow editing of category _}</h4>

		{% for cat_id, level, indent, title in m.category.all_flat_meta %}
                <div class="controls">
		    <label class="checkbox inline" for="{{ #category.cat_id }}">
			{{ indent }}<input type="checkbox" id="{{ #category.cat_id }}" name="acl_cat" {% if title|as_atom|member:acl.categories %}checked="checked" {% endif %} value="{{ title }}" />{{ title }}<br/>
		    </label>
                </div>
		{% endfor %}
	    </div>
	    
	    <div class="span4">
		<h4>{_ Manage modules _}</h4>

		{% with m.modules.all as modules %}
		{% for mod in m.modules.enabled %}
                <div class="controls">
		    <label class="checkbox inline" for="{{ #module.mod }}">
			<input type="checkbox" id="{{ #module.mod }}" name="acl_mod"  {% if mod|member:acl.modules %}checked="checked" {% endif %} value="{{ mod|escape }}" />{{ modules[mod]|escape }}<br/>
		    </label>
                </div>
		{% endfor %}
		{% endwith %}
	    </div>
	</div>
        
	<hr />

	<h4>{_ File uploads _}</h4>
	<div class="form-item clearfix">
	    <label for="field-file-upload-size">{_ Maximum allowed file size for uploads (in KB) _}</label>
	    <input id="field-file-upload-size" style="width: 100px" type="text" name="acl_file_upload_size" value="{{ acl.file_upload_size|default:4096|escape }}" />
	    {% validate id="field-file-upload-size" name="acl_file_upload_size" type={numericality} %}
	</div>
	
	<h4>{_ File types allowed to be uploaded _}</h4>
	
	<p>{_ <strong>Security notice</strong>: When you allow */* files then all members of this role will be able to obtain full access to your whole site and all underlying data. _}</p>

        <div class="row">
	    {% for mimes in [
	    "image/jpeg",
	    "image/png",
	    "image/gif",
	    "image/tiff",
	    "image/bmp",
	    "image/vnd.adobe.photoshop",
	    "application/pdf",
	    "application/postscript",
	    "image/*",
	    "-",
	    "audio/mpeg",
	    "audio/x-wav",
	    "audio/x-aiff",
	    "audio/*",
	    "-",
	    "video/mp4",
	    "video/mpeg",
	    "video/msvideo",
	    "video/x-ms-asf",
	    "video/*",
	    "application/x-shockwave-flash",
	    "-",
	    "application/msword",
	    "application/vnd.ms-excel",
	    "application/vnd.ms-powerpoint",
	    "application/vnd.ms-project",
	    "-",
	    "application/zip",
	    "application/x-gzip",
	    "application/x-tar",
	    "application/x-gzip+tar",
	    "-",
	    "text/plain",
	    "text/json",
	    "text/css",
	    "-",
	    "*/*"
	    ]|vsplit_in:2 %}
	    <div class="span4">
		{% for mime in mimes %}
		{% if mime == "-" %}
                <div class="controls"></div>
		{% else %}
                <div class="controls">
		    <label class="inline checkbox" for="{{ #acl.mime }}">
			<input type="checkbox" id="{{ #acl.mime }}" name="acl_mime"
			       {% if mime|member:acl.file_mime %}checked="checked"{% endif %}
			    value="{{ mime }}"
                            />
                            {{ mime }}
                    </label>
                </div>
		{% endif %}
		{% endfor %}
	    </div>
	    {% endfor %}
	    
	        
	    {% include "_admin_save_buttons.tpl" %}
        </div>
    </div>
</div>
{% endwith %}
