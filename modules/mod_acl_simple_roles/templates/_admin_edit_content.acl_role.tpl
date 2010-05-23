{% with m.rsc[id].acl as acl %}

		<div class="item-wrapper">
			<h3 class="above-item clearfix do_blockminifier { minifiedOnInit: false }">
				<span class="title">Permissions</span>
				<span class="arrow">make smaller</span>
			</h3>
			<div class="item">
				<fieldset class="admin-form">
					<div class="notification notice">
						Check below what people that are member of this role are allowed to do. <a href="javascript:void(0)" class="do_dialog {title: 'Help about ACL roles.', text: 'Users can be member of multiple roles. Depending on the roles an user is allowed to create and edit certain categories or is able to manage certain modules.', width: '450px'}">Need more help?</a>
					</div>

					<div class="form-item clearfix">
						<input id="field-view-all" type="checkbox" class="do_fieldreplace" name="acl_view_all" {% if acl.view_all %}checked="checked"{% endif %} value="1" />
						<label for="field-view-all">All members are allowed to view all content.  (Check this for supervisors.)</label>
					</div>

					<div class="form-item clearfix">
						<input id="field-update-own" type="checkbox" class="do_fieldreplace" name="acl_only_update_own" {% if acl.only_update_own %}checked="checked"{% endif %} value="1" />
						<label for="field-update-own">Only allow to update content created by the user himself. (Check for user generated content sites.)</label>
					</div>
				
					<div class="zp-40">
						<h4>Allow editing of category</h4>
						<p>
							{% for cat_id, level, indent, title in m.category.all_flat_meta %}
							<label for="{{ #category.cat_id }}">
								{{ indent }}<input type="checkbox" id="{{ #category.cat_id }}" name="acl_cat" {% if title|as_atom|member:acl.categories %}checked="checked" {% endif %} value="{{ title }}" />{{ title }}<br/>
							</label>
							{% endfor %}
						</p>
					</div>
					
					<div class="zp-10">
						&nbsp;
					</div>
				
					<div class="zp-50">
						<h4>Manage modules</h4>
						<p>
						{% with m.modules.all as modules %}
							{% for mod in m.modules.enabled %}
							<label for="{{ #module.mod }}">
								<input type="checkbox" id="{{ #module.mod }}" name="acl_mod"  {% if mod|member:acl.modules %}checked="checked" {% endif %} value="{{ mod|escape }}" />{{ modules[mod]|escape }}<br/>
							</label>
							{% endfor %}
						{% endwith %}
						</p>
					</div>
					
					<hr style="clear:left" />


					<h4>File uploads</h4>
					<div class="form-item clearfix">
						<label for="field-file-upload-size">Maximum allowed file size for uploads (in KB)</label>
						<input id="field-file-upload-size" style="width: 100px" type="text" name="acl_file_upload_size" value="{{ acl.file_upload_size|default:4096 }}" />
						{% validate id="field-file-upload-size" name="acl_file_upload_size" type={numericality} %}
					</div>
					
					<h4>File types allowed to be uploaded</h4>
					
					<p><strong>Security notice</strong>: When you allow */* files then all members of this role will be able to obtain full access to your whole site and all underlying data.</p>
					
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
						<div class="zp-50">
						<ul>
							{% for mime in mimes %}
								{% if mime == "-" %}
								<li><br/></li>
								{% else %}
								<li><label for="{{ #acl.mime }}">
									<input type="checkbox" id="{{ #acl.mime }}" name="acl_mime"
										{% if mime|member:acl.file_mime %}checked="checked"{% endif %}
									 	value="{{ mime }}" /> {{ mime }}</label></li>
								{% endif %}
							{% endfor %}
						</ul>
						</div>
					{% endfor %}
					
					<hr style="clear:left" />
					
					{% include "_admin_save_buttons.tpl" %}
				</fieldset>
			</div>
		</div>
{% endwith %}