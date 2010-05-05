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
				
					<div class="zp-30">
						<h4>Allow editing category</h4>
						<p>
							{% for cat_id, level, indent, title in m.category.all_flat_meta %}
							<label for="{{ #category.cat_id }}">
								{{ indent }}<input type="checkbox" id="{{ #category.cat_id }}" name="acl_cat" {% if title|as_atom|member:acl.categories %}checked="checked" {% endif %} value="{{ title }}" />{{ title }}<br/>
							</label>
							{% endfor %}
						</p>
					</div>
					
					<div class="zp-20">
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
					{% include "_admin_save_buttons.tpl" %}
				</fieldset>
			</div>
		</div>
{% endwith %}