{% if r.is_a.group %}
	{% with m.group[r.id] as g %}
		<div class="item-wrapper">
			<h3 class="above-item clearfix do_blockminifier { minifiedOnInit: false }">
				<span class="title">Additional site wide rights</span>
				<span class="arrow">make smaller</span>
			</h3>
			<div class="item">
				<fieldset class="admin-form">
					<div class="notification notice">
						Additional rights members of this group have for the whole site. <a href="javascript:void(0)" class="do_dialog {title: 'Help about extra site rights.', text: '<strong>Administrators</strong> can do anything, be careful with this setting.<br/><strong>Supervisors</strong> can see everything.<br/><strong>Community publishers</strong> can publish content visible for logged on users.<br/><strong>Public publishers</strong> can publish content visible for the whole world.', width: '450px'}">Need more help?</a>
					</div>

					{% if m.acl.is_admin %}
						<div class="form-item clearfix">
							<input id="field-is-admin" type="checkbox" class="do_fieldreplace" name="group_is_admin" {% if g.is_admin %}checked="checked"{% endif %} value="1" />
							<label for="field-is-admin">Administrator</label>
						</div>

						<div class="form-item clearfix">
							<input id="field-is-supervisor" type="checkbox" class="do_fieldreplace" name="group_is_supervisor" {% if g.is_supervisor %}checked="checked"{% endif %} value="1"  />
							<label for="field-is-supervisor">Supervisor</label>
						</div>
						
						<div class="form-item clearfix">
							<input id="field-is-community-publisher" type="checkbox" class="do_fieldreplace" name="group_is_community_publisher" {% if g.is_community_publisher %}checked="checked"{% endif %} value="1"  />
							<label for="field-is-community-publisher">Community publisher</label>
						</div>

						<div class="form-item clearfix">
							<input id="field-is-public-publisher" type="checkbox" class="do_fieldreplace" name="group_is_public_publisher" {% if g.is_public_publisher %}checked="checked"{% endif %} value="1"  />
							<label for="field-is-public-publisher">Public publisher</label>
						</div>
					
					{% else %}

						{% if g.is_admin or g.is_supervisor or g.is_community_publisher or g.is_public_publisher %}
							<h4>
								{% if g.is_admin %}Administrator<br/>{% endif %}
								{% if g.is_supervisor %}Supervisor<br/>{% endif %}
								{% if g.is_community_publisher %}Community publisher<br/>{% endif %}
								{% if g.is_public_publisher %}Public publisher<br/>{% endif %}
							</h4>
						{% else %}
							<div class="notification notice">
								Members of this group do not have any additional rights.
							</div>
						{% endif %}

					{% endif %}

					{% include "_admin_save_buttons.tpl" %}
				</fieldset>
			</div>
		</div>
	{% endwith %}
{% endif %}
