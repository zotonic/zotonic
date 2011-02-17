{% with m.predicate[id] as p %}
		<div class="item-wrapper">
			<h3 class="above-item clearfix do_blockminifier">
				<span class="title">Valid between</span>
				<span class="arrow">make smaller</span>
			</h3>
			<div class="item">
				<fieldset class="admin-form">
					<div class="notification notice">
						This predicate can be used between two pages of the following categories. <a href="javascript:void(0)" class="do_dialog" data-dialog="title: 'Help about predicates.', text: 'You can define for which categories the predicate is shown on the edit page.  You can also define which categories of objects will be found when searching for a page to connect to.  When you don\'t check anything then all categories are valid.', width: '450px'">Need more help?</a>
					</div>

					<div class="form-item clearfix">
						<input id="field-reversed" type="checkbox" class="do_fieldreplace" name="reversed" {% if r.reversed %}checked="checked"{% endif %} value="1" />
						<label for="field-reversed">The direction (from/to) of this predicate is reversed from the normal definition.</label>
					</div>
				
					<div class="zp-30">
						<h4>From category</h4>
						<p>
							{% for cat_id, level, indent, title in m.category.all_flat_meta %}
							<label for="{{ #subject.cat_id }}">
								{{ indent }}<input type="checkbox" id="{{ #subject.cat_id }}" name="predicate_subject" {% if cat_id|member:p.subject %}checked="checked" {% endif %} value="{{ cat_id }}" />{{ title }}<br/>
							</label>
							{% endfor %}
						</p>
					</div>
					
					<div class="zp-20">
						&nbsp;
					</div>
				
					<div class="zp-30">
						<h4>To category</h4>
						<p>
							{% for cat_id, level, indent, title in m.category.all_flat_meta %}
							<label for="{{ #object.cat_id }}">
								{{ indent }}<input type="checkbox" id="{{ #object.cat_id }}" name="predicate_object"  {% if cat_id|member:p.object %}checked="checked" {% endif %} value="{{ cat_id }}" />{{ title }}<br/>
							</label>
							{% endfor %}
						</p>
					</div>

					<div class="zp-20">
						&nbsp;
					</div>
					
					<hr style="clear:left" />
					{% include "_admin_save_buttons.tpl" %}
				</fieldset>
			</div>
		</div>
{% endwith %}
