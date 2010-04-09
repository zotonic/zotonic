									<div class="form-item clearfix">
										{% button class="discard-resource right" text="cancel" action={redirect back} %}
										{% if not r.is_editable %}
											{% button class="save-resource right" text="save this page" title="test" disabled=not is_editable %}
										{% else %}
											{% button class="save-resource right" text="save this page" title="test" %}
										{% endif %}
									</div>
