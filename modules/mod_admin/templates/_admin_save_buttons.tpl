									<div class="form-item clearfix">
										{% button class="discard-resource right" text=_"cancel" action={redirect back} %}
										{% if not r.is_editable %}
											{% button class="save-resource right" text=_"save this page" disabled=not is_editable %}
										{% else %}
											{% button class="save-resource right" text=_"save this page" %}
										{% endif %}
									</div>
