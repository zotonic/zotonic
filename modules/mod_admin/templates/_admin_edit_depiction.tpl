                        {% if is_editable or m.rsc[id].depiction %}
						<div class="item-wrapper">
							<h3 class="above-item clearfix do_blockminifier { minifiedOnInit: false }">
								<span class="title">{_ Attached media _}</span>
								<span class="arrow">{_ make smaller _}</span>
							</h3>
							<div class="item clearfix">
								<div id="{{ #media }}">
									{% include "_edit_media.tpl" media=media div_id=#media %}
								</div>
								<div class="clear">
									{% if is_editable %}
										{% button
												text=_"add a new media item"
												action={dialog_media_upload subject_id=id stay
													action={postback postback={reload_media rsc_id=id div_id=#media} delegate="resource_admin_edit"}}
										%}

										{% button text=_"add existing media item"
											action={dialog_link subject_id=id predicate="depiction"
												action={postback
															postback={reload_media rsc_id=id div_id=#media}
															delegate="resource_admin_edit"}
											} %}
									{% else %}
										&nbsp;
									{% endif %}
								</div>
							</div>
						</div>
                        {% endif %}
