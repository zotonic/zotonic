{% extends "admin_base.tpl" %}

{% block title %}{_ Edit _} “{{ m.rsc[id].title }}”{% endblock %}

{% block tinymce %}
	<script type="text/javascript" src="/lib/js/modules/tinymce3.3.2a/tiny_mce.js"></script>
	<script type="text/javascript">
		$(document).ready(function(){tinyMCE.init(tinyInit);});
	</script>
{% endblock %}

{% block js_extra %}
	<script type="text/javascript">
		$(function()
		{
			if($('#map_canvas').length)
			{
				googleMapsControl.buildMap({lat: 70.0, lng: -50.0, mapId: 'map_canvas', zoom: 2});

				window.marker = false; 
				var map = googleMapsControl.getMap();
				
				{% if m.rsc[id].location_lat and m.rsc[id].location_lng %}
				addMarker(new google.maps.LatLng({{ m.rsc[id].location_lat }}, {{ m.rsc[id].location_lng }}));
				{% endif %}
				
				$('#fill-geo').click(function()
				{
					var address = $('#address_street_1').val() + ", "
								+ $('#address_city').val() + ", "
								+ $('#address_state').val() + ", "
								+ $('#address_country').val();
					var position = googleMapsControl.getGeoForAddress(address);
					
					googleMapsControl.getGeoForAddress($('#field-title').val(), function(data)
					{
						if(window.marker)
						{
							removeMarker(window.marker)
						}
						
						addMarker(data.geometry.location);
						fillLatLng(data.geometry.location);
						
						google.maps.event.addListener(window.marker, 'dragend', function(e)
						{
							fillLatLng(window.marker.getPosition())
							map.setCenter(window.marker.getPosition());
						});

						google.maps.event.addListener(window.marker, 'drag', function(e)
						{
							fillLatLng(window.marker.getPosition());
						});
					})
				});
				
				google.maps.event.addListener(map, 'click', function(e)
				{
					if(window.marker)
					{
						removeMarker(window.marker)
					}
					
					addMarker(e.latLng);
					
					google.maps.event.addListener(window.marker, 'click', function(e)
					{
						fillLatLng(window.marker.getPosition())
						map.setCenter(window.marker.getPosition());
					});

					clickMarker(window.marker);

					google.maps.event.addListener(window.marker, 'dragend', function(e)
					{
						fillLatLng(window.marker.getPosition())
						map.setCenter(window.marker.getPosition());
					});
					
					google.maps.event.addListener(window.marker, 'drag', function(e)
					{
						fillLatLng(window.marker.getPosition());
					});
				});
			}
			
			function fillLatLng(pos)
			{
				$('#location_lat').val(pos.lat());
				$('#location_lng').val(pos.lng());
			}
			
			function addMarker(position)
			{
				window.marker = new google.maps.Marker(
				{
					position:	position,
					map: 		map,
					icon:		'/lib/images/map_icon.png',
					flat: 		false,
					draggable:  true
				});

				google.maps.event.trigger(window.marker, "click");
				
				map.setCenter(window.marker.getPosition());
			}
			
			function removeMarker(marker)
			{
				marker.setMap(null);
			}
		
			function clickMarker(marker)
			{
				google.maps.event.trigger(marker, "click");
			}
			
			
			/* Initialize translation tabs, select correct language */
			$(".translations").tabs();

			$(".translations").bind('tabsshow', function(event, ui) {
				$(".tinymce-init", ui.panel).each(function() { 
					var mce_id = $(this).attr('id');
					setTimeout(function() { tinyMCE.execCommand('mceAddControl',false, mce_id); }, 200);
				}).removeClass('tinymce-init').addClass('tinymce');
				$(".translations").tabs("select", ui.index);
			});
			
			var tab_index = $(".translations ul.ui-tabs-nav .tab-{{ z_language }}:visible").attr('data-index');
			if (typeof(tab_index) == 'undefined') {
				tab_index = $(".translations ul.ui-tabs-nav li:visible").attr('data-index');
			}
			if (typeof(tab_index) != "undefined") {
				$(".translations").tabs("select", parseInt(tab_index));
			}
			
			/* Initialize all non-initialized tinymce controls */
			$(".tinymce-init:visible").each(function() { 
				var mce_id = $(this).attr('id');
				setTimeout(function() { tinyMCE.execCommand('mceAddControl',false, mce_id); }, 200);
			}).removeClass('tinymce-init').addClass('tinymce');
		}
		);
	</script>
{% endblock %}

{% block content %}
{% with m.rsc[id] as r %}
  {% with r.is_editable as is_editable %}
	{% with m.config.i18n.language_list.list as languages %}
	<div id="content" class="zp-85">
		<div class="block clearfix">

			{% if not is_editable %}
				<h2>
					{_ You are not allowed to edit the _} {{ m.rsc[r.category_id].title|lower }} “{{ r.title|striptags }}”
				</h2>
			{% else %}
				<p class="admin-chapeau">{_ editing _}:
					<span class="right" style="text-align: right">
						{_ Modified _} {{ r.modified|timesince }}
						{_ by _} {{ m.rsc[r.modifier_id].title }}.<br/>
						{_ Created by _} {{ m.rsc[r.creator_id].title }}.<br/>
					</span>
				</p>
				<h2>{{ r.title|striptags|default:"<em>untitled</em>" }}
					<span>{{ m.rsc[r.category_id].title|lower }} <a href="#category">{_ change _}</a></span>
				</h2>
			{% endif %}

			{% wire id="rscform" type="submit" postback="rscform" %}
			<form id="rscform" method="post" action="postback">
				<input type="hidden" name="id" value="{{ id }}" />

				<div class="zp-67" id="poststuff">
					<div class="padding">

						{% all catinclude "_admin_edit_basics.tpl" id is_editable=is_editable languages=languages %}

						{% all catinclude "_admin_edit_content.tpl" id is_editable=is_editable languages=languages %}

						{% if r.is_a.media or r.medium %}
						<div class="item-wrapper">
							<h3 class="above-item clearfix do_blockminifier { minifiedOnInit: false }">
								<span class="title">{_ File / media content _}</span>
								<span class="arrow">{_ make smaller _}</span>
							</h3>
							<div class="item clearfix">
								{% with r.medium as medium %}
									<div id="media-edit-view">
										{% include "_admin_edit_media_view.tpl" id=id languages=languages %}
									</div>

									{% button text=_"Replace this media item" action={dialog_media_upload id=id action={update target="media-edit-view" template="_admin_edit_media_view.tpl" id=id}} disabled=not is_editable %}
								{% endwith %}
							</div>
						</div>

                        {% if is_editable %}
						<div class="item-wrapper">
							<h3 class="above-item clearfix do_blockminifier { minifiedOnInit: true }">
								<span class="title">{_ Website _}</span>
								<span class="arrow">{_ make smaller _}</span>
							</h3>
							<div class="item">
								<fieldset class="admin-form">
									<div class="form-item clearfix">
										<label for="media-website">{_ Website for clicks on image _}</label>
										<input type="text" id="media-website" name="website" class="zp-100" value="{{ r.website }}"/>
									</div>
								</fieldset>
							</div>
						</div>
                        {% endif %}{# website #}
						{% endif %}{# medium #}

						{% catinclude "_admin_edit_body.tpl" id is_editable=is_editable languages=languages %}

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

						<div class="item-wrapper">
							<h3 class="above-item clearfix do_blockminifier { minifiedOnInit: {{ not r.is_a.meta }} }">
								<span class="title">{_ Advanced _}</span>
								<span class="arrow">{_ make smaller _}</span>
							</h3>
							<div class="item clearfix">
								<fieldset class="admin-form">

                                    {% if m.rsc[id].is_authoritative %}
									<div class="path-unique-name-wrapper clearfix">
										<div class="zp-50">
											<div class="form-item clearfix">
												<label for="field-page-path">{_ Page path, default is _} <em>{{ r.default_page_url|escape }}</em></label>
												<input type="text" id="field-page-path" name="page_path" value="{{ r.page_path }}" {% if not is_editable %}disabled="disabled"{% endif %} />
											</div>
										</div>

										{% if m.acl.use.mod_admin_config %}
											<div class="zp-50">
												<div class="form-item clearfix">
													<label for="field-name">{_ Unique name _}</label>
													<input type="text" id="field-name" name="name" value="{{ r.name }}" {% if not is_editable or id == 1 %}disabled="disabled"{% endif %} />
												</div>
											</div>
										{% else %}
											<div class="zp-50">
												<div class="form-item clearfix">
													&nbsp;
												</div>
											</div>
										{% endif %}
									</div>
									{% endif %}

									{% if m.acl.use.mod_admin_config %}
										{% if r.is_a.meta or not r.is_authoritative %}
											<div class="form-item clearfix">
												<label for="field-name">{_ Unique uri _}</label>
												<input type="text" id="field-name" name="uri" value="{{ r.uri }}" {% if not is_editable %}disabled="disabled"{% endif %} />
											</div>
										{% endif %}
									{% endif %}

								</fieldset>
							</div>
						</div>

						<div class="item-wrapper">
							<h3 class="above-item clearfix do_blockminifier { minifiedOnInit: true }">
								<span class="title">{_ Seo content _}</span>
								<span class="arrow">{_ make smaller _}</span>
							</h3>
							<div class="item clearfix">
								<fieldset class="admin-form">
									<div class="form-item clearfix">
										<input id="no-google" type="checkbox" class="do_fieldreplace" name="seo_noindex" {% if r.seo_noindex %}checked="checked"{% endif %} value="1" />
										<label for="no-google">{_ Ask google to not index this page _}</label>
									</div>

									<div class="form-item clearfix">
										<label for="seo_title">{_ Page title _}</label>
										<input type="text" id="seo_title" name="seo_title" class="zp-100" value="{{ r.seo_title }}"/>
									</div>

									<div class="form-item clearfix">
										<label for="title">{_ Page slug _}</label>
										<input type="text" id="slug" name="slug" class="zp-100" value="{{ r.slug }}" {% if not r.custom_slug %}disabled="disabled"{% endif %} />
										<input id="custom-slug" type="checkbox" class="do_fieldreplace" name="custom_slug" {% if r.custom_slug %}checked="checked"{% endif %} 
                                               value="1" onclick="$('#slug').attr('disabled', $('#custom-slug:checked').val() ? '' : 'disabled');" />
										<label for="custom-slug">{_ Customize page slug _}</label>
									</div>

									<div class="form-item clearfix">
										<label for="seo_keywords">{_ Page keywords _}</label>
										<input type="text" id="seo_keywords" name="seo_keywords" class="zp-100" value="{{ r.seo_keywords }}"/>
									</div>

									<div class="form-item clearfix">
										<label for="seo_desc">{_ Page description _}</label>
										<textarea rows="2" cols="10" id="seo_desc" name="seo_desc" class="seo-desc zp-100">{{ r.seo_desc }}</textarea>
									</div>

									{% include "_admin_save_buttons.tpl" %}
								</fieldset>
							</div>
						</div>

					</div>
				</div>

				<div class="zp-33" id="sidebar">
					<div class="padding" id="sort">

						<div class="item-wrapper" id="sort-publish">
							<h3 class="above-item">{_ Publish this page _}</h3>
							<div class="item clearfix">
								<div class="admin-form ">
									<div class="form-item clearfix">
										{% button type="submit" id="save_stay" class="save-resource do_tooltip" text=_"save" title=_"Save this page." disabled=not is_editable %}
										{% if is_editable %}
											{% button type="submit" id="save_view" class="save-resource do_tooltip" text=_"save &amp; view" title=_"Save and view the page." %}
										{% else %}
											{% button id="save_view" class="save-resource do_tooltip" text=_"view" title=_"View this page." action={redirect id=id} %}
										{% endif %}

										{% button class="discard-resource right do_tooltip" text=_"Cancel" action={redirect back} title=_"Go back." %}

									</div>

									<div class="form-item clearfix">
										<input type="checkbox" class="do_fieldreplace" id="is_published" name="is_published" value="1" {% if r.is_published %}checked="checked"{% endif %}/>
										<label for="is_published" class="left">{_ Published _}</label>

										<input type="checkbox" class="do_fieldreplace" id="is_featured" name="is_featured" value="1" {% if r.is_featured %}checked="checked"{% endif %}/>
										<label for="is_featured" class="left">{_ Featured _}</label>

										<input type="checkbox" class="do_fieldreplace" id="is_protected" name="is_protected" value="1" {% if r.is_protected %}checked="checked"{% endif %} {% ifequal id 1 %}disabled="disabled"{% endifequal %} />
										<label for="is_protected" class="left">{_ Protect from deletion _}</label>
									</div>

									<div class="form-item clearfix">
										{% ifnotequal id 1 %}
											{% button class="discard-resource do_tooltip" disabled=(r.is_protected or not m.rsc[id].is_deletable) id="delete-button" text=_"Delete" action={dialog_delete_rsc id=r.id on_success={redirect back}} title=_"Delete this page." %}
										{% endifnotequal %}
										{% if is_editable %}
											{% button type="submit" id="save_duplicate" class="save-resource do_tooltip" text=_"Duplicate" title=_"Duplicate this page." %}
										{% else %}
											{% button class="save-resource do_tooltip" 
													text=_"Duplicate" 
													action={dialog_duplicate_rsc id=id} 
													title=_"Duplicate this page."
													disabled=(not m.acl.insert[r.category.name]) %}
										{% endif %}

                                        {% for id in m.search[{next id=id cat=m.rsc[id].category.name pagelen=1}] %}
										{% button class="goto-resource right do_tooltip" text="&raquo;" action={redirect dispatch="admin_edit_rsc" id=id} title=_"Next in category: "|append:m.rsc[id].title %}
                                        {% endfor %}
                                        {% for id in m.search[{previous id=id cat=m.rsc[id].category.name pagelen=1}] %}
										{% button class="goto-resource right do_tooltip" text="&laquo;" action={redirect dispatch="admin_edit_rsc" id=id} title=_"Previous in category: "|append:m.rsc[id].title %}
                                        {% endfor %}
									</div>
								</div>
							</div>
						</div>

						<div class="item-wrapper" id="sort-access">
							<h3 class="above-item clearfix do_blockminifier { minifiedOnInit: true }">
								<span class="title">{_ Access control _}</span>
								<span class="arrow">{_ make smaller _}</span>
							</h3>
							<div class="item clearfix">
								<div class="admin-form clearfix">
									<div class="notification notice">
										{_ Define who can see or edit this page. _}
									</div>

									{% include "_admin_edit_visible_for.tpl" id=id %}
								</div>
							</div>
						</div>

						{% if not r.is_a.meta %}
						<div class="item-wrapper" id="sort-date">
							<h3 class="above-item clearfix do_blockminifier { minifiedOnInit: {{ r.publication_start|in_past and r.publication_end|in_future }} }">
								<span class="title">{_ Publication period _}</span>
								<span class="arrow">{_ make smaller _}</span>
							</h3>
							<div class="item clearfix">
								<div class="admin-form form-item">
									<div class="notification notice">
										{_ Show this article only between the two dates. _}
										<a href="javascript:void(0)" class="do_dialog {title: '{_ Help about publication date range. _}', text: '{_ When a page has a publication date range then it will only be visible between the two dates. Note that when you are allowed to edit the page then you can always see it._}', width: '450px'}">{_ Need more help? _}</a>
									</div>
									<fieldset>
										<div class="form-item">
											<label>{_ Visible from _}</label>
											{% include "_edit_date.tpl" date=r.publication_start name="publication_start" is_end=0 %}
										</div>
										<div class="form-item">
											<label>{_ Visible till _}</label>
											{% include "_edit_date.tpl" date=r.publication_end name="publication_end" is_end=1 %}
										</div>
									</fieldset>
								</div>
							</div>
						</div>

						<div class="item-wrapper" id="sort-date">
							<h3 class="above-item clearfix do_blockminifier { minifiedOnInit: {{ not r.is_a.event }} }">
								<span class="title">{_ Date range _}</span>
								<span class="arrow">{_ make smaller _}</span>
							</h3>
							<div class="item clearfix">
								<div class="admin-form form-item">
									<div class="notification notice">
										{_ Used for events and other periods. _}
										<a href="javascript:void(0)" class="do_dialog {title: '{_ Help about date ranges. _}', text: '{_ Every page can have a date range. For example if the page is an event or description of someone\'s life. _}', width: '450px'}">{_ Need more help? _}</a>
									</div>
									<fieldset>
										<div class="form-item">
											<label>{_ From _}</label>
											{% include "_edit_date.tpl" date=r.date_start name="date_start" is_end=0 %}
										</div>
										<div class="form-item">
											<label>{_ Till _}</label>
											{% include "_edit_date.tpl" date=r.date_end name="date_end" is_end=1 %}
										</div>
										<div class="form-item clear">
											<label>{_ Remarks _}</label>
											<input type="text" name="date_remarks" value="{{ r.date_remarks }}" />
										</div>
									</fieldset>
								</div>
							</div>
						</div>
						{% endif %}

						{% all catinclude "_admin_edit_sidebar.tpl" id languages=languages %}


						<div class="item-wrapper" id="sort-connections">
							<h3 class="above-item clearfix do_blockminifier { minifiedOnInit: false }">
								<span class="title">{_ Page connections _}</span>
								<span class="arrow">{_ make smaller _}</span>
							</h3>
							<div class="item clearfix">
								<div class="notification notice">
									{_ This page is able to connect to others. For example you can connect it to an actor or a brand. _}
									<a href="javascript:void(0)" class="do_dialog {title: '{_ Help about page connections. _}', text: '{_ This page is able to connect to others. For example you can connect it to an actor or a brand. _}', width: '450px'}">{_ Need more help? _}</a>
								</div>

								<div id="unlink-undo-message"></div>

								{% with r.predicates_edit as pred_shown %}
									{% for name, p in m.predicate %}
										{% if p.id|member:pred_shown %}
											{% ifnotequal name "depiction" %}
												<div class="clearfix">
													<h4>{{ p.title }}</h4>
													{% if is_editable %}
														{% dialog_link_add subject_id=id predicate=name %}
													{% endif %}
												</div>
												<div class="unlink-wrapper clearfix">
													{% sorter id=["links",id|format_integer,name]|join:"-" tag={object_sorter predicate=name id=id} group="edges" handle=".unlink-mover" %}
													<ul id="links-{{ id }}-{{ name }}" class="clearfix edge-sidebar-sorter" style="min-height: 10px">
													{% for o_id, edge_id in m.edge.o[id][name] %}
														{% include "_rsc_edge.tpl" subject_id=id predicate=name object_id=o_id edge_id=edge_id %}
													{% endfor %}
													</ul>
												</div>
												<hr />
											{% endifnotequal %}
										{% endif %}
									{% endfor %}
								{% endwith %}

								<div class="button-wrapper clearfix">
									{% button action={redirect dispatch="admin_referrers" id=id} text=_"View all referrers"%}
								</div>
							</div>
						</div>

                        {% if m.acl.insert[r.category.name|as_atom] %}

						{# meta categories (predicate, category and group) cannot be changed #}
						{% if not r.is_a.meta %}
						<div class="item-wrapper" id="sort-category">
							<h3 class="above-item clearfix do_blockminifier { minifiedOnInit: true }">
								<a name="category"></a>
								<span class="title">{_ Category _}</span>
								<span class="arrow">{_ make smaller _}</span>
							</h3>
							<div class="item clearfix admin-form">
								<div class="notification notice">
									{_ The category defines what the page represents. _} <a href="javascript:void(0)" class="do_dialog {title: '{_ Help about category. _}', text: '{_ Every page is categorized in exactly one category.  The category defines what the page represents. For example an event, a product or a person.  The categories are hierarchically defined. In that way you can have a vehicles category with subcategories car and bicycle. _}', width: '450px'}">{_ Need more help? _}</a>
								</div>
								<p>
									{% with r.category_id as r_cat %}
										<select id="category_id" name="category_id">
										{% for cat_id, level, indent, name in m.category.all_flat %}
                                            {% if m.acl.insert[name|as_atom] %}
											<option value="{{cat_id}}" {% ifequal r_cat cat_id %}selected="selected"{% endifequal %}>
												{{ indent }}{{ m.rsc[cat_id].title|default:name }}
											</option>
                                            {% endif %}
										{% endfor %}
										</select>
									{% endwith %}
								</p>

								<div class="form-item clearfix">
									{% button type="submit" id="save_stay" class="save-resource do_tooltip" text=_"Save this page" title=_"Save this page and change category." disabled=not is_editable %}
									{% button class="discard-resource" text=_"Cancel" action={redirect back} %}
								</div>
							</div>
						</div>
						{% else %}
						<div class="item-wrapper" id="sort-category">
							<h3 class="above-item clearfix do_blockminifier { minifiedOnInit: true }">
								<span class="title">{_ Category _}</span>
								<span class="arrow">{_ make smaller _}</span>
							</h3>
							<div class="item clearfix admin-form">

								<div class="notification notice">
									{_ This page is a _} {{ m.rsc[r.category_id].title }}.
									{_ Predicates, groups and categories can't be changed into another category. _}
								</div>
							</div>
						</div>
						{% endif %}
                        {% endif %}{# acl check #}
					</div>
				</div>
			</form>
		</div>
	</div>
	{% endwith %}
  {% endwith %}
{% endwith %}
{% endblock %}
