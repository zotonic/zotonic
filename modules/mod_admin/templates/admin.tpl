{% extends "admin_base.tpl" %}

{% block title %}{_ Dashboard _}{% endblock %}

{% block content %}
	<div id="content" class="zp-85">
		<div class="block clearfix">

			<h2>{_ Dashboard _}</h2>

			<div class="clearfix">
				{% all include "_admin_make_page_buttons.tpl" %}

				{% button   class="" 
					    text=_"Make a new page" 
					    action={dialog_new_rsc title=""} %}

				{% button   class=""
					    text=_"Make a new media item"
					    action={dialog_media_upload title=""}
					    disabled=(not m.acl.insert.media and not m.acl.insert.image) %}
			</div>

			<hr class="clear" />
			
			<div class="zp-50">
				{# Latest modified texts #}
				{% include "_admin_latest_texts.tpl" %}

				{# Latest modified persons #}
				{% include "_admin_latest_persons.tpl" %}
			</div>

			<div class="zp-50">

				{# Latest modified locations #}
				{% if m.rsc['location'].id and m.acl.view['location'] %}
					{% include "_admin_latest_locations.tpl" %}
				{% endif %}


				{# Latest modified events #}
				{% if m.rsc['event'].id and m.acl.view['event'] %}
					{% include "_admin_latest_events.tpl" %}
				{% endif %}


				{# Latest modified media items #}
				{% include "_admin_latest_media_items.tpl" %}
			</div>

		</div>
		<div class="push"></div>
	</div>
{% endblock %}
