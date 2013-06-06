<div class="navbar navbar-fixed-bottom">
	<div class="row-fluid">
		<div class="span4">
			<span class="brand pull-right">{_ This page _}</span>
		</div>
		<div class="span8">
			{% button class="btn pull-right" text=_"Cancel" action={update target="editcol" template="_admin_frontend_nopage.tpl"} tag="a" %}
			{% button type="submit" id="save_stay" class="btn btn-primary" text=_"Save" title=_"Save this page." disabled=not id.is_editable %}
		
			{% if id.page_url %}
				{% if id.is_editable %}
					{% button type="submit" id="save_view" class="btn" text=_"Save &amp; view" title=_"Save and view the page." %}
				{% else %}
					{% button id="save_view" class="btn btn-primary" text=_"View" title=_"View this page." action={redirect id=id} %}
				{% endif %}
			{% endif %}

			<label for="is_published" class="checkbox inline">
	    		<input type="checkbox" id="is_published" name="is_published" value="1" {% if id.is_published %}checked="checked"{% endif %}/>
	    	    {_ Published _}
    	    </label>
		</div>
	</div>
</div>
