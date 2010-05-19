{% if m.acl.use.mod_translation %}
	<div class="clearfix">
	    {% button text="Generate .po Templates" 
				  action={postback postback="translation_generate" delegate="mod_translation"} %}
	    <span class="expl">Scan all templates for translation tags and generate .po files that can be used for translating the templates.</span>
	</div>
{% endif %}
