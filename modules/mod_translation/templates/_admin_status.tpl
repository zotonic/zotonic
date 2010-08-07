{% if m.acl.use.mod_translation %}
<div class="clearfix">
	<h2>Translations</h2>
	
	<div class="clearfix">
	    {% button text="Generate .po Templates" 
				  action={postback postback="translation_generate" delegate="mod_translation"} %}
	    <span class="expl">Scan all templates for translation tags and generate .po files that can be used for translating the templates.</span>
	</div>
	
	<div class="clearfix">
	    {% button text="Reload Translations" 
				  action={postback postback="translation_reload" delegate="mod_translation"} %}
	    <span class="expl">Reload all translations from the modules and site. All templates will be recompiled.</span>
	</div>
</div>	
{% endif %}
