<div class="form-group row">
	<label class="control-label col-md-3" for="block-{{name}}-style">{_ Show as _}</label>
	<div class="col-md-9">
	    <select class="form-control" id="block-{{name}}-style" name="block-{{name}}-style">
	         <option value="inline">{_ Inline block _}</option>
	         {% if is_page_block %}
		         <option value="info" {% if blk.style == "info" %}selected="selected"{% endif %}>{_ Info link with modal popup _}</option>
	         {% endif %}
	         <option value="quote" {% if blk.style == "quote" %}selected="selected"{% endif %}>{_ Block quote _}</option>
	         <option value="aside" {% if blk.style == "aside" %}selected="selected"{% endif %}>{_ Aside _}</option>
	    </select>
	</div>
</div>
