{% with is_new|if:element_id:#s as element_id %}

{% if not is_new %}
	<li class="block" id="{{ #s }}">
{% endif %}

	<div class="drag-handle"></div>

    <!-- New block entry -->
	<input type="hidden" name="blocks[]." value="">
	<input type="hidden" class="block-type" name="blocks[].type" value="{{ blk.type|default:block_type }}" />

	<div class="row">
		<div class="col-md-12">
			<br>
			<label class="block-type col-md-2">{{ blk.type|replace:"survey_":""|escape }}</label>
			<div class="col-md-4">
				<input name="blocks[].name"
	                   id="block-{{ #s }}-name"
					   type="text"
					   class="form-control block-name"
					   placeholder="{_ name _}"
					   value="{{ blk.name|escape }}"
	                   noautocomplete
	                   required
				>
			</div>
		</div>
		<div class="col-md-12">
			<div class="block-options">
				{% optional include ["blocks/_admin_edit_block_li_",blk.type,".tpl"]|join name=element_id blk=blk id=id is_new=is_new %}
			</div>
		</div>
	</div>

	<div class="actions btn-group pull-right">
		<a class="btn btn-default btn-sm question-up" href="#question-up" title="{_ Move question up _}">&uarr;</a>
		<a class="btn btn-default btn-sm question-down" href="#question-down" style="margin-right: 10px" title="{_ Move question down _}">&darr;</a>
		<a class="btn btn-default btn-sm dropdown-toggle" data-bs-toggle="dropdown" href="#">
			<span class="caret"></span>
		</a>
		<ul class="dropdown-menu" role="menu" aria-labelledby="dLabel">
			<li><a tabindex="-1" href="#question-below">{_ Add question after _}</a></li>
			<li><a tabindex="-1" href="#question-delete">{_ Delete question _}</a></li>
		</ul>
	</div>

	{% if is_new %}
	    {% javascript %}
	        $("#{{ element_id }}").effect('highlight');
	        setTimeout(function() {
	            z_editor_init();
	            z_admin_ensure_block_names();
	        }, 100);
	    {% endjavascript %}
	{% endif %}

{% if not is_new %}
	</li>
{% endif %}

{% endwith %}
