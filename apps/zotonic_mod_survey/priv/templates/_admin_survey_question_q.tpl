<li class="block" id="{{ #s }}">
	<div class="row">
		<div class="col-lg-2 col-md-2">
			<input name="block-{{ #s }}-name" type="text" class="input-block-level block-name {% if nosubmit %}nosubmit{% endif %}" placeholder="name" value="{{ blk.name }}" />
			<label class="block-type">{% if blk and blk.type %}{{ blk.type|replace:"survey_":"" }}{% endif %}</label>
		</div>
		<div class="col-lg-10 col-md-10">
			<div class="block-options">
			{% if blk and blk.type %}
				<input type="hidden" class="block-type" name="block-{{#s}}-type" value="{{ blk.type }}" />
				{% optional include ["blocks/_admin_edit_block_li_",blk.type,".tpl"]|join name=#s blk=blk id=id is_editable=is_editable is_new=is_new %}
			{% endif %}
			</div>
		</div>
	</div>

	<div class="actions btn-group pull-right">
		<a class="btn btn-default btn-sm dropdown-toggle" data-toggle="dropdown" href="#">
			<span class="caret"></span>
		</a>
		<ul class="dropdown-menu" role="menu" aria-labelledby="dLabel">
			<li><a tabindex="-1" href="#question-below">{_ Add question after _}</a></li>
			<li><a tabindex="-1" href="#question-delete">{_ Delete question _}</a></li>
		</ul>
	</div>
</li>
