<li class="block" id="{{ #s }}">
	<div class="row-fluid">
		<div class="span2">
			<input name="block-{{ #s }}-name" type="text" class="input-block-level block-name {% if nosubmit %}nosubmit{% endif %}" placeholder="name" value="{{ blk.name }}" />
		</div>
		<div class="span10">
			<div class="block-options">
			{% if blk and blk.type %}
				<input type="hidden" class="block-type" name="block-{{#s}}-type" value="{{ blk.type }}" />
				{% include ["blocks/_admin_edit_block_li_",blk.type,".tpl"]|join name=#s blk=blk id=id is_editable=is_editable is_new=is_new %}
			{% endif %}
			</div>
			<ul class="nav nav-pills">
				<li><a href="#question-above">{_ Add question above _}</a></li>
				<li><a href="#question-below">{_ Add question below _}</a></li>
				<li><a href="#question-delete">{_ Delete question _}</a></li>
			</ul>
		</div>
	</div>
</li>
