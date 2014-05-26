<p>{_ Copy texts from a language to the currently selected language. _}</p>

<div class="controls">
	<label>{_ Select the language to copy from: _}</label>
	<select id="{{ #langs }}"></select>
</div>

<div class="modal-footer">
    {% button class="btn" action={dialog_close} text=_"Cancel" tag="a" %}
    {% button id=#copy class="btn btn-primary" text=_"Copy" %}
</div>

{% javascript %}
var active = $('#edit-basics .language-tabs li.active').attr('lang');
$('#admin-translation-checkboxes input[type=checkbox]:checked').each(function() {
	var code = $(this).attr('value');
	if (code != active) {
		var name = $(this).parent().children('span').text();
		$('<option>', { value: code }).text(name).appendTo('#{{ #langs }}');
	}
});

$('#{{ #copy }}').click(function(e) {
	var selected = $('#{{ #langs }} option:selected');
	if (selected.length > 0) {
		var from = selected.attr('value');

		z_editor_save($('body'));

		// Copy language to the active language
		$('.tab-pane.language-'+active).each(function() {
			$("input,textarea", this).each(function() {
				if ($.trim($(this).val()) == '') {
					var to_id = $(this).attr('id');
					var from_id = to_id.split('--')[0] + '--' + from;
					$('#'+to_id).val($('#'+from_id).val());
				}
			});
		});
	}
	z_dialog_close();
	e.preventDefault();
});

{% endjavascript %}
