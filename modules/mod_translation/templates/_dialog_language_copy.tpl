<p>{_ Copy texts from a language to the currently selected language. _}</p>

<div class="form-group">
    <div class="hide-when-empty">
        <label>{_ Select the language to copy from: _}</label>
        <select class="form-control" id="{{ #langs }}"></select>
    </div>
    <div class="show-when-empty" style="display: none;">
        <p class="form-field-error">{_ More than one language must be active. _}</p>
    </div>
</div>

<div class="modal-footer">
    {% button class="btn btn-default" action={dialog_close} text=_"Cancel" tag="a" %}
    {% button id=#copy class="btn btn-primary" type="submit" text=_"Copy" %}
</div>

{% javascript %}
var active = $('#edit-basics .language-tabs li.active').attr('lang');
$('#admin-translation-checkboxes input[type=checkbox]:checked').each(function() {
	var code = $(this).attr('value');
	if (code !== active) {
		var name = $(this).parent().children('span').text();
		$('<option>', { value: code }).text(name).appendTo('#{{ #langs }}');
	}
});
var count = $("#{{ #langs }}").find("option").length;
if (count == 0) {
    $(".hide-when-empty").hide();
    $(".show-when-empty").show();
    $("#{{ #copy }}").attr("disabled", "disabled");
}

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
