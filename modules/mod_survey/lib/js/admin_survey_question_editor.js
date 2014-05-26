/**
 * Support functions for the survey questions editor
 */

function ZSurveyEditor() {
	var self = this;
	$('#admin-survey-questions').on('click', 'a', function(e) { self.action(e); });

	$('#admin-survey-questions').on('click', '.is_stop_page', function(e) {
		var $jumps = $(this).closest('.jumps');
		if ($(this).is(':checked')) {
			$jumps.children('ul').hide();
		} else {
			$jumps.children('ul').show();
		}
	});

	$(".pages").sortable({
		//helper: 'clone',
		handle: '.page-header',
		revert: 'invalid',
		axis: 'y',
		start: function(event, ui) {
			z_editor_save(ui.item);
			z_editor_remove(ui.item);
		},
		stop: function(event, ui) {
			z_editor_add(ui.item);
		}
	});

	this.set_page_sorters($(".pages"));

	$('#modal-question .span4').hover(
		function () {
			$(this).addClass("hover");
		},
		function() {
			$(this).removeClass("hover");
		});

	$('#modal-question .span4').on('click', function(e) {
		$('#modal-question').modal('hide');
		$('#modal-question .hover').removeClass('hover');
		self.question_add_select($(this).data('block-type'));
	});

	this._unique_id = 1;
}

ZSurveyEditor.prototype.action = function(e) {
	switch ($(e.target).attr('href'))
	{
		case '#page-above':
			this.page_new(e, 'above');
			e.preventDefault();
			break;
		case '#page-below':
			this.page_new(e, 'below');
			e.preventDefault();
			break;
		case '#page-append':
			this.page_new(e, 'append');
			e.preventDefault();
			break;
		case '#page-delete':
			this.page_delete($(e.target).closest(".page").attr('id'));
			e.preventDefault();
			break;
		case '#question-above':
			this.question_new(e, 'above');
			e.preventDefault();
			break;
		case '#question-below':
			this.question_new(e, 'below');
			e.preventDefault();
			break;
		case '#question-append':
			this.question_new(e, 'append');
			e.preventDefault();
			break;
		case '#question-prepend':
			this.question_new(e, 'prepend');
			e.preventDefault();
			break;
		case '#question-delete':
			this.question_delete($(e.target).closest(".block").attr('id'));
			e.preventDefault();
			break;
		case '#jump-add':
			this.jump_new(e);
			e.preventDefault();
			break;
		case '#jump-delete':
			this.jump_delete($(e.target).closest(".page-jump").attr('id'));
			e.preventDefault();
			break;
		case '#jump-go':
			this.jump_go($(e.target).closest(".page-jump"));
			e.preventDefault();
			break;
		case '#outline-toggle':
			$("#survey-questions").toggleClass("hide-expanded");
			e.preventDefault();
			break;
		case '#page-preview':
		case '#question-preview':
			// Collect args
			// Send to server
			// Server puts args in session, loads iframe.
			// Server opens iframe
			e.preventDefault();
			break;
		default:
			break;
	}
};


ZSurveyEditor.prototype.question_new = function(e, where) {
	this._event_target = e.target;
	this._event_where = where;
	$('#modal-question').modal();
	e.preventDefault();
};

ZSurveyEditor.prototype.question_add_select = function(block) {
	var id = this.unique_id();
	var q = $('#question-template > ul > li')
				.clone()
				.attr('id', id);
	$(".nosubmit", q).removeClass('nosubmit');
	$("label.block-type", q).text(block.split("_").pop());
	switch (this._event_where) {
		case 'above':
			q.insertBefore($(this._event_target).closest(".block"));
			break;
		case 'below':
			q.insertAfter($(this._event_target).closest(".block"));
			break;
		case 'append':
			$(this._event_target)
				.closest('li.page')
				.find('.question-list')
				.append(q);
			break;
		case 'prepend':
			$(this._event_target)
				.closest('li.page')
				.find('.question-list')
				.prepend(q);
			break;
		default:
			break;
	}
    var langs = '';
    
    $('input[name=language]:checked').each(function() { langs += ',' + $(this).val(); });

	z_event("insert-block", {
			element: id,
			block: block,
			language: langs,
            edit_language: $('.language-tabs .active').attr('lang')
	});
};

ZSurveyEditor.prototype.question_delete = function(id) {
	z_dialog_confirm({
		text: z_translate("Are you sure you want to delete this question?"),
		ok: z_translate("Delete"),
		on_confirm: function() {
					$('#'+id).fadeOut('fast',
							function() {
								z_editor_remove($(this));
								this.remove();
							});
					}
	});
};

ZSurveyEditor.prototype.page_new = function(e, where) {
	var id = this.unique_id();
	var p = $('#page-template > ul > li')
				.clone()
				.attr('id', id);
	$(".nosubmit", p).removeClass('nosubmit');

	switch (where) {
		case 'above':
			p.insertBefore($(e.target).closest(".page"));
			break;
		case 'below':
			p.insertAfter($(e.target).closest(".page"));
			break;
		case 'append':
			$('.pages').append(p);
			break;
		default:
			break;
	}
	this.set_page_sorters(p);
};

ZSurveyEditor.prototype.set_page_sorters = function(p) {
	$(".question-list", p).sortable({
		//helper: 'clone',
		//handle: '.widget-header',
		revert: 'invalid',
		axis: 'y',
		connectWith: ".question-list",
		start: function(event, ui) {
			z_editor_save(ui.item);
			z_editor_remove(ui.item);
		},
		stop: function(event, ui) {
			z_editor_add(ui.item);
		}
	});
	$(".jump-list", p).sortable({
		helper: 'clone',
		//handle: '.widget-header',
		revert: 'invalid',
		connectWith: ".jump-list",
		axis: 'y'
	});
};

ZSurveyEditor.prototype.page_delete = function(id) {
	z_dialog_confirm({
		text: z_translate("Are you sure you want to delete this page?<br/>This also deletes all questions on this page."),
		ok: z_translate("Delete"),
		on_confirm: function() {
					$('#'+id).fadeOut('fast',
							function() {
								z_editor_remove($(this));
								this.remove();
							});
					}
	});
};

ZSurveyEditor.prototype.jump_new = function(e) {
	var id = this.unique_id();
	var g = $('#jump-template > ul > li')
				.clone()
				.attr('id', id);
	$("input.jump-condition", g)
		.attr('name', 'jump-condition-'+id)
		.attr('id', 'jump-condition-'+id)
		.removeClass('nosubmit');
	$("input.jump-target", g)
		.attr('name', 'jump-target-'+id)
		.attr('id', 'jump-target-'+id)
		.removeClass('nosubmit');
	$(e.target)
		.closest('.jumps')
		.children('.jump-list')
		.append(g);
};

ZSurveyEditor.prototype.jump_delete = function(id) {
	z_dialog_confirm({
		text: z_translate("Are you sure you want to delete this page jump?"),
		ok: z_translate("Delete"),
		on_confirm: function() { $('#'+id).fadeOut('fast', function() { this.remove(); }); }
	});
};

ZSurveyEditor.prototype.jump_go = function($jump) {
	var $target_input = $(".jump-target", $jump);
	var target_name = $target_input.val();
	var $target = $('input.block-name').filter(function() { return $(this).val() == target_name; });

	if ($target.length) {
		$('html, body').animate({ scrollTop: Math.max(0, $target.offset().top-100) }, 500);
	} else {
		$target_input
			.stop()
			.css("background-color", "#f88")
			.animate({backgroundColor: "white"}, 500);
	}
};

ZSurveyEditor.prototype.unique_id = function() {
	return "zsurvey" + this._unique_id++;
};
