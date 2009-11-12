/* Zotonic basic Javascript library
----------------------------------------------------------

@package:	Zotonic 2009	
@Author:	Tim Benniks <tim@timbenniks.nl>
@Author:	Marc Worrell <marc@worrell.nl>

Copyright 2009 Tim Benniks, Marc Worrell

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at
 
http://www.apache.org/licenses/LICENSE-2.0
 
Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.

Based on nitrogen.js which is copyright 2008-2009 Rusty Klophaus

---------------------------------------------------------- */

var z_comet_is_running		= false;
var z_is_in_postback		= false;
var z_postbacks				= [];
var z_default_form_postback = false;
var z_input_updater			= false;

/* 
We need to set the domain of the session cookie to 'test'
Then we can try to make an xhr request to 0.test, 1.test etc.
document.domain  = 'test';
var z_xhr_domain = 'test';
*/

function z_dialog_open(title, text)
{
	$('.dialog').remove();
	$.dialogAdd({title: title, text: text});
}

function z_dialog_close()
{
	$('.dialog-close').click();
}

$(function()
{
	$(window).bind('ajaxStart', function()
	{
		$(document.body).addClass('wait');
	});

	$(window).bind('ajaxStop', function()
	{
		$(document.body).removeClass('wait');
	});
});


/* Growl messages
---------------------------------------------------------- */

function z_growl_add(message, stay, type)
{
	stay = stay || false;
	type = type || 'notice';
	
	$.noticeAdd(
	{
		text: message,
		stay: stay,
		type: type
	});
	
	if(type == 'error' && window.console)
	{
		console.error(message);
	}
}

function z_growl_close()
{
	jQuery. jQuery.noticeRemove($('.notice-item-wrapper'), 400);
}


/* Postback loop
---------------------------------------------------------- */

function z_postback_loop() 
{
	if (!z_is_in_postback && z_postbacks.length != 0) 
	{
		// For now, allow only a single postback at a time.
		z_is_in_postback++;

		var o = z_postbacks.shift();
		z_do_postback(o.triggerID, o.postback, o.extraParams);

		setTimeout("z_postback_loop()", 2);
	}
	else
	{
		setTimeout("z_postback_loop()", 20);
	}
}

function z_opt_cancel(obj)
{
	var nodeName = obj.nodeName.toLowerCase();
	var nodeType = $(obj).attr("type");

	if (nodeName == 'input' &&	(nodeType == 'checkbox' || nodeType == 'radio'))
	{
		return true;
	}
	else
	{
		return false;
	}
}

function z_queue_postback(triggerID, postback, extraParams, noTriggerValue) 
{
	var triggerValue = '';

	if (triggerID != '' && !noTriggerValue)
	{
		var trigger = $('#'+triggerID).get(0);
		var nodeName = trigger.nodeName.toLowerCase();
		
		if (nodeName == 'input' || nodeName == 'button' || nodeName == 'textarea' || nodeName == 'select')
			triggerValue = $('#'+triggerID).val() || '';
	}

	extraParams = extraParams || new Array(); 
	extraParams.push({name: 'triggervalue', value: triggerValue})
	
	var o			= new Object();
	o.triggerID		= triggerID;
	o.postback		= postback;
	o.extraParams	= extraParams;
	
	z_postbacks.push(o);
}

function z_do_postback(triggerID, postback, extraParams) 
{
	// Get params...
	var params = 
		"postback=" + urlencode(postback) + 
		"&z_trigger_id=" + urlencode(triggerID) +
		"&z_pageid=" + urlencode(z_pageid) + 
		"&" + $.param(extraParams);
	
	z_ajax(params);
}

function z_ajax(params)
{
	z_start_spinner();	

	$.ajax({ 
		url:		'/postback',
		type:		'post',
		data:		params,
		dataType:	'text',
		success: function(data, textStatus) 
		{
			z_is_in_postback--;
			z_stop_spinner();
			
			try 
			{
				eval(data);
				z_init_postback_forms();
			} 
			catch(e)
			{
				$.misc.error("Error evaluating ajax return value: " + data);
				$.misc.warn(e);
			}
		},
		error: function(xmlHttpRequest, textStatus, errorThrown) 
		{
			z_is_in_postback--;
			z_stop_spinner();
			
			$.misc.error("FAIL: " + textStatus);
		}
	});
}


/* Comet long poll
---------------------------------------------------------- */

function z_comet_start()
{
	if (!z_comet_is_running)
	{
		setTimeout("z_comet();", 1000);
		z_comet_is_running = true;
	}
}

function z_comet() 
{
	$.ajax({ 
		url: '/comet',
		type:'post',
		data: "z_pageid=" + urlencode(z_pageid),
		dataType: 'text',
		success: function(data, textStatus) 
		{
			try 
			{
				eval(data);
				z_init_postback_forms();
			} 
			catch (e)
			{
				alert("Error evaluating Comet return value: " + data);
				alert(e);
			}
		
			setTimeout("z_comet();", 10);
		},
		error: function(xmlHttpRequest, textStatus, errorThrown) 
		{
			setTimeout("z_comet();", 1000);
		}
	});
	return;
}


/* Utility functions
---------------------------------------------------------- */

function z_is_enter_key(event) 
{
	return (event && event.keyCode == 13);
}


/* Spinner, showen when waiting for a postback
---------------------------------------------------------- */

function z_start_spinner()
{
	if(z_is_in_postback > 0)
	{
		$('#spinner').fadeIn(100);
	}
}

function z_stop_spinner() 
{
	if(z_is_in_postback == 0)
	{
		$('#spinner').fadeOut(100);
	}
}


/* Drag & drop interface to the postback
---------------------------------------------------------- */

function z_draggable(dragObj, dragOptions, dragTag) 
{
	$(dragObj).draggable(dragOptions).data("z_drag_tag", dragTag);	
}

function z_droppable(dropObj, dropOptions, dropPostbackInfo) 
{
	dropOptions.greedy = true;
	dropOptions.drop = function(ev, ui) 
	{
		var dragTag = $(ui.draggable[0]).data("z_drag_tag");
		var dragItem = new Array({name: 'drag_item', value: dragTag});
		z_queue_postback(this.id, dropPostbackInfo, dragItem, true);
	};

	$(dropObj).droppable(dropOptions);
}


/* Sorter and sortables interface to the postback
---------------------------------------------------------- */

function z_sortable(sortableItem, sortTag) 
{
	$(sortableItem).data("z_sort_tag", sortTag);
}

function z_sorter(sortBlock, sortOptions, sortPostbackInfo) 
{
	sortOptions.update = function() 
	{
		var sortItems = "";

		for (var i = 0; i < this.childNodes.length; i++) 
		{
			var sortTag = $(this.childNodes[i]).data("z_sort_tag");
			if (sortTag)
			{
				if (sortItems != "") 
				{
					sortItems += ",";
				}
				
				sortItems += sortTag
			}
		}
		
		var sortItem = new Array({name: 'sort_items', value: sortItems});
		
		z_queue_postback(this.id, sortPostbackInfo, sortItem, true);
	};
	
	$(sortBlock).sortable(sortOptions);
}


/* typeselect input field
---------------------------------------------------------- */

function z_typeselect(ElementId, postbackInfo)
{
	if (z_input_updater)
	{
		clearTimeout(z_input_updater);
		z_input_updater = false;
	}
	
	z_input_updater = setTimeout(function()
	{
		var obj = $('#'+ElementId);

		if(obj.val().length >= 2)
		{
			obj.addClass('loading');
			z_queue_postback(ElementId, postbackInfo)
		}
	}, 400);
}


/* Form element validations
----------------------------------------------------------

Grab all "postback" forms, let them be handled by Ajax postback calls.
This function can be run multiple times.

---------------------------------------------------------- */

function z_init_postback_forms()
{
	$("form[action*='postback']").each(function() 
	{
		// store options in hash
		$(":submit,input:image", this).bind('click.form-plugin',function(e) 
		{
			var form = this.form;
			form.clk = this;
		
			if (this.type == 'image') 
			{
				if (e.offsetX != undefined) 
				{
					form.clk_x = e.offsetX;
					form.clk_y = e.offsetY;
				} 
				else if (typeof $.fn.offset == 'function') 
				{ // try to use dimensions plugin
					var offset = $(this).offset();
					form.clk_x = e.pageX - offset.left;
					form.clk_y = e.pageY - offset.top;
				} 
				else 
				{
					form.clk_x = e.pageX - this.offsetLeft;
					form.clk_y = e.pageY - this.offsetTop;
				}
			}
		});
	})
	.submit(function(event)
	{
		if ($('#field-content', this).length > 0 && tinyMCE)
		{
			tinyMCE.triggerSave(true,true);
		}

		var arguments = $(this).formToArray();

		this.clk = this.clk_x = this.clk_y = null;

		var postback	= $(this).data("z_submit_postback");
		var action		= $(this).data("z_submit_action");
		var form_id		= $(this).attr('id');
		var validations = $(this).formValidationPostback();
		
		if(!postback) 
		{
			postback = z_default_form_postback;
		}
		
		if(action) 
		{
			setTimeout(action, 10);
		}

		var files = $('input:file', this).fieldValue();
		var found = false;

		for (var j=0; j < files.length; j++)
		{
			if (files[j])
			{
				found = true;
			}
		}

		if(found) 
		{
			$(this).postbackFileForm(form_id, postback, validations);
		}
		else
		{
			z_queue_postback(form_id, postback, arguments.concat(validations)); 
		}

		event.stopPropagation();
		return false;
	})
	.attr('action', 'pb:installed');
}

$.fn.postbackFileForm = function(trigger_id, postback, validations)
{
	var a = validations;

	a.push({name: "postback", value: postback});
	a.push({name: "z_trigger_id", value: trigger_id});
	a.push({name: "z_pageid", value: z_pageid});

	var $form = this;
	var options = {
		url:  '/postback?' + $.param(a),
		type: 'POST',
		dataType: 'text/javascript'
	};

	// hack to fix Safari hang (thanks to Tim Molendijk for this)
	// see:	 http://groups.google.com/group/jquery-dev/browse_thread/thread/36395b7ab510dd5d
	if ($.browser.safari)
		$.get('/close-connection', fileUpload);
	else
		fileUpload();
	
	// private function for handling file uploads (hat tip to YAHOO!)
	function fileUpload() {
		var form = $form[0];

		if ($(':input[name=submit]', form).length) {
			alert('Error: Form elements must not be named "submit".');
			return;
		}

		var opts = $.extend({}, $.ajaxSettings, options);
		var s = $.extend(true, {}, $.extend(true, {}, $.ajaxSettings), opts);

		var id = 'jqFormIO' + (new Date().getTime());
		var $io = $('<iframe id="' + id + '" name="' + id + '" src="about:blank" />');
		var io = $io[0];

		$io.css({ position: 'absolute', top: '-1000px', left: '-1000px' });

		var xhr = { // mock object
			aborted: 0,
			responseText: null,
			responseXML: null,
			status: 0,
			statusText: 'n/a',
			getAllResponseHeaders: function() {},
			getResponseHeader: function() {},
			setRequestHeader: function() {},
			abort: function() {
				this.aborted = 1;
				$io.attr('src','about:blank'); // abort op in progress
			}
		};

		var g = opts.global;
		
		// trigger ajax global events so that activity/block indicators work like normal
		if (g && ! $.active++) $.event.trigger("ajaxStart");
		if (g) $.event.trigger("ajaxSend", [xhr, opts]);

		if (s.beforeSend && s.beforeSend(xhr, s) === false) {
			s.global && $.active--;
			return;
		}
		if (xhr.aborted)
			return;

		var cbInvoked = 0;
		var timedOut = 0;

		// add submitting element to data if we know it
		var sub = form.clk;
		if (sub) {
			var n = sub.name;
			if (n && !sub.disabled) {
				options.extraData = options.extraData || {};
				options.extraData[n] = sub.value;
				if (sub.type == "image") {
					options.extraData[name+'.x'] = form.clk_x;
					options.extraData[name+'.y'] = form.clk_y;
				}
			}
		}

		// take a breath so that pending repaints get some cpu time before the upload starts
		setTimeout(function() {
			// make sure form attrs are set
			var t = $form.attr('target');
			var a = $form.attr('action');

			// update form attrs in IE friendly way
			form.setAttribute('target',id);
			if (form.getAttribute('method') != 'POST')
				form.setAttribute('method', 'POST');
			if (form.getAttribute('action') != opts.url)
				form.setAttribute('action', opts.url);

			// ie borks in some cases when setting encoding
			if (! options.skipEncodingOverride) {
				$form.attr({
					encoding: 'multipart/form-data',
					enctype:  'multipart/form-data'
				});
			}

			// support timout
			if (opts.timeout)
				setTimeout(function() { timedOut = true; cb(); }, opts.timeout);

			// add "extra" data to form if provided in options
			var extraInputs = [];
			try {
				if (options.extraData)
					for (var n in options.extraData)
						extraInputs.push(
							$('<input type="hidden" name="'+n+'" value="'+options.extraData[n]+'" />')
								.appendTo(form)[0]);
				
				// add iframe to doc and submit the form
				$io.appendTo('body');
				io.attachEvent ? io.attachEvent('onload', cb) : io.addEventListener('load', cb, false);
				form.submit();
			}
			finally {
				// reset attrs and remove "extra" input elements
				form.setAttribute('action',a);
				t ? form.setAttribute('target', t) : $form.removeAttr('target');
				$(extraInputs).remove();
			}
		}, 10);

		var nullCheckFlag = 0;

		function cb() {
			if (cbInvoked++) return;

			io.detachEvent ? io.detachEvent('onload', cb) : io.removeEventListener('load', cb, false);

			var ok = true;
			try {
				if (timedOut) throw 'timeout';
				// extract the server response from the iframe
				var data, doc;

				doc = io.contentWindow ? io.contentWindow.document : io.contentDocument ? io.contentDocument : io.document;

				if ((doc.body == null || doc.body.innerHTML == '') && !nullCheckFlag) {
					// in some browsers (cough, Opera 9.2.x) the iframe DOM is not always traversable when
					// the onload callback fires, so we give them a 2nd chance
					nullCheckFlag = 1;
					cbInvoked--;
					setTimeout(cb, 100);
					return;
				}

				xhr.responseText = doc.body ? doc.body.innerHTML : null;

				xhr.getResponseHeader = function(header){
					var headers = {'content-type': opts.dataType};
					return headers[header];
				};

				var ta = doc.getElementsByTagName('textarea')[0];
				xhr.responseText = ta ? ta.value : xhr.responseText;
				data = $.httpData(xhr, opts.dataType);
			}
			catch(e){
				ok = false;
				$.handleError(opts, xhr, 'error', e);
			}

			// ordering of these callbacks/triggers is odd, but that's how $.ajax does it
			if (ok) {
				eval(data);
				if (g) 
				{
					$.event.trigger("ajaxSuccess", [xhr, opts]);
				}
			}
			if (g) $.event.trigger("ajaxComplete", [xhr, opts]);
			if (g && ! --$.active) $.event.trigger("ajaxStop");
			if (opts.complete) opts.complete(xhr, ok ? 'success' : 'error');

			// clean up
			setTimeout(function() {
				$io.remove();
				xhr.responseXML = null;
			}, 100);
		};
	};
}


// Collect all postback validations from the form elements
$.fn.formValidationPostback = function() 
{
	var a = [];
	if(this.length == 0) return a;

	var form = this[0];
	var els	 = form.elements;

	if (!els) return a;

	for(var i=0, max=els.length; i < max; i++) 
	{
		var el = els[i];
		var n  = el.name;

		if (n)
		{
			var v = $(el).data("z_postback_validation");
			if (v)
			{
				a.push({name: "z_v", value: n+":"+v})
			}
		}
	}
	return a;
}

// Initialize a validator for the element #id
function z_init_validator(id, args)
{
	var elt = $('#'+id);
	if (elt)
	{
		if (!$(elt).data("z_live_validation"))
		{
			$(elt).data("z_live_validation", new LiveValidation(id, args));
		}
	}
	else
	{
		$.misc.error('Validator error: no element with id #'+id, $(id));
	}
}

// Add a validator to the input field
function z_add_validator(id, type, args)
{
	var v = $('#'+id).data("z_live_validation");

	if(v)
	{
		switch (type)
		{
			case 'email':			v.add(Validate.Email, args);		break;
			case 'presence':		v.add(Validate.Presence, args);		break;
			case 'confirmation':	v.add(Validate.Confirmation, args); break;
			case 'acceptance':		v.add(Validate.Acceptance, args);	break;
			case 'length':			v.add(Validate.Length, args);		break;
			case 'format':			v.add(Validate.Format, args);		break;
			case 'numericality':	v.add(Validate.Numericality, args); break;
			default:				$.misc.error("unknown validation: "+type);
		}
	}
}

function z_set_validator_postback(id, postback)
{
	if (postback)
	{
		var pb = $('#'+id).data("z_postback_validation");
		
		if (pb)
		{
			$.misc.error("Element #"+id+" had already a validation postback, add all validations as one batch.", $('#' +id));
		}

		$('#'+id).data("z_postback_validation", postback);
	}
}

function z_validation_error(id, error)
{
	if (error == 'invalid')
	{
		// Generic error - handle it ourselves
		error = "please correct";
	}
	$('#'+id).addClass("form-field-error");
}

// URL encode function that is more RFC compatible.	 Also encodes +, *, / and @.
function urlencode(s)
{
	s = escape(s);
	s = s.replace(/\+/g, '%2B');
	s = s.replace(/\*/g, '%2A');
	s = s.replace(/\//g, '%2F');
	s = s.replace(/@/g, '%40');
	return s;
}

// HTML escape a string so it is safe to concatenate when making tags.
function html_escape(s)
{
	s.replace(/&/, "&amp;").replace(/</, "&lt;").replace(/>/, "&gt;").replace(/"/, "&quot;");
}


// From: http://malsup.com/jquery/form/jquery.form.js

/*
 * jQuery Form Plugin
 * version: 2.28 (10-MAY-2009)
 * @requires jQuery v1.2.2 or later
 *
 * Examples and documentation at: http://malsup.com/jquery/form/
 * Dual licensed under the MIT and GPL licenses:
 *	 http://www.opensource.org/licenses/mit-license.php
 *	 http://www.gnu.org/licenses/gpl.html
 */

/**
 * formToArray() gathers form element data into an array of objects that can
 * be passed to any of the following ajax functions: $.get, $.post, or load.
 * Each object in the array has both a 'name' and 'value' property.	 An example of
 * an array for a simple login form might be:
 *
 * [ { name: 'username', value: 'jresig' }, { name: 'password', value: 'secret' } ]
 *
 * It is this array that is passed to pre-submit callback functions provided to the
 * ajaxSubmit() and ajaxForm() methods.
 */
$.fn.formToArray = function(semantic) {
	var a = [];
	if (this.length == 0) return a;

	var form = this[0];
	var els = semantic ? form.getElementsByTagName('*') : form.elements;
	if (!els) return a;
	for(var i=0, max=els.length; i < max; i++) {
		var el = els[i];
		var n = el.name;
		if (!n) continue;

		var v = $.fieldValue(el, true);
		if (v && v.constructor == Array) {
			for(var j=0, jmax=v.length; j < jmax; j++)
				a.push({name: n, value: v[j]});
		}
		else if (v !== null && typeof v != 'undefined')
			a.push({name: n, value: v});
	}

	// add submitting element to data if we know it
	var sub = form.clk;
	if (sub) {
		var n = sub.name;
		if (n && !sub.disabled) {
			a.push({name: n, value: ''});
		}
	}


	return a;
};

/**
 * Serializes form data into a 'submittable' string. This method will return a string
 * in the format: name1=value1&amp;name2=value2
 */
$.fn.formSerialize = function(semantic) {
	//hand off to jQuery.param for proper encoding
	return $.param(this.formToArray(semantic));
};

/**
 * Serializes all field elements in the jQuery object into a query string.
 * This method will return a string in the format: name1=value1&amp;name2=value2
 */
$.fn.fieldSerialize = function(successful) {
	var a = [];
	this.each(function() {
		var n = this.name;
		if (!n) return;
		var v = $.fieldValue(this, successful);
		if (v && v.constructor == Array) {
			for (var i=0,max=v.length; i < max; i++)
				a.push({name: n, value: v[i]});
		}
		else if (v !== null && typeof v != 'undefined')
			a.push({name: this.name, value: v});
	});
	//hand off to jQuery.param for proper encoding
	return $.param(a);
};

/**
 * Returns the value(s) of the element in the matched set.	For example, consider the following form:
 *
 *	<form><fieldset>
 *		<input name="A" type="text" />
 *		<input name="A" type="text" />
 *		<input name="B" type="checkbox" value="B1" />
 *		<input name="B" type="checkbox" value="B2"/>
 *		<input name="C" type="radio" value="C1" />
 *		<input name="C" type="radio" value="C2" />
 *	</fieldset></form>
 *
 *	var v = $(':text').fieldValue();
 *	// if no values are entered into the text inputs
 *	v == ['','']
 *	// if values entered into the text inputs are 'foo' and 'bar'
 *	v == ['foo','bar']
 *
 *	var v = $(':checkbox').fieldValue();
 *	// if neither checkbox is checked
 *	v === undefined
 *	// if both checkboxes are checked
 *	v == ['B1', 'B2']
 *
 *	var v = $(':radio').fieldValue();
 *	// if neither radio is checked
 *	v === undefined
 *	// if first radio is checked
 *	v == ['C1']
 *
 * The successful argument controls whether or not the field element must be 'successful'
 * (per http://www.w3.org/TR/html4/interact/forms.html#successful-controls).
 * The default value of the successful argument is true.  If this value is false the value(s)
 * for each element is returned.
 *
 * Note: This method *always* returns an array.	 If no valid value can be determined the
 *		 array will be empty, otherwise it will contain one or more values.
 */
$.fn.fieldValue = function(successful) {
	for (var val=[], i=0, max=this.length; i < max; i++) {
		var el = this[i];
		var v = $.fieldValue(el, successful);
		if (v === null || typeof v == 'undefined' || (v.constructor == Array && !v.length))
			continue;
		v.constructor == Array ? $.merge(val, v) : val.push(v);
	}
	return val;
};

/**
 * Returns the value of the field element.
 */
$.fieldValue = function(el, successful) {
	var n = el.name, t = el.type, tag = el.tagName.toLowerCase();
	if (typeof successful == 'undefined') successful = true;

	if (successful && (!n || el.disabled || t == 'reset' || t == 'button' ||
		t == 'radio' && !el.checked ||
		(t == 'submit' || t == 'image') && el.form && el.form.clk != el ||
		tag == 'select' && el.selectedIndex == -1))
			return null;
	
	// Return empty value for non-checked checkboxes
	if (successful && t == 'checkbox' && !el.checked)
		return '';

	if (tag == 'select') {
		var index = el.selectedIndex;
		if (index < 0) return null;
		var a = [], ops = el.options;
		var one = (t == 'select-one');
		var max = (one ? index+1 : ops.length);
		for(var i=(one ? index : 0); i < max; i++) {
			var op = ops[i];
			if (op.selected) {
				var v = op.value;
				if (!v) // extra pain for IE...
					v = (op.attributes && op.attributes['value'] && !(op.attributes['value'].specified)) ? op.text : op.value;
				if (one) return v;
				a.push(v);
			}
		}
		return a;
	}
	return el.value;
};


/**
 * Clears the form data.  Takes the following actions on the form's input fields:
 *	- input text fields will have their 'value' property set to the empty string
 *	- select elements will have their 'selectedIndex' property set to -1
 *	- checkbox and radio inputs will have their 'checked' property set to false
 *	- inputs of type submit, button, reset, and hidden will *not* be effected
 *	- button elements will *not* be effected
 */
$.fn.clearForm = function() {
	return this.each(function() {
		$('input,select,textarea', this).clearFields();
	});
};

/**
 * Clears the selected form elements.
 */
$.fn.clearFields = $.fn.clearInputs = function() {
	return this.each(function() {
		var t = this.type, tag = this.tagName.toLowerCase();
		if (t == 'text' || t == 'password' || tag == 'textarea')
			this.value = '';
		else if (t == 'checkbox' || t == 'radio')
			this.checked = false;
		else if (tag == 'select')
			this.selectedIndex = -1;
	});
};

/**
 * Resets the form data.  Causes all form elements to be reset to their original value.
 */
$.fn.resetForm = function() {
	return this.each(function() {
		// guard against an input with the name of 'reset'
		// note that IE reports the reset function as an 'object'
		if (typeof this.reset == 'function' || (typeof this.reset == 'object' && !this.reset.nodeType))
			this.reset();
	});
};

/**
 * Enables or disables any matching elements.
 */
$.fn.enable = function(b) {
	if (b == undefined) b = true;
	return this.each(function() {
		this.disabled = !b;
	});
};

/**
 * Checks/unchecks any matching checkboxes or radio buttons and
 * selects/deselects and matching option elements.
 */
$.fn.selected = function(select) {
	if (select == undefined) select = true;
	return this.each(function() {
		var t = this.type;
		if (t == 'checkbox' || t == 'radio')
			this.checked = select;
		else if (this.tagName.toLowerCase() == 'option') {
			var $sel = $(this).parent('select');
			if (select && $sel[0] && $sel[0].type == 'select-one') {
				// deselect all other options
				$sel.find('option').selected(false);
			}
			this.selected = select;
		}
	});
};

// helper fn for console logging
// set $.fn.ajaxSubmit.debug to true to enable debug logging
function log() {
	if ($.fn.ajaxSubmit.debug && window.console && window.console.log)
		window.console.log('[jquery.form] ' + Array.prototype.join.call(arguments,''));
}
