/* Zotonic basic Javascript library
----------------------------------------------------------

@package:	Zotonic 2009	
@Author:	Tim Benniks <tim@timbenniks.nl>
@Author:	Marc Worrell <marc@worrell.nl>

Copyright 2009-2011 Tim Benniks, Marc Worrell

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

var z_ws					= false;
var z_ws_opened				= false;
var z_comet_is_running		= false;
var z_doing_postback		= false;
var z_spinner_show_ct		= 0;
var z_postbacks				= [];
var z_default_form_postback = false;
var z_input_updater			= false;
var z_drag_tag				= [];
var z_registered_events		= new Object();
var z_on_visible_checks		= [];
var z_on_visible_timer		= undefined;
var z_unique_id_counter		= 0;
var z_language				= "en";

/* Non modal dialogs
---------------------------------------------------------- */

function z_dialog_open(options)
{
	$.dialogAdd(options);
}

function z_dialog_close()
{
	$.dialogClose();
}

function z_dialog_confirm(options)
{
	html = '<div class="confirm">' + options.text + '</div>'
		 + '<div class="modal-footer">'
		 + '<button class="btn z-dialog-cancel-button">'
		 + (options.cancel||z_translate('Cancel')) 
		 + '</button>'
		 + '<button class="btn btn-primary z-dialog-ok-button">'
		 + (options.ok||z_translate('OK'))
		 + '</button>'
		 + '</div>';
	$.dialogAdd({ 
		title: (options.title||z_translate('Confirm')),
		text: html,
		width: (options.width||'350px')
	});
	$(".z-dialog-cancel-button").click(function() { z_dialog_close(); });
	$(".z-dialog-ok-button").click(function() { 
		z_dialog_close();
		if (options.on_confirm) options.on_confirm();
	});
}

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
	$.noticeRemove($('.notice-item-wrapper'), 400);
}


/* Registered events for javascript triggered actions/postbacks
---------------------------------------------------------- */

function z_event_register(name, func)
{
	z_registered_events[name] = func;
}

function z_event(name, extraParams)
{
	if (z_registered_events[name])
	{
		z_registered_events[name](ensure_name_value(extraParams));
	}
	else if (window.console)
	{
		console.error("z_event: no registered event named: '"+name+"'");
	}
}

/* Call the server side notifier for {postback_notify, Message, Context}
---------------------------------------------------------- */

function z_notify(message, extraParams)
{
	var trigger_id = '';
	if (extraParams != undefined && extraParams.z_trigger_id != undefined) {
		trigger_id = extraParams.z_trigger_id;
		extraParams.z_trigger_id = undefined;
	}

	var extra = ensure_name_value(extraParams);
	if (typeof extra != 'object')
	{
		extra = [];
	}
	extra.push({name: 'z_msg', value: message});
	z_queue_postback(trigger_id, 'notify', extra, true);
}

/* Postback loop
---------------------------------------------------------- */

function z_postback_check() 
{
	if (z_postbacks.length == 0)
	{
		z_doing_postback = false;
	}
	else
	{
		if (z_postback_connected())
		{
			// Send only a single postback at a time.
			z_doing_postback = true;

			var o = z_postbacks.shift();
			z_do_postback(o.triggerID, o.postback, o.extraParams);
		}
		else
		{
			setTimeout("z_postback_check()", 10);
		}
	}
}

function z_opt_cancel(obj)
{
	if(typeof obj.nodeName == 'undefined')
		return false;

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

function z_httpdata( xhr, type, s ) 
{ 
	// lifted from jq1.4.4
	var ct = xhr.getResponseHeader("content-type") || "",
	  xml = type === "xml" || !type && ct.indexOf("xml") >= 0,
	  data = xml ? xhr.responseXML : xhr.responseText;

	if ( xml && data.documentElement.nodeName === "parsererror" ) {
	  $.error( "parsererror" );
	}
	if ( s && s.dataFilter ) {
	  data = s.dataFilter( data, type );
	}
	if ( typeof data === "string" ) {
	  if ( type === "json" || !type && ct.indexOf("json") >= 0 ) {
		data = $.parseJSON( data );
	  } else if ( type === "script" || !type && ct.indexOf("javascript") >= 0 ) {
		$.globalEval( data );
	  }
	}
	return data;
}

function z_queue_postback(triggerID, postback, extraParams, noTriggerValue) 
{
	var triggerValue = '';

	if (triggerID != '' && !noTriggerValue)
	{
		var trigger = $('#'+triggerID).get(0);
		if (trigger)
		{
			if ($(trigger).is(":checkbox") || $(trigger).is(":radio"))
			{
				if ($(trigger).is(":checked"))
				{
					triggerValue = $(trigger).val() || 'on';
				}
			}
			else
			{
				var nodeName = trigger.nodeName.toLowerCase();
		
				if (nodeName == 'input' || nodeName == 'button' || nodeName == 'textarea' || nodeName == 'select')
					triggerValue = $(trigger).val() || '';
			}
		}
	}

	extraParams = extraParams || new Array(); 
	extraParams.push({name: 'triggervalue', value: triggerValue})
	
	var o			= new Object();
	o.triggerID		= triggerID;
	o.postback		= postback;
	o.extraParams	= extraParams;
	z_postbacks.push(o);
	z_postback_check();
}


// Wait with sending postbacks till the websocket connection is open
function z_postback_connected()
{
	return !z_ws || z_ws.readyState != 0;
}


function z_do_postback(triggerID, postback, extraParams) 
{
	// Get params...
	var params = 
		"postback=" + urlencode(postback) + 
		"&z_trigger_id=" + urlencode(triggerID) +
		"&z_pageid=" + urlencode(z_pageid) + 
		"&" + $.param(extraParams);
	
	// logon_form and .setcookie forms are always posted, as they will set cookies.
	if (   z_ws
		&& z_ws.readyState == 1 
		&& triggerID != "logon_form" 
		&& (triggerID == '' || !$('#'+triggerID).hasClass("setcookie")))
	{
		z_ws.send(params);
	}
	else
	{
		z_ajax(triggerID, params);
	}
}

function z_ajax(triggerID, params)
{
	z_start_spinner();

	$.ajax({ 
		url:		'/postback',
		type:		'post',
		data:		params,
		dataType:	'text',
		success: function(data, textStatus) 
		{
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
			setTimeout("z_postback_check()", 0);
		},
		error: function(xmlHttpRequest, textStatus, errorThrown) 
		{
			z_stop_spinner();
			
			$.misc.error("FAIL: " + textStatus);
			z_unmask_error(triggerID);
		}
	});
}

function z_unmask(id)
{
	if (id)
	{
		var trigger = $('#'+id).get(0);
	
		if (trigger && trigger.nodeName.toLowerCase() == 'form') 
		{
			try { $(trigger).unmask(); } catch (e) {};
		}
		$(trigger).removeClass("z_error_upload");
	}
}


function z_unmask_error(id)
{
	if (id)
	{
		z_unmask(id);
		$('#'+id).addClass("z_error_upload");
	}
}


function z_progress(id, value)
{
	if (id)
	{
		var trigger = $('#'+id).get(0);
	
		if (trigger.nodeName.toLowerCase() == 'form') 
		{
			try { $(trigger).maskProgress(value); } catch (e) {};
		}
	}
}

function z_reload(args)
{
	var page = $('#logon_form input[name="page"]');

	if (page.length > 0 && page.val() != "") {
		window.location.href = window.location.protocol+"//"+window.location.host+page.val();
	} else {
		if (typeof args == "undefined")
			window.location.reload(true);
		else {
			var qs = ensure_name_value(args);

			if (qs.length == 1 &&  typeof args.z_language == "string") {
				var href;
				
				if (  window.location.pathname.substring(0,2+z_language.length) == "/"+z_language+"/") {
					href = window.location.protocol+"//"+window.location.host
							+"/"+args.z_language+"/"
							+window.location.pathname.substring(2+args.z_language.length);
				} else {
					href = window.location.protocol+"//"+window.location.host
							+"/"+args.z_language
							+window.location.pathname;
				}
				if (window.location.search == "")
					window.location.href = href;
				else
					window.location.href = href + "?" + window.location.search;
			} else {
				var href = window.location.protocol+"//"+window.location.host+window.location.pathname;
				if (window.location.search == "") {
					window.location.href = href + '?' + $.param(qs);
				} else {
					var loc_qs = $.parseQuery();
					for (var prop in loc_qs) {
						if (typeof loc_qs[prop] != "undefined" && typeof args[prop] == "undefined")
							qs.push({name: prop, value: loc_qs[prop]});
					}
					window.location.href = href+"?" + $.param(qs);
				}
			}
		}
	}
}

/* translations
---------------------------------------------------------- */

function z_translate(text)
{
	if (typeof z_translations != "undefined" && typeof z_translations[text] != "undefined")
		return z_translations[text];
	return text;
}


/* Render text as html nodes
---------------------------------------------------------- */

function z_text_to_nodes(text)
{
	if (text == "") {
		return $(text);
	} else {
		var len = text.length;
		
		if (text.charAt(0) == "<" && text.charAt(len-1) == ">") {
			return $(text);
		} else {
			return $("<span></span>"+text+"<span></span>").slice(1,-1);
		}
	}
}

/* tinyMCE stuff
---------------------------------------------------------- */

/* Initialize all non-initialized tinymce controls */
function z_tinymce_init()
{
	$(".tinymce-init:visible").each(function() { 
		var self = $(this);
		setTimeout(function() { 
			var ti = jQuery.extend({}, tinyInit);
			if (self.attr('dir')) {
				ti.directionality = self.attr('dir');
			}
			self.tinymce(ti); 
		}, 200);
	}).removeClass('tinymce-init').addClass('tinymce');
}

function z_tinymce_add(element) 
{
	$("textarea.tinymce", element).each(function() {
		if (typeof $(this).tinymce == 'function') {
			var self = $(this);
			setTimeout(function() { 
				if (typeof tinyInit == 'object') self.tinymce(tinyInit);
				else self.tinymce({}); 
			}, 200);
		} else if (typeof tinyMCE == 'object') {
			var mce_id = $(this).attr('id');
			setTimeout(function() { tinyMCE.execCommand('mceAddControl',false, mce_id); }, 200);
		}
	});
}

function z_tinymce_save(element)
{
	var tiny = $("textarea.tinymce", element);
	if (tiny.length > 0) {
		if (typeof tiny.tinymce == "function") {
			tiny.each(function() { $(this).tinymce().save(); });
		} else if (typeof tinyMCE == 'object') {
			tinyMCE.triggerSave(true,true);
		}
	}
}

function z_tinymce_remove(element) 
{
	$("textarea.tinymce", element).each( function() {
		if (typeof(tinyMCE) != 'undefined') {
			tinyMCE.execCommand('mceRemoveControl',false, $(this).attr('id')); 
		} else if (typeof $(this).tinymce == 'function') {
			$(this).tinymce().remove();
		}
	});
}


/* Comet long poll or WebSockets connection
---------------------------------------------------------- */

function z_stream_start(host)
{
	if (!z_ws && !z_comet_is_running)
	{
		if ("WebSocket" in window && window.location.protocol == "http:") 
		{
			z_websocket_start(host);
		}
		else
		{
			setTimeout(function() { z_comet(host); }, 2000);
			z_comet_is_running = true;
		}
	}
}

function z_comet(host) 
{
	if (host != window.location.host && window.location.protocol == "http:")
	{
		var url = window.location.protocol + '//' + host + "/comet/subdomain?z_pageid=" + urlencode(z_pageid);
		var comet = $('<iframe id="z_comet_connection" name="z_comet_connection" src="'+url+'" />');
		comet.css({ position: 'absolute', top: '-1000px', left: '-1000px' });
		comet.appendTo("body");
	}
	else
	{
		z_comet_host()
	}
}

function z_comet_host()
{
	$.ajax({ 
		url: window.location.protocol + '//' + window.location.host + '/comet',
		type:'post',
		data: "z_pageid=" + urlencode(z_pageid),
		dataType: 'text',
		success: function(data, textStatus) 
		{
			z_comet_data(data);
			setTimeout(function() { z_comet_host(); }, 1000);
		},
		error: function(xmlHttpRequest, textStatus, errorThrown) 
		{
			setTimeout(function() { z_comet_host(); }, 1000);
		}
	});
}


function z_comet_data(data)
{
	try 
	{
		eval(data);
		z_init_postback_forms();
	} 
	catch (e)
	{
		$.misc.error("Error evaluating ajax return value: " + data);
		$.misc.warn(e);
	}
}


function z_websocket_start(host)
{
	z_ws = new WebSocket("ws://"+document.location.host+"/websocket?z_pageid="+z_pageid);

	z_ws.onopen = function() { z_ws_opened = true; };
	z_ws.onerror = function() { };

	z_ws.onclose = function (evt) 
	{
		if (z_ws_opened)
		{
			// Try to reopen once, might be closed due to an server side error
			z_ws_opened = false;
			setTimeout(function() { z_websocket_start(host); }, 100);
		}
		else
		{
			// Failed opening websocket connection - try to start comet
			z_ws = undefined;
			setTimeout(function() { z_comet(host); }, 2000);
			z_comet_is_running = true;
		}
	};

	z_ws.onmessage = function (evt)
	{
		z_comet_data(evt.data);
		setTimeout("z_postback_check()", 0);
	};
}


/* Utility functions
---------------------------------------------------------- */

function z_is_enter_key(event) 
{
	return (event && event.keyCode == 13);
}


function z_has_flash()
{
	if (navigator.plugins && navigator.plugins.length>0) {
		var type = 'application/x-shockwave-flash';
		var mimeTypes = navigator.mimeTypes;
		return (mimeTypes && mimeTypes[type] && mimeTypes[type].enabledPlugin);
	} else if(navigator.appVersion.indexOf("Mac")==-1 && window.execScript) {
		try {
			obj = new ActiveXObject("ShockwaveFlash.ShockwaveFlash");
			return true;
		} catch(err) {
			return false;
		}
	}
	return false;
}


function z_ensure_id(elt)
{
	var id = $(elt).attr('id');
	if (id == undefined) {
		id = z_unique_id();
		$(elt).attr('id', id);
	}
	return id;
}

function z_unique_id()
{
	do {
		var id = '-z-' + z_unique_id_counter++;
	} while ($('#'+id).length > 0);
	return id;
}


/* Spinner, show when waiting for a postback
---------------------------------------------------------- */

function z_start_spinner()
{
	if (z_spinner_show_ct++ == 0)
	{
		$(document.body).addClass('wait');
		$('#spinner').fadeIn(100);
	}
}

function z_stop_spinner() 
{
	if (--z_spinner_show_ct == 0)
	{
		$('#spinner').fadeOut(100);
		$(document.body).removeClass('wait');
	}
}


/* Drag & drop interface to the postback
---------------------------------------------------------- */

function z_draggable(dragObj, dragOptions, dragTag) 
{
	dragObj.draggable(dragOptions).data("z_drag_tag", dragTag);
	z_drag_tag[dragObj.attr('id')] = dragTag;
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

function z_sortable(sortableObj, sortTag) 
{
	sortableObj.data("z_sort_tag", sortTag);
}

function z_sorter(sortBlock, sortOptions, sortPostbackInfo) 
{
	sortOptions.update = function() 
	{
		var sortItems = "";

		for (var i = 0; i < this.childNodes.length; i++) 
		{
			var sortTag = $(this.childNodes[i]).data("z_sort_tag") 
			if (!sortTag && this.childNodes[i].id)
			{
				sortTag = z_drag_tag[this.childNodes[i].id];
			}
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


/* Lazy loading of content, based on visibility of an element
---------------------------------------------------------- */

function z_on_visible(CssSelector, Func)
{
	z_on_visible_checks.push({selector: CssSelector, func: Func});
	if (z_on_visible_timer == undefined) {
		z_on_visible_timer = setInterval(function() {
			z_on_visible_check();
		}, 350);
	}
}

function z_on_visible_check()
{
	for (var i = z_on_visible_checks.length-1; i>=0; i--) {
		var elt = $(z_on_visible_checks[i].selector).get(0);
		if (elt != undefined) {
			if ($(elt).is(":visible") && isScrolledIntoView(elt)) {
				z_on_visible_checks[i].func.call(elt);
				z_on_visible_checks.splice(i, 1);
			}
		}
	}
	if (z_on_visible_checks.length == 0) {
		clearInterval(z_on_visible_timer);
		z_on_visible_timer = undefined;
	}
}


function isScrolledIntoView(elem)
{
	var docViewTop = $(window).scrollTop();
	var docViewBottom = docViewTop + $(window).height();

	var elemTop = $(elem).offset().top;
	var elemBottom = elemTop + $(elem).height();

	return (elemBottom >= docViewTop) && (elemTop <= docViewBottom);
	// && (elemBottom <= docViewBottom) &&	(elemTop >= docViewTop);
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
		theForm = this;
		z_tinymce_save(theForm);
		
		submitFunction = function(ev) {
			var arguments = $(theForm).formToArray();

			try { $(theForm).mask("", 100); } catch (e) {};

			var postback	= $(theForm).data("z_submit_postback");
			var action		= $(theForm).data("z_submit_action");
			var form_id		= $(theForm).attr('id');
			var validations = $(theForm).formValidationPostback();
		
			if(!postback) 
			{
				postback = z_default_form_postback;
			}
		
			if(action) 
			{
				setTimeout(action, 10);
			}

			var use_post = $(theForm).hasClass("z_cookie_form");
			if (typeof(z_only_post_forms) != "undefined" && z_only_post_forms)
			{
				use_post = true;
			}
			else
			{
				var files = $('input:file', theForm).fieldValue();
				for (var j=0; j < files.length && !use_post; j++) 
				{
					if (files[j])
					{
						use_post = true;
					}
				}
			}
		
			if (use_post) 
			{
				$(theForm).postbackFileForm(form_id, postback, validations);
			}
			else
			{
				theForm.clk = theForm.clk_x = theForm.clk_y = null;
				z_queue_postback(form_id, postback, arguments.concat(validations)); 
			}
			ev.stopPropagation();
			return false;
		};
		
		return z_form_submit_validated_delay(theForm, event, submitFunction);
	})
	.attr('action', '#pb-installed');
}

function z_form_submit_validated_delay(theForm, event, submitFunction) 
{
	var validations = $(theForm).formValidationPostback();
	
	if (validations.length > 0 && !event.zIsValidated)
	{
		// There are form validations and they are not done yet.
		if (!event.zAfterValidation) 
		{
			event.zAfterValidation = new Array();
		}
		event.zAfterValidation.push({ func: submitFunction, context: theForm });
		return true;
	}
	else
	{
		// No form validations, or already validated
		return submitFunction.call(theForm, event);
	}
}

function z_form_submit_validated_do(event)
{
	var ret = true;
	
	if (event.zAfterValidation)
	{
		$.each(event.zAfterValidation, function(){
			ret = typeof this.func == 'function' && this.func.call(this.context, event) && ret;
		});
		event.zAfterValidation.length = 0;
	}
	return ret;
}


$.fn.postbackFileForm = function(trigger_id, postback, validations)
{
	var a = validations;

	a.push({name: "postback", value: postback});
	a.push({name: "z_trigger_id", value: trigger_id});
	a.push({name: "z_pageid", value: z_pageid});
	a.push({name: "z_comet", value: z_comet_is_running || z_ws});
	
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
				options.extraData['z_submitter'] = n;
				options.extraData[n] = sub.value;
				if (sub.type == "image") {
					options.extraData[name+'.x'] = form.clk_x;
					options.extraData[name+'.y'] = form.clk_y;
				}
			}
		}
		form.clk = form.clk_x = form.clk_y = null;

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

		var domCheckCount = 3;

		function cb() {
			if (cbInvoked++) return;

			io.detachEvent ? io.detachEvent('onload', cb) : io.removeEventListener('load', cb, false);

			var ok = true;
			try {
				if (timedOut) throw 'timeout';
				// extract the server response from the iframe
				var data, doc;

				doc = io.contentWindow ? io.contentWindow.document : io.contentDocument ? io.contentDocument : io.document;
				if (doc.body == null || doc.body.innerHTML == '') {
					if (--domCheckCount) {
						// in some browsers (Opera) the iframe DOM is not always traversable when
						// the onload callback fires, so we loop a bit to accommodate

						// MW: looks like this is not a timing issue but Opera triggering a
						//	   load event on the 100 continue.
						cbInvoked = 0;
						io.addEventListener('load', cb, false);
						return;
					}
					log('Could not access iframe DOM after 50 tries.');
					return;
				}

				xhr.responseText = doc.body ? doc.body.innerHTML : null;
				
				xhr.getResponseHeader = function(header){
					var headers = {'content-type': opts.dataType};
					return headers[header];
				};

				var ta = doc.getElementsByTagName('textarea')[0];
				xhr.responseText = ta ? ta.value : xhr.responseText;
				data = z_httpdata(xhr, opts.dataType);
			}
			catch(e){
				ok = false;
				$.event.trigger("ajaxError", [xhr, opts, e]);
			}
			
			// ordering of these callbacks/triggers is odd, but that's how $.ajax does it
			if (ok) {
				try {
					eval(data);
				} catch (e) {
					z_unmask_error(form.id);
				}
				if (g) 
				{
					$.event.trigger("ajaxSuccess", [xhr, opts]);
				}
			} else {
				z_unmask_error(form.id);
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

		if (n && !el.disabled)
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
		if (elt.attr('type') == 'radio')
		{
			$('input[name='+elt.attr('name')+']').each(function() {
				addLiveValidation(this, args);
			});
		}
		else
		{
			addLiveValidation(elt, args);
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
	var elt = $('#'+id);
	
	if (elt.attr('type') == 'radio')
		elt = $('input[name='+elt.attr('name')+']');

	elt.each(function() {
		var v = getLiveValidation(this);
		if (v)
		{
			if (args['pattern'])
			{
				args['pattern'] = new RegExp(args['pattern']);
			}
			switch (type)
			{
				case 'email':			v.add(Validate.Email, args);		break;
								case 'date':					v.add(Validate.Date, args);				break;
				case 'presence':		v.add(Validate.Presence, args);		break;
				case 'confirmation':	v.add(Validate.Confirmation, args); break;
				case 'acceptance':		v.add(Validate.Acceptance, args);	break;
				case 'length':			v.add(Validate.Length, args);		break;
				case 'format':			v.add(Validate.Format, args);		break;
				case 'numericality':	v.add(Validate.Numericality, args); break;
				case 'custom':			v.add(Validate.Custom, args);		break;
				case 'postback':		
					args['z_id'] = id;
					v.add(Validate.Postback, args);
					break;
				default:
					$.misc.error("unknown validation: "+type);
					break;
			}
		}
	});
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

function z_validation_on_invalid(id, on_invalid)
{
	$('#'+id).each(function() {
		if (this.tagName.toLowerCase() == 'form')
		{
			var formObj = LiveValidationForm.getInstance(this);
			formObj.onInvalid = on_invalid;
		}
	});
}


function z_async_validation_result(id, isValid, testedValue)
{
	var v = getLiveValidation($('#'+id));
	if (v && $('#'+id).val() == testedValue)
	{
		v.asyncValidationResult(isValid, testedValue);
	}
}

// Called by the server on validation errors
function z_validation_error(id, error)
{
	var v = getLiveValidation($('#'+id));
	if (v)
	{
		if (error == 'invalid')
		{
			// Generic error - handle it ourselves
			error = "please correct";
		}
		v.showErrorMessage(error);
	}
}


// Execute a function by name
function z_call_function_by_name(name, context)
{
	var args = Array.prototype.slice.call(arguments).splice(2);
	var namespaces = name.split(".");
	var func = namespaces.pop();
	for(var i = 0; i < namespaces.length; i++) {
		context = context[namespaces[i]];
	}
	return context[func].apply(this, args);
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


// Convert an object to an array with {name: xxx, value: yyy} pairs
function ensure_name_value(a)
{
	if ((typeof a == 'object') && !(a instanceof Array))
	{
		var n = []
		for (var prop in a)
		{
			if (a[prop] != undefined)
				n.push({name: prop, value: a[prop]});
		}
		return n;
	}
	else
	{
		return a;
	}
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
		if ($(el).hasClass("nosubmit")) continue;
		if ($(el).attr("type") == 'submit') continue;

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
			a.push({name: 'z_submitter', value: n});
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
	if (window.console && window.console.log)
		window.console.log('[jquery.form] ' + Array.prototype.join.call(arguments,''));
}



function is_equal(x, y) {
	if ( x === y ) return true;
	if ( ! ( x instanceof Object ) || ! ( y instanceof Object ) ) return false;
	if ( x.constructor !== y.constructor ) return false;
	for ( var p in x ) {
		if ( ! x.hasOwnProperty( p ) ) continue;
		if ( ! y.hasOwnProperty( p ) ) return false;
		if ( x[ p ] === y[ p ] ) continue;
		if ( typeof( x[ p ] ) !== "object" ) return false;
		if ( ! is_equal( x[ p ],  y[ p ] ) ) return false;
	}
	for ( p in y ) {
		if ( y.hasOwnProperty( p ) && ! x.hasOwnProperty( p ) ) return false;
	}
	return true;
}


/**
 * A simple querystring parser.
 * Example usage: var q = $.parseQuery(); q.fooreturns	"bar" if query contains "?foo=bar"; multiple values are added to an array. 
 * Values are unescaped by default and plus signs replaced with spaces, or an alternate processing function can be passed in the params object .
 * http://actingthemaggot.com/jquery
 *
 * Copyright (c) 2008 Michael Manning (http://actingthemaggot.com)
 * Dual licensed under the MIT (MIT-LICENSE.txt)
 * and GPL (GPL-LICENSE.txt) licenses.
 **/
$.parseQuery = function(qs,options) {
	var q = (typeof qs === 'string'?qs:window.location.search), o = {'f':function(v){return unescape(v).replace(/\+/g,' ');}}, options = (typeof qs === 'object' && typeof options === 'undefined')?qs:options, o = jQuery.extend({}, o, options), params = {};
	jQuery.each(q.match(/^\??(.*)$/)[1].split('&'),function(i,p){
		p = p.split('=');
		p[1] = o.f(p[1]);
		params[p[0]] = params[p[0]]?((params[p[0]] instanceof Array)?(params[p[0]].push(p[1]),params[p[0]]):[params[p[0]],p[1]]):p[1];
	});
	return params;
}

