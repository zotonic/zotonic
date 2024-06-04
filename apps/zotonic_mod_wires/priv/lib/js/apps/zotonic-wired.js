/* Zotonic basic Javascript library
----------------------------------------------------------

@package:   Zotonic 2009
@Author:    Tim Benniks <tim@timbenniks.nl>
@Author:    Marc Worrell <marc@worrell.nl>

Copyright 2009-2023 Tim Benniks, Marc Worrell

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

var zotonic                 = zotonic || {};

// Client state
var z_language              = "en";
var z_languages             = ["en"];
// var z_userid;
var z_editor;
var z_default_form_postback;
var z_init_postback_forms_timeout = false;

// Misc state
var z_input_updater         = false;
var z_drag_tag              = [];
var z_registered_events     = {};
var z_on_visible_checks     = [];
var z_on_visible_timer;
var z_unique_id_counter     = 0;
var z_transport_queue       = [];


/* Startup
---------------------------------------------------------- */

cotonic.ready.then(function() { zotonic_startup() });

// Lazy css loading - set the correct media on load.
window.addEventListener('load',
    function() {
        const elts = document.querySelectorAll("link[media='none']");
        elts.forEach(function(e) {
            if (e.hasAttribute('media-onload')) {
                e.setAttribute('media', e.getAttribute('media-onload'));
                e.removeAttribute('media-onload');
            }
        });
    }, false);


function zotonic_startup() {
    // Initialize the wires if the bridge is starting up
    // cotonic.broker.subscribe("$bridge/origin/status", function() {
    //     cotonic.broker.unsubscribe("$bridge/origin/status", { wid: 'zotonicjs' });
    //     if (typeof window.zotonicPageInit == 'function') {
    //         window.zotonicPageInit();
    //     }
    // }, { wid: 'zotonicjs'});

    // Handle data sent by the server
    cotonic.broker.subscribe("zotonic-transport/eval", function(msg) {
        try {
            eval(msg.payload);
            if (z_init_postback_forms_timeout) {
                clearTimeout(z_init_postback_forms_timeout);
            }
            z_init_postback_forms_timeout = setTimeout(function() {
                    z_init_postback_forms_timeout = false;
                    z_init_postback_forms();
                }, 10);
        } catch(e) {
            console.log("Error on eval", e, msg.payload);
        }
    }, { wid: 'zotonicjs'});

    cotonic.broker.subscribe(
        "zotonic-transport/progress",
        function(msg) {
            if (msg.payload.is_auto_unmask && msg.payload.percentage >= 100) {
                z_unmask(msg.payload.form_id);
            } else {
                z_progress(msg.payload.form_id, msg.payload.percentage);
            }
        }, { wid: 'zotonicprogress'});

    cotonic.broker.subscribe(
        "model/alert/post",
        function(msg) {
            z_dialog_alert(msg.payload);
        }, { wid: 'zotonicalert'});

    cotonic.broker.subscribe(
        "model/clipboard/post/copy",
        function(msg) {
            if (msg.payload?.message['data-text'] !== undefined) {
                navigator.clipboard?.writeText(msg.payload.message['data-text']);
            } else if (msg.payload?.text) {
                navigator.clipboard?.writeText(msg.payload.text);
            }
        }, { wid: 'zotonicclip'});

    // Register the client-id to reuse on subsequent pages
    cotonic.broker.subscribe(
            "$bridge/origin/status",
            function(msg) {
                if (msg.payload.is_connected) {
                    cotonic.broker.publish("model/sessionStorage/post/mqtt-origin-client-id", msg.payload.client_id);
                }
            }, { wid: "zotonicjs" });

    // Start the client-server bridge
    cotonic.broker.call("model/sessionStorage/get/mqtt-origin-client-id")
        .then(
            function(msg) {
                cotonic.mqtt_bridge.newBridge('origin', {
                    client_id: msg.payload || '',
                    clean_start: true
                });
            });

    // Start bridge to opener if opened from a window with a cotonic broker
    if (window.opener && typeof window.opener.cotonic === 'object') {
        cotonic.mqtt_bridge.newBridge('opener', {
            client_id: '',
            clean_start: true
        });
    }

    setInterval(function() { z_transport_queue_check(); }, 500);
}


/* Non modal dialogs
---------------------------------------------------------- */

function z_dialog_open(options)
{
    $.dialogAdd(options);
}

function z_dialog_close(options)
{
    if (typeof options === "number") {
        $.dialogClose({ level: options });
    } else if (options === "top") {
        $.dialogClose({ level: "top" });
    } else {
        $.dialogClose(options);
    }
}

function z_dialog_confirm(options)
{
    var html,
        backdrop,
        is_danger,
        btn_class;

    if (typeof options.backdrop == 'undefined') {
        backdrop = options.backdrop
    } else {
        backdrop = true;
    }
    is_danger = options.is_danger || false;
    btn_class = is_danger ? "btn-danger" : "btn-primary";
    html = '<div class="confirm">' + options.text + '</div>'
         + '<div class="modal-footer">'
         + '<button class="btn btn-default z-dialog-cancel-button">'
         + (options.cancel||z_translate('Cancel'))
         + '</button>'
         + '<button class="btn '+btn_class+' z-dialog-ok-button">'
         + (options.ok||z_translate('OK'))
         + '</button>'
         + '</div>';
    $.dialogAdd({
        title: (options.title||z_translate('Confirm')),
        text: html,
        width: (options.width),
        backdrop: backdrop,
        level: options.level ?? "top"
    });
    $(".z-dialog-cancel-button").click(function() {
        z_dialog_close();
        if (options.on_cancel) options.on_cancel();
    });
    $(".z-dialog-ok-button").click(function() {
        z_dialog_close();
        if (options.on_confirm) options.on_confirm();
    });
}

function z_dialog_alert(options)
{
    var html,
        backdrop;

    if (typeof options.backdrop == 'undefined') {
        backdrop = options.backdrop
    } else {
        backdrop = true;
    }
    html = '<div class="confirm">' + options.text + '</div>'
         + '<div class="modal-footer">'
         + '<button class="btn btn-primary z-dialog-ok-button">'
         + (options.ok||z_translate('OK'))
         + '</button>'
         + '</div>';
    $.dialogAdd({
        title: (options.title||z_translate('Alert')),
        title_icon: "glyphicon glyphicon-exclamation-sign",
        text: html,
        width: (options.width),
        backdrop: backdrop
    });
    $(".z-dialog-ok-button").click(function() {
        z_dialog_close();
        if (options.on_confirm) options.on_confirm();
    });
}

function z_dialog_overlay_open(options)
{
    let overlay_id = 'modal-overlay';
    let level;
    let is_top = false;

    if (options.level === 'top') {
        is_top = true;
        level = 3;
        overlay_id = overlay_id + "-level-" + level;
    } else if (typeof options.level !== 'undefined' && options.level > 0) {
        level = options.level;
        overlay_id = overlay_id + "-level-" + level;
    } else {
        level = 0;
    }

    let $overlay = $('#'+overlay_id);
    let style;

    if (is_top) {
        style = { zIndex: 9000 };
    } else {
        style = {};
    }

    if ($overlay.length > 0) {
        $overlay
            .html('<a href="#close" class="modal-overlay-close" onclick="return z_dialog_overlay_close(this)">&times;</a>' + options.html)
            .attr('class', 'modal-overlay')
            .css(style)
            .show();
    } else {
        const html = '<div class="modal-overlay modal-overlay-level-' + level + '" id="' + overlay_id + '">' +
                     '<a href="#close" class="modal-overlay-close" onclick="return z_dialog_overlay_close(this)">&times;</a>' +
                     options.html +
                     '</div>';
        $('body').append(html);
        $overlay = $('#'+overlay_id);
        $overlay.css(style);

        setTimeout(function() {
            // If there already is an input field with focus, do nothing
            if ($overlay.find("input:focus").length == 0) {
                $overlay.find('.survey-overlay-close').focus();
            }
        }, 50);
    }

    if (options.class) {
        $overlay.addClass(options.class);
    }

    $(document).keyup(function(e) {
         if (e.key === "Escape") { // escape key maps to keycode `27`
            z_dialog_overlay_close();
        }
    });
    
    $('body').addClass('overlay-open');
    
    if (typeof($.widgetManager) != 'undefined') {
        $overlay.widgetManager();
    }

    z_editor_add($overlay);
}

function z_dialog_overlay_close( closeButton )
{
    var $overlay;

    if (typeof closeButton !== 'undefined') {
        $overlay = $(closeButton).closest(".modal-overlay");
    } else {
        $overlay = $('.modal-overlay');
    }

    $overlay.remove();
    
    if ($('.modal-overlay').length == 0) {
        $('body').removeClass('overlay-open');
    }

    return false;
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

function z_event_remove(name)
{
    delete z_registered_events[name];
}

function z_event(name, extraParams)
{
    if (z_registered_events[name])
    {
        z_registered_events[name](ensure_name_value(extraParams));
    }
    else
    {
        $.misc.error("z_event: no registered event named: '"+name+"'");
    }
}

/* Call the server side notifier for {postback_notify, Message, Context}
---------------------------------------------------------- */

function z_notify(message, extraParams)
{
    var trigger_id = '';
    var target_id;
    var params = extraParams || [];
    var delegate = params.z_delegate || 'notify';

    if (typeof params == 'object') {
        if (params.z_trigger_id !== undefined) {
            trigger_id = params.z_trigger_id;
            delete params.z_trigger_id;
        }
        target_id = params.z_target_id || undefined;
    }
    params = ensure_name_value(params);

    var postbackAttr = document.body.getAttribute("data-wired-postback");
    if (postbackAttr) {
        params.push({ name: "z_postback_data", value: JSON.parse(postbackAttr) });
    }

    var notify = {
        _type: "postback_notify",
        message: message,
        trigger: trigger_id,
        target: target_id,
        data: {
            q: params
        }
    };
    var options = {
        trigger_id: trigger_id
    };
    if (trigger_id) {
        options.ack = function(_ack_msg, _options) {
            z_unmask(trigger_id);
        };
    }

    z_transport_queue_add(delegate, 'ubf', notify, options);
    z_transport_queue_check();
}


/* Transport between user-agent and server
---------------------------------------------------------- */

// Queue any data to be transported to the server
function z_transport(delegate, content_type, data, options)
{
    options = options || {};
    if (options.transport == 'fileuploader' && cotonic.whereis("fileuploader")) {
        // Post via the fileuploader worker
        let fileInputs = $('input:file', options.post_form);
        let files = [];

        fileInputs.each(function() {
            let fs = $(this).get(0).files;
            for (let i = 0; i < fs.length; i++) {
                files.push({
                    name: $(this).attr('name'),
                    file: fs[i]
                })
            }
        });
        // TODO: fill in failure topic and progress
        data.data.q = data.data.q.concat($(options.post_form).formToArray());
        let msg = {
            files: files,
            ready_msg: data,
            ready_topic: "$promised/bridge/origin/zotonic-transport/" + delegate,
            progress_topic: "zotonic-transport/progress",
            progress_msg: { form_id: data.trigger },
            failure_topic: "model/alert/post",
            failure_msg: { text: "Error during upload" }
        }
        cotonic.broker.publish("model/fileuploader/post/new", msg);
    } else if (options.transport == 'form' || options.transport == 'fileuploader') {
        cotonic.broker.call("$promised/bridge/origin/model/mqtt_ticket/post/new")
            .then( function(msg) {
                if (msg.payload.status == 'ok') {
                    const ticket = msg.payload.result;
                    z_transport_form({
                        url: "/mqtt-transport/" + ticket + "/zotonic-transport/" + delegate,
                        postback: data,
                        options: options,
                        progress_topic: "~client/zotonic-transport/progress",
                        reply_topic: "~client/zotonic-transport/eval"
                    });

                } else {
                    console.error("z_transport: could not obtain MQTT ticket for form post", [ msg.payload ]);
                    z_transport_queue_add(delegate, content_type, data, options);
                }
            });
    } else if (options.dedup_key) {
        const message = {
            topic: "$promised/bridge/origin/zotonic-transport/" + delegate,
            payload: data,
        }
        cotonic.broker.publish(
            "model/dedup/post/message/" + btoa(options.dedup_key),
            message,
            { qos: 1 });
    } else {
        cotonic.broker.publish(
            "$promised/bridge/origin/zotonic-transport/" + delegate,
            data,
            { qos: 1 });
    }
}


function z_transport_queue_add( delegate, content_type, data, options )
{
    z_transport_queue.push({
        delegate: delegate,
        content_type: content_type,
        data: data,
        options: options
    });
}

function z_transport_queue_check()
{
    const html = document.body.parentElement;

    if(!html) return;

    const authState = html.dataset.uiStateAuthAuth;

    // Make sure the auth worker is ready so the postbacks are
    // either authenticated, or not... Not something in between
    // when the auth worker is still starting.
    if (    z_transport_queue.length > 0
        &&  html.classList.contains('ui-state-bridge-connected')
        &&  (authState === 'anonymous' || authState === 'user')) {
        let trans = z_transport_queue.shift();
        z_transport(trans.delegate, trans.content_type, trans.data, trans.options);
    }
}

// Queue form data to be transported to the server
// This is called by the server generated javascript and jquery triggered postback events.
// 'transport' is one of: '', 'form', 'fileuploader'
function z_queue_postback(trigger_id, postback, extraParams, noTriggerValue, transport, optPostForm, extraOptions)
{
    let triggervalue = '';
    let trigger;
    let target_id;

    if (typeof extraParams == 'object') {
        target_id = extraParams.z_target_id || undefined;
    }
    if (transport === true) {
        transport = '';
    }
    if (trigger_id) {
        trigger = $('#'+trigger_id).get(0);
    }
    if (trigger && !noTriggerValue) {
        if ($(trigger).is(":checkbox") || $(trigger).is(":radio")) {
            if ($(trigger).is(":checked")) {
                triggervalue = $(trigger).val() || 'on';
            }
        } else {
            var nodeName = trigger.nodeName.toLowerCase();
            if (nodeName == 'input' || nodeName == 'button' || nodeName == 'textarea' || nodeName == 'select') {
                triggervalue = $(trigger).val() || '';
            }
        }
    }
    params = extraParams || [];
    params = ensure_name_value(params);

    const postbackAttr = document.body.getAttribute("data-wired-postback");
    if (postbackAttr) {
        params.push({ name: "z_postback_data", value: JSON.parse(postbackAttr) });
    }

    const pb_event = {
        _type: "postback_event",
        postback: postback,
        trigger: trigger_id,
        target: target_id,
        triggervalue: triggervalue,
        data: {
            q: params
        }
    };

    // if (!transport) {
    //     if ((trigger_id == "logon_form") || (trigger && $(trigger).hasClass("setcookie"))) {
    //         transport = 'ajax';
    //     }
    // }

    // logon_form and .setcookie forms are always posted, as they will set cookies.
    const options = {
        transport: transport,
        trigger_id: trigger_id,
        post_form: optPostForm,
        dedup_key: extraOptions?.dedup_key
    };

    z_transport_queue_add('postback', 'ubf', pb_event, options);
    z_transport_queue_check();
}

// function z_postback_opt_qs(extraParams)
// {
//     if (typeof extraParams == 'object' && extraParams instanceof Array) {
//         return {
//             _type: "q",
//             q: ensure_name_value(extraParams)
//         };
//     } else {
//         return extraParams;
//     }
// }


function z_mask(id)
{
    if (id && typeof id == "string")
    {
        var trigger;
        if (id.charAt(0) == ' ') {
            trigger = $(id);
        } else {
            trigger = $('#'+id);
        }
        trigger.each(function() { try { $(this).mask(); } catch (e) {}});
    }
}


function z_unmask(id)
{
    if (id && typeof id == "string")
    {
        var trigger;
        if (id == "body" || id.charAt(0) == ' ') {
            trigger = $(id);
        } else {
            trigger = $('#'+id);
        }
        trigger.each(function() { try { $(this).unmask(); } catch (e) {}});
        trigger.each(function() { $(this).removeClass("z_error_upload"); });
    }
}

function z_unmask_error(id)
{
    if (id && typeof id == "string")
    {
        var trigger;
        if (id == "body" || id.charAt(0) == ' ') {
            trigger = $(id);
        } else {
            trigger = $('#'+id);
        }
        trigger.each(function() { try { $(this).unmask(); } catch (e) {}});
        trigger.each(function() { $(this).addClass("z_error_upload"); });
    }
}


function z_progress(id, value)
{
    if (id)
    {
        var trigger;
        if (id == "body" || id.charAt(0) == ' ') {
            trigger = $(id);
        } else {
            trigger = $('#'+id);
        }
        trigger = trigger.get(0);

        if (trigger) {
            switch (trigger.nodeName.toLowerCase()) {
                case 'input':
                case 'button':
                    break;
                case 'form':
                case 'body':
                case 'div':
                    try { $(trigger).maskProgress(value); } catch (e) {};
                    break;
                default:
                    break;
            }
        }
    }
}

function z_reload(args)
{
    var page = $('#logon_form input[name="page"]'),
        qs = ensure_name_value(args),
        rewriteUrl,
        newLanguage,
        href,
        re,
        pathname,
        parts;

    if (page.length > 0 && page.val() !== "" && page.val() !== '#reload') {
        window.location.href = `${ window.location.protocol }//${ window.location.host }${ page.val() }`;
        return;
    } 

    if (typeof args === "undefined") {
        window.location.reload(true);
        return;
    }

    newLanguage = args.z_language;

    if (typeof newLanguage === "string") {
        // Change the language cookie when it is set. 
        for (const cookie of document.cookie.split(';') ){
            const kv = cookie.trim().split("=");
            if(kv[0] === "z.lang" && kv[1] !== newLanguage) {
                const expirationDate = new Date();
                expirationDate.setTime(expirationDate.getTime() + (365 * 24 * 60 * 60 * 1000));
                const expires = "expires=" + expirationDate.toUTCString();
                document.cookie = `z.lang=${ newLanguage };${ expires };path=/`;
            }
        }

        // Add or remove language from URL:
        pathname = window.location.pathname.substring(1);
        if (z_language) {
            // Remove current language
            re = new RegExp("^" + z_language);
            pathname = pathname.replace(re, "");
        }

        // Get path parts
        parts = pathname.split("/")
            .filter(function(p) {
                return p !== "";
            });

        rewriteUrl = Boolean(args["z_rewrite_url"]);
        if (rewriteUrl) {
            // Add language to start
            parts.unshift(newLanguage);
        }

        href = window.location.protocol
            + "//"
            + window.location.host
            + "/"
            + parts.join("/")
            + ((rewriteUrl && (pathname === "" || pathname === "/")) ? "/" : "")
    } else {
        href = `${ window.location.protocol }//${ window.location.host }${ window.location.pathname }`;
    }

    if (window.location.search == "") {
        window.location.href = href;
        return;
    } 

    // remove z_language and z_rewrite_url, keep other query params
    var kvs;
    kvs = window.location.search.substring(1)
        .split(/[&;]/)
        .map(function(kv) {
            return (kv.match("^z_language") || kv.match("^z_rewrite_url"))
                ? ""
                : kv;
        })
        .filter(function(kv) {
            return kv !== "";
        });

    if (kvs === "") {
        window.location.href = href;
    } else {
        window.location.href = `${ href }?${ kvs.join("&") }`;
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

function z_translation_set(text, trans)
{
    if (typeof z_translations == "undefined") {
        z_translations = {};
    }
    z_translations[text] = trans;
}

/* Render text as html nodes
---------------------------------------------------------- */

function z_text_to_nodes(text)
{
    var text1 = $.trim(text);

    if (text1 === "") {
        return $("");
    } else {
        var $ns;
        if (text1.charAt(0) == "<" && text1.charAt(text1.length-1) == ">") {
            $ns = $(text);
        } else {
            $ns = $("<span></span>"+text+"<span></span>").slice(1,-1);
        }
        return $ns.filter(function(i) { return $ns[i].nodeType != 3 || $ns[i].nodeValue.trim() !== ""; });
    }
}

/* WYSYWIG editor
---------------------------------------------------------- */

function z_editor_init()
{
    if (z_editor !== undefined) {
        z_editor.init();
    }
}

function z_editor_add(element)
{
    if (z_editor !== undefined) {
        var $element = (typeof element == "string") ? $(element) : element;
        z_editor.add($element);
    }
}

function z_editor_save(element)
{
    if (z_editor !== undefined) {
        var $element = (typeof element == "string") ? $(element) : element;
        z_editor.save($element);
    }
}

function z_editor_remove(element)
{
    if (z_editor !== undefined) {
        var $element = (typeof element == "string") ? $(element) : element;
        z_editor.remove($element);
    }
}

/* Support legacy code */

function z_tinymce_init()
{
    z_editor_init();
}

function z_tinymce_add($element)
{
    z_editor_add($element);
}

function z_tinymce_save($element)
{
    z_editor_save($element);
}

function z_tinymce_remove($element)
{
    z_editor_remove($element);
}


/* Utility functions
---------------------------------------------------------- */

// Should an event be canceled or passed through.
function z_opt_cancel(obj)
{
    if(typeof obj.nodeName == 'undefined')
        return false;

    var nodeName = obj.nodeName.toLowerCase();
    var nodeType = $(obj).attr("type");

    if (nodeName == 'input' &&  (nodeType == 'checkbox' || nodeType == 'radio'))
    {
        return true;
    }
    else
    {
        return false;
    }
}

function z_is_enter_key(event)
{
    return (event && event.keyCode == 13);
}

function z_ensure_id(elt)
{
    var id = $(elt).attr('id');
    if (id === undefined || id === "") {
        id = z_unique_id();
        $(elt).attr('id', id);
    }
    return id;
}

function z_unique_id(no_dom_check)
{
    var id;
    do {
        id = '-z-' + z_unique_id_counter++;
    } while (!no_dom_check && $('#'+id).length > 0);
    return id;
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
            var sortTag = $(this.childNodes[i]).data("z_sort_tag");
            if (sortTag)
            {
                if (sortItems !== "")
                {
                    sortItems += ",";
                }
                sortItems += sortTag;
            }
        }

        var sortItem = new Array({name: 'sort_items', value: sortItems});

        z_queue_postback(this.id, sortPostbackInfo, sortItem, true);
    };
    sortOptions.receive = function (ev, ui) {
        var $target = $(this).data().uiSortable.element;
        var $source = $(ui.sender);
        $target.data('z_sort_tag', $source.data('z_drag_tag'));
    };
    sortOptions.helper = 'clone';
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
        const obj = $('#'+ElementId);
        obj.addClass('loading');
        z_queue_postback(ElementId, postbackInfo);
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
    for (var i = 0; i < z_on_visible_checks.length; i++) {
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
    // && (elemBottom <= docViewBottom) &&  (elemTop >= docViewTop);
}


/* Error handling
----------------------------------------------------------

Fetch the error event and log it to the server.
Which should log it in a separate ui error log.

---------------------------------------------------------- */

var oldOnError = window.onerror;
var z_page_unloading = false;

window.addEventListener("unload", function(event) {
    z_page_unloading = true;
});

window.addEventListener("beforeunload", function(event) {
    z_page_unloading = true;
    setTimeout(function() { z_page_unloading = false }, 5000);
});

window.onerror = function(message, file, line, col, error) {
    if (!z_page_unloading) {
        // Some code (plugin?) on Safari assumes that a JSON-LD context is always a string.
        // As it can also be an object that code will throw an error, ignore this error here.
        if (message.indexOf('r["@context"].toLowerCase') == -1) {
            let payload = {
                type: 'error',
                message: message,
                file: file,
                line: line,
                col: col,
                stack: error ? error.stack : null,
                user_agent: navigator.userAgent,
                url: window.location.href
            };

            let xhr = new XMLHttpRequest();
            xhr.open('POST', '/log-client-event', true);
            xhr.setRequestHeader("Content-Type", "application/json");
            xhr.send(JSON.stringify(payload));

            if ($("form.masked").length > 0 || (payload.stack && payload.stack.match(/(submitFunction|doValidations)/))) {
                alert("Sorry, something went wrong.\n\n(" + message + ")");
                try { $("form.masked").unmask(); } catch (e) {}
            }
        }
    }

    if (oldOnError) {
        return oldOnError(message, file, line, col, error);
    } else {
        return false;
    }
};


/* Form element validations
----------------------------------------------------------

Grab all "postback" forms, let them be handled by Ajax postback calls.
This function can be run multiple times.

---------------------------------------------------------- */

function z_init_postback_forms()
{
    $("form[action='postback']").each(function() {
        // store options in hash
        $(this).on('click.form-plugin', ":submit,input:image", function(e) {
            var form = this.form;
            form.clk = this;

            if (this.type == 'image')
            {
                if (e.offsetX !== undefined)
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
    .submit(function(event) {
        theForm = this;
        z_editor_save(theForm);

        submitFunction = function(ev) {
            try { $(theForm).mask("", 100); } catch (e) {}

            var postback     = $(theForm).data("z_submit_postback");
            var action       = $(theForm).data("z_submit_action");
            var form_id      = $(theForm).attr('id');
            var validations  = $(theForm).formValidationPostback();
            var transport    = '';
            var files        = $('input:file', theForm).fieldValue();
            var is_file_form = false;

            if (!postback) {
                postback = z_default_form_postback;
            }
            if (action) {
                setTimeout(action, 10);
            }

            for (var j=0; j < files.length; j++) {
                if (files[j]) {
                    is_file_form = true;
                    break;
                }
            }

            if (   $(theForm).hasClass("z_cookie_form")
                || $(theForm).hasClass("z_logon_form")
                || (typeof(z_only_post_forms) != "undefined" && z_only_post_forms)) {
                transport = 'form';
                args = validations;
            } else if (is_file_form) {
                transport = 'fileuploader';
                args = validations;
            } else {
                transport = '';
                args = validations.concat($(theForm).formToArray());
            }

            // add submitting element to data if we know it
            var sub = theForm.clk;
            if (sub) {
                var n = sub.name;
                if (n && !sub.disabled) {
                    args.push({name: n, value: $(sub).val()});
                    args.push({name: 'z_submitter', value: n});
                    if (sub.type == "image") {
                        args.push({name: name+'.x', value: theForm.clk_x});
                        args.push({name: name+'.y', value: theForm.clk_y});
                    }
                }
            }

            // Queue the postback, or use a post to an iframe (if requested)
            z_queue_postback(form_id, postback, args, false, transport, theForm);

            theForm.clk   = null;
            theForm.clk_x = null;
            theForm.clk_y = null;
            ev.stopPropagation();
            $(theForm).trigger('z:formSubmit');
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
            event.zAfterValidation = [];
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


function z_transport_form(qmsg)
{
    var options = {
        url:  qmsg.url,
        type: 'POST',
        dataType: 'text'
    };
    var $form = $(qmsg.options.post_form);
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

    // var cbInvoked = 0;
    var timedOut = 0;

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
        if (opts.timeout) {
            setTimeout(function() { timedOut = true; cb(); }, opts.timeout);
        }

        let extraInputs = [];

        extraInputs.push(
            $('<input />')
                .attr('type', 'hidden')
                .attr('name', 'z_postback')
                .attr('value', JSON.stringify(qmsg.postback))
             .prependTo(form)[0]);

        extraInputs.push(
            $('<input />')
                .attr('type', 'hidden')
                .attr('name', 'zotonic_topic_reply')
                .attr('value', qmsg.reply_topic)
             .prependTo(form)[0]);

        extraInputs.push(
            $('<input />')
                .attr('type', 'hidden')
                .attr('name', 'zotonic_topic_progress')
                .attr('value', qmsg.progress_topic)
             .prependTo(form)[0]);

        extraInputs.push(
            $('<input />')
                .attr('type', 'hidden')
                .attr('name', 'z_trigger_id')
                .attr('value', $form.attr('id') || "")
             .prependTo(form)[0]);

        // Prepend all unchecked checkboxes as empty hidden input values
        $(form).find('input[type="checkbox"]:not(:checked):not(.nosubmit)').each(
            function() {
                const name = $(this).attr('name');
                if (name) {
                    extraInputs.push(
                        $('<input />')
                            .attr('type', 'hidden')
                            .attr('name', name)
                            .attr('value', '')
                         .prependTo(form)[0]);
                }
            });

        try {
            // add iframe to doc and submit the form
            $io.appendTo('body');
            if (io.attachEvent) {
                io.attachEvent('onload', cb);
            } else {
                io.addEventListener('load', cb, false);
            }
            form.submit();
        }
        finally {
            // reset attrs and remove "extra" input elements
            form.setAttribute('action',a);
            if (t) {
                form.setAttribute('target', t);
            } else {
                $form.removeAttr('target');
            }
            for (let n in extraInputs) {
                $(n).remove();
            }
        }
    }, 10);

    function cb() {
        if (io.detachEvent) {
            io.detachEvent('onload', cb);
        } else {
            io.removeEventListener('load', cb, false);
        }
        if (timedOut) {
            $.event.trigger("ajaxError", [xhr, opts, e]);
            z_unmask_error(form.id);
        } else {
            $.event.trigger("ajaxSuccess", [xhr, opts]);
            z_unmask(form.id);
        }
        if (g) {
            $.event.trigger("ajaxComplete", [xhr, opts]);
            $.event.trigger("ajaxStop");
        }
        if (opts.complete) {
            opts.complete(xhr, ok ? 'success' : 'error');
        }
    }
}


// Initialize a validator for the element #id
function z_init_validator(id, args)
{
    var elt = $('#'+id);
    if (elt)
    {
        switch (elt.attr('type'))
        {
            case 'radio':
            case 'checkbox':
                $('input[name="'+elt.attr('name')+'"]').each(function() {
                    addLiveValidation(this, args);
                });
                break;
            default:
                addLiveValidation(elt, args);
                break;
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

    switch (elt.attr('type'))
    {
        case 'radio':
        case 'checkbox':
            elt = $('input[name="'+elt.attr('name')+'"]');
            break;
        default:
            break;
    }

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
                case 'email':           v.add(Validate.Email, args);        break;
                case 'date':            v.add(Validate.Date, args);         break;
                case 'presence':        v.add(Validate.Presence, args);     break;
                case 'confirmation':    v.add(Validate.Confirmation, args); break;
                case 'acceptance':      v.add(Validate.Acceptance, args);   break;
                case 'length':          v.add(Validate.Length, args);       break;
                case 'format':          v.add(Validate.Format, args);       break;
                case 'numericality':    v.add(Validate.Numericality, args); break;
                case 'json':            v.add(Validate.Json, args);         break;
                case 'custom':          v.add(Validate.Custom, args);       break;
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

// URL encode function that is more RFC compatible.      Also encodes +, *, / and @.
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
    return s.replace(/&/g, "&amp;")
            .replace(/</g, "&lt;")
            .replace(/>/g, "&gt;")
            .replace(/"/g, "&quot;")
            .replace(/'/g, "&#39;");
}

// HTML unescape a string.
function html_unescape(s)
{
    return s.replace(/&lt;/g, "<")
            .replace(/&gt;/g, ">")
            .replace(/&quot;/g, "\"")
            .replace(/&#39;/g, "'")
            .replace(/&amp;/g, "&");
}


// Convert an object to an array with {name: xxx, value: yyy} pairs
function ensure_name_value(a)
{
    if ((typeof a == 'object') && !(a instanceof Array))
    {
        var n = [];
        for (var prop in a)
        {
            if (a[prop] !== undefined)
                n.push({name: prop, value: a[prop]});
        }
        return n;
    }
    else
    {
        return a;
    }
}

// Update the contents of an iframe
function z_update_iframe(name, doc)
{
    var iframe = window.frames[name];
    if (iframe) {
        var iframe_doc = iframe.document || iframe.contentDocument || iframe.contentWindow.document;
        iframe_doc.open();
        iframe_doc.write(doc);
        iframe_doc.close();
    }
}


// Store the current cookie consent status
function z_cookie_consent_store( status )
{
    if (status !== 'all') {
        z_cookie_remove_all();
    }
    switch (status) {
        case "functional":
        case "stats":
        case "all":
            const prev = z_cookie_consent_cache;
            window.z_cookie_consent_cache = status;
            try {
                // Use stringify to be compatible with model.localStorage
                localStorage.setItem('z_cookie_consent', JSON.stringify(status));
            } catch (e) {
            }
            const ev = new CustomEvent("zotonic:cookie-consent", {
                detail: {
                    cookie_consent: status
                }
            });
            if (prev != status) {
                window.dispatchEvent(ev);
            }
            break;
        default:
            console.error("Cookie consent status must be one of 'all', 'stats' or 'functional'", status);
            break;
    }
}

// Trigger on consent changes in other windows/tabs
window.addEventListener("storage", function(ev) {
    if (ev.key == 'z_cookie_consent') {
        if (ev.newValue === null) {
            window.z_cookie_consent_cache = 'functional';
        } else if (ev.oldValue != ev.newValue) {
            z_cookie_consent_store(ev.newValue);
        }
    }
}, false);

// Fetch the current cookie consent status - default to 'functional'
function z_cookie_consent_fetch()
{
    if (window.z_cookie_consent_cache) {
        return window.z_cookie_consent_cache;
    } else {
        let status;
        try {
            status = localStorage.getItem('z_cookie_consent');
        } catch (e) {
            status = null;
        }
        if (status !== null) {
            status = JSON.parse(status);
        } else {
            status = 'functional';
        }
        window.z_cookie_consent_cache = status
        return status;
    }
}

// Check if the user consented to some cookies
function z_cookie_consent_given()
{
    try {
        return typeof (localStorage.getItem('z_cookie_consent')) === 'string';
    } catch (e) {
        return false;
    }
}

// Check if something is allowed according to the stored consent status
function z_cookie_consented( wanted )
{
    const consent = z_cookie_consent_fetch();
    switch (wanted) {
        case 'functional':
            return true;
        case 'stats':
            return consent === 'all' || consent === 'stats';
        case 'all':
            return consent === 'all';
        default:
            return false;
    }
}

// Remove all non-functional cookies from the current document domain
function z_cookie_remove_all()
{
    for ( const cookie of document.cookie.split(';') ){
        const cookieName = cookie.split('=')[0].trim();
        switch (cookieName) {
            case "z_sid":
            case "z_rldid":
            case "z_ua":
            case "z.sid":
            case "z.lang":
            case "z.auth":
            case "z.autologon":
                // Functional - keep the cookie
                break;
            default:
                // Non-functional - remove the cookie
                let domains = window.location.hostname.split('.');
                while ( domains.length > 0 ) {
                    const domain = domains.join('.');
                    const cookieReset = encodeURIComponent(cookieName) + '=; expires=Thu, 01-Jan-1970 00:00:01 GMT';
                    document.cookie = cookieReset;
                    document.cookie = cookieReset + '; domain=' + domain + ' ;path=/';
                    let pathSegments = location.pathname.split('/');
                    while ( pathSegments.length > 0 ){
                        const path = pathSegments.join('/');
                        document.cookie = cookieReset + '; domain=' + domain + ' ;path=' + path;
                        pathSegments.pop();
                    }
                    domains.shift();
                }
                break;
        }
    }
}


// Used in z.feedback.js
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

// Initialize the Zotonic page on load.

window.addEventListener('load', () => {
    z_jquery_init_await();
}, { capture: true, passive: true, once: true });

function z_jquery_init_await() {
    if (typeof window.$ !== 'undefined') {
        z_jquery_init();
    } else {
        setTimeout(function() { z_jquery_init_await(); }, 50);
    }
}

// Publish the presence of activity max once per 100 msec
let z_last_act_publish = 0;
function publish_activity(type) {
    const now = Math.floor(Date.now() / 100);
    if (now > z_last_act_publish) {
        cotonic.broker.publish("model/activity/event", { type: type });
        z_last_act_publish = now;
    }
}
window.addEventListener('scroll', (e) => publish_activity(e.type), { capture: true, passive: true });
window.addEventListener('mousemove', (e) => publish_activity(e.type), { capture: true, passive: true });
window.addEventListener('keyup', (e) => publish_activity(e.type), { capture: true, passive: true });
window.addEventListener('touchstart', (e) => publish_activity(e.type), { capture: true, passive: true });


function z_jquery_init() {
    if (typeof window.zotonicPageInit == 'function') {
        window.zotonicPageInit();
    }

    // Collect all postback validations from the form elements
    $.fn.formValidationPostback = function()
    {
        var a = [];
        if(this.length > 0) {
            var form = this[0];
            var els      = form.elements;

            if (!els) return a;

            for(var i=0, max=els.length; i < max; i++)
            {
                var el = els[i];
                var n  = el.name;

                if (n && !el.disabled && !$(el).hasClass("nosubmit"))
                {
                    var v = $(el).data("z_postback_validation");
                    if (v)
                    {
                        a.push({name: "z_v", value: n+":"+v});
                    }
                }
            }
        }
        return a;
    };

    // From: http://malsup.com/jquery/form/jquery.form.js

    /*
     * jQuery Form Plugin
     * version: 2.28 (10-MAY-2009)
     * @requires jQuery v1.2.2 or later
     *
     * Examples and documentation at: http://malsup.com/jquery/form/
     * Dual licensed under the MIT and GPL licenses:
     *   http://www.opensource.org/licenses/mit-license.php
     *   http://www.gnu.org/licenses/gpl.html
     */

    /**
     * formToArray() gathers form element data into an array of objects that can
     * be passed to any of the following ajax functions: $.get, $.post, or load.
     * Each object in the array has both a 'name' and 'value' property.      An example of
     * an array for a simple login form might be:
     *
     * [ { name: 'username', value: 'jresig' }, { name: 'password', value: 'secret' } ]
     *
     * It is this array that is passed to pre-submit callback functions provided to the
     * ajaxSubmit() and ajaxForm() methods.
     */
    $.fn.formToArray = function(options) {
        var a = [];
        options = options || {};
        if (this.length > 0) {
            var form = this[0];
            var els = form.elements;
            var n;

            if (els) {
                for(var i=0, max=els.length; i < max; i++) {
                    var el = els[i];
                    n = el.name;
                    if (n && (!$(el).hasClass("nosubmit") || options.all)) {
                        switch ($(el).attr("type")) {
                            case "submit":
                                break;
                            case "file":
                                break;
                            default:
                                var v = $.fieldValue(el, true);
                                if (v && v.constructor == Array) {
                                    for(var j=0, jmax=v.length; j < jmax; j++)
                                        a.push({name: n, value: v[j]});
                                }
                                else if (v !== null && typeof v != 'undefined') {
                                    a.push({name: n, value: v});
                                }
                        }
                    }
                }
            }
        }
        return a;
    };


    /**
     * Returns the value(s) of the element in the matched set.  For example, consider the following form:
     *
     *  <form><fieldset>
     *      <input name="A" type="text" />
     *      <input name="A" type="text" />
     *      <input name="B" type="checkbox" value="B1" />
     *      <input name="B" type="checkbox" value="B2"/>
     *      <input name="C" type="radio" value="C1" />
     *      <input name="C" type="radio" value="C2" />
     *  </fieldset></form>
     *
     *  var v = $(':text').fieldValue();
     *  // if no values are entered into the text inputs
     *  v == ['','']
     *  // if values entered into the text inputs are 'foo' and 'bar'
     *  v == ['foo','bar']
     *
     *  var v = $(':checkbox').fieldValue();
     *  // if neither checkbox is checked
     *  v === undefined
     *  // if both checkboxes are checked
     *  v == ['B1', 'B2']
     *
     *  var v = $(':radio').fieldValue();
     *  // if neither radio is checked
     *  v === undefined
     *  // if first radio is checked
     *  v == ['C1']
     *
     * The successful argument controls whether or not the field element must be 'successful'
     * (per http://www.w3.org/TR/html4/interact/forms.html#successful-controls).
     * The default value of the successful argument is true.  If this value is false the value(s)
     * for each element is returned.
     *
     * Note: This method *always* returns an array.      If no valid value can be determined the
     *       array will be empty, otherwise it will contain one or more values.
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
        if (successful && t == 'checkbox' && !el.checked) {
            if (el.hasAttribute('value-unchecked')) {
                return el.getAttribute('value-unchecked');
            }
            return '';
        }

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
     *  - input text fields will have their 'value' property set to the empty string
     *  - select elements will have their 'selectedIndex' property set to -1
     *  - checkbox and radio inputs will have their 'checked' property set to false
     *  - inputs of type submit, button, reset, and hidden will *not* be effected
     *  - button elements will *not* be effected
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
        if (b === undefined) b = true;
        return this.each(function() {
            this.disabled = !b;
        });
    };

    /**
     * Checks/unchecks any matching checkboxes or radio buttons and
     * selects/deselects and matching option elements.
     */
    $.fn.selected = function(select) {
        if (select === undefined) select = true;
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

    $.extend({
        keys: function(obj){
            if (typeof Object.keys == 'function')
                return Object.keys(obj);
            var a = [];
            $.each(obj, function(k){ a.push(k) });
            return a;
        }
    });
}

// Signal start the base Zotonic wired functions are loaded.
if (typeof zotonic.wiresLoadedResolve == "function") {
    zotonic.wiresLoadedResolve();
}

