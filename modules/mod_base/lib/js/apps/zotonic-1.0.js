/* Zotonic basic Javascript library
----------------------------------------------------------

@package:   Zotonic 2009
@Author:    Tim Benniks <tim@timbenniks.nl>
@Author:    Marc Worrell <marc@worrell.nl>

Copyright 2009-2014 Tim Benniks, Marc Worrell

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

// Client state
var z_language              = "en";
var z_ua                    = "desktop";
var z_pageid                = '';
var z_userid;
var z_editor;

// Session state
var z_session_valid         = false;
var z_session_restart_count = 0;

// Transport to/from server
var z_ws                    = false;
var z_ws_pong_count         = 0;
var z_ws_ping_timeout;
var z_stream_host;
var z_stream_starter;
var z_stream_start_timeout;
var z_websocket_host;
var z_default_form_postback = false;
var z_page_unloading        = false;
var z_comet_reconnect_timeout = 1000;
var z_transport_check_timer;
var z_transport_queue       = [];
var z_transport_acks        = [];
var z_transport_delegates   = {
    javascript: function (data, _msg) { eval(data); },
    session: z_transport_session_status,
    reload: z_session_invalid_dialog
};

var TRANSPORT_TIMEOUT       = 30000;
var TRANSPORT_TRIES         = 3;

// Misc state
var z_spinner_show_ct       = 0;  // Set when performing an AJAX callback
var z_input_updater         = false;
var z_drag_tag              = [];
var z_registered_events     = {};
var z_on_visible_checks     = [];
var z_on_visible_timer;
var z_unique_id_counter     = 0;


function z_set_page_id( page_id, user_id )
{
    ubf.add_spec('z_msg_v1', [
        "qos", "dup", "msg_id", "timestamp", "content_type", "delegate",
        "push_queue", "ua_class", "session_id", "page_id",
        "data"
        ]);
    ubf.add_spec('z_msg_ack', [
        "qos", "msg_id", "push_queue", "session_id", "page_id", "result"
        ]);
    ubf.add_spec('postback_notify', [
        "message", "trigger", "target", "data"
        ]);
    ubf.add_spec('postback_event', [
        "postback", "trigger", "target",
        "triggervalue", "data"
        ]);
    ubf.add_spec('session_state', [
        "page_id", "user_id"
        ]);
    ubf.add_spec('auth_change', [
        "page_id"
        ]);
    ubf.add_spec('q', [
        "q"
        ]);

    if (z_pageid != page_id) {
        z_session_valid = true;
        z_pageid = page_id;
        z_userid = user_id;

        if (typeof pubzub == "object") {
            setTimeout(function() { pubzub.publish("pageinit", page_id); }, 10);
        }
    }
    $(window).bind('beforeunload', function() {
        z_page_unloading = true;
        setTimeout(function() {
            z_page_unloading = false;
        }, 10000);
    });
}

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
        width: (options.width)
    });
    $(".z-dialog-cancel-button").click(function() { z_dialog_close(); });
    $(".z-dialog-ok-button").click(function() {
        z_dialog_close();
        if (options.on_confirm) options.on_confirm();
    });
}

function z_dialog_alert(options)
{
    html = '<div class="confirm">' + options.text + '</div>'
         + '<div class="modal-footer">'
         + '<button class="btn btn-primary z-dialog-ok-button">'
         + (options.ok||z_translate('OK'))
         + '</button>'
         + '</div>';
    $.dialogAdd({
        title: (options.title||z_translate('Alert')),
        text: html,
        width: (options.width)
    });
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
    var params = extraParams || [];

    if (typeof params == 'object' && params.z_trigger_id !== undefined) {
        trigger_id = params.z_trigger_id;
        delete params.z_trigger_id;
    }
    var notify = {
        _record: "postback_notify",
        message: message,
        trigger: trigger_id,
        target: params.z_target_id || undefined,
        data: {
            _record: 'q',
            q: ensure_name_value(params) || []
        }
    };
    var delegate = params.z_delegate || 'notify';
    var options = {
        trigger_id: trigger_id
    };
    if (trigger_id) {
        options.ack = function(_ack_msg, _options) {
            z_unmask(trigger_id);
        };
    }
    return z_transport(delegate, 'ubf', notify, options);
}


/* Session handling and restarts
---------------------------------------------------------- */


function z_session_restart(invalid_page_id)
{
    if (z_session_valid) {
        z_session_valid = false;
        z_session_restart_count = 0;
    }
    setTimeout(function() { z_session_restart_check(invalid_page_id); }, 500);
}

function z_session_restart_check(invalid_page_id)
{
    if (z_pageid == invalid_page_id) {
        if (z_spinner_show_ct === 0) {
            if (z_session_restart_count == 3) {
                z_session_invalid_reload(z_pageid);
            } else {
                z_session_restart_count++;
                z_transport('session', 'ubf', 'ensure', {is_expect_cookie: true});
            }
        } else {
            setTimeout(function() { z_session_restart_check(invalid_page_id); }, 200);
        }
    }
}

function z_session_status_ok(page_id, user_id)
{
    if (page_id != z_pageid || user_id != z_userid) {
        var status = {
            status: "restart",
            user_id: user_id,
            page_id: page_id,
            prev_user_id: z_userid,
            prev_page_id: z_pageid
        };

        z_pageid = page_id;
        z_userid = user_id;

        z_transport_queue = [];
        z_transport_acks = [];

        // checks pubzub registry for the "session" topic
        // if any handlers then publish the new user to the topic
        // if no handlers then the default reload dialog is shown
        if (typeof pubzub == "object" && pubzub.subscribers("session").length > 0) {
            z_session_valid = true;
            pubzub.publish("session", status);
            z_stream_restart();
        } else {
            z_session_invalid_reload(z_pageid, status);
        }
    }
}

function z_session_invalid_reload(page_id, status)
{
    if (page_id == z_pageid) {
        if (z_spinner_show_ct === 0 && !z_page_unloading) {
            z_transport_delegates.reload(status);
        } else {
            setTimeout(function() {
                z_session_invalid_reload(page_id, status);
            }, 1000);
        }
    }
}

// Default action for delegates.reload
function z_session_invalid_dialog()
{
    z_dialog_confirm({
        title: z_translate("Reload"),
        text: "<p>" +
            z_translate("Your session has expired or is invalid. Reload the page to continue.") +
            "</p>",
        ok: z_translate("Reload"),
        on_confirm: function() { z_reload(); }
    });
}

/* Transport between user-agent and server
---------------------------------------------------------- */

// Register a handler for incoming data (aka delegates)
function z_transport_delegate_register(name, func)
{
    z_transport_delegates[name] = func;
}

// Called for 'session' transport delegates, handles the session status
function z_transport_session_status(data, msg)
{
    switch (data)
    {
        case 'session_invalid':
            if (window.z_sid) {
                window.z_sid = undefined;
            }
            z_session_restart(z_pageid);
            break;
        case 'page_invalid':
            z_session_restart(z_pageid);
            break;
        case 'ok':
            z_session_valid = true;
            break;
        default:
            if (typeof data == 'object') {
                switch (data._record) {
                    case 'session_state':
                        z_session_status_ok(data.page_id, data.user_id);
                        break;
                    case 'auth_change':
                        // The user-id of the session is changed.
                        // A new session cookie might still be on its way, so wait a bit
                        if (data.page_id == z_pageid) {
                            z_session_restart(z_pageid);
                        }
                        break;
                    default:
                        console.log("Transport, unknown session status ", data);
                        break;
                }
            } else {
                console.log("Transport, unknown session status ", data);
            }
            break;
    }
}


// Queue any data to be transported to the server
function z_transport(delegate, content_type, data, options)
{
    var msg_id = z_pageid + z_unique_id(true);
    var timestamp = new Date().getTime();

    options = options || {};
    if (typeof options.qos == 'undefined') {
        if (options.ack) {
            options.qos = 1;
        } else {
            options.qos = 0;
        }
    }
    var msg = {
            "_record": "z_msg_v1",
            "qos": options.qos,
            "dup": false,
            "msg_id": msg_id,
            "timestamp": timestamp,
            "content_type": z_transport_content_type(content_type),
            "delegate": z_transport_delegate(delegate),
            "ua_class": ubf.constant(z_ua),
            "page_id": z_pageid,
            "session_id": window.z_sid || undefined,
            "data": data
        };

    options.timeout = options.timeout || TRANSPORT_TIMEOUT;
    if (options.qos > 0) {
        var t = setTimeout(function() {
                    z_transport_timeout(msg_id);
                }, options.timeout);
        z_transport_acks[msg_id] = {
            msg: msg,
            msg_id: msg_id,
            options: options,
            timestamp: timestamp,
            timeout_timer: t,
            timeout_count: 0,
            is_queued: true
        };
    }
    z_transport_queue.push({
        msg: msg,
        msg_id: msg_id,
        options: options || {}
    });
    z_transport_check();
    return msg_id;
}

// Map some special content types to an atom
function z_transport_content_type(content_type)
{
    switch (content_type || 'ubf')
    {
        case 'ubf':        return ubf.constant('ubf');
        case 'json':       return ubf.constant('json');
        case 'form':       return ubf.constant('form');
        case 'javascript': return ubf.constant('javascript');
        case 'text':       return ubf.constant('text');
        default: return content_type;
    }
}

// Map some special delegates to an atom
function z_transport_delegate(delegate)
{
    switch (delegate)
    {
        case 'mqtt':     return ubf.constant('mqtt');
        case 'notify':   return ubf.constant('notify');
        case 'postback': return ubf.constant('postback');
        case 'session':  return ubf.constant('session');
        case '$ping':    return ubf.constant('$ping');
        default: return delegate;
    }
}

// Ensure that a transport is scheduled for fetching data queued at the server
function z_transport_ensure()
{
    if (z_transport_queue.length === 0 && !z_websocket_is_connected()) {
        z_transport('$ping');
    }
}

function z_transport_incoming(data)
{
    if (data !== undefined && data.length > 0) {
        var msgs = ubf.decode(data);

        if (typeof msgs == 'object' && msgs.ubf_type == ubf.LIST) {
            for (i=0; i<msgs.length; i++) {
                z_transport_incoming_msg(msgs[i]);
            }
        } else {
            z_transport_incoming_msg(msgs);
        }
    }
}

function z_transport_incoming_msg(msg)
{
    switch (msg._record)
    {
        case 'z_msg_v1':
            z_transport_maybe_ack(msg);
            var data = z_transport_incoming_data_decode(msg.content_type.valueOf(), msg.data);
            var fun = z_transport_delegates[msg.delegate.valueOf()];
            if (typeof fun == 'function') {
                fun(data, msg);
            } else {
                console.log("No delegate registered for ",msg);
            }
            break;
        case 'z_msg_ack':
            if (!z_websocket_pong(msg) && typeof z_transport_acks[msg.msg_id] == 'object') {
                var ack = z_transport_acks[msg.msg_id];
                delete z_transport_acks[msg.msg_id];

                clearTimeout(ack.timeout_timer);
                if (typeof ack.options.ack == 'function') {
                    ack.options.ack(msg, ack.options);
                }
            }
            break;
        default:
            console.log("Don't know where to delegate incoming message ", msg);
            break;
    }
}

function z_transport_maybe_ack(msg)
{
    if (msg.qos >= 1) {
        var ack = {
            "_record": "z_msg_ack",
            "qos": msg.qos,
            "msg_id": msg.msg_id,
            "push_queue": msg.push_queue,
            "session_id": window.z_sid || undefined,
            "page_id": msg.page_id || z_pageid,
        };
        z_transport_queue.push({
            msg: ack,
            msg_id: msg.msg_id,
            options: {}
        });
        z_transport_check();
    }
}

// If a transport times-out whilst in transit then it is reposted
function z_transport_timeout(msg_id)
{
    if (typeof z_transport_acks[msg_id] == 'object') {
        if (z_transport_acks[msg_id].timeout_count++ < TRANSPORT_TRIES) {
            // Requeue the request (if it is not waiting in the queue)
            if (!z_transport_acks[msg_id].is_queued) {
                z_transport_acks[msg_id].msg.dup = true;
                z_transport_queue.push({
                    msg: z_transport_acks[msg_id].msg,
                    msg_id: msg_id,
                    options: z_transport_acks[msg_id].options || {},
                });
                z_transport_acks[msg_id].is_queued = true;
            }
            z_transport_acks[msg_id].timeout_timer = setTimeout(function() {
                z_transport_timeout(msg_id);
            }, z_transport_acks[msg_id].options.timeout);
        } else {
            // Final timeout, remove from all queues
            if (z_transport_acks[msg_id].fail) {
                z_transport_acks[msg_id].fail(msg_id, z_transport_acks[msg_id].options);
            }
            if (z_transport_acks[msg_id].is_queued) {
                for (var i=0; i<z_transport_queue.length; i++) {
                    if (z_transport_queue[i].msg_id == msg_id) {
                        z_transport_queue.splice(i,i);
                        break;
                    }
                }
            }
            delete z_transport_acks[msg_id];
        }
    }
}

function z_transport_incoming_data_decode(type, data)
{
    switch (type)
    {
        case 'ubf':
            // Decoded by decoding the z_msg_v1 record 
            return data;
        case 'json':
            return $.parseJSON(data.valueOf());
        case 'javascript':
            return data.valueOf();
        case 'form':
            return $.parseQuery(data.valueOf());
        case 'text':
            return data.valueOf();
        default:
            console.log("Unknown message data format: ", type, data);
            return data;
    }
}


// Queue form data to be transported to the server
// This is called by the server generated javascript and jquery triggered postback events.
function z_queue_postback(trigger_id, postback, extraParams, noTriggerValue, isExpectCookie)
{
    var triggervalue = '';
    var trigger;

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
    extraParams = extraParams || [];
    // extraParams.push({name: 'triggervalue', value: triggervalue});

    var pb_event = {
        _record: "postback_event",
        postback: postback,
        trigger: trigger_id,
        target: extraParams.target_id || undefined,
        triggervalue: triggervalue,
        data: {
            _record: 'q',
            q: ensure_name_value(extraParams) || []
        }
    };

    // logon_form and .setcookie forms are always posted, as they will set cookies.
    var options = {
        is_expect_cookie: isExpectCookie ||
                          (trigger_id == "logon_form") ||
                          (trigger && $(trigger).hasClass("setcookie")),
        trigger_id: trigger_id
    };
    if (trigger_id) {
        options.ack = function(_ack_msg, _options) {
            z_unmask(trigger_id);
        };
    }
    z_transport('postback', 'ubf', pb_event, options);
}

function z_postback_opt_qs(extraParams)
{
    if (typeof extraParams == 'object' && extraParams instanceof Array) {
        return {
            _record: "q",
            q: ensure_name_value(extraParams)
        };
    } else {
        return extraParams;
    }
}

function z_transport_check()
{
    if (z_transport_queue.length > 0)
    {
        // Delay transport messages till the z_pageid is initialized.
        if (z_pageid !== '') {
            var qmsg = z_transport_queue.shift();

            if (z_transport_acks[qmsg.msg_id]) {
                z_transport_acks[qmsg.msg_id].is_queued = false;
            }
            z_do_transport(qmsg);
        } else if (!z_transport_check_timer) {
            z_transport_check_timer = setTimeout(function() { z_transport_check_timer = undefined; z_transport_check(); }, 50);
        }
    }
}

function z_do_transport(qmsg)
{
    var data = ubf.encode(qmsg.msg);
    if (z_websocket_is_connected() && !qmsg.options.is_expect_cookie && z_pageid)
    {
        z_ws.send(data);
    }
    else
    {
        z_ajax(qmsg.options, data);
    }
}

function z_ajax(options, data)
{
    z_start_spinner();
    $.ajax({
        url:        '/postback',
        type:       'post',
        data:       data,
        dataType:   'text',
        contentType: 'text/x-ubf',
        success: function(received_data, textStatus)
        {
            try
            {
                z_transport_incoming(received_data);
                z_init_postback_forms();
                z_unmask(options.trigger_id);
            }
            catch(e)
            {
                console.log("Error evaluating ajax return value: ", received_data);
                $.misc.error("Error evaluating ajax return value: " + received_data, e);
            }
            setTimeout(function() { z_stop_spinner(); z_transport_check(); }, 0);
        },
        error: function(xmlHttpRequest, textStatus, errorThrown)
        {
            z_stop_spinner();
            $.misc.error("FAIL: " + textStatus);
            z_unmask_error(options.trigger_id);
        }
    });
}

function z_fetch_cookies()
{
    $.ajax({
        url: '/z_session/cookies',
        type: 'post',
        dataType: 'text'
    });
}

function z_unmask(id)
{
    if (id)
    {
        var trigger;
        if (id.charAt(0) == ' ') {
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
    if (id)
    {
        var trigger;
        if (id.charAt(0) == ' ') {
            trigger = $(id);
        } else {
            trigger = $('#'+id);
        }
        z_unmask(id);
        trigger.each(function() { try { $(this).unmask(); } catch (e) {}});
        trigger.each(function() { $(this).addClass("z_error_upload"); });
    }
}


function z_progress(id, value)
{
    if (id)
    {
        var trigger = $('#'+id).get(0);

        if (trigger.nodeName.toLowerCase() == 'form')
        {
            try { $(trigger).maskProgress(value); } catch (e) {}
        }
    }
}

function z_reload(args)
{
    var page = $('#logon_form input[name="page"]');
    z_start_spinner();
    if (page.length > 0 && page.val() != "") {
        window.location.href = window.location.protocol+"//"+window.location.host+page.val();
    } else {
        if (typeof args == "undefined")
            window.location.reload(true);
        else {
            var qs = ensure_name_value(args);
            var href;

            if (qs.length == 1 &&  typeof args.z_language == "string") {
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
                href = window.location.protocol+"//"+window.location.host+window.location.pathname;
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

function z_editor_add($element)
{
    if (z_editor !== undefined) {
        z_editor.add($element);
    }
}

function z_editor_save($element)
{
    if (z_editor !== undefined) {
        z_editor.save($element);
    }
}

function z_editor_remove($element)
{
    if (z_editor !== undefined) {
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

/* Comet long poll or WebSockets connection
---------------------------------------------------------- */

function z_stream_start(host, websocket_host)
{
    if (!z_session_valid) {
        setTimeout(function() {
            z_stream_start(host, websocket_host);
        }, 100);
    } else {
        z_stream_host = host;
        z_websocket_host = websocket_host || window.location.host;
        z_stream_restart();
    }
}

function z_stream_restart()
{
    if (z_websocket_host) {
        $('#z_comet_connection').remove();
        setTimeout(function() { z_comet_start(); }, 1000);
        if ("WebSocket" in window)
        {
            setTimeout(function() { z_websocket_start(); }, 200);
        }
    }
}

function z_comet_start()
{
    if (z_stream_host != window.location.host && window.location.protocol == "http:")
    {
        var $zc = $('#z_comet_connection');
        if ($zc.length > 0) {
            $zc.attr('src', $zc.attr('src'));
        } else {
            var qs = z_stream_args('comet');
            var url = window.location.protocol + '//' + z_stream_host + "/comet/subdomain?"+qs;
            var comet = $('<iframe id="z_comet_connection" name="z_comet_connection" src="'+url+'" />');
            comet.css({ position: 'absolute', top: '-1000px', left: '-1000px' });
            comet.appendTo("body");
        }
    }
    else
    {
        z_comet_poll_ajax();
    }
}

function z_comet_poll_ajax()
{
    if (z_ws_pong_count === 0 && z_session_valid)
    {
        $.ajax({
            url: window.location.protocol + '//' + window.location.host + '/comet',
            type:'post',
            data: z_stream_args('comet'),
            dataType: 'text',
            statusCode: {
                    /* Handle incoming data */
                    200: function(data, _textStatus) {
                            z_comet_data(data);
                            z_timeout_comet_poll_ajax(100);
                    },
                    204: function() {
                        z_timeout_comet_poll_ajax(1000);
                    },

                    /* Reload the page on forbidden/auth responses. */
                    401: function() {
                        z_transport_delegates.session("session_invalid");
                    },
                    403: function() {
                        z_transport_delegates.session("session_invalid");
                    },
                },
            error: function(xmlHttpRequest, textStatus, errorThrown) {
                       setTimeout(function() { z_comet_poll_ajax(); }, z_comet_reconnect_timeout);
                       if(z_comet_reconnect_timeout < 60000)
                           z_comet_reconnect_timeout = z_comet_reconnect_timeout * 2;
                   }
        });
    }
    else
    {
        z_timeout_comet_poll_ajax(5000);
    }
}


function z_timeout_comet_poll_ajax(timeout)
{
    setTimeout(function() {
        z_comet_reconnect_timeout = 1000;
        z_comet_poll_ajax();
    }, timeout);
}


function z_comet_data(data)
{
    try
    {
        z_transport_incoming(data);
        z_init_postback_forms();
    }
    catch (e)
    {
        console.log("Error evaluating push return value: ", data);
        $.misc.error("Error evaluating push return value: " + data, e);
    }
}


function z_websocket_start()
{
    var protocol = "ws:";
    if (window.location.protocol == "https:") {
        protocol = "wss:";
    }

    try {
        z_ws = new WebSocket(protocol+"//"+z_websocket_host+"/websocket");
    } catch (e) {
        z_ws_pong_count = 0;
    }

    z_ws.onopen = function() {
        z_websocket_ping();
    };
    
    z_ws.onerror = function() {
        z_websocket_restart();
    };

    z_ws.onclose = function (evt) {
        z_websocket_restart();
    };

    z_ws.onmessage = function (evt) {
        z_comet_data(evt.data);
        setTimeout(function() { z_transport_check(); }, 0);
    };
}

function z_websocket_ping()
{
    setTimeout(function() {
        if (z_ws && z_ws.readyState == 1) {
            var msg = ubf.encode({
                    "_record": "z_msg_v1",
                    "qos": 1,
                    "dup" : false,
                    "msg_id": '$ws-'+z_pageid,
                    "timestamp": new Date().getTime(),
                    "content_type": ubf.constant("ubf"),
                    "delegate": ubf.constant('$ping'),
                    "ua_class": ubf.constant(z_ua),
                    "page_id": z_pageid,
                    "session_id": window.z_sid || undefined,
                    "data": z_ws_pong_count
                });
            z_ws.send(msg);
        }
    }, 0);
    if (typeof z_ws_ping_timeout !== 'undefined') {
        clearTimeout(z_ws_ping_timeout);
        z_ws_ping_timeout = undefined;
    }
    z_ws_ping_timeout = setTimeout(function() {
        z_websocket_restart();
    }, 5000);
}

function z_websocket_pong( msg )
{
    if (msg.msg_id == '$ws-'+z_pageid) {
        if (typeof z_ws_ping_timeout !== 'undefined') {
            clearTimeout(z_ws_ping_timeout);
            z_ws_ping_timeout = undefined;
        }
        z_ws_pong_count++;
        setTimeout(function() { z_websocket_ping(); }, 20000);
        return true;
    } else {
        return false;
    }
}

function z_websocket_is_connected()
{
    return z_ws && z_ws.readyState !== 0 && z_ws_pong_count > 0;
}

function z_websocket_restart()
{
    if (z_ws) {
        try { z_ws.close(); } catch(e) {} // closing an already closed ws can raise exceptions.
        z_ws = undefined;
    }
    if (z_ws_pong_count > 0 && z_session_valid) {
        z_ws_pong_count = 0;
        z_websocket_start();
    }
}

function z_stream_args(stream_type)
{
    var args = "z_pageid=" + urlencode(z_pageid);

    // Set the z_ua because we can't derive it from the ws handshake.
    if(stream_type == "websocket")
        args += "&z_ua=" + urlencode(z_ua);

    // Set z_sid when it is available. Can be used if there are problems with cookies.
    if(window.z_sid)
        args += "&z_sid=" + urlencode(z_sid);

    return args;
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


/* Spinner, show when waiting for a postback
---------------------------------------------------------- */

function z_start_spinner()
{
    if (z_spinner_show_ct++ === 0)
    {
        $(document.body).addClass('wait');
        $('#spinner').fadeIn(100);
    }
}

function z_stop_spinner()
{
    if (--z_spinner_show_ct === 0)
    {
        $('#spinner').fadeOut(100);
        $(document.body).removeClass('wait');
    }
    else if (z_spinner_show_ct < 0) {
        z_spinner_show_ct = 0;
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
            z_queue_postback(ElementId, postbackInfo);
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

/* Form element validations
----------------------------------------------------------

Grab all "postback" forms, let them be handled by Ajax postback calls.
This function can be run multiple times.

---------------------------------------------------------- */

function z_init_postback_forms()
{
    $("form[action='postback']").each(function()
    {
        // store options in hash
        $(this).on('click.form-plugin', ":submit,input:image", function(e)
        {
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
    .submit(function(event)
    {
        theForm = this;
        z_editor_save(theForm);

        submitFunction = function(ev) {
            var args = $(theForm).formToArray();

            try { $(theForm).mask("", 100); } catch (e) {}

            var postback    = $(theForm).data("z_submit_postback");
            var action      = $(theForm).data("z_submit_action");
            var form_id     = $(theForm).attr('id');
            var validations = $(theForm).formValidationPostback();

            if(!postback)
            {
                postback = z_default_form_postback;
            }

            if(action)
            {
                setTimeout(action, 10);
            }

            var files = $('input:file', theForm).fieldValue();
            var use_post = false;
            for (var j=0; j < files.length && !use_post; j++)
            {
                if (files[j])
                {
                    use_post = true;
                    break;
                }
            }

            if (use_post)
            {
                $(theForm).postbackFileForm(form_id, postback, validations);
            }
            else
            {
                var cookie_form = $(theForm).hasClass("z_cookie_form") || $(theForm).hasClass("z_logon_form");
                if (typeof(z_only_post_forms) != "undefined" && z_only_post_forms)
                {
                    cookie_form = true;
                }
                theForm.clk = theForm.clk_x = theForm.clk_y = null;
                z_queue_postback(form_id, postback, args.concat(validations), false, cookie_form);
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


$.fn.postbackFileForm = function(trigger_id, postback, validations)
{
    var a = validations;

    a.push({name: "postback",     value: postback});
    a.push({name: "z_trigger_id", value: trigger_id});
    a.push({name: "z_pageid",     value: z_pageid});
    a.push({name: "z_ua",         value: z_ua});
    a.push({name: "z_comet",      value: typeof z_stream_host != 'undefined'});

    var $form = this;
    var options = {
        url:  '/postback?' + $.param(a),
        type: 'POST',
        dataType: 'text/plain'
    };

    // hack to fix Safari hang (thanks to Tim Molendijk for this)
    // see:      http://groups.google.com/group/jquery-dev/browse_thread/thread/36395b7ab510dd5d
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
        form.clk   = null;
        form.clk_x = null;
        form.clk_y = null;

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
                $(extraInputs).remove();
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
            z_transport_ensure();
        }
    }
};


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

// Initialize a validator for the element #id
function z_init_validator(id, args)
{
    var elt = $('#'+id);
    if (elt)
    {
        if (elt.attr('type') == 'radio')
        {
            $('input[name="'+elt.attr('name')+'"]').each(function() {
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
        elt = $('input[name="'+elt.attr('name')+'"]');

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
                                case 'date':                    v.add(Validate.Date, args);             break;
                case 'presence':        v.add(Validate.Presence, args);     break;
                case 'confirmation':    v.add(Validate.Confirmation, args); break;
                case 'acceptance':      v.add(Validate.Acceptance, args);   break;
                case 'length':          v.add(Validate.Length, args);       break;
                case 'format':          v.add(Validate.Format, args);       break;
                case 'numericality':    v.add(Validate.Numericality, args); break;
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
    s.replace(/&/, "&amp;").replace(/</, "&lt;").replace(/>/, "&gt;").replace(/"/, "&quot;");
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
$.fn.formToArray = function(semantic) {
    var a = [];
    if (this.length === 0) return a;

    var form = this[0];
    var els = semantic ? form.getElementsByTagName('*') : form.elements;
    var n;

    if (!els) return a;
    for(var i=0, max=els.length; i < max; i++) {
        var el = els[i];
        n = el.name;
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
        n = sub.name;
        if (n && !sub.disabled) {
            a.push({name: n, value: $(sub).val()});
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

$.extend({ 
    keys: function(obj){
        if (typeof Object.keys == 'function') 
            return Object.keys(obj);
        var a = [];
        $.each(obj, function(k){ a.push(k) });
        return a;
    }
});


/**
 * A simple querystring parser.
 * Example usage: var q = $.parseQuery(); q.fooreturns  "bar" if query contains "?foo=bar"; multiple values are added to an array.
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
};
