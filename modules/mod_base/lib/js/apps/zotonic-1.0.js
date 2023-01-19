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
var z_session_reload_check  = false;

// Transport to/from server
var z_websocket_host;
var z_ws                    = false;
var z_ws_pong_count         = 0;
var z_ws_ping_timeout;
var z_ws_ping_interval;
var z_comet;
var z_comet_poll_timeout;
var z_comet_reconnect_timeout = 1000;
var z_comet_poll_count      = 0;
var z_stream_starter;
var z_stream_start_timeout;
var z_default_form_postback = false;
var z_page_unloading        = false;
var z_transport_check_timer;
var z_transport_queue       = [];
var z_transport_acks        = [];
var z_transport_delegates   = {
    javascript: z_transport_delegate_javascript,
    session: z_transport_session_status,
    reload: z_session_invalid_dialog
};
var z_transport_retransmission_enabled = false;
var z_force_unload_beacon   = false;
var z_init_postback_forms_timeout = false;

var TRANSPORT_TIMEOUT       = 30000;
var TRANSPORT_TRIES         = 3;

var WEBSOCKET_PING_INTERVAL = 20000; // Send ping messages every 20 seconds
var ACTIVITY_PERIOD         = 5000;  // Inactive if silent for 5 seconds

// Misc state
var z_spinner_show_ct       = 0;  // Set when performing an AJAX callback
var z_last_active           = 0;
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
    ubf.add_spec("unload_beacon", [
        "session_id",
        "page_id",
        ]);
    ubf.add_spec("rsc_update_done", [
        "action", "id", "pre_is_a", "post_is_a",
        "pre_props", "post_props" // Always empty
        ]);
    ubf.add_spec("media_replace_file", [
        "id",
        "medium" // Always empty
        ]);
    ubf.add_spec("edge_insert", [
        "subject_id", "predicate", "object_id", "edge_id"
        ]);
    ubf.add_spec("edge_delete", [
        "subject_id", "predicate", "object_id", "edge_id"
        ]);
    ubf.add_spec("edge_update", [
        "subject_id", "predicate", "object_id", "edge_id"
        ]);
    ubf.add_spec('q', [
        "q"
        ]);

    z_activity_init();

    if (z_pageid != page_id) {
        z_session_valid = true;
        z_pageid = page_id;
        z_userid = user_id;

        if (typeof pubzub == "object") {
            setTimeout(function() { pubzub.publish("~pagesession/pageinit", page_id); }, 10);
        }
    }
    $(window).bind("pageshow", function(event) {
        // After back button on iOS / Safari
        if (typeof event.originalEvent == 'object' && event.originalEvent.persisted) {
            z_page_unloading = false;
            setTimeout(function() {
                z_stream_onreload();
            }, 10);
        }
    });
    $(window).bind('beforeunload', function() {
        z_page_unloading = true;

        // Keep the connection open, but because the unloading flag is set
        // the connection will not be automatically restored if it is dropped.
        
        setTimeout(function() {
            z_page_unloading = false;
        }, 10000);
    });
    $(window).bind('unload', function() {
        var msg = {
            "_record": "unload_beacon",
            "page_id": z_pageid,
            "session_id": window.z_sid || undefined
        };

        // Stop the websocket. This prevents a connection interrupted error.
        z_websocket_stop();

        // Abort an open comet connection
        if (z_comet) {
            try { z_comet.abort(); } catch(e) { }
            z_comet = undefined;
        }

        if(navigator.sendBeacon) {
            navigator.sendBeacon("/beacon", ubf.encode(msg));
            return;
        }

        // If the browser doesn't have the beacon api, and we are forced to send the
     	// unload beacon we have to send it via a synchronous ajax request.
        if(z_force_unload_beacon) {
            $.ajax({url: "/beacon",
        	    type: "post",
        	    data: ubf.encode(msg),
        	    dataType: "text",
        	    contentType: "text/x-ubf",
        	    async: false});
        }
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
    var html,
        backdrop;

    if (typeof options.backdrop == 'undefined') {
        backdrop = options.backdrop
    } else {
        backdrop = true;
    }
    html = '<div class="confirm">' + options.text + '</div>'
         + '<div class="modal-footer">'
         + '<button class="btn btn-default z-dialog-cancel-button">'
         + (options.cancel||z_translate('Cancel'))
         + '</button>'
         + '<button class="btn btn-primary z-dialog-ok-button">'
         + (options.ok||z_translate('OK'))
         + '</button>'
         + '</div>';
    $.dialogAdd({
        title: (options.title||z_translate('Confirm')),
        text: html,
        width: (options.width),
        backdrop: backdrop
    });
    $(".z-dialog-cancel-button").click(function() { z_dialog_close(); });
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
    var $overlay = $('.modal-overlay');
    if ($overlay.length > 0) {
        $overlay
            .html(options.html)
            .attr('class', 'modal-overlay')
            .show();
    } else {
        html = '<div class="modal-overlay">' +
               '<a href="#close" class="modal-overlay-close" onclick="return z_dialog_overlay_close()">&times;</a>' +
               options.html +
               '</div>';
        $('body').append(html);
    }
    if (options.class) {
        $('.modal-overlay').addClass(options.class);
    }
}

function z_dialog_overlay_close()
{
    $('.modal-overlay').remove();
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
    else if (window.console)
    {
        console.log("z_event: no registered event named: '"+name+"'");
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
    setTimeout(function() { z_session_restart_check(invalid_page_id); }, 50);
}

function z_session_restart_check(invalid_page_id)
{
    if (z_pageid == invalid_page_id) {
        if (z_spinner_show_ct === 0) {
            if (z_session_restart_count == 3 || !z_pageid) {
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

        // checks pubzub registry for the local "session" topic
        // if any handlers then publish the new user to the topic
        // if no handlers then the default reload dialog is shown
        if (typeof pubzub == "object" && pubzub.subscribers("~pagesession/session").length > 0) {
            z_session_valid = true;
            pubzub.publish("~pagesession/session", status);
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
    var is_editing = false;

    z_editor_save($('body'));
    $('textarea').each(function() {
        is_editing = is_editing || ($(this).val() !== "");
    });

    if (is_editing) {
        z_dialog_confirm({
            title: z_translate("Reload"),
            text: "<p>" +
                z_translate("Your session has expired or is invalid. Reload the page to continue.") +
                "</p>",
            ok: z_translate("Reload"),
            on_confirm: function() { z_reload(); }
        });
    } else {
        z_reload();
    }
}

/* Track activity, for stopping inactive sessions
---------------------------------------------------------- */

function z_activity_init()
{
    /* Use passive event capturing when it is supported */
    var passive_if_supported = false;

    try {
        window.addEventListener("test", null,
            Object.defineProperty({}, "passive", {
                get: function() {
                    passive_if_supported = {
                        passive: false
                    };
                }
            }));
    } catch(err) {
    }

    z_last_active = 0;
    z_activity_event();

    document.addEventListener("visibilitychange", z_activity_event, passive_if_supported);
    document.addEventListener("scroll", z_activity_event, passive_if_supported);
    document.addEventListener("keydown", z_activity_event, passive_if_supported);
    document.addEventListener("mousemove", z_activity_event, passive_if_supported);
    document.addEventListener("click", z_activity_event, passive_if_supported);
    document.addEventListener("focus", z_activity_event, passive_if_supported);
}

function z_activity_ignore() {
    document.removeEventListener("visibilitychange", z_activity_event);
    document.removeEventListener("scroll", z_activity_event);
    document.removeEventListener("keydown", z_activity_event);
    document.removeEventListener("mousemove", z_activity_event);
    document.removeEventListener("click", z_activity_event);
    document.removeEventListener("focus", z_activity_event);

    z_last_active = null;
}

function z_activity_event()
{
    if (!document.hidden) {
        z_last_active = Date.now();
    }
}

function z_is_active(period)
{
    period = period || ACTIVITY_PERIOD;
    var now = Date.now();

    /* Return true when we are ignoring activity monitoring */
    if(z_last_active === null) return true;
    

    return z_last_active > now - period;
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
            if (z_session_reload_check) {
                z_reload();
            } else {
                z_session_reload_check = false;
                z_session_restart(z_pageid);
            }
            break;
        case 'page_invalid':
            if (z_session_reload_check) {
                z_reload();
            } else {
                z_session_reload_check = false;
                z_session_restart(z_pageid);
            }
            break;
        case 'ok':
            z_session_valid = true;
            if (z_session_reload_check) {
                z_session_reload_check = false;
                z_stream_restart();
            }
            break;
        default:
            if (typeof data == 'object') {
                switch (data._record) {
                    case 'session_state':
                        z_session_status_ok(data.page_id, data.user_id);
                        break;
                    case 'auth_change':
                        if (data.page_id == z_pageid) {
                            // The user-id of the session is changed.
                            // A new session cookie might still be on its way, so wait a bit
                            setTimeout(function() {
                                z_session_restart(z_pageid);
                            }, 1000);
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
    var msg_id = z_unique_id(true);

    if (!z_pageid) {
        z_transport_wait(msg_id, delegate, content_type, data, options);
        return msg_id;
    } else {
        return z_transport_do(msg_id, delegate, content_type, data, options);
    }
}

function z_transport_wait(msg_id, delegate, content_type, data, options)
{
    if (!z_pageid) {
        setTimeout(function() {
                z_transport_wait(msg_id, delegate, content_type, data, options);
            }, 100);
    } else {
        return z_transport_do(msg_id, delegate, content_type, data, options);
    }
}


function z_transport_do(msg_id, delegate, content_type, data, options)
{
    var timestamp = new Date().getTime();

    options = options || {};
    options.transport = options.transport || '';

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
    if (options.qos > 0 && options.transport !== 'form') {
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

    if (options.transport == 'form') {
        z_transport_form({
            msg: msg,
            msg_id: msg_id,
            options: options
        });
    } else {
        z_transport_queue.push({
            msg: msg,
            msg_id: msg_id,
            options: options
        });
        z_transport_check();
    }
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
            for (var i=0; i<msgs.length; i++) {
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

                if (ack.timeout_timer) {
                    clearTimeout(ack.timeout_timer);
                }
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

function z_transport_delegate_javascript(data, _msg)
{
    if (z_init_postback_forms_timeout) {
        clearTimeout(z_init_postback_forms_timeout);
    }
    eval(data);
    z_init_postback_forms_timeout = setTimeout(function() {
            z_init_postback_forms_timeout = false;
            z_init_postback_forms();
        }, 10);
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
            "page_id": msg.page_id || z_pageid
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
    z_log_error("Transport timeout for message: "+msg_id, "zotonic-1.0.js", 0, null);

    if (typeof z_transport_acks[msg_id] == 'object') {
        if (z_transport_acks[msg_id].timeout_count++ < TRANSPORT_TRIES) {
            // Requeue the request (if it is not waiting in the queue)
            if (!z_transport_acks[msg_id].is_queued && z_transport_retransmission_enabled) {
                z_transport_acks[msg_id].msg.dup = true;
                z_transport_queue.push({
                    msg: z_transport_acks[msg_id].msg,
                    msg_id: msg_id,
                    options: z_transport_acks[msg_id].options || {}
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
// 'transport' is one of: '', 'ajax', 'form'
function z_queue_postback(trigger_id, postback, extraParams, noTriggerValue, transport, optPostForm)
{
    var triggervalue = '';
    var trigger;

    if (transport === true) {
        transport = 'ajax';
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

    if (!transport) {
        if ((trigger_id == "logon_form") || (trigger && $(trigger).hasClass("setcookie"))) {
            transport = 'ajax';
        }
    }

    // logon_form and .setcookie forms are always posted, as they will set cookies.
    var options = {
        transport: transport,
        trigger_id: trigger_id,
        post_form: optPostForm
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
            if (!qmsg.page_id) {
                qmsg.page_id = z_pageid;
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
    if (qmsg.options.transport == 'ajax' || !z_websocket_is_connected() || !z_pageid) {
        z_ajax(qmsg.options, data);
    } else {
        z_ws.send(data);
    }
}

function z_ajax(options, data)
{
    z_start_spinner();
    $.ajax({
        url: '/postback',
        type: 'post',
        data: data,
        dataType: 'ubf text',
        accepts: {ubf: 'text/x-ubf'},
        converters: {"text ubf": window.String},
        contentType: 'text/x-ubf',
        async: true,
        success: function(received_data, textStatus)
        {
            try
            {
                z_transport_incoming(received_data);
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
            z_unmask_error(options.trigger_id);
            if (!z_page_unloading) {
                if (textStatus == 'error') {
                    $.misc.error("Error fetching data from server.");
                } else {
                    $.misc.error("Error fetching data from server: " + textStatus);
                }
            }
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
    if (page.length > 0 && page.val() !== "" && page.val() !== '#reload') {
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
                window.location.href = href + window.location.search;
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
                    window.location.href = href + "?" + $.param(qs);
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

/* Comet long poll or WebSockets connection
---------------------------------------------------------- */

function z_stream_start(_host, websocket_host)
{
    if (!z_session_valid) {
        setTimeout(function() {
            z_stream_start(_host, websocket_host);
        }, 100);
    } else {
        z_websocket_host = websocket_host || window.location.host;
        z_stream_restart();
    }
}

function z_stream_onreload()
{
    z_websocket_stop();

    if (z_comet) {
        try { z_comet.abort(); } catch(e) { }
        z_comet = undefined;
    }
    z_session_reload_check = true;
    z_page_unloading = false;
    z_transport('session', 'ubf', 'check', { transport: 'ajax' });
}

function z_stream_restart()
{
    if (z_websocket_host) {
        z_timeout_comet_poll_ajax(1000);
        if ("WebSocket" in window) {
            setTimeout(function() { z_websocket_start(); }, 200);
        }
    }
}

function z_stream_is_connected()
{
    return z_websocket_is_connected() || z_comet_is_connected();
}

function z_comet_poll_ajax()
{
    // Do not start a new poll when there is already a poll running.
    if (z_comet) return;

    if (z_ws_pong_count === 0 && z_session_valid && !z_page_unloading)
    {
        z_comet_poll_count++;
        var msg = ubf.encode({
                "_record": "z_msg_v1",
                "qos": 0,
                "dup" : false,
                "msg_id": '$comet-'+z_pageid+'-'+z_comet_poll_count,
                "timestamp": new Date().getTime(),
                "content_type": ubf.constant("ubf"),
                "delegate": ubf.constant('$comet'),
                "ua_class": ubf.constant(z_ua),
                "page_id": z_pageid,
                "session_id": window.z_sid || undefined,
                "data": {
                    count: z_comet_poll_count,
                    is_active: z_is_active()
                }
            });
        z_comet = $.ajax({
            url: window.location.protocol + '//' + window.location.host + '/comet',
            type:'post',
            data: msg,
            dataType: 'ubf text',
            accepts: {ubf: "text/x-ubf"},
            converters: {"text ubf": window.String},
            contentType: 'text/x-ubf',
            statusCode: {
                    /* Handle incoming data */
                    200: function(data, _textStatus) {
                        z_transport_handle_push_data(data);
                        z_timeout_comet_poll_ajax(100);
                    },
                    204: function() {
                        z_timeout_comet_poll_ajax(1000);
                    }
                },
            error: function(xmlHttpRequest, textStatus, errorThrown) {
                       setTimeout(function() { z_comet_poll_ajax(); }, z_comet_reconnect_timeout);
                       if(z_comet_reconnect_timeout < 60000)
                           z_comet_reconnect_timeout = z_comet_reconnect_timeout * 2;
                   }
        }).done(function() {
            z_comet = undefined;
        });
    }
    else
    {
        z_timeout_comet_poll_ajax(5000);
    }
}


function z_comet_is_connected()
{
    return z_comet && z_comet.readyState != 0;
}

function z_timeout_comet_poll_ajax(timeout)
{
    if (z_comet_poll_timeout) {
        clearTimeout(z_comet_poll_timeout);
    }
    z_comet_poll_timeout = setTimeout(function() {
        z_comet_poll_timeout = false;
        z_comet_reconnect_timeout = 1000;
        z_comet_poll_ajax();
    }, timeout);
}


function z_transport_handle_push_data(data)
{
    try
    {
        z_transport_incoming(data);
    }
    catch (e)
    {
        console.log("Error evaluating push return value: ", data);
        $.misc.error("Error evaluating push return value: " + data, e);
    }
}


function z_websocket_start()
{
    // Do not start a new websocket when there is already a websocket.
    if(z_ws) return; 

    var protocol = "ws:";
    if (window.location.protocol == "https:") {
        protocol = "wss:";
    }

    try {
        z_ws = new WebSocket(protocol+"//"+z_websocket_host+"/websocket");
    } catch (e) {
        z_ws_pong_count = 0;
    }

    z_ws.onopen = z_websocket_ping;
    z_ws.onerror = z_websocket_restart;
    z_ws.onclose = z_websocket_restart;

    z_ws.onmessage = function (evt) {
        z_transport_handle_push_data(evt.data);
        setTimeout(function() { z_transport_check(); }, 0);
    };
}

function z_websocket_stop()
{
    if (!z_ws) return;

    z_ws.onclose = undefined;
    z_ws.onerror = undefined;
    z_ws.onmessage = undefined;

    try {
        z_ws.close();
    } catch(e) {
        // closing an already closed ws can raise exceptions.
    }
    z_ws = undefined;
}

function z_websocket_ping()
{
    z_clear_ws_ping_timeout();
    z_ws_ping_timeout = setTimeout(z_websocket_restart, 5000);

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
                    "data": {
                        count: z_ws_pong_count,
                        is_active: z_is_active(WEBSOCKET_PING_INTERVAL)
                    }
                });
        z_ws.send(msg);
    }
}

function z_clear_ws_ping_timeout()
{
    if (z_ws_ping_timeout) {
        clearTimeout(z_ws_ping_timeout);
        z_ws_ping_timeout = undefined;
    }
}

function z_clear_ws_ping_interval()
{
    if (z_ws_ping_interval) {
        clearTimeout(z_ws_ping_interval);
        z_ws_ping_interval = undefined;
    }
}

function z_websocket_pong( msg )
{
    if (msg.msg_id == '$ws-'+z_pageid) {
        z_clear_ws_ping_timeout();

        z_clear_ws_ping_interval();
        z_ws_ping_interval = setTimeout(z_websocket_ping, WEBSOCKET_PING_INTERVAL);

        z_ws_pong_count++;

        return true;
    }

    return false;
}

function z_websocket_is_connected()
{
    return z_ws && z_ws.readyState == 1 && z_ws_pong_count > 0;
}

function z_websocket_restart(e)
{
    z_clear_ws_ping_timeout();
    z_clear_ws_ping_interval();

    z_websocket_stop();

    if (z_ws_pong_count > 0 && z_session_valid && !z_page_unloading) {
        z_ws_pong_count = 0;
        z_websocket_start();
    }
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


/* Error handling
----------------------------------------------------------

Fetch the error event and log it to the server.
Which should log it in a separate ui error log.

---------------------------------------------------------- */

var oldOnError = window.onerror;

window.onerror = function(message, file, line, col, error) {
    z_log_error(message, file, line, col, error);
    if (oldOnError) {
        return oldOnError(message, file, line, col, error);
    } else {
        return false;
    }
};

function z_log_error ( message, file, line, col, error ) {
    if (!z_page_unloading) {
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
        xhr.send(JSON.stringify(payload));

        if ($("form.masked").length > 0 || (payload.stack && payload.stack.match(/(submitFunction|doValidations)/))) {
            alert("Sorry, something went wrong.\n\n(" + message + ")");
            try { $("form.masked").unmask(); } catch (e) {}
        }
    }
}


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
        var theForm = this;

        event.preventDefault();

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
            var args;
            
            if (!postback) {
                postback = z_default_form_postback;
            }
            if (action) {
                setTimeout(action, 10);
            }

            for (var j=0; j < files.length && !is_file_form; j++) {
                if (files[j]) {
                    is_file_form = true;
                    break;
                }
            }
            if (is_file_form) {
                transport = 'form';
                args = validations;
            } else {
                if ($(theForm).hasClass("z_cookie_form") ||
                    $(theForm).hasClass("z_logon_form") ||
                    (typeof(z_only_post_forms) != "undefined" && z_only_post_forms)) {
                    transport = 'ajax';
                }
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

            // Queue the postback, or use a post to an iframe (if files present)
            z_queue_postback(form_id, postback, args, false, transport, theForm);

            theForm.clk   = null;
            theForm.clk_x = null;
            theForm.clk_y = null;
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


function z_transport_form(qmsg)
{
    var options = {
        url:  '/postback',
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

    var cbInvoked = 0;
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
                .attr('name', 'z_msg')
                .attr('value', ubf.encode(qmsg.msg))
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
        z_transport_ensure();
    }
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
                case 'date':            v.add(Validate.Date, args);         break;
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

// Check is the user consented to some cookies
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
        var els = options.semantic ? form.getElementsByTagName('*') : form.elements;
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

/**
 * Patch jQuery.find to return [] on queries for '#'
 * This fixes issue https://github.com/zotonic/zotonic/issues/1934
 */
(function( jQuery, window, undefined ) {
    var oldFind = jQuery.find;

    jQuery.find = function( selector ) {
        if (typeof selector == "string" && selector == '#') {
            if (window.console) {
                window.console.log("Zotonic jQuery patch: returning [] for illegal selector '#'");
            }
            return $([]);
        } else {
            var args = Array.prototype.slice.call( arguments );
            return oldFind.apply( this, args );
        }
    };

    // Copy properties attached to original jQuery.find method (e.g. .attr, .isXML)
    var findProp;
    for ( findProp in oldFind ) {
        if ( Object.prototype.hasOwnProperty.call( oldFind, findProp ) ) {
            jQuery.find[ findProp ] = oldFind[ findProp ];
        }
    }
})( jQuery, window );
