/**
 * Copyright 2019 Marc Worrell <marc@worrell.nl>
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS-IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

"use strict";


////////////////////////////////////////////////////////////////////////////////
// Model
//

var model = {
    status: 'start',
    logon_view: '',
    is_connected: false,
    is_error: false,
    error: undefined,
    email: undefined,
    username: undefined,
    secret: undefined,
    need_passcode: false,

    // Depends on these models, wait for a ping
    depends: {
        "location": false,
        "auth": false
    }
};

model.present = function(data) {
    model.is_refresh = false;

    if (state.start(model)) {
        self.subscribe(
            "model/auth-ui/post/view/+view",
            function(msg, bindings) { actions.view({ logon_view: bindings.view }); });

        self.subscribe(
            "$bridge/origin/status",
            function(msg) { actions.setBridgeStatus(msg.payload); });

        self.subscribe(
            "model/auth/event/auth-error",
            function(msg) { actions.authError(msg.payload); });

        self.subscribe(
            "model/auth/event/auth-user-id",
            function(msg) { actions.authUserId(msg.payload); });

        self.subscribe(
            "model/auth-ui/post/form/reminder",
            function(msg) { actions.reminderForm(msg.payload); });

        self.subscribe(
            "model/auth-ui/post/form/reset",
            function(msg) { actions.resetForm(msg.payload); });

        self.subscribe(
            "model/+model/event/ping",
            function(msg, bindings) {
                actions.modelPing({ model: bindings.model, payload: msg.payload });
            });

        self.publish("model/auth-ui/event/ping", "pong", { retain: true });

        model.logon_view = data.logon_view || 'logon';
        model.secret = data.secret;
        model.username = data.username;
        model.status = "waiting";
    }

    if ("is_error" in data) {
        model.is_error = data.is_error;
        model.error = data.error;
        model.status = 'updated';
    }

    if ("model" in data) {
        model.depends[data.model] = data.is_active;
    }

    if (typeof data.is_connected == "boolean") {
        model.is_connected = data.is_connected;
    }

    if (model.status == 'waiting' && !state.waiting(model)) {
        model.status = 'connected'
    }

    if (data.is_view_loaded) {
        model.status = 'loaded';
    }

    if (typeof data.logon_view === "string") {
        model.logon_view = data.logon_view;
        model.error = data.error || undefined;
        if (state.loaded(model)) {
            model.status = 'updated';
        }
    }

    if (data.reminder) {
        self.call("bridge/origin/model/authentication/post/request-reminder",
                  { email: data.email },
                  { qos: 1 })
            .then(actions.reminderResponse)
            .catch(actions.fetchError);
    }

    if ("is_reminder_sent" in data) {
        if (data.is_reminder_sent) {
            model.logon_view = 'reminder_sent';
            model.error = undefined;
        } else {
            model.logon_view = 'reminder';
            model.error = data.error;
        }
        if (state.loaded(model)) {
            model.status = 'updated';
        }
        model.email = data.email
    }

    if (data.is_reset_check) {
        if (data.status == 'ok') {
            model.is_error = false;
            model.error = undefined;
            model.need_passcode = data.need_passcode;
            model.username = data.username;
            model.secret = data.secret
        } else {
            model.is_error = true;
            model.error = data.error || "error"
        }
        model.logon_view = 'reset_form';
    }

    if (data.reset) {
        if (data.is_password_equal) {
            let reset = {
                username: model.username,
                secret: model.secret,
                passcode: data.passcode,
                password: data.password,
                onauth: "#"
            };
            self.publish("model/auth/post/reset", reset);
            model.status = 'reset_wait';
            // The error response comes async via the auth-error topic
            //
            // If reset was done ok then we should show a 'success' screen with
            // a link to the home page and/or the user's page.
        } else {
            model.is_error = true;
            model.error = 'unequal';
            model.logon_view = 'reset_form';
        }
    }

    if (data.auth_user_id && model.status == 'reset_wait') {
        model.logon_view = 'reset_done';
        model.status = 'updated';
    }

    state.render(model) ;
};


////////////////////////////////////////////////////////////////////////////////
// View
//
var view = {} ;

// Initial State
view.init = function(model) {
    return view.ready(model) ;
}

// State representation of the ready state
view.ready = function(model) {
    return "";
}


//display the state representation
view.display = function(representation) {
}

// Display initial state
view.display(view.init(model)) ;


////////////////////////////////////////////////////////////////////////////////
// State
//
var state =  { view: view };

model.state = state ;

// Derive the state representation as a function of the systen control state
state.representation = function(model) {
    if (state.waiting(model)) {
        self.publish(
            "model/ui/insert/signup_logon_box",
            { inner: true, initialData: "<img src='/lib/images/spinner.gif' class='loading'>" });
    }

    if (state.running(model)) {
        self.publish(
            "model/ui/render-template/signup_logon_box",
            {
                topic: "bridge/origin/model/template/get/render/_logon_box.tpl",
                dedup: true,
                data: {
                    logon_view: model.logon_view,
                    error: model.error,
                    email: model.email,
                    username: model.username,
                    secret: model.secret,
                    need_passcode: model.need_passcode
                }
            });
    }

    // var representation = 'oops... something went wrong, the system is in an invalid state' ;
    // if (state.ready(model)) {
    //     representation = state.view.ready(model) ;
    // }
    // ...
    // state.view.display(representation) ;
};

// Derive the current state of the system
state.start = function(model) {
    return model.status === 'start';
};

state.connected = function(model) {
    return model.status === 'connected';
}

state.updated = function(model) {
    return model.status === 'updated';
}

state.loaded = function(model) {
    return model.status === 'loaded';
}

state.waiting = function(model) {
    return !model.is_connected || !model.depends.auth;
}

state.running = function(model) {
    return model.is_connected && model.depends.auth;
}


// Next action predicate, derives whether
// the system is in a (control) state where
// an action needs to be invoked

state.nextAction = function (model) {
    if (state.connected(model) || state.updated(model)) {
        actions.loaded()
    }
    if (state.running(model) && model.logon_view == 'reset') {
        actions.resetCodeCheck({
                secret: model.secret,
                username: model.username
            });
    }
}

state.render = function(model) {
    state.representation(model)
    state.nextAction(model) ;
}


////////////////////////////////////////////////////////////////////////////////
// Actions
//

var actions = {} ;

actions.start = function(data) {
    data = data || {};
    // TODO: wait for location model to be up and running
    self.call("model/location/get/q")
        .then(function(msg) {
            data.logon_view = msg.payload.logon_view || "logon";
            data.secret = msg.payload.secret || undefined;
            data.username = msg.payload.u || undefined,
            model.present(data);
        });
};

actions.setBridgeStatus = function(data) {
    const is_connected = data.is_connected || false;
    model.present({ is_connected: is_connected });
};

actions.loaded = function(_data) {
    model.present({ is_view_loaded: true })
};

actions.view = function(data) {
    model.present(data);
}

actions.reminderForm = function(data) {
    let dataReminder = {
        reminder: true,
        email: data.value.email
    }
    model.present(dataReminder);
}

actions.resetForm = function(data) {
    let dataReset = {
        reset: true,
        password: data.value.password_reset1,
        is_password_equal: data.value.password_reset1 === data.value.password_reset2,
        passcode: data.value.passcode || "",
        setautologon: data.value.rememberme ? true : false
    }
    model.present(dataReset);
}

actions.reminderResponse = function(data) {
    let payload = data.payload || {};
    switch (payload.status) {
        case "ok":
            model.present({
                is_reminder_sent: true,
                is_fetch_error: false,
                email: payload.result.email
            });
            break;
        case "error":
            model.present({
                    is_reminder_sent: false,
                    is_fetch_error: false,
                    error: payload.message
                });
            break;
        default:
            console.log("Unkown reminderResponse payload", data);
            break;
    }
}

actions.fetchError = function(_data) {
    model.present({ is_error: true, error: 'timeout' });
}

actions.authError = function(data) {
    model.present({ is_error: true, error: data.error });
}

actions.authUserId = function(data) {
    model.present({ auth_user_id: data });
}

actions.resetCodeCheck = function(data) {
    self.call("model/auth/post/reset-code-check", { secret: data.secret, username: data.username } )
        .then(function(msg) {
            let d = {
                is_reset_check: true,
                status: msg.payload.status,
                error: msg.payload.error || undefined,
                secret: data.secret,
                username: msg.payload.username || data.username,
                need_passcode: msg.payload.need_pascode || false
            };
            model.present(d)
        });
}

actions.modelPing = function(data) {
    model.present({ model: data.model, is_active: data.payload === 'pong' });
}

////////////////////////////////////////////////////////////////////////////////
// Worker Startup
//

self.on_connect = function() {
    actions.start({});
}

self.connect();
