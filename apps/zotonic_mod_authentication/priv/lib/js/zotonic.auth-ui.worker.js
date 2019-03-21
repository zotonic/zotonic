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
    logon_view: 'logon',
    is_connected: false,
    is_error: false,
    error: undefined,
    email: undefined
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
            "model/auth-ui/post/form/reminder",
            function(msg) { actions.reminderForm(msg.payload); });

        model.status = "waiting_for_bridge";
    }

    if ("is_error" in data) {
        model.is_error = data.is_error;
        model.error = data.error;
        model.status = 'updated';
    }

    if (typeof data.is_connected == "boolean") {
        if (state.waitingForBridge(model) && data.is_connected) {
            model.status = "connected";
        }
        model.is_connected = data.is_connected;
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
        self.call("bridge/origin/model/authentication/post/request-reminder", { email: data.email })
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
    if (state.waitingForBridge(model)) {
        self.publish(
            "model/ui/insert/signup_logon_box",
            { inner: true, initialData: "<img src='/lib/images/spinner.gif' class='loading'>" });
    }

    if (state.connected(model) || (model.is_connected && state.updated(model))) {
        self.publish(
            "bridge/origin/model/template/get/render/_logon_box.tpl",
            {
                logon_view: model.logon_view,
                error: model.error,
                email: model.email
            },
            { properties: { response_topic: "model/ui/update/signup_logon_box" }, qos: 1 });
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

state.waitingForBridge = function(model) {
    return model.status === 'waiting_for_bridge';
}


// Next action predicate, derives whether
// the system is in a (control) state where
// an action needs to be invoked

state.nextAction = function (model) {
    if (state.connected(model) || state.updated(model)) {
        actions.loaded()
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
    model.present(data);
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

////////////////////////////////////////////////////////////////////////////////
// Worker Startup
//

self.on_connect = function() {
    actions.start({});
}

self.connect();
