/**
 * Copyright 2020 Marc Worrell <marc@worrell.nl>
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
    oauth_step: undefined,
    authorize_url: undefined,
    state_data: undefined,
    state_id: undefined,
    status: 'start',
    is_depends_provided: false
};

model.present = function(data) {

    console.log(data);

    if (state.start(model)) {
        switch (data.oauth_step) {
            case "authorize":
                model.authorize_url = data.authorize_url;
                model.state_data = data.oauth_state;
                model.state_id = data.oauth_state_id;
                break;
            case "redirect":
                break;
            default:
                console.log("OAuth: unknown oauth_step, must be 'authorize' or 'include'");
                break;
        }
        model.oauth_step = data.oauth_step;
        model.status = "waiting";
    }

    if (data.is_depends_provided) {
        model.is_depends_provided = true;

        self.subscribe('model/auth/event/auth-user-id',
                       function(msg) {
                            actions.auth_changed();
                       });

        self.subscribe('model/auth/event/service-confirm',
                       function(msg) {
                            actions.authServiceConfirm(msg.payload);
                       });

        if (state.waiting(model)) {
            if (model.oauth_step == "authorize") {
                model.status = "storing";
                self.call("model/sessionStorage/post/oauth-data",
                          { id: model.state_id, data: model.state_data })
                    .then( function() { actions.redirect(); } )
            } else {
                model.status = "fetching";
                self.call("model/sessionStorage/get/oauth-data")
                    .then( function(msg) { actions.oauth_data(msg); } )
            }
        }
    }

    if (data.is_redirect) {
        self.publish("model/location/post/redirect", { url: model.authorize_url });
        model.status = "redirecting";
    }

    if (state.fetching(model) && data.data) {
        model.status = "location";
        model.state_data = data.data;
        model.state_id = data.id;
        self.publish("model/sessionStorage/delete/oauth-data");
        self.call("model/location/get/q")
            .then(function(msg) {
                actions.qargs(msg);
            });
    }

    if (state.location(model) && data.is_qargs) {
        self.publish(
            "model/ui/insert/oauth-status",
            { inner: true });

        model.qargs = data.qargs;
        self.call("bridge/origin/model/oauth2_service/post/oauth-redirect",
                  {
                    state_id: model.state_id,
                    state_data: model.state_data,
                    qargs: model.qargs
                  },
                  {
                    qos: 2
                  })
            .then( function(msg) {
                actions.authServiceConfirm(msg.payload);
            });
    }

    if (state.authenticating(model) && data.is_auth_changed) {
        setTimeout(
            function() { self.publish("model/window/post/close"); },
            200);
    }

    if (data.is_auth_confirm) {
        if (data.payload.status == 'ok') {
            switch (data.payload.result.result) {
                case "token":
                    // Authenticated - exchange the token for a z.auth cookie.
                    self.publish("model/auth/post/onetime-token",
                                 { token: data.payload.result.token });
                    model.status = "authenticating";
                    break;
                case "confirm":
                    // Needs confirmation to create a new account.
                    self.publish(
                        "model/ui/render-template/oauth-status",
                        {
                            topic: "bridge/origin/model/template/get/render/_logon_service_error.tpl",
                            dedup: true,
                            data: {
                                error: "confirm",
                                auth: data.payload.result.auth
                            }
                        });
                    model.status = "confirming";
                    break;
            }
        } else if (data.payload.message == 'denied') {
            self.publish("model/window/post/close");
        } else if (data.payload.message == 'cancel') {
            self.publish("model/window/post/close");
        } else {
            self.publish(
                "model/ui/render-template/oauth-status",
                {
                    topic: "bridge/origin/model/template/get/render/_logon_service_done.tpl",
                    dedup: true,
                    data: {
                        error: data.payload.message
                    }
                });
            model.status = "error";
        }
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
        // ...
    }

    if (state.running(model)) {
        // ...
    }
};

// Derive the current state of the system
state.start = function(model) {
    return model.status === 'start';
};

state.active = function(model) {
    return model.status === 'active';
};

state.waiting = function(model) {
    return model.status == 'waiting';
};

state.fetching = function(model) {
    return model.status == 'fetching';
};

state.location = function(model) {
    return model.status == 'location';
};

state.authenticating = function(model) {
    return model.status == 'authenticating';
};

state.running = function(model) {
    return model.is_depends_provided;
};


// Next action predicate, derives whether
// the system is in a (control) state where
// an action needs to be invoked

state.nextAction = function (model) {
}

state.render = function(model) {
    state.representation(model)
    state.nextAction(model) ;
}


////////////////////////////////////////////////////////////////////////////////
// Actions
//

var actions = {} ;

actions.init = function(data) {
    data = data || {};
    model.present(data);
};

actions.dependsProvided = function() {
    let data = {};
    data.is_depends_provided = true;
    model.present(data);
};

actions.redirect = function() {
    let data = {};
    data.is_redirect = true;
    model.present(data);
};

actions.oauth_data = function( msg ) {
    model.present(msg.payload);
};

actions.qargs = function( msg ) {
    let data = {
        is_qargs: true,
        qargs: msg.payload
    };
    model.present(data);
};

actions.auth_changed = function() {
    model.present({ is_auth_changed: true });
};

actions.authServiceConfirm = function(payload) {
    let data = {
        is_auth_confirm: true,
        payload: payload
    }
    model.present(data);
}


////////////////////////////////////////////////////////////////////////////////
// Worker Startup
//

self.worker_init = function(args) {
    actions.init(args);
}

self.on_connect = function() {
}

self.on_depends_provided = function() {
    actions.dependsProvided();
}

self.connect({
    depends: [ "bridge/origin", "model/auth", "model/location", "model/sessionStorage" ],
    provides: [ ]
});
