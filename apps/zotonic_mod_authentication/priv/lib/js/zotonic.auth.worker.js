/**
 * Copyright 2018 Marc Worrell <marc@worrell.nl>
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

console.log("zotonic-auth-worker here");

////////////////////////////////////////////////////////////////////////////////
// Model
//

var model = {
        status: 'start'
    } ;

model.present = function(data) {
    if (state.start(model)) {
        if (data.check_session_state) {
            model.status = "check_sessionstate";
            self.call("model/sessionStorage/get/auth")
                .then(function(msg) {
                    actions.newSessionState({ auth: msg.payload });
                });
        }
    }

    if (state.checkSessionState(model)) {
        if (data.received_session_state) {
            if (!data.auth) {
                // Unknown - check 'remember-me' cookie with server
                model.status = "check_autologon";
            } else if (data.auth.user_id) {
                model.status = "authenticated";
                model.user_id = data.auth.user_id;
            } else {
                model.status = "anonymous";
                model.user_id = undefined;
            }
        }
    }

    if (state.checkAutoLogon(model)) {
        if (data.check_autologon) {
            // TODO: Check auth-uri on server for autologon token
            //       Uri should return JSON for new connection details
            //       Similar to authentication with username+password
            //
            // Autologin cookie:  <userid>:<series>
            // In localStorage: <random>
            // After every login, change the random token.
            // If presented with illegal random token, remove the series.
            // (Reuse might happen when cookie+token is stolen).
            model.status = "authenticating";
            self.call("model/localStorage/get/autologon")
                .then((msg) => {
                    if (msg.payload) {
                        return fetch( self.abs_url("/zotonic-auth/check"), {
                            method: "POST",
                            cache: "no-cache",
                            headers: {
                                "Accept": "application/json; charset=utf-8"
                            },
                            body: ""
                        } )
                    } else {
                        throw new Error("No autologon in localStorage");
                    }
                })
                .then((resp) => {
                    console.log("jaaa", resp);
                })
                .catch((e) => {
                    // Error - assume anonymous
                    model.status = "anonymous";
                });
        }
    }

    console.log("AUTH state", model);

    // .. change state
    state.render(model) ;
}


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
var state =  { view: view} ;

model.state = state ;

// Derive the state representation as a function of the systen control state
state.representation = function(model) {
    // var representation = 'oops... something went wrong, the system is in an invalid state' ;
    // if (state.ready(model)) {
    //     representation = state.view.ready(model) ;
    // }
    // ...
    // state.view.display(representation) ;
}

// Derive the current state of the system
state.start = function(model) {
    return (model.status === 'start');
}

state.checkSessionState = function(model) {
    return model.status === 'check_sessionstate';
}

state.anonymous = function(model) {
    return (model.status === 'anonymous');
}

state.authenticating = function(model) {
    return (model.status === 'authenticating');
}

state.exchanging = function(model) {
    return (model.status === 'exchanging');
}

state.checkAutoLogon = function(model) {
    return (model.status === 'check_autologon');
}

state.authenticated = function(model) {
    return (model.status === 'authenticated');
}

state.cleanup = function(model) {
    return (model.status === 'cleanup');
}


// Next action predicate, derives whether
// the system is in a (control) state where
// an action needs to be invoked

state.nextAction = function (model) {
    if (state.checkAutoLogon(model)) {
        actions.checkAutoLogon();
    }
    if (state.cleanup(model)) {
        // cleanup - and transfer to anonymous
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
    data.check_session_state = true ;
    model.present(data);
}

actions.newSessionState = function(data) {
    data = data || {};
    if ("auth" in data) {
        data.received_session_state = true;
        model.present(data);
    }
}

actions.checkAutoLogon = function(data) {
    data = data || {};
    data.check_autologon = true;
    model.present(data);
}

////////////////////////////////////////////////////////////////////////////////
// Worker Startup
//

self.on_connect = function() {
    // self.call("model/sessionStorage/get/a")
    //     .then(function(msg) {
    //         console.log("1", msg.payload);
    //     });
    // self.publish("model/sessionStorage/post/a", "From the worker");
    // self.call("model/sessionStorage/get/a")
    //     .then(function(msg) {
    //         console.log("2", msg.payload);
    //     });
    setTimeout(function() {
        actions.start();
    }, 0);
};

self.connect();
