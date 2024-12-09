/**
 * Copyright 2020-2022 Marc Worrell <marc@worrell.nl>
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
    is_depends_provided: false,
    cotonic_sid: undefined,
    passcode_data: undefined
};

model.present = function(data) {

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
                console.log("OAuth: unknown oauth_step, must be 'authorize' or 'redirect'");
                break;
        }

        model.oauth_step = data.oauth_step;
        model.status = 'waiting';
    }

    // After all dependencies are resolved, wait for the auth worker check
    // with the server and stabilize.
    if (data.is_depends_provided && state.waiting(model)) {
        model.is_depends_provided = true;
        model.status = 'authsync';

        self.call("model/sessionId/get")
            .then(function(msg) {
                model.cotonic_sid = msg.payload;
                self.subscribe('model/auth/event/auth',
                               function(msg) {
                                    if (msg.payload.status == 'ok'
                                        && state.authsync(model))
                                    {
                                        actions.auth_stable();
                                    }
                              });
            });
    }

    // After the auth worker stabilizes, either:
    // - redirect to the remote server (step 1 of OAuth dance); or
    // - handle the response from the remote server (for this we first need
    //   to reload the page, so we are sure all same-site cookies are passed).
    if (data.is_auth_stable && state.authsync(model)) {
        model.status = 'active';

        self.subscribe('model/auth/event/auth-user-id',
                       function(msg) {
                            actions.auth_changed();
                       });

        self.subscribe('model/auth/event/service-confirm',
                       function(msg) {
                            actions.authServiceConfirm(msg.payload);
                       });

        if (model.oauth_step == "authorize") {
            model.status = "storing";
            self.call("model/localStorage/post/oauth-data",
                      { id: model.state_id, data: model.state_data })
                .then( function() { actions.redirect(); } )
        } else {
            self.call("model/sessionStorage/get/oauth-reload-done")
                .then( function(msg) {
                    if (msg.payload) {
                        model.status = "fetching";
                        self.publish("model/sessionStorage/delete/oauth-reload-done");
                        self.call("model/localStorage/get/oauth-data")
                            .then( function(msg) { actions.oauth_data(msg); } )
                    } else {
                        self.call("model/sessionStorage/post/oauth-reload-done", true)
                            .then(
                                function() { self.publish("model/location/post/reload"); }
                            );
                    }
                })
        }
    }

    // Redirect to the external authentication service (step 1 of the OAuth dance).
    if (data.is_redirect) {
        self.publish("model/location/post/redirect", { url: model.authorize_url });
        model.status = "redirecting";
    }

    // Fetched the OAuth data from the session storage, which was stored before we redirected
    // to the external service. Start fetching the query args, passed by the remote service.
    if (state.fetching(model) && data.data) {
        model.status = "location";
        model.state_data = data.data;
        model.state_id = data.id;
        self.publish("model/localStorage/delete/oauth-data");
        self.call("model/location/get/q")
            .then(function(msg) {
                actions.qargs(msg);
            });
    }

    // Return from the external service. The initial data and the query args are
    // loaded. Prepare the UI placeholder for the OAuth status updates and pass
    // the OAuth data and the query args to the oauth2_service model which will fetch
    // an access token from the remote service.
    if (state.location(model) && data.is_qargs) {
        self.publish(
            "model/ui/insert/oauth-status",
            { inner: true });

        model.qargs = data.qargs;
        self.call("bridge/origin/model/oauth2_service/post/oauth-redirect",
                  {
                    state_id: model.state_id,
                    state_data: model.state_data,
                    qargs: model.qargs,
                    cotonic_sid: model.cotonic_sid
                  },
                  {
                    qos: 2
                  })
            .then( function(msg) {
                actions.authServiceConfirm(msg.payload);
            });
    }

    // After the access_token has been fetched. Either:
    // - known users: log on with the onetime-token from the oauth2_service model
    // - new users: request the user to confirm account creation
    // - on 'denied' or 'cancel' status: silently close the window
    // - on error: display an error message (template)
    if (data.is_auth_confirm) {
        const error_message = (data.payload.message ?? data.payload.error);
        if (data.payload.status == 'ok') {
            switch (data.payload.result.result) {
                case "token":
                    // Authenticated - exchange the token for a z.auth cookie.
                    // After the cookie has been set, the window will either be closed
                    // or redirected to a new location.
                    self.publish("model/auth/post/onetime-token",
                                {
                                    token: data.payload.result.token,
                                    url: data.payload.result.url || undefined
                                });

                    if (data.payload.result.url) {
                        model.status = "redirect-success";
                    } else {
                        model.status = "authenticating";
                    }
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
                                auth: data.payload.result.auth,
                                url: data.payload.result.url || undefined
                            }
                        });
                    model.status = "confirming";
                    break;
                case "need_logon":
                    // Found matching account - need confirmation before connecting
                    // Reload the opener window with the logon confirm form. The external
                    // logon buttons will be hidden.
                    // TODO: check if there is an opener, otherwise we need to redirect to
                    // the confirm form, keeping the url.
                    self.publish(
                        "bridge/opener/model/auth-ui/post/form/confirm",
                        {
                            // url: data.payload.result.url || undefined,
                            authuser: data.payload.result.authuser,
                            username: data.payload.result.username
                        });
                    setTimeout(
                        () => self.publish("model/window/post/close", {}),
                        10);
                    model.status = "confirming";
                    break;
                case "need_passcode":
                    // Auth ok, but matching account is protected by 2FA
                    self.publish(
                        "model/ui/render-template/oauth-status",
                        {
                            topic: "bridge/origin/model/template/get/render/_logon_service_error.tpl",
                            dedup: true,
                            data: {
                                error: "need_passcode",
                                authuser: data.payload.result.authuser,
                                url: data.payload.result.url || undefined
                            }
                        });
                    model.passcode_data = data.payload.result;
                    model.status = "confirming";
                    break;
                case "set_passcode":
                    // Auth ok, but matching account needs to add a 2FA code
                    self.publish(
                        "model/ui/render-template/oauth-status",
                        {
                            topic: "bridge/origin/model/template/get/render/_logon_service_error.tpl",
                            dedup: true,
                            data: {
                                error: "set_passcode",
                                authuser: data.payload.result.authuser,
                                url: data.payload.result.url || undefined
                            }
                        });
                    model.passcode_data = data.payload.result;
                    model.status = "confirming";
                    break;
            }
        } else if (error_message == 'passcode') {
            // Auth ok, wrong 2FA passcode entered for matching account
            self.publish(
                "model/ui/render-template/oauth-status",
                {
                    topic: "bridge/origin/model/template/get/render/_logon_service_error.tpl",
                    dedup: true,
                    data: {
                        error: "passcode",
                        authuser: model.passcode_data.authuser,
                        url: model.passcode_data.url || undefined
                    }
                });
            model.status = "confirming";
        } else if (error_message == 'denied') {
            // Auth failed, remote denied. Close the window or redirect.
            if (data.payload.url) {
                self.publish("model/location/post/redirect", {
                    url: data.payload.url
                });
            } else {
                self.publish("model/window/post/close", { url: "/" });
            }
        } else if (error_message == 'cancel') {
            // Auth failed, user canceled. Close the window or redirect.
            if (data.payload.url) {
                self.publish("model/location/post/redirect", {
                    url: data.payload.url
                });
            } else {
                self.publish("model/window/post/close", { url: "/" });
            }
        } else {
            // Some error or other state that needs feedback to the user.
            self.publish(
                "model/ui/render-template/oauth-status",
                {
                    topic: "bridge/origin/model/template/get/render/_logon_service_error.tpl",
                    dedup: true,
                    data: {
                        error: error_message
                    }
                });
            model.status = "error";
        }
    }

    // After succes return and request of auth cookie the auth is changed
    // and the window is closed (with a small timeout to ensure proper cookies
    // on some browsers).
    if (state.authenticating(model) && data.is_auth_changed) {
        setTimeout(
            function() { self.publish("model/window/post/close", { url: "/" }); },
            200);
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
    if (state.running(model)) {
        // ...
    }
};

// Derive the current state of the system
state.start = function(model) {
    return model.status === 'start';
};

state.waiting = function(model) {
    return model.status === 'waiting';
};

state.authsync = function(model) {
    return model.status === 'authsync';
};

state.active = function(model) {
    return model.status === 'active';
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

actions.depends_provided = function() {
    let data = {
        is_depends_provided: true
    };
    model.present(data);
};

actions.auth_stable = function() {
    let data = {
        is_auth_stable: true
    }
    model.present(data);
};

actions.redirect = function() {
    let data = {};
    data.is_redirect = true;
    model.present(data);
};

actions.oauth_data = function( msg ) {
    model.present(msg.payload || {});
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

self.on_init = function(args) {
    actions.init(args);
}

self.connect({
    depends: [  "bridge/origin", "model/auth", "model/sessionId",
                "model/location", "model/sessionStorage", "model/localStorage" ],
    provides: [ "model/oauth"]
}).then(
    function() {
        actions.depends_provided();
    }
);
