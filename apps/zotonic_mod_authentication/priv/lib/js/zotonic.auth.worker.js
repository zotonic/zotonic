/**
 * Copyright 2019-2024 Marc Worrell <marc@worrell.nl>
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

// Period between checking with the server if the authentication is still valid.
var AUTH_CHECK_PERIOD = 30;

// TODO:
// - recheck auth after ws connect and no recent auth check (or failed check)
//   this could be due to browser wakeup or server down time.

function fetchWithUA( body ) {
    return self.call("model/sessionId/get")
        .then( function(sid) {
            body.timestamp = Math.floor(Date.now() / 1000);
            return self.call("model/document/get/all")
                .then( function(msg) {
                    body.document = msg.payload;
                    body.cotonic_sid = sid.payload;
                    return fetch( self.abs_url("/zotonic-auth"), {
                        method: "POST",
                        cache: "no-cache",
                        headers: {
                            "Accept": "application/json",
                            "Content-Type": "application/json"
                        },
                        body: JSON.stringify(body)
                    })
                })
        });
}

////////////////////////////////////////////////////////////////////////////////
// Model
//

var model = {
        status: 'start',
        is_status_changed: false,
        is_keep_alive: false,
        is_fetch_error: false,
        authentication_error: null,
        auth: {
            status: 'pending',
            is_authenticated: false,
            user_id: null,
            username: null,
            preferences: {
            }
        },
        auth_check_timer: null,
        next_check: 0
    };

model.present = function(data) {
    let previous_auth_user_id = model.auth.user_id;

    if (data.is_start && state.start(model)) {
        if (data.auth) {
            model.auth = data.auth;

            if (model.auth.user_id) {
                // User is known - broadcast this user
                model.state_change('auth_known');
            } else {
                // User might be known after a refresh
                // This because on initial load the SameSite=Strict session
                // cookie is not passed to the page controller.
                model.state_change('auth_unknown');

                self.call("model/sessionStorage/get/auth-user-id")
                    .then((msg) => {
                        model.auth.user_id = msg.payload;
                    });
            }
        } else {
            model.state_change('auth_unknown');

            self.call("model/sessionStorage/get/auth-user-id")
                .then((msg) => {
                    model.auth.user_id = msg.payload;
                });
        }

        // Handle auth changes forced by changes of the session storage
        self.subscribe("model/sessionStorage/event/auth-user-id", function(msg) {
            actions.setUserId({ user_id: msg.payload });
        });

        // Synchronize tabs and windows of same user-agent
        self.subscribe("model/serviceWorker/event/broadcast/auth-sync", function(msg) {
            actions.authCheck();
        });

        // Self or applicaton requested check on the cookie
        self.subscribe("model/auth/post/check", function(msg) {
            actions.authCheck();
        });

        // Auth requests from the JS applications
        self.subscribe("model/auth/post/logon", function(msg) {
            actions.logon(msg.payload);
        });
        self.subscribe("model/auth/post/logoff", function(msg) {
            actions.logoff(msg.payload);
        });

        self.subscribe("model/auth/post/refresh", function(msg) {
            actions.authRefresh(msg.payload);
        });

        self.subscribe("model/auth/post/form/logon", function(msg) {
            actions.logonForm(msg.payload);
        });

        self.subscribe("model/auth/post/switch-user", function(msg) {
            actions.switchUser(msg.payload);
        });

        // Onetime token
        self.subscribe("model/auth/post/onetime-token", function(msg) {
            actions.onetimeToken(msg);
        });

        // Check reset codes
        self.subscribe("model/auth/post/reset-code-check", function(msg) {
            actions.resetCodeCheck(msg);
        });

        self.subscribe("model/auth/post/reset", function(msg) {
            actions.resetPassword(msg);
        });

        self.subscribe("model/auth/post/change", function(msg) {
            actions.changePassword(msg);
        });

        // Keep-alive ping for token refresh
        self.subscribe("model/ui/event/recent-activity", function(msg) {
            if (msg.payload.is_active) {
                actions.keepAlive(msg.payload);
            }
        });

        self.publish("model/auth/event/ping", "pong", { retain: true });

        // Initial check on user status
        fetchWithUA({ cmd: "status" })
        .then(function(resp) { return resp.json(); })
        .then(function(body) { actions.authResponse(body); })
        .catch((e) => { actions.fetchError(); });
    }

    if ("is_fetch_error" in data) {
        model.is_fetch_error = data.is_fetch_error;
        model.next_check = Math.floor(Math.random() * AUTH_CHECK_PERIOD);
    }

    if ("user_id" in data && data.user_id !== model.auth.user_id) {
        model.state_change('auth_unknown');

        // Refresh the current auth status by probing the server
        fetchWithUA({ cmd: "status" })
        .then(function(resp) { return resp.json(); })
        .then(function(body) { actions.authResponse(body); })
        .catch((e) => { actions.fetchError(); });
    }

    if (data.is_auth_check && (state.authKnown(model) || state.fetchError(model))) {
        let auth_check_cmd = 'status';
        if (model.is_keep_alive && model.auth.is_authenticated) {
            auth_check_cmd = 'refresh';
        }
        model.is_keep_alive = false;

        fetchWithUA({ cmd: auth_check_cmd })
        .then(function(resp) { return resp.json(); })
        .then(function(body) { actions.authResponse(body); })
        .catch((e) => { actions.fetchError(); });
    }

    if (data.is_refresh && (state.authKnown(model) || state.fetchError(model))) {
        fetchWithUA({ cmd: 'refresh', options: data.options || {} })
        .then(function(resp) { return resp.json(); })
        .then(function(body) { actions.authResponse(body); })
        .catch((e) => { actions.fetchError(); });
    }

    if (data.logon) {
        model.authentication_error = null;
        model.onauth = data.onauth || null;
        model.state_change('authenticating');

        fetchWithUA({
                    cmd: "logon",
                    username: data.username,
                    password: data.password,
                    passcode: data.passcode,
                    "code-new": data["code-new"],
                    test_passcode: data.test_passcode,
                    authuser: data.authuser || null,
                    setautologon: !!data.setautologon
                })
        .then(function(resp) { return resp.json(); })
        .then(function(body) { actions.authLogonResponse(body); })
        .catch((e) => { actions.fetchError(); });
    }

    if (data.switch_user) {
        model.authentication_error = null;
        model.onauth = null;
        model.state_change('authenticating');

        fetchWithUA({
                    cmd: "switch_user",
                    user_id: data.user_id
                })
        .then(function(resp) { return resp.json(); })
        .then(function(body) { actions.authLogonResponse(body); })
        .catch((e) => { actions.fetchError(); });
    }

    if (data.is_onetime_token) {
        model.authentication_error = null;
        model.onauth = null;
        model.state_change('authenticating');

        fetchWithUA({
                    cmd: "onetime_token",
                    token: data.token,
                    url: data.url
                })
        .then(function(resp) { return resp.json(); })
        .then(function(body) { actions.authLogonResponse(body); })
        .catch((e) => { actions.fetchError(); });
    }

    if (data.logoff) {
        model.authentication_error = null;
        model.onauth = data.onauth || null;
        model.state_change('authenticating');

        fetchWithUA({ cmd: "logoff" })
        .then(function(resp) { return resp.json(); })
        .then(function(body) { actions.authResponse(body); })
        .catch((e) => { actions.fetchError(); });
    }

    if ("auth_response" in data) {
        if (data.auth_response.status == 'ok') {
            if (data.is_auth_error === false) {
                model.authentication_error = null;
            }
            model.auth = data.auth_response;
            if (data.is_auth_error === false && data.auth_response.url) {
                self.publish("model/location/post/redirect-local", {
                    url: data.auth_response.url
                });
            }

            if (model.auth.user_id == previous_auth_user_id) {
                model.state_change('auth_known');
            } else {
                model.state_change('auth_changing');
            }

            model.next_check = AUTH_CHECK_PERIOD;
            if (data.auth_response.expires) {
                let timeout = data.auth_response.expires;

                if (timeout < model.next_check) {
                    timeout = Math.max(0, timeout - 1);
                } else {
                    // Check the status somewhere in the last quarter of the
                    // expirarion period. Use random to prevent multiple tabs
                    // checking at the same time.
                    let t = Math.floor(Math.random() * Math.floor(timeout/4));
                    timeout = Math.max(1, timeout - t - 4);
                }
                model.next_check = timeout;
            } else {
                model.next_check = Math.floor(Math.random() * AUTH_CHECK_PERIOD);
            }
        } else {
            model.next_check = Math.floor(Math.random() * AUTH_CHECK_PERIOD);
        }
    }

    if (data.is_auth_error) {
        if (state.authenticating(model)) {
            model.authentication_error = data.error;
            if (model.auth.status == 'ok') {
                model.state_change('auth_known');
            } else {
                model.state_change('auth_unknown');
            }
            self.publish("model/auth/event/auth-error", {
                error: model.authentication_error,
                data: data.data
            });
        } else {
            self.publish("model/auth/event/auth-error", {
                error: data.error,
                data: data.data
            });
        }
    }

    if (data.is_auth_changed && state.authChanging(model)) {
        model.state_change('auth_known');
    }

    if (data.is_keep_alive) {
        model.is_keep_alive = true;
    }

    if (data.is_reset) {
        model.state_change('authenticating');
        model.onauth = data.onauth || null;

        fetchWithUA({
            cmd: "reset",
            username: data.username,
            password: data.password,
            secret: data.secret,
            passcode: data.passcode,
            "code-new": data["code-new"],
            test_passcode: data.test_passcode
        })
        .then(function(resp) { return resp.json(); })
        .then(function(body) { actions.authLogonResponse(body); })
        .catch((e) => { actions.fetchError(); });
    }

    if (data.is_change) {
        model.onauth = data.onauth || null;

        fetchWithUA({
            cmd: "change",
            password: data.password,
            password_reset: data.password_reset,
            passcode: data.passcode,
            "code-new": data["code-new"],
            test_passcode: data.test_passcode,
            url: "#"
        })
        .then(function(resp) { return resp.json(); })
        .then(function(body) { actions.authChangeResponse(body); })
        .catch((e) => { actions.fetchError(); });
    }

    if (data.is_change_response) {
        self.publish('model/auth/event/auth-change-result', data.data);
    }

    if (model.next_check > 0) {
        if (model.auth_check_timer) {
            clearTimeout(model.auth_check_timer);
        }
        model.auth_check_timer = setTimeout(
            function() {
                model.auth_check_timer = null;
                self.publish("model/auth/post/check", {});
            },
            model.next_check * 1000);
        model.next_check = 0;
    }

    // console.log("AUTH state", model);
    state.render(model) ;
}

model.state_change = function(status) {
    if (status != model.status) {
        switch (status) {
            case 'auth_changing':
                self.publish("model/sessionId/post/reset");
                self.call('model/sessionStorage/post/auth-user-id', model.auth.user_id)
                    .then(function() {
                        self.publish('model/auth/event/auth-changing', {
                            onauth: model.onauth,
                            auth: model.auth
                        })
                    });
                setTimeout(function() { actions.authChanged(); }, 20);
                break;
            case 'auth_known':
                self.publish('model/auth/event/auth-user-id', model.auth.user_id);
                self.publish('model/serviceWorker/post/broadcast/auth-sync', model.auth);
                break;
            default:
                break;
        }
        model.status = status;
    }
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

// Derive the state representation as a function of the system control state
state.representation = function(model) {
    self.publish('model/auth/event/auth', model.auth, { retain: true });

    // Publish the model's UI status
    let ui = {
        classes: [],
        status: {
            'auth': model.auth.is_authenticated ? "user" : "anonymous"
        }
    }
    if (state.authenticating(model)) {
        ui.classes.push("authenticating");
    }
    if (model.authentication_error) {
        ui.classes.push("error");
        ui.classes.push("error-" + model.authentication_error);
    }
    if (model.is_fetch_error) {
        ui.classes.push("error-fetch");
    }
    if (model.auth.is_authenticated) {
        ui.classes.push("user");
    } else {
        ui.classes.push("anonymous");
    }
    self.publish("model/auth/event/ui-status", ui);

    // var representation = 'oops... something went wrong, the system is in an invalid state' ;
    // if (state.ready(model)) {
    //     representation = state.view.ready(model) ;
    // }
    // ...
    // state.view.display(representation) ;
}

// Derive the current state of the system
state.start = function(model) {
    return model.status === 'start';
}

state.authKnown = function(model) {
    return model.status === 'auth_known' || model.auth.status == 'ok';
}

state.authUnknown = function(model) {
    return !state.authKnown(model);
}

state.authChanging = function(model) {
    return model.status === 'auth_changing';
}

state.authenticating = function(model) {
    return model.status === 'authenticating';
}

state.authError = function(model) {
    return model.authentication_error !== null;
}

state.fetchError = function(model) {
    return model.is_fetch_error;
}

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

// On startup we continue with the previous page user-id
// todo: check the returned html for any included user-id (from the cookie
//       when generating the page, should be data attribute in html tag).
actions.start = function(init_args) {
    let data = init_args || {};
    model.present({
        is_start: true,
        auth: data.auth
    });
}

actions.setUserId = function(data) {
    data = data || {};
    if ("user_id" in data) {
        model.present(data);
    }
}

actions.authResponse = function(data) {
    data = data || {};
    model.present({
        is_fetch_error: false,
        auth_response: data
    });
}

actions.authLogonResponse = function(data) {
    switch (data.status) {
        case "ok":
            model.present({
                is_auth_error: false,
                is_fetch_error: false,
                auth_response: data
            });
            break;
        case "error":
            model.present({
                is_auth_error: true,
                is_fetch_error: false,
                error: data.error,
                data: data
            });
            break;
        default:
            console.log("Unkown LogonResponse payload", data);
            break;
    }
}

actions.authChangeResponse = function(data) {
    model.present({
        is_change_response: true,
        data: data
    });
}

actions.fetchError = function(_data) {
    model.present({ is_fetch_error: true });
}

actions.authChanged = function(_data) {
    model.present({ is_auth_changed: true });
}

actions.authCheck = function(_data) {
    model.present({ is_auth_check: true });
}

actions.authRefresh = function(data) {
    model.present({ is_refresh: true, options: data });
}

actions.logon = function(data) {
    let dataLogon = {
        logon: true,
        username: data.username,
        password: data.password,
        passcode: data.passcode,
        "code-new": data["code-new"],
        test_passcode: data.test_passcode,
        setautologon: data.rememberme ? true : false
    };
    model.present(dataLogon)
}

actions.logonForm = function(data) {
    let username;

    if (data.message && data.message.username && data.message.username !== "") {
        username = data.message.username;
    } else {
        username = data.value.username;
    }
    let dataLogon = {
        logon: true,
        username: username,
        password: data.value.password || null,
        passcode: data.value.passcode || null,
        "code-new": data.value["code-new"],
        test_passcode: data.value.test_passcode,
        setautologon: data.value.rememberme ? true : false,
        authuser: data.value.authuser || null,
        onauth: data.value.onauth
    }
    model.present(dataLogon);
}

actions.switchUser = function(data) {
    let dataSwitch = {
        switch_user: true,
        user_id: data.user_id
    }
    model.present(dataSwitch);
}

actions.logoff = function(data) {
    model.present({ logoff: true });
}

actions.keepAlive = function(_date) {
    model.present({ is_keep_alive: true });
}

actions.onetimeToken = function(msg) {
    let onetime = {
        is_onetime_token: true,
        token: msg.payload.token,
        url: msg.payload.url
    }
    model.present(onetime);
}

actions.resetCodeCheck = function(msg) {
    let body = {
        cmd: "reset_check",
        username: msg.payload.username || "",
        secret: msg.payload.secret,
        passcode: msg.payload.passcode || ""
    };

    fetch( self.abs_url("/zotonic-auth"), {
        method: "POST",
        cache: "no-cache",
        headers: {
            "Accept": "application/json",
            "Content-Type": "application/json"
        },
        body: JSON.stringify(body)
    })
    .then(function(resp) { return resp.json(); })
    .then(function(body) {
        if (msg.properties.response_topic) {
            self.publish(msg.properties.response_topic, body, { qos: msg.qos });
        }
    })
    .catch(function(_error) {
        if (msg.properties.response_topic) {
            let result = {
                result: "error",
                error: "fetch"
            };
            self.publish(msg.properties.response_topic, result, { qos: msg.qos });
        }
    });
}

actions.resetPassword = function(msg) {
    let data = {
        is_reset: true,
        username: msg.payload.username,
        password: msg.payload.password,
        secret: msg.payload.secret,
        passcode: msg.payload.passcode,
        "code-new": msg.payload["code-new"],
        test_passcode: msg.payload.test_passcode,
        setautologon: msg.payload.rememberme ? true : false,
        onauth: msg.payload.onauth || null
    };
    model.present(data);
}

actions.changePassword = function(msg) {
    let data = {
        is_change: true,
        password: msg.payload.password,
        password_reset: msg.payload.password_reset,
        passcode: msg.payload.passcode,
        "code-new": msg.payload["code-new"],
        test_passcode: msg.payload.test_passcode,
        onauth: msg.payload.onauth || null
    };
    model.present(data);
}

actions.sentVerificationMessage = function(msg) {
    let data = {
        is_sent_verification_message: true
    };
    model.present(data)
}

////////////////////////////////////////////////////////////////////////////////
// Worker Startup
//

self.connect({
    depends: [ "model/sessionStorage", "model/localStorage", "model/sessionId" ],
    provides: [ "model/auth" ]
}).then( function(args) {
    actions.start(args[0]);
});
