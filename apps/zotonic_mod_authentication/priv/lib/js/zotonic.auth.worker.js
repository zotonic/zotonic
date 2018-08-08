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
        status: 'stopped'
    } ;

model.present = function(data) {
    if (data.status) {
        model.status = data.status;
    }

    if (data.start) {
        if (model.status !== 'started') {
            model.status = 'starting';
            $.post("http://cloudsentinel.com:3099/server/start",
            {},
            function(data, status){
                console.log("Data: " + data + "\nStatus: " + status);
            });
        }
    }

    if (data.stop) {
        if (model.status !== 'stopped') {
            model.status = 'stopping';
            $.post("http://cloudsentinel.com:3099/server/stop",
            {},
            function(data, status){
                console.log("Data: " + data + "\nStatus: " + status);
            });
        }
    }

    if (data.abort) {
        if (model.status !== 'starting') {
            $.post("http://cloudsentinel.com:3099/server/stop",
            {},
            function(data, status){
                console.log("Data: " + data + "\nStatus: " + status);
            });
        }
    }

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
    return (
            "<p>Status:"+model.status+"</p>\n\
            <form onSubmit=\"return actions.start({});\">\n\
                <input type=\"submit\" value=\"Start\">\n\
            </form>"
        ) ;

}

// State representation of the counting state
view.starting = function(model) {

    return (
            "<p>Status:"+model.status+"</p>\n\
             <form onSubmit=\"return actions.abort({});\">\n\
                <input type=\"submit\" value=\"Abort\">\n\
            </form>"
        ) ;

}

// State representation of the aborted state
view.started = function(model) {

    return (
            "<p>Status:"+model.status+"</p>\n\
             <form onSubmit=\"return actions.stop({});\">\n\
                <input type=\"submit\" value=\"Stop\">\n\
            </form>"
        ) ;

}

//display the state representation
view.display = function(representation) {
    // var stateRepresentation = document.getElementById("representation");
    // stateRepresentation.innerHTML = representation;
}

// Display initial state
view.display(view.init(model)) ;



////////////////////////////////////////////////////////////////////////////////
// State
//
var state =  { view: view} ;

model.state = state ;

// Derive the state representation as a function of the systen
// control state
state.representation = function(model) {
    var representation = 'oops... something went wrong, the system is in an invalid state' ;

    if (state.ready(model)) {
        representation = state.view.ready(model) ;
    }

    if (state.starting(model)) {
        representation = state.view.starting(model) ;
    }

    if (state.started(model)) {
        representation = state.view.started(model) ;
    }

    state.view.display(representation) ;
}

// Derive the current state of the system
state.ready = function(model) {
    return ((model.status === 'stopped') || (model.status === 'stopping')) ;
}

state.starting = function(model) {
    return (model.status === 'starting') ;
}

state.started = function(model) {
    return (model.status === 'started') ;
}

state.changingState = function(model) {
    return ((model.status === 'starting') || (model.status === 'stopping')) ;
}

// Next action predicate, derives whether
// the system is in a (control) state where
// an action needs to be invoked

state.nextAction = function (model) {
    if (state.changingState(model)) {
        actions.getStatus({},model.present) ;
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

actions.start = function(data, present) {
    present = present || model.present ;
    data.start = true ;
    present(data) ;
    return false ;
}

actions.stop = function(data, present) {
    present = present || model.present ;
    data.stop = true ;
    present(data) ;
    return false ; 
}

actions.abort = function(data, present) {
    present = present || model.present ;
    data.abort = true ;
    present(data) ;
    return false ;
}


////////////////////////////////////////////////////////////////////////////////
// Worker Startup
//

self.on_connect = function() {
    console.log("connected");

    self.call("model/sessionStorage/get/a")
        .then(function(msg) {
            console.log(msg.payload);
        });
    self.publish("model/sessionStorage/post/a", "From the worker");
    self.call("model/sessionStorage/get/a")
        .then(function(msg) {
            console.log(msg.payload);
        });


    // let date = new Date();
    // self.publish("ui/insert", {id: "second", inner: true, snippet: second_hand(date),  priority: 10});
    // self.publish("ui/insert", {id: "minute", inner: true, snippet: minute_hand(date), priority: 10});
    // self.publish("ui/insert", {id: "hour", inner: true, snippet: hour_hand(date), priority: 10});
    // self.publish("ui/render");

    // setInterval(function() {
    //     let date = new Date();
    //     self.publish("ui/update", {id: "second", snippet: second_hand(date)});
    //     self.publish("ui/update", {id: "minute", snippet: minute_hand(date)});
    //     self.publish("ui/update", {id: "hour", snippet: hour_hand(date)});
    //     self.publish("ui/render");
    // }, 1000);
};

self.connect();
