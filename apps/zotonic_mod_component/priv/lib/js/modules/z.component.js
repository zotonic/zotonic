/*
 * Zotonic View Controller, build on top of Mithril.js
 */

var z_components = {}; /* The components on this page */
var z_views = {}; /* The views on this page */

ubf.add_spec("load_component", ["name", "loaded", "pending"]);

/** 
 * Return the loaded js and css on this page.
 */
function z_get_loaded_resources() {
    var i, url, scripts, links, js=[], css=[];

    scripts = document.getElementsByTagName("script");
    for(i=0; i<scripts.length; i++) {
        url = scripts[i].getAttribute("src");
        if(!url) continue;
        js.push(url);
    }

    links = document.getElementsByTagName("link");
    for(i=0; i < links.length; i++) {
        if(links[i].getAttribute("type") !== "text/css") continue;
        url = links[i].getAttribute("href");
        if(!url) continue;
        css.push(url);
    }

    return {js: js, css: css};
}

/**
 * Load a component.
 *
 * Request loading a new component.
 */
function z_load_component(name) {
    var loaded;

    /* The module is already loaded, pending or failed */
    if(z_components[name]) return;

    /* Collected the loaded resources */
    loaded = z_get_loaded_resources();

    /* Send message that we want to load a component */ 
    z_transport("mod_component", "ubf", {_record: "load_component",
        name: name, loaded: loaded}, {qos: 1});

    /* State can be pending | loading | ready */
    z_components[name] = {state: "pending", wait_for: [], on_load: []};
}

/** 
 * Queue view initialization
 *
 */
function z_mount_view(id, view_controller, name, controller_options) {
    var component = z_components[name];

    function mount() {
        view_controller.mount(z_components[name], controller_options);
    }
    
    if(!component) 
        throw "Component not found";

    switch(component.state) {
        case "ready":
            mount();
            break;
        case "pending":
        case "loading":
            component.on_load.push(mount);
            pubzub.publish("~pagesession/component/" + id + "/loading", name);
            break;
        default:
            throw "Unknown state";
    }
}

function z_load_view(id, view_controller, component_name, controller_options) {
    z_load_component(component_name);
    z_mount_view(id, view_controller, component_name, controller_options);
    z_views[id] = view_controller;
}

function z_start_view(id, component_name, controller_options) {
    var view_component = {}

    view_component.controller = function() {
        this.loaded = m.prop(false);
        this.mounted = m.prop(false);

        this.component_view =  undefined;
        this.component_controller = undefined;
        
        this.mount = function(component, controller_options) {
            if(this.mounted()) return;

            m.startComputation();
            this.component_view = component.view;
            this.component_controller = new component.controller(controller_options, this);
            this.loaded(true);
            this.mounted(true);
            m.endComputation();

            pubzub.publish("~pagesession/component/" + id + "/mounted", component_name);
        }.bind(this);
        
        this.unmount = function() {
             if(!this.loaded()) return;
             if(!this.mounted()) return;
             
             m.startComputation();
             this.mounted(false);
             m.endComputation();
                  
             pubzub.publish("~pagesession/component/" + id + "/unmounted", component_name);
        }.bind(this);

        this.onunload = function(e) {
            if(this.component_controller && this.component_controller.onunload) {
                this.component_controller.onunload(e);
            }
                
            this.component_view = undefined;
            this.component_controller = undefined;
                
            pubzub.publish("~pagesession/component/"  + id + "/unloaded", component_name);
        }.bind(this);

        /* Load the view */
        z_load_view(id, this, component_name, controller_options);
    }

    view_component.view = function(ctrl) {
        if(!ctrl.loaded()) 
            return m("div.loading", {}, "Loading....");

        if(!ctrl.mounted()) 
            return m("div");

        return ctrl.component_view(ctrl.component_controller, ctrl);
    }

    m.module(document.getElementById(id), view_component);
}

function z_check_loaded(name, component) {
    if(component.wait_for.length != 0) 
        return;

    /* Initialize */
    component.init();
    component.init = undefined;
    component.state = "ready";

    /* Trigger all on_load callbacks */
    component.on_load.forEach(function(cb) { cb() });
    component.on_load = undefined;

    /* Notify that this component is loaded, other components could be waiting. */
    pubzub.publish("~pagesession/loaded", name);
}

/**
 * Load a new component on the page.
 */
function z_load(name, init, wait_for) {
    var component = z_components[name];
    if(!component) {
        throw "Component not found";
    }

    if(component.state != "pending") {
        /* Ignore load requests for components which are not pending */
        return;
    }

    component.state = "loading";
    component.init = init;
    component.wait_for = wait_for;

    /* Check if the component is loaded */
    z_check_loaded(name, component);
}

function z_lazy_load(url, name) {
    function loaded() {
        pubzub.publish("~pagesession/loaded", name); 
    }
    if(url.slice(-3) === ".js") LazyLoad.js([url], loaded);
    if(url.slice(-4) === ".css") LazyLoad.css([url], loaded);
}

pubzub.subscribe("~pagesession/loaded", function(topic, msg) {
    var c, i;
    for(var name in z_components) {
        c = z_components[name];
        if(!c.state == "loading") continue;

        i = c.wait_for.indexOf(msg) 
        if(i > -1) {
            c.wait_for.splice(i, 1);
            z_check_loaded(name, c);
        }
    }
});

