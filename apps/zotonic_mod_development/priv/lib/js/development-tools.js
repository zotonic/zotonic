/* Read-only inspection of the current site through its authorized model API. */
(() => {
    "use strict";
    if (window.z_development_tools) return;

    const get = async (path, payload = {}) => {
        await cotonic.ready;
        const response = await cotonic.broker.call(
            `bridge/origin/model/development_tools/get/${path}`, payload);
        if (response.payload.status !== "ok") {
            const code = response.payload.error || "inspection_failed";
            const error = new Error(typeof code === "string" ? code : JSON.stringify(code));
            error.code = code;
            throw error;
        }
        return response.payload.result;
    };

    window.z_development_tools = {
        /** List active dispatch rules for this site. */
        dispatch: () => get("dispatch/list"),
        /** Trace a local path; defaults to the current page's pathname. */
        dispatchTrace: (path = location.pathname) => get("dispatch/trace", {path}),
        /** Resolve a template, optionally using a resource's category hierarchy. */
        template: (name, id) => get("template/lookup", id === undefined ? {name} : {name, id}),
        /** Read the winner or a zero-based override candidate from the resolved index. */
        templateSource: (name, id, {candidate} = {}) => get("template/source", {
            name,
            ...(id === undefined ? {} : {id}),
            ...(candidate === undefined ? {} : {candidate})
        }),
        /** Inspect static inheritance, local blocks and literal includes for a candidate. */
        templateComposition: (name, id, {candidate} = {}) => get("template/composition", {
            name,
            ...(id === undefined ? {} : {id}),
            ...(candidate === undefined ? {} : {candidate})
        }),
        /** List winning indexed templates with prefix filtering and pagination. */
        templates: ({prefix = "", offset = 0, limit = 50} = {}) =>
            get("templates", {prefix, offset, limit}),
        /** Discover core and active-site models and their exported API methods. */
        models: ({prefix = "", offset = 0, limit = 50} = {}) =>
            get("models", {prefix, offset, limit}),
        /** Read model/API documentation and incomplete static GET-path hints. */
        modelDocumentation: name => get("model/documentation", {name}),
        /** List active modules in priority order. */
        modules: () => get("modules")
    };
})();
