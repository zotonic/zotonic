# Resource Graph (Sigma.js)

This is a self-contained HTML demo that renders a directed resource graph using Sigma.js + Graphology (via CDN) and vanilla JavaScript only.

## Open

Open `/Users/marc/Sites/misc/resource-graph/index.html` in a browser.

## Features

- Directed graph with labeled edges.
- Click a resource to highlight incoming/outgoing edges.
- Click event emitted for resource selection so you can render details elsewhere.
- Incremental updates (add nodes/edges).
- Reset the graph to start fresh.

## APIs

The page exposes a global `ResourceGraph` object:

```js
ResourceGraph.setGraph({ nodes: [...], edges: [...] });
ResourceGraph.resetGraph();
ResourceGraph.addResources([{ id: 200 }, { id: 201 }]);
ResourceGraph.addEdges([{ from: 200, to: 201, label: "author" }]);
ResourceGraph.applyIncrementalUpdate({
  nodes: [{ id: 300 }],
  edges: [{ from: 201, to: 300, label: "haspart" }]
});
ResourceGraph.enqueueBatch({ nodes: [...], edges: [...], runLayout: true });
ResourceGraph.stepBack();
ResourceGraph.runLayout({ iterations: 400 });
ResourceGraph.fitCamera();
ResourceGraph.refresh();
```

### API Details

- `setGraph({ nodes, edges, startNodeId, useWorker })`
  - Clears the graph, inserts data in batches, and runs the layout. Use `useWorker: true` to attempt worker layout.
- `resetGraph()`
  - Clears all nodes/edges and resets selection.
- `addResources(nodes, createdNodes?, nearResourceId?)`
  - Adds nodes immediately (no batching). Optionally seed positions near a resource id.
- `addEdges(edges)`
  - Adds edges immediately (no batching).
- `applyIncrementalUpdate({ nodes, edges, nearResourceId })`
  - Adds nodes/edges in batches and runs the layout. Optionally seed positions near a resource id.
- `enqueueBatch({ nodes, edges, runLayout, resetHistory, nearResourceId })`
  - Low-level batched insert; `runLayout` triggers layout after the batch.
- `stepBack()`
  - Removes nodes/edges added by the last completed batch (stops at initial graph).
- `runLayout(options)`
  - Runs the ForceAtlas2-like layout; accepts tuning options. Use `useWorker: true` to run in a web worker if available.
- `setDefaultUseWorker(enabled)`
  - Sets the default worker usage for future incremental batches.
- `fitCamera()`
  - Resets the camera to fit the graph.
- `refresh()`
  - Forces a Sigma render.
- `setActiveResource(resourceId)`
  - Activates a node by resource id and triggers click/edge-load logic.
- `setStartNode(resourceId | null)`
  - Sets the optional start node for path highlighting.
- `setPathOnlyMode(enabled)`
  - When `true`, hide everything except the focus path.
- `setPathWeights({ outgoing, incoming })`
  - Configure pathfinding costs (default outgoing `1`, incoming `10`).
- `getNodeInfo(resourceId)`
  - Returns node attributes and in/out edges.
- `getEdgeInfo(edgeId)`
  - Returns edge attributes and endpoints.
- `markEdgesLoaded(resourceIds, loaded = true)`
  - Marks edges for resources as loaded and clears loading flags.
- `markEdgesLoading(resourceIds, loading = true)`
  - Sets a loading flag (auto-timeout applies).
- `setEdgesLoadingTimeout(ms)`
  - Sets edge-loading timeout in milliseconds.
- `setCategoryVisibility(categoryId, visible)`
  - Show/hide nodes by category id.
- `hideCategory(categoryId)` / `showCategory(categoryId)`
  - Convenience wrappers.
- `getHiddenCategories()`
  - Returns hidden category ids.
- `setPredicateVisibility(predicateId, visible)`
  - Show/hide edges by predicate id.
- `hidePredicate(predicateId)` / `showPredicate(predicateId)`
  - Convenience wrappers.
- `getHiddenPredicates()`
  - Returns hidden predicate ids.

## UI Button IDs

If present in the DOM, the following button IDs are wired up:
- `zoom-in` → zoom in
- `zoom-out` → zoom out
- `step-back` → remove the last inserted batch
- `reset-graph` → clear the graph
- `toggle-path-only` → toggle path-only visibility

## Click Event

The graph dispatches a `resource:click` event on `window` when a node is clicked:

```js
window.addEventListener("resource:click", (e) => {
  console.log("Clicked resource", e.detail.id, e.detail);
});
```

You can also provide a callback:

```js
window.onResourceClick = (resourceId, attrs) => {
  console.log("Clicked resource", resourceId, attrs);
};
```

## Active Resource Event

When the active resource changes, a `resource:active-change` event is dispatched:

```js
window.addEventListener("resource:active-change", (e) => {
  console.log("Active resource changed", e.detail.id, e.detail.path, e.detail.pathNodes, e.detail);
});
```

Or use a callback:

```js
window.onResourceActiveChange = (resourceId, attrs, path) => {
  console.log("Active resource changed", resourceId, path, attrs);
};
```

## Data Shape

Nodes and edges follow this shape:

```js
{ id: 101 }
{ id: 102, label: "Alice", category: "person", category_id: 2, edgesLoaded: true }
{ id: 1, from: 101, to: 102, label: "author" }
```

Node `id` must be an integer. Optional node fields:
- `label` string (defaults to `id` if empty)
- `category` string (affects node color/size; e.g. `article`, `person`, `keyword`, `collection`, `image`, `video`)
- `category_id` integer (used for visibility filtering)
- `edgesLoaded` boolean (whether all edges for this resource have been loaded)
- `edgesLoading` boolean (whether edge loading is currently in progress)

Edge fields:
- `id` integer (required, used to dedupe edges)
- `from` integer
- `to` integer
- `label` string
- `predicate_id` integer (used for visibility filtering)

## Edge Loading Events

When a resource is clicked and `edgesLoaded` is falsey, a `resource:needs-edges` event is dispatched:

```js
window.addEventListener("resource:needs-edges", (e) => {
  console.log("Need edges for resource", e.detail.id, e.detail);
});
```

You can also set a callback:

```js
window.onResourceNeedsEdges = (resourceId, attrs) => {
  console.log("Need edges for resource", resourceId, attrs);
};
```

When you start loading, you can set a loading flag (also set automatically on click):

```js
ResourceGraph.markEdgesLoading(101, true);
```

Configure the loading timeout (ms). After the timeout, `edgesLoading` is cleared:

```js
ResourceGraph.setEdgesLoadingTimeout(20000);
```

When a timeout happens, a `resource:edges-timeout` event is dispatched:

```js
window.addEventListener("resource:edges-timeout", (e) => {
  console.log("Edge load timed out", e.detail.id, e.detail);
});
```

Or use a callback:

```js
window.onResourceEdgesTimeout = (resourceId, attrs) => {
  console.log("Edge load timed out", resourceId, attrs);
};
```

After you fetch edges, mark the resource as loaded:

```js
ResourceGraph.markEdgesLoaded([101, 102], true);
```

## Category Visibility

You can toggle resource categories, and edges will hide if either endpoint is hidden:

```js
ResourceGraph.setCategoryVisibility("person", false);
ResourceGraph.setCategoryVisibility("person", true);
```

Convenience helpers:

```js
ResourceGraph.hideCategory(2);
ResourceGraph.showCategory(2);
ResourceGraph.getHiddenCategories(); // -> [2, 5]
```

## Predicate Visibility

You can toggle predicate IDs, and nodes will hide if all their edges are hidden:

```js
ResourceGraph.setPredicateVisibility(42, false);
ResourceGraph.setPredicateVisibility(42, true);
```

Convenience helpers:

```js
ResourceGraph.hidePredicate(42);
ResourceGraph.showPredicate(42);
ResourceGraph.getHiddenPredicates(); // -> [42, 91]
```

## Dependencies

Loaded via CDN in `/Users/marc/Sites/misc/resource-graph/index.html`:

- Sigma.js
- Graphology
