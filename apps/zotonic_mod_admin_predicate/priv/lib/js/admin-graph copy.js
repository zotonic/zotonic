(function () {
  const Graphology = window.graphology || {};
  const GraphCtor =
    Graphology.Graph || Graphology.default || window.Graph || window.graphology;
  const SigmaCtor =
    window.Sigma ||
    (window.sigma && (window.sigma.Sigma || window.sigma.default)) ||
    window.sigma;

  if (!GraphCtor || !SigmaCtor) {
    throw new Error("Sigma.js or Graphology failed to load.");
  }

  const container = document.getElementById("graph");
  const detailMeta = document.getElementById("selected-meta");
  const detailBody = document.getElementById("selected-detail");
  const resetButton = document.getElementById("reset-graph");
  const zoomInButton = document.getElementById("zoom-in");
  const zoomOutButton = document.getElementById("zoom-out");

  let graph = createGraph();
  let sigma = null;
  let activeNodeId = null;
  let hoverNodeId = null;
  let edgeCounter = 0;
  let edgesLoadingTimeoutMs = 15000;
  const edgesLoadingTimers = new Map();
  let layoutFrame = null;
  let layoutRunning = false;
  const layoutSettings = {
    iterations: 300,
    stepsPerFrame: 5,
    gravity: 1,
    scalingRatio: 10,
    slowDown: 1,
    strongGravityMode: false
  };
  const batchSettings = {
    nodesPerBatch: 50,
    edgesPerBatch: 100
  };
  let batchQueue = null;

  function createGraph() {
    return new GraphCtor({ type: "directed", multi: true, allowSelfLoops: true });
  }

  function ensureSigma() {
    if (sigma) {
      sigma.kill();
    }

    sigma = new SigmaCtor(graph, container, {
      defaultNodeColor: "#0f4c5c",
      defaultEdgeColor: "#6b6b6b",
      defaultEdgeType: "arrow",
      renderEdgeLabels: true,
      labelFont: "Source Serif 4, Palatino, Georgia, serif",
      labelSize: 14,
      edgeLabelSize: 12,
      edgeLabelColor: { color: "#7a7368" },
      nodeReducer: (node, data) => {
        const isActive = node === activeNodeId;
        const isHover = node === hoverNodeId;
        if (!isActive && !isHover) return data;
        return {
          ...data,
          color: isActive ? "#e4572e" : data.color,
          size: isActive ? data.size + 2 : data.size,
          label: isHover ? data.fullLabel || data.label : data.label
        };
      },
      edgeReducer: (edge, data) => {
        if (!activeNodeId) return data;
        const source = graph.source(edge);
        const target = graph.target(edge);
        if (source === activeNodeId && target === activeNodeId) {
          return { ...data, color: "#e4572e", size: Math.max(2, data.size || 1) + 1 };
        }
        if (source === activeNodeId) {
          return { ...data, color: "#e4572e", size: Math.max(2, data.size || 1) + 1 };
        }
        if (target === activeNodeId) {
          return { ...data, color: "#2a9d8f", size: Math.max(2, data.size || 1) + 1 };
        }
        return { ...data, color: data.color || "#6b6b6b", size: data.size || 1 };
      }
    });

    sigma.on("clickNode", ({ node }) => {
      activeNodeId = node;
      updateDetailPanel(node);
      sigma.refresh();
      dispatchResourceClick(node);
      if (
        !graph.getNodeAttribute(node, "edgesLoaded") &&
        !graph.getNodeAttribute(node, "edgesLoading")
      ) {
        graph.setNodeAttribute(node, "edgesLoading", true);
        scheduleEdgesLoadingTimeout(node);
        dispatchNeedsEdges(node);
      }
    });

    sigma.on("clickStage", () => {
      activeNodeId = null;
      updateDetailPanel(null);
      sigma.refresh();
    });

    sigma.on("enterNode", ({ node }) => {
      hoverNodeId = node;
      sigma.refresh();
    });

    sigma.on("leaveNode", () => {
      hoverNodeId = null;
      sigma.refresh();
    });
  }


  function dispatchResourceClick(nodeId) {
    const resourceId = graph.getNodeAttribute(nodeId, "resourceId");
    const event = new CustomEvent("resource:click", {
      detail: {
        id: resourceId,
        nodeId: nodeId,
        attributes: graph.getNodeAttributes(nodeId)
      }
    });
    window.dispatchEvent(event);
    if (typeof window.onResourceClick === "function") {
      window.onResourceClick(resourceId, graph.getNodeAttributes(nodeId));
    }
  }

  function dispatchNeedsEdges(nodeId) {
    const resourceId = graph.getNodeAttribute(nodeId, "resourceId");
    const event = new CustomEvent("resource:needs-edges", {
      detail: {
        id: resourceId,
        nodeId: nodeId,
        attributes: graph.getNodeAttributes(nodeId)
      }
    });
    window.dispatchEvent(event);
    if (typeof window.onResourceNeedsEdges === "function") {
      window.onResourceNeedsEdges(resourceId, graph.getNodeAttributes(nodeId));
    }
  }

  function dispatchEdgesTimeout(nodeId) {
    const resourceId = graph.getNodeAttribute(nodeId, "resourceId");
    const event = new CustomEvent("resource:edges-timeout", {
      detail: {
        id: resourceId,
        nodeId: nodeId,
        attributes: graph.getNodeAttributes(nodeId)
      }
    });
    window.dispatchEvent(event);
    if (typeof window.onResourceEdgesTimeout === "function") {
      window.onResourceEdgesTimeout(resourceId, graph.getNodeAttributes(nodeId));
    }
  }

  function scheduleEdgesLoadingTimeout(nodeId) {
    if (edgesLoadingTimeoutMs <= 0) return;
    clearEdgesLoadingTimeout(nodeId);
    const timer = setTimeout(() => {
      if (!graph.hasNode(nodeId)) return;
      if (!graph.getNodeAttribute(nodeId, "edgesLoaded")) {
        graph.setNodeAttribute(nodeId, "edgesLoading", false);
        dispatchEdgesTimeout(nodeId);
      }
    }, edgesLoadingTimeoutMs);
    edgesLoadingTimers.set(String(nodeId), timer);
  }

  function clearEdgesLoadingTimeout(nodeId) {
    const key = String(nodeId);
    const timer = edgesLoadingTimers.get(key);
    if (timer) {
      clearTimeout(timer);
      edgesLoadingTimers.delete(key);
    }
  }

  function updateDetailPanel(nodeId) {
    if (!nodeId) {
      detailMeta.textContent = "No selection";
      detailBody.textContent = "Click a node to view its details here.";
      return;
    }

    const attrs = graph.getNodeAttributes(nodeId);
    detailMeta.textContent = `Resource ${attrs.resourceId}`;
    detailBody.textContent = JSON.stringify(attrs, null, 2);
  }

  function initializePositions() {
    const nodes = graph.nodes();
    const total = nodes.length;
    if (!total) return;

    let hasNonZero = false;
    nodes.forEach((nodeId) => {
      const x = graph.getNodeAttribute(nodeId, "x") || 0;
      const y = graph.getNodeAttribute(nodeId, "y") || 0;
      if (Math.abs(x) > 0.001 || Math.abs(y) > 0.001) {
        hasNonZero = true;
      }
    });

    if (hasNonZero) return;

    const centerX = 0;
    const centerY = 0;
    const radius = Math.max(2.5, total * 1.3);
    nodes.forEach((nodeId, index) => {
      const angle = (index / total) * Math.PI * 2;
      graph.setNodeAttribute(nodeId, "x", centerX + Math.cos(angle) * radius);
      graph.setNodeAttribute(nodeId, "y", centerY + Math.sin(angle) * radius);
    });
  }

  function stopLayout() {
    if (layoutFrame) {
      cancelAnimationFrame(layoutFrame);
      layoutFrame = null;
    }
    layoutRunning = false;
  }

  function runForceAtlas2(options = {}) {
    const iterations = options.iterations || layoutSettings.iterations;
    const stepsPerFrame = options.stepsPerFrame || layoutSettings.stepsPerFrame;
    const gravity = options.gravity ?? layoutSettings.gravity;
    const scalingRatio = options.scalingRatio ?? layoutSettings.scalingRatio;
    const slowDown = options.slowDown ?? layoutSettings.slowDown;
    const strongGravityMode =
      options.strongGravityMode ?? layoutSettings.strongGravityMode;
    const nodePadding = options.nodePadding ?? 6;

    stopLayout();
    initializePositions();
    layoutRunning = true;

    const nodes = graph.nodes();
    const edges = graph.edges();
    const index = new Map(nodes.map((id, i) => [id, i]));
    const nodeCount = nodes.length;
    const x = new Float32Array(nodeCount);
    const y = new Float32Array(nodeCount);
    const dx = new Float32Array(nodeCount);
    const dy = new Float32Array(nodeCount);
    const mass = new Float32Array(nodeCount);

    nodes.forEach((id, i) => {
      x[i] = graph.getNodeAttribute(id, "x") || 0;
      y[i] = graph.getNodeAttribute(id, "y") || 0;
      mass[i] = 1 + graph.degree(id);
    });

    const edgePairs = edges.map((edgeId) => ({
      source: index.get(graph.source(edgeId)),
      target: index.get(graph.target(edgeId))
    }));

    let iteration = 0;

    const step = () => {
      if (!layoutRunning) return;

      for (let s = 0; s < stepsPerFrame && iteration < iterations; s += 1) {
        dx.fill(0);
        dy.fill(0);

        for (let i = 0; i < nodeCount; i += 1) {
          for (let j = i + 1; j < nodeCount; j += 1) {
            const rx = x[i] - x[j];
            const ry = y[i] - y[j];
            const dist = Math.sqrt(rx * rx + ry * ry) + 0.01;
            const minDist =
              ((graph.getNodeAttribute(nodes[i], "size") || 6) +
                (graph.getNodeAttribute(nodes[j], "size") || 6) +
                nodePadding) /
              2;
            const overlap = Math.max(0, minDist - dist);
            const overlapForce = overlap * overlap * 0.1;
            const repulsion = (scalingRatio * scalingRatio) / dist + overlapForce;
            const fx = (rx / dist) * repulsion;
            const fy = (ry / dist) * repulsion;
            dx[i] += fx;
            dy[i] += fy;
            dx[j] -= fx;
            dy[j] -= fy;
          }
        }

        for (let k = 0; k < edgePairs.length; k += 1) {
          const { source, target } = edgePairs[k];
          if (source === undefined || target === undefined) continue;
          const rx = x[source] - x[target];
          const ry = y[source] - y[target];
          const dist = Math.sqrt(rx * rx + ry * ry) + 0.01;
          const attraction = (dist * dist) / scalingRatio;
          const fx = (rx / dist) * attraction;
          const fy = (ry / dist) * attraction;
          dx[source] -= fx;
          dy[source] -= fy;
          dx[target] += fx;
          dy[target] += fy;
        }

        for (let i = 0; i < nodeCount; i += 1) {
          const dist = Math.sqrt(x[i] * x[i] + y[i] * y[i]) + 0.01;
          const gravityForce = strongGravityMode ? gravity : gravity * dist;
          dx[i] -= (x[i] / dist) * gravityForce * mass[i];
          dy[i] -= (y[i] / dist) * gravityForce * mass[i];
        }

        for (let i = 0; i < nodeCount; i += 1) {
          x[i] += (dx[i] / (slowDown || 1)) * 0.01;
          y[i] += (dy[i] / (slowDown || 1)) * 0.01;
        }

        iteration += 1;
      }

      nodes.forEach((id, i) => {
        graph.setNodeAttribute(id, "x", x[i]);
        graph.setNodeAttribute(id, "y", y[i]);
      });
      sigma.refresh();

      if (iteration < iterations) {
        layoutFrame = requestAnimationFrame(step);
      } else {
        applyNudge(nodes, x, y, nodePadding);
        nodes.forEach((id, i) => {
          graph.setNodeAttribute(id, "x", x[i]);
          graph.setNodeAttribute(id, "y", y[i]);
        });
        sigma.refresh();
        layoutRunning = false;
        layoutFrame = null;
      }
    };

    layoutFrame = requestAnimationFrame(step);
  }

  function applyNudge(nodes, x, y, nodePadding) {
    const passes = 6;
    for (let pass = 0; pass < passes; pass += 1) {
      for (let i = 0; i < nodes.length; i += 1) {
        for (let j = i + 1; j < nodes.length; j += 1) {
          const rx = x[i] - x[j];
          const ry = y[i] - y[j];
          const dist = Math.sqrt(rx * rx + ry * ry) + 0.0001;
          const minDist =
            ((graph.getNodeAttribute(nodes[i], "size") || 6) +
              (graph.getNodeAttribute(nodes[j], "size") || 6) +
              nodePadding) /
            2;
          if (dist >= minDist) continue;
          const push = (minDist - dist) * 0.5;
          const fx = (rx / dist) * push;
          const fy = (ry / dist) * push;
          x[i] += fx;
          y[i] += fy;
          x[j] -= fx;
          y[j] -= fy;
        }
      }
    }
  }

  const CATEGORY_STYLES = {
    article: { color: "#355c7d", size: 8 },
    person: { color: "#6c5b7b", size: 9 },
    keyword: { color: "#c06c84", size: 7 },
    collection: { color: "#2a9d8f", size: 9 },
    image: { color: "#f4a261", size: 8 },
    video: { color: "#e76f51", size: 8 }
  };

  function resolveCategoryStyle(category) {
    if (!category) return null;
    const key = String(category).toLowerCase();
    return CATEGORY_STYLES[key] || null;
  }

  function addResources(resources) {
    resources.forEach((resource) => addResource(resource));
  }

  function addResource(resource) {
    const nodeId = String(resource.id);
    if (graph.hasNode(nodeId)) return;
    const categoryStyle = resolveCategoryStyle(resource.category);
    const labelText =
      resource.label === undefined || resource.label === null || resource.label === ""
        ? String(resource.id)
        : String(resource.label);
    const displayLabel = truncateLabel(labelText, 30);
    graph.addNode(nodeId, {
      label: displayLabel,
      fullLabel: labelText,
      resourceId: resource.id,
      category: resource.category || null,
      edgesLoaded: Boolean(resource.edgesLoaded),
      edgesLoading: Boolean(resource.edgesLoading),
      size: resource.size || (categoryStyle && categoryStyle.size) || 8,
      color: resource.color || (categoryStyle && categoryStyle.color) || "#0f4c5c",
      x: 0,
      y: 0
    });
  }

  function addEdges(edges) {
    edges.forEach((edge) => addEdge(edge));
  }

  function addEdge(edge) {
    const edgeId = edge.id !== undefined && edge.id !== null ? String(edge.id) : null;
    const sourceId = String(edge.from);
    const targetId = String(edge.to);

    if (!graph.hasNode(sourceId)) {
      addResource({ id: edge.from });
    }
    if (!graph.hasNode(targetId)) {
      addResource({ id: edge.to });
    }

    const edgeKey = edgeId || `e:${sourceId}->${targetId}:${edge.label || "rel"}:${edgeCounter++}`;
    if (graph.hasEdge(edgeKey)) return;
    graph.addEdgeWithKey(edgeKey, sourceId, targetId, {
      label: edge.label || "rel",
      color: edge.color || "#6b6b6b",
      size: edge.size || 1,
      type: "arrow"
    });
  }

  function setGraph(data) {
    resetGraph();
    enqueueBatch({
      nodes: data.nodes || [],
      edges: data.edges || [],
      runLayout: true
    });
  }

  function resetGraph() {
    activeNodeId = null;
    edgeCounter = 0;
    graph.clear();
    updateDetailPanel(null);
    stopLayout();
    sigma.refresh();
  }

  function applyIncrementalUpdate(update) {
    enqueueBatch({
      nodes: update.nodes || [],
      edges: update.edges || [],
      runLayout: true
    });
  }

  function markEdgesLoaded(resourceIds, loaded = true) {
    const ids = Array.isArray(resourceIds) ? resourceIds : [resourceIds];
    ids.forEach((id) => {
      const nodeId = String(id);
      if (!graph.hasNode(nodeId)) return;
      graph.setNodeAttribute(nodeId, "edgesLoaded", Boolean(loaded));
      if (loaded) {
        graph.setNodeAttribute(nodeId, "edgesLoading", false);
        clearEdgesLoadingTimeout(nodeId);
      }
    });
  }

  function markEdgesLoading(resourceIds, loading = true) {
    const ids = Array.isArray(resourceIds) ? resourceIds : [resourceIds];
    ids.forEach((id) => {
      const nodeId = String(id);
      if (!graph.hasNode(nodeId)) return;
      graph.setNodeAttribute(nodeId, "edgesLoading", Boolean(loading));
      if (loading) {
        scheduleEdgesLoadingTimeout(nodeId);
      } else {
        clearEdgesLoadingTimeout(nodeId);
      }
    });
  }

  function setEdgesLoadingTimeout(ms) {
    edgesLoadingTimeoutMs = Math.max(0, Number(ms) || 0);
  }

  function enqueueBatch(payload) {
    if (!batchQueue) {
      batchQueue = {
        nodes: [],
        edges: [],
        runLayout: false
      };
    }
    if (payload.nodes && payload.nodes.length) {
      batchQueue.nodes.push(...payload.nodes);
    }
    if (payload.edges && payload.edges.length) {
      batchQueue.edges.push(...payload.edges);
    }
    if (payload.runLayout) {
      batchQueue.runLayout = true;
    }
    if (!batchQueue.scheduled) {
      batchQueue.scheduled = true;
      requestAnimationFrame(processBatchQueue);
    }
  }

  function processBatchQueue() {
    if (!batchQueue) return;
    const { nodes, edges } = batchQueue;
    const nodesChunk = nodes.splice(0, batchSettings.nodesPerBatch);
    const edgesChunk = edges.splice(0, batchSettings.edgesPerBatch);

    if (nodesChunk.length) addResources(nodesChunk);
    if (edgesChunk.length) addEdges(edgesChunk);
    sigma.refresh();

    if (nodes.length || edges.length) {
      requestAnimationFrame(processBatchQueue);
      return;
    }

    const shouldRunLayout = batchQueue.runLayout;
    batchQueue = null;
    if (shouldRunLayout) {
      runForceAtlas2();
    }
  }

  function truncateLabel(text, maxLength) {
    if (!text) return "";
    const normalized = String(text);
    if (normalized.length <= maxLength) return normalized;
    return `${normalized.slice(0, Math.max(0, maxLength - 1))}…`;
  }

  function setActiveResource(resourceId) {
    const nodeId = String(resourceId);
    if (!graph.hasNode(nodeId)) return false;
    activeNodeId = nodeId;
    updateDetailPanel(nodeId);
    sigma.refresh();
    dispatchResourceClick(nodeId);
    if (
      !graph.getNodeAttribute(nodeId, "edgesLoaded") &&
      !graph.getNodeAttribute(nodeId, "edgesLoading")
    ) {
      graph.setNodeAttribute(nodeId, "edgesLoading", true);
      scheduleEdgesLoadingTimeout(nodeId);
      dispatchNeedsEdges(nodeId);
    }
    return true;
  }

  ensureSigma();

  resetButton.addEventListener("click", () => resetGraph());
  zoomInButton.addEventListener("click", () => zoomBy(1.2));
  zoomOutButton.addEventListener("click", () => zoomBy(0.8));

  function zoomBy(factor) {
    if (!sigma || !sigma.getCamera) return;
    const camera = sigma.getCamera();
    if (typeof camera.animatedZoom === "function") {
      camera.animatedZoom(factor);
      return;
    }

    const state = camera.getState ? camera.getState() : { ratio: 1, x: 0, y: 0 };
    const currentRatio = state.ratio || 1;
    const nextRatio = currentRatio / factor;

    if (typeof camera.animate === "function") {
      camera.animate({ ratio: nextRatio });
      return;
    }

    if (typeof camera.setState === "function") {
      camera.setState({ ratio: nextRatio });
    }
  }

  window.ResourceGraph = {
    setGraph,
    resetGraph,
    addResources,
    addEdges,
    applyIncrementalUpdate,
    enqueueBatch,
    markEdgesLoaded,
    markEdgesLoading,
    setEdgesLoadingTimeout,
    runLayout: runForceAtlas2,
    setActiveResource,
    refresh() {
      if (sigma) sigma.refresh();
    },
    setDebugOverlay() {}
  };
})();
