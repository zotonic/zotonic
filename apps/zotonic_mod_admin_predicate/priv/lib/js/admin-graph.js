(function () {
  const DEFAULT_COLORS = {
    nodeDefault: "#0f4c5c",
    edgeDefault: "#6b6b6b",
    edgeIncoming: "#2a9d8f",
    edgeOutgoing: "#e4572e",
    edgePath: "#4c6ef5",
    edgeHoverPath: "#7c3aed",
    nodeActive: "#e4572e",
    nodeStartOutline: "#1b1b1b",
    dimNode: "#c7c3bb",
    dimEdge: "#d1ccc4"
  };

  // Read a CSS variable with a fallback for theming.
  function readCssColor(varName, fallback) {
    const value = getComputedStyle(document.documentElement).getPropertyValue(varName).trim();
    return value || fallback;
  }

  // Resolve the runtime color palette from CSS variables.
  function resolveColors() {
    return {
      nodeDefault: readCssColor("--graph-node", DEFAULT_COLORS.nodeDefault),
      edgeDefault: readCssColor("--graph-edge", DEFAULT_COLORS.edgeDefault),
      edgeIncoming: readCssColor("--graph-edge-incoming", DEFAULT_COLORS.edgeIncoming),
      edgeOutgoing: readCssColor("--graph-edge-outgoing", DEFAULT_COLORS.edgeOutgoing),
      edgePath: readCssColor("--graph-edge-path", DEFAULT_COLORS.edgePath),
      edgeHoverPath: readCssColor("--graph-edge-hover-path", DEFAULT_COLORS.edgeHoverPath),
      nodeActive: readCssColor("--graph-node-active", DEFAULT_COLORS.nodeActive),
      nodeStartOutline: readCssColor("--graph-node-outline", DEFAULT_COLORS.nodeStartOutline),
      dimNode: readCssColor("--graph-dim-node", DEFAULT_COLORS.dimNode),
      dimEdge: readCssColor("--graph-dim-edge", DEFAULT_COLORS.dimEdge)
    };
  }

  let COLORS = resolveColors();
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
  const stepBackButton = document.getElementById("step-back");
  const zoomInButton = document.getElementById("zoom-in");
  const zoomOutButton = document.getElementById("zoom-out");
  const togglePathOnlyButton = document.getElementById("toggle-path-only");

  let graph = createGraph();
  let sigma = null;
  let activeNodeId = null;
  let hoverNodeId = null;
  let startNodeId = null;
  const focusNodes = new Set();
  const focusEdges = new Set();
  let pathOnlyMode = false;
  let showActiveNeighborsInPathOnly = true;
  let showActiveConnectionsInNormal = true;
  const pathWeights = { outgoing: 1, incoming: 10 };
  let activePathCache = { edges: [], nodes: [] };
  let suppressInitialFocus = false;
  let edgeCounter = 0;
  let edgesLoadingTimeoutMs = 15000;
  const edgesLoadingTimers = new Map();
  let layoutFrame = null;
  let layoutRunning = false;
  let layoutJobId = 0;
  let layoutWorker = null;
  let defaultUseWorker = false;
  const layoutSettings = {
    iterations: 300,
    stepsPerFrame: 5,
    gravity: 1,
    scalingRatio: 10,
    slowDown: 1,
    strongGravityMode: false,
    attractScale: 0.004,
    repelScale: 0.009,
    gravityScale: 0.006,
    edgeAttractionWeight: 3
  };
  const layoutGuard = {
    maxDegreeSoftLimit: 200,
    maxNodesSoftLimit: 600
  };
  const batchSettings = {
    nodesPerBatch: 50,
    edgesPerBatch: 100
  };
  const pendingEdgeHints = new Map();
  let batchQueue = null;
  const batchHistory = [];
  let baseGraphSaved = false;
  const resourceCategoryVisibility = new Map();
  const predicateVisibility = new Map();

  // Create the directed Graphology instance for Sigma.
  function createGraph() {
    return new GraphCtor({ type: "directed", multi: true, allowSelfLoops: true });
  }

  // Instantiate Sigma and wire graph interaction handlers.
  function ensureSigma() {
    if (sigma) {
      sigma.kill();
    }

    sigma = new SigmaCtor(graph, container, {
      defaultNodeColor: COLORS.nodeDefault,
      defaultEdgeColor: COLORS.edgeDefault,
      defaultEdgeType: "arrow",
      renderEdgeLabels: true,
      labelFont: "Source Serif 4, Palatino, Georgia, serif",
      labelSize: 14,
      edgeLabelSize: 12,
      edgeLabelColor: { color: "#7a7368" },
      nodeReducer: (node, data) => {
        if (data.hidden) return { ...data, hidden: true };
        const isActive = node === activeNodeId;
        const isHover = node === hoverNodeId;
        const isStart = node === startNodeId;
        const inFocus = focusNodes.size === 0 || focusNodes.has(node);
        if (data.hidden || (pathOnlyMode && !inFocus)) {
          return { ...data, hidden: true };
        }
        if (!inFocus) {
          return {
            ...data,
            color: COLORS.dimNode,
            label: data.label,
            borderColor: data.borderColor,
            borderSize: data.borderSize
          };
        }
        if (!isActive && !isHover && !isStart) return data;
        const showOutline = isHover || isStart;
        return {
          ...data,
          color: isActive ? COLORS.nodeActive : data.color,
          size: isActive ? data.size + 2 : data.size,
          borderColor: showOutline ? COLORS.nodeStartOutline : data.borderColor,
          borderSize: showOutline ? 2 : data.borderSize,
          label: isHover ? data.fullLabel || data.label : data.label
        };
      },
      edgeReducer: (edge, data) => {
        const inFocus = focusEdges.size === 0 || focusEdges.has(edge);
        if (data.hidden || (pathOnlyMode && !inFocus)) {
          return { ...data, hidden: true };
        }
        if (!inFocus) {
          return { ...data, color: COLORS.dimEdge, size: Math.max(1, (data.size || 1) - 0.3) };
        }
        if (!activeNodeId) return data;
        const source = graph.source(edge);
        const target = graph.target(edge);
        const isPathEdge = graph.getEdgeAttribute(edge, "pathActive");
        const isHoverPathEdge = graph.getEdgeAttribute(edge, "pathHover");
        if (source === activeNodeId && target === activeNodeId) {
          return { ...data, color: COLORS.edgeOutgoing, size: Math.max(2, data.size || 1) + 1 };
        }
        if (source === activeNodeId) {
          return { ...data, color: COLORS.edgeOutgoing, size: Math.max(2, data.size || 1) + 1 };
        }
        if (target === activeNodeId) {
          return { ...data, color: COLORS.edgeIncoming, size: Math.max(2, data.size || 1) + 1 };
        }
        if (isPathEdge) {
          return { ...data, color: COLORS.edgePath, size: Math.max(2, data.size || 1) + 1 };
        }
        if (isHoverPathEdge) {
          return { ...data, color: COLORS.edgeHoverPath, size: Math.max(2, data.size || 1) + 1 };
        }
        return { ...data, color: data.color || "#6b6b6b", size: data.size || 1 };
      }
    });

    sigma.on("clickNode", ({ node }) => {
      if (!pathOnlyMode && activeNodeId === node) {
        showActiveConnectionsInNormal = !showActiveConnectionsInNormal;
        if (hoverNodeId === node) {
          hoverNodeId = null;
        }
      }

      if (pathOnlyMode && activeNodeId === node) {
        showActiveNeighborsInPathOnly = !showActiveNeighborsInPathOnly;
      } else {
        showActiveNeighborsInPathOnly = true;
        if (activeNodeId !== node) {
          showActiveConnectionsInNormal = true;
        }
      }
      activeNodeId = node;
      updateDetailPanel(node);
      updatePathHighlight();
      updateHoverPathHighlight();
      updateFocusSets();
      sigma.refresh();
      dispatchActiveResourceChange(node);
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
      if (!activeNodeId) return;
      if (pathOnlyMode) {
        showActiveNeighborsInPathOnly = !showActiveNeighborsInPathOnly;
      } else {
        showActiveConnectionsInNormal = !showActiveConnectionsInNormal;
      }
      if (hoverNodeId === activeNodeId) {
        hoverNodeId = null;
      }
      updatePathHighlight();
      updateHoverPathHighlight();
      updateFocusSets();
      sigma.refresh();
    });

    sigma.on("enterNode", ({ node }) => {
      hoverNodeId = node;
      updatePathHighlight();
      updateHoverPathHighlight();
      updateFocusSets();
      sigma.refresh();
    });

    sigma.on("leaveNode", () => {
      hoverNodeId = null;
      updatePathHighlight();
      clearHoverPathHighlight();
      updateFocusSets();
      sigma.refresh();
    });
  }


  // Emit app-level event when a resource is clicked.
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

  // Emit app-level event when a node needs more edges.
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

  // Emit app-level event when edge loading times out.
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

  // Start a timeout to clear loading state if needed.
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

  // Clear any active edge-loading timeout for a node.
  function clearEdgesLoadingTimeout(nodeId) {
    const key = String(nodeId);
    const timer = edgesLoadingTimers.get(key);
    if (timer) {
      clearTimeout(timer);
      edgesLoadingTimers.delete(key);
    }
  }

  // Update the side panel to reflect the current selection.
  // Update the side panel to reflect the current selection.
  function updateDetailPanel(nodeId) {
    if (!detailMeta || !detailBody) {
      return;
    }
    if (!nodeId) {
      detailMeta.textContent = "No selection";
      detailBody.textContent = "Click a node to view its details here.";
      return;
    }

    const attrs = graph.getNodeAttributes(nodeId);
    detailMeta.textContent = `Resource ${attrs.resourceId}`;
    detailBody.textContent = JSON.stringify(attrs, null, 2);
  }

  // Seed node positions so layouts have a stable start.
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

  // Guard against NaN/Infinity positions after layout updates.
  function ensureFinitePositions() {
    let hasIssue = false;
    graph.forEachNode((nodeId) => {
      const x = graph.getNodeAttribute(nodeId, "x");
      const y = graph.getNodeAttribute(nodeId, "y");
      if (!Number.isFinite(x) || !Number.isFinite(y)) {
        hasIssue = true;
      }
    });
    if (hasIssue) {
      initializePositions();
    }
  }

  // Reset the camera so the graph stays in view.
  function fitCamera() {
    if (!sigma || !sigma.getCamera) return;
    const camera = sigma.getCamera();
    if (typeof camera.animatedReset === "function") {
      camera.animatedReset();
      return;
    }
    if (typeof camera.reset === "function") {
      camera.reset();
      return;
    }
    if (typeof camera.setState === "function") {
      camera.setState({ x: 0, y: 0, ratio: 1 });
    }
  }

  // Stop any running layout animation loop.
  // Stop any running layout animation loop.
  function stopLayout() {
    if (layoutFrame) {
      cancelAnimationFrame(layoutFrame);
      layoutFrame = null;
    }
    layoutRunning = false;
    layoutJobId += 1;
    if (layoutWorker) {
      layoutWorker.terminate();
      layoutWorker = null;
    }
  }

  // Run the force layout simulation with animation frames.
  function runForceAtlas2(options = {}) {
    if (options.useWorker) {
      const ok = runForceAtlas2InWorker(options);
      if (ok) return;
    }
    runForceAtlas2Main(options);
  }

  // Main-thread layout for environments without workers.
  function runForceAtlas2Main(options = {}) {
    const iterations = options.iterations || layoutSettings.iterations;
    const stepsPerFrame = options.stepsPerFrame || layoutSettings.stepsPerFrame;
    const gravity = options.gravity ?? layoutSettings.gravity;
    const scalingRatio = options.scalingRatio ?? layoutSettings.scalingRatio;
    const slowDown = options.slowDown ?? layoutSettings.slowDown;
    const strongGravityMode =
      options.strongGravityMode ?? layoutSettings.strongGravityMode;
    const attractScale = options.attractScale ?? layoutSettings.attractScale;
    const repelScale = options.repelScale ?? layoutSettings.repelScale;
    const gravityScale = options.gravityScale ?? layoutSettings.gravityScale;
    const nodePadding = options.nodePadding ?? 6;
    const edgeAttractionWeight =
      options.edgeAttractionWeight ?? layoutSettings.edgeAttractionWeight;

    stopLayout();
    initializePositions();
    ensureFinitePositions();
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
    const sizeCache = new Float32Array(nodeCount);

    let maxDegree = 0;
    nodes.forEach((id, i) => {
      const currentX = graph.getNodeAttribute(id, "x");
      const currentY = graph.getNodeAttribute(id, "y");
      if (Number.isFinite(currentX) && Number.isFinite(currentY)) {
        x[i] = currentX;
        y[i] = currentY;
      } else {
        x[i] = (Math.random() - 0.5) * 10;
        y[i] = (Math.random() - 0.5) * 10;
      }
      const degree = graph.degree(id);
      maxDegree = Math.max(maxDegree, degree);
      mass[i] = 1 + degree;
      sizeCache[i] = graph.getNodeAttribute(id, "size") || 6;
    });

    const guardHighDegree = maxDegree > layoutGuard.maxDegreeSoftLimit;
    const guardManyNodes = nodes.length > layoutGuard.maxNodesSoftLimit;
    const guardScale = guardHighDegree || guardManyNodes;
    const safeAttractScale = guardScale ? attractScale * 0.5 : attractScale;
    const safeRepelScale = guardScale ? repelScale * 0.8 : repelScale;
    const safeGravityScale = guardScale ? gravityScale * 1.2 : gravityScale;
    const safeEdgeWeight = guardScale ? edgeAttractionWeight * 0.7 : edgeAttractionWeight;
    const safeIterations = guardScale ? Math.min(iterations, 120) : iterations;
    const safeStepsPerFrame = guardScale ? Math.min(stepsPerFrame, 3) : stepsPerFrame;

    const edgePairs = edges.map((edgeId) => ({
      source: index.get(graph.source(edgeId)),
      target: index.get(graph.target(edgeId))
    }));

    let iteration = 0;

    const step = () => {
      if (!layoutRunning) return;

      for (let s = 0; s < safeStepsPerFrame && iteration < safeIterations; s += 1) {
        dx.fill(0);
        dy.fill(0);

        for (let i = 0; i < nodeCount; i += 1) {
          const xi = x[i];
          const yi = y[i];
          const si = sizeCache[i];
          for (let j = i + 1; j < nodeCount; j += 1) {
            const rx = xi - x[j];
            const ry = yi - y[j];
            const dist = Math.sqrt(rx * rx + ry * ry) + 0.01;
            const minDist = (si + sizeCache[j] + nodePadding) / 2;
            const overlap = Math.max(0, minDist - dist);
            const overlapForce = overlap * overlap * 0.1;
            const repulsion = (scalingRatio * scalingRatio) / dist + overlapForce;
            const fx = (rx / dist) * repulsion;
            const fy = (ry / dist) * repulsion;
            dx[i] += fx * safeRepelScale;
            dy[i] += fy * safeRepelScale;
            dx[j] -= fx * safeRepelScale;
            dy[j] -= fy * safeRepelScale;
          }
        }

        for (let k = 0; k < edgePairs.length; k += 1) {
          const { source, target } = edgePairs[k];
          if (source === undefined || target === undefined) continue;
          const rx = x[source] - x[target];
          const ry = y[source] - y[target];
          const dist = Math.sqrt(rx * rx + ry * ry) + 0.01;
          const attraction = ((dist * dist) / scalingRatio) * safeEdgeWeight;
          const fx = (rx / dist) * attraction;
          const fy = (ry / dist) * attraction;
          dx[source] -= fx * safeAttractScale;
          dy[source] -= fy * safeAttractScale;
          dx[target] += fx * safeAttractScale;
          dy[target] += fy * safeAttractScale;
        }

        for (let i = 0; i < nodeCount; i += 1) {
          const dist = Math.sqrt(x[i] * x[i] + y[i] * y[i]) + 0.01;
          const gravityForce = strongGravityMode ? gravity : gravity * dist;
          dx[i] -= (x[i] / dist) * gravityForce * mass[i] * safeGravityScale;
          dy[i] -= (y[i] / dist) * gravityForce * mass[i] * safeGravityScale;
        }

        for (let i = 0; i < nodeCount; i += 1) {
          x[i] += (dx[i] / (slowDown || 1)) * 0.1;
          y[i] += (dy[i] / (slowDown || 1)) * 0.1;
          if (!Number.isFinite(x[i]) || !Number.isFinite(y[i])) {
            stopLayout();
            initializePositions();
            fitCamera();
            return;
          }
        }

        iteration += 1;
      }

      nodes.forEach((id, i) => {
        graph.setNodeAttribute(id, "x", x[i]);
        graph.setNodeAttribute(id, "y", y[i]);
      });
      sigma.refresh();

      if (iteration < safeIterations) {
        layoutFrame = requestAnimationFrame(step);
      } else {
        applyNudge(nodes, x, y, nodePadding);
        ensureFinitePositions();
        nodes.forEach((id, i) => {
          graph.setNodeAttribute(id, "x", x[i]);
          graph.setNodeAttribute(id, "y", y[i]);
        });
        sigma.refresh();
        layoutRunning = false;
        layoutFrame = null;
        fitCamera();
      }
    };

    layoutFrame = requestAnimationFrame(step);
  }

  // Try running the layout in a web worker; returns true if started.
  function runForceAtlas2InWorker(options = {}) {
    if (typeof Worker === "undefined") return false;
    const nodes = graph.nodes();
    if (!nodes.length) return false;
    const edges = graph.edges();

    const nodeIndex = new Map(nodes.map((id, i) => [id, i]));
    const positions = nodes.map((id) => ({
      x: Number(graph.getNodeAttribute(id, "x")) || 0,
      y: Number(graph.getNodeAttribute(id, "y")) || 0,
      size: graph.getNodeAttribute(id, "size") || 6,
      degree: graph.degree(id) || 0
    }));

    const edgePairs = edges
      .map((edgeId) => ({
        source: nodeIndex.get(graph.source(edgeId)),
        target: nodeIndex.get(graph.target(edgeId))
      }))
      .filter((pair) => pair.source !== undefined && pair.target !== undefined);

    const jobId = ++layoutJobId;
    if (layoutWorker) {
      layoutWorker.terminate();
      layoutWorker = null;
    }

    try {
      const workerSource = `
        self.onmessage = (event) => {
          const { jobId, nodes, edges, settings, guard } = event.data;
          const count = nodes.length;
          const x = new Float32Array(count);
          const y = new Float32Array(count);
          const dx = new Float32Array(count);
          const dy = new Float32Array(count);
          const mass = new Float32Array(count);
          const sizeCache = new Float32Array(count);
          let maxDegree = 0;
          for (let i = 0; i < count; i++) {
            const n = nodes[i];
            x[i] = Number.isFinite(n.x) ? n.x : (Math.random() - 0.5) * 10;
            y[i] = Number.isFinite(n.y) ? n.y : (Math.random() - 0.5) * 10;
            mass[i] = 1 + n.degree;
            sizeCache[i] = n.size || 6;
            if (n.degree > maxDegree) maxDegree = n.degree;
          }

          const guardScale = maxDegree > guard.maxDegreeSoftLimit || count > guard.maxNodesSoftLimit;
          const safeAttractScale = guardScale ? settings.attractScale * 0.5 : settings.attractScale;
          const safeRepelScale = guardScale ? settings.repelScale * 0.8 : settings.repelScale;
          const safeGravityScale = guardScale ? settings.gravityScale * 1.2 : settings.gravityScale;
          const safeEdgeWeight = guardScale ? settings.edgeAttractionWeight * 0.7 : settings.edgeAttractionWeight;
          const safeIterations = guardScale ? Math.min(settings.iterations, 120) : settings.iterations;
          const safeStepsPerFrame = guardScale ? Math.min(settings.stepsPerFrame, 3) : settings.stepsPerFrame;

          let iteration = 0;
          const nodePadding = settings.nodePadding;
          const scalingRatio = settings.scalingRatio;
          const slowDown = settings.slowDown || 1;
          const gravity = settings.gravity;
          const strongGravityMode = settings.strongGravityMode;

          let lastPost = Date.now();
          const postInterval = settings.postInterval || 80;

          while (iteration < safeIterations) {
            for (let s = 0; s < safeStepsPerFrame && iteration < safeIterations; s++) {
              dx.fill(0);
              dy.fill(0);

              for (let i = 0; i < count; i++) {
                const xi = x[i];
                const yi = y[i];
                const si = sizeCache[i];
                for (let j = i + 1; j < count; j++) {
                  const rx = xi - x[j];
                  const ry = yi - y[j];
                  const dist = Math.sqrt(rx * rx + ry * ry) + 0.01;
                  const minDist = (si + sizeCache[j] + nodePadding) / 2;
                  const overlap = Math.max(0, minDist - dist);
                  const overlapForce = overlap * overlap * 0.1;
                  const repulsion = (scalingRatio * scalingRatio) / dist + overlapForce;
                  const fx = (rx / dist) * repulsion;
                  const fy = (ry / dist) * repulsion;
                  dx[i] += fx * safeRepelScale;
                  dy[i] += fy * safeRepelScale;
                  dx[j] -= fx * safeRepelScale;
                  dy[j] -= fy * safeRepelScale;
                }
              }

              for (let k = 0; k < edges.length; k++) {
                const { source, target } = edges[k];
                const rx = x[source] - x[target];
                const ry = y[source] - y[target];
                const dist = Math.sqrt(rx * rx + ry * ry) + 0.01;
                const attraction = ((dist * dist) / scalingRatio) * safeEdgeWeight;
                const fx = (rx / dist) * attraction;
                const fy = (ry / dist) * attraction;
                dx[source] -= fx * safeAttractScale;
                dy[source] -= fy * safeAttractScale;
                dx[target] += fx * safeAttractScale;
                dy[target] += fy * safeAttractScale;
              }

              for (let i = 0; i < count; i++) {
                const dist = Math.sqrt(x[i] * x[i] + y[i] * y[i]) + 0.01;
                const gravityForce = strongGravityMode ? gravity : gravity * dist;
                dx[i] -= (x[i] / dist) * gravityForce * mass[i] * safeGravityScale;
                dy[i] -= (y[i] / dist) * gravityForce * mass[i] * safeGravityScale;
              }

              for (let i = 0; i < count; i++) {
                x[i] += (dx[i] / slowDown) * 0.1;
                y[i] += (dy[i] / slowDown) * 0.1;
                if (!Number.isFinite(x[i]) || !Number.isFinite(y[i])) {
                  self.postMessage({ jobId, failed: true });
                  return;
                }
              }
              iteration++;
            }

            const now = Date.now();
            if (now - lastPost >= postInterval) {
              lastPost = now;
              self.postMessage({
                jobId,
                partial: true,
                positions: Array.from(x).map((value, i) => ({ x: value, y: y[i] }))
              });
            }
          }

          // Nudge pass
          const passes = 6;
          for (let pass = 0; pass < passes; pass++) {
            for (let i = 0; i < count; i++) {
              for (let j = i + 1; j < count; j++) {
                const rx = x[i] - x[j];
                const ry = y[i] - y[j];
                const dist = Math.sqrt(rx * rx + ry * ry) + 0.0001;
                const minDist = ((nodes[i].size || 6) + (nodes[j].size || 6) + nodePadding) / 2;
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

          self.postMessage({
            jobId,
            positions: Array.from(x).map((value, i) => ({ x: value, y: y[i] }))
          });
        };
      `;

      const blob = new Blob([workerSource], { type: "application/javascript" });
      const workerUrl = URL.createObjectURL(blob);
      layoutWorker = new Worker(workerUrl);

      layoutWorker.onmessage = (event) => {
        const { jobId: resultId, positions, failed, partial } = event.data;
        if (resultId !== jobId) return;
        if (failed || !positions) {
          layoutWorker.terminate();
          layoutWorker = null;
          initializePositions();
          fitCamera();
          sigma.refresh();
          return;
        }
        positions.forEach((pos, i) => {
          const nodeId = nodes[i];
          graph.setNodeAttribute(nodeId, "x", pos.x);
          graph.setNodeAttribute(nodeId, "y", pos.y);
        });
        sigma.refresh();
        if (!partial) {
          layoutWorker.terminate();
          layoutWorker = null;
          fitCamera();
        }
      };

      layoutWorker.postMessage({
        jobId,
        nodes: positions,
        edges: edgePairs,
        settings: {
          iterations,
          stepsPerFrame,
          gravity,
          scalingRatio,
          slowDown,
          strongGravityMode,
          attractScale,
          repelScale,
          gravityScale,
          edgeAttractionWeight,
          nodePadding: options.nodePadding ?? 6,
          postInterval: options.postInterval ?? 80
        },
        guard: layoutGuard
      });
      return true;
    } catch (err) {
      if (layoutWorker) {
        layoutWorker.terminate();
        layoutWorker = null;
      }
      return false;
    }
  }

  // Post-process layout to reduce remaining node overlap.
  function applyNudge(nodes, x, y, nodePadding) {
    const passes = 6;
    const sizeCache = new Float32Array(nodes.length);
    for (let i = 0; i < nodes.length; i += 1) {
      sizeCache[i] = graph.getNodeAttribute(nodes[i], "size") || 6;
    }
    for (let pass = 0; pass < passes; pass += 1) {
      for (let i = 0; i < nodes.length; i += 1) {
        const xi = x[i];
        const yi = y[i];
        const si = sizeCache[i];
        for (let j = i + 1; j < nodes.length; j += 1) {
          const rx = xi - x[j];
          const ry = yi - y[j];
          const dist = Math.sqrt(rx * rx + ry * ry) + 0.0001;
          const minDist = (si + sizeCache[j] + nodePadding) / 2;
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

  // Map category names to default style hints.
  function resolveCategoryStyle(category) {
    if (!category) return null;
    const key = String(category).toLowerCase();
    return CATEGORY_STYLES[key] || null;
  }

  // Bulk-add resources, optionally tracking created nodes.
  // Bulk-add resources, optionally tracking created nodes.
  function addResources(resources, createdNodes = null, nearResourceId = null) {
    resources.forEach((resource) => addResource(resource, createdNodes, nearResourceId));
  }

  // Insert or update a single node with current attributes.
  function addResource(resource, createdNodes = null, nearResourceId = null) {
    const nodeId = String(resource.id);
    const categoryStyle = resolveCategoryStyle(resource.category);
    const categoryId =
      resource.category_id !== undefined && resource.category_id !== null
        ? Number(resource.category_id)
        : null;
    const labelText =
      resource.label === undefined || resource.label === null || resource.label === ""
        ? String(resource.id)
        : String(resource.label);
    const displayLabel = truncateLabel(labelText, 30);
    if (graph.hasNode(nodeId)) {
      const updates = {
        label: displayLabel,
        fullLabel: labelText,
        resourceId: resource.id,
        category: resource.category || null,
        category_id: categoryId,
        edgesLoaded: resource.edgesLoaded !== undefined ? Boolean(resource.edgesLoaded) : undefined,
        edgesLoading: resource.edgesLoading !== undefined ? Boolean(resource.edgesLoading) : undefined,
        size: resource.size || (categoryStyle && categoryStyle.size) || 8,
        color: resource.color || (categoryStyle && categoryStyle.color) || "#0f4c5c"
      };
      Object.entries(updates).forEach(([key, value]) => {
        if (value !== undefined) {
          graph.setNodeAttribute(nodeId, key, value);
        }
      });
      const x = Number(resource.x);
      const y = Number(resource.y);
      if (Number.isFinite(x) && Number.isFinite(y)) {
        graph.setNodeAttribute(nodeId, "x", x);
        graph.setNodeAttribute(nodeId, "y", y);
        graph.setNodeAttribute(nodeId, "posAuto", false);
      }
      return;
    }

    const position = resolveInitialPosition(resource, nearResourceId);
    const explicitPos = Number.isFinite(Number(resource.x)) && Number.isFinite(Number(resource.y));
    graph.addNode(nodeId, {
      label: displayLabel,
      fullLabel: labelText,
      resourceId: resource.id,
      category: resource.category || null,
      category_id: categoryId,
      edgesLoaded: Boolean(resource.edgesLoaded),
      edgesLoading: Boolean(resource.edgesLoading),
      size: resource.size || (categoryStyle && categoryStyle.size) || 8,
      color: resource.color || (categoryStyle && categoryStyle.color) || COLORS.nodeDefault,
      x: position.x,
      y: position.y,
      posAuto: !explicitPos
    });
    if (createdNodes) createdNodes.add(nodeId);
  }

  // Choose an initial position for a newly added node.
  function resolveInitialPosition(resource, nearResourceId = null) {
    const x = Number(resource.x);
    const y = Number(resource.y);
    if (Number.isFinite(x) && Number.isFinite(y)) {
      return { x, y };
    }

    if (nearResourceId !== null && graph.hasNode(String(nearResourceId))) {
      const nx = graph.getNodeAttribute(String(nearResourceId), "x") || 0;
      const ny = graph.getNodeAttribute(String(nearResourceId), "y") || 0;
      const jitter = 3;
      return {
        x: nx + (Math.random() - 0.5) * jitter,
        y: ny + (Math.random() - 0.5) * jitter
      };
    }

    const hint = pendingEdgeHints.get(String(resource.id));
    if (hint) {
      return {
        x: hint.x + (Math.random() - 0.5) * hint.jitter,
        y: hint.y + (Math.random() - 0.5) * hint.jitter
      };
    }

    const existing = graph.nodes();
    if (existing.length === 0) {
      return { x: 0, y: 0 };
    }

    let cx = 0;
    let cy = 0;
    existing.forEach((id) => {
      cx += graph.getNodeAttribute(id, "x") || 0;
      cy += graph.getNodeAttribute(id, "y") || 0;
    });
    cx /= existing.length;
    cy /= existing.length;

    const jitter = 8;
    return {
      x: cx + (Math.random() - 0.5) * jitter,
      y: cy + (Math.random() - 0.5) * jitter
    };
  }

  // Bulk-add edges, optionally tracking created edges.
  function addEdges(edges, createdEdges = null, createdNodes = null) {
    edges.forEach((edge) => addEdge(edge, createdEdges, createdNodes));
  }

  // Insert a single edge and ensure endpoints exist.
  function addEdge(edge, createdEdges = null, createdNodes = null) {
    const edgeId = edge.id !== undefined && edge.id !== null ? Number(edge.id) : null;
    const sourceId = String(edge.from);
    const targetId = String(edge.to);
    const predicateId =
      edge.predicate_id !== undefined && edge.predicate_id !== null
        ? Number(edge.predicate_id)
        : null;

    const hintJitter = 3;
    if (!graph.hasNode(sourceId) && graph.hasNode(targetId)) {
      pendingEdgeHints.set(sourceId, {
        x: graph.getNodeAttribute(targetId, "x") || 0,
        y: graph.getNodeAttribute(targetId, "y") || 0,
        jitter: hintJitter
      });
    }
    if (!graph.hasNode(targetId) && graph.hasNode(sourceId)) {
      pendingEdgeHints.set(targetId, {
        x: graph.getNodeAttribute(sourceId, "x") || 0,
        y: graph.getNodeAttribute(sourceId, "y") || 0,
        jitter: hintJitter
      });
    }

    if (!graph.hasNode(sourceId)) {
      addResource({ id: edge.from }, createdNodes);
    }
    if (!graph.hasNode(targetId)) {
      addResource({ id: edge.to }, createdNodes);
    }

    const edgeKey = edgeId;
    if (graph.hasEdge(edgeKey)) return;
    graph.addEdgeWithKey(edgeKey, sourceId, targetId, {
      label: edge.label || "rel",
      predicate_id: predicateId,
      color: edge.color || COLORS.edgeDefault,
      size: edge.size || 1,
      type: "arrow"
    });
    if (createdEdges) createdEdges.add(edgeKey);
  }

  // Replace the graph with a new baseline dataset.
  function setGraph(data) {
    resetGraph();
    baseGraphSaved = false;
    const useWorker = data.useWorker === true;
    enqueueBatch({
      nodes: data.nodes || [],
      edges: data.edges || [],
      runLayout: true,
      resetHistory: true,
      useWorker
    });
    setDefaultUseWorker(useWorker);
    if (data && data.startNodeId !== undefined && data.startNodeId !== null) {
      startNodeId = String(data.startNodeId);
    }
    if (data && data.setActiveToStart === true && data.startNodeId !== undefined && data.startNodeId !== null) {
      scheduleActiveStart(data.startNodeId);
    }
  }

  // Clear the graph and reset UI state.
  function resetGraph() {
    activeNodeId = null;
    edgeCounter = 0;
    graph.clear();
    updateDetailPanel(null);
    stopLayout();
    sigma.refresh();
  }

  // Append a batch of nodes/edges and relayout.
  function applyIncrementalUpdate(update) {
    enqueueBatch({
      nodes: update.nodes || [],
      edges: update.edges || [],
      runLayout: true,
      nearResourceId: update.nearResourceId ?? null,
      useWorker: update.useWorker ?? null
    });
  }

  // Mark resources as fully loaded and clear loading state.
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

  // Mark resources as loading and schedule timeout.
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

  // Configure the edge-loading timeout duration.
  function setEdgesLoadingTimeout(ms) {
    edgesLoadingTimeoutMs = Math.max(0, Number(ms) || 0);
  }

  // Set the start node for path highlighting.
  function setStartNode(resourceId) {
    if (resourceId === undefined || resourceId === null) {
      startNodeId = null;
      clearPathHighlight();
      clearFocusSets();
      sigma.refresh();
      return;
    }
    const nodeId = String(resourceId);
    if (!graph.hasNode(nodeId)) return;
    startNodeId = nodeId;
    updatePathHighlight();
    updateFocusSets();
    sigma.refresh();
  }

  // Compute and flag the active-to-start path edges.
  // Compute and flag the active-to-start path edges.
  function updatePathHighlight() {
    clearPathHighlight();
    activePathCache = { edges: [], nodes: [] };
    if (!activeNodeId || !startNodeId) return;

    const { found, prevEdge, prevNode } = findWeightedPath(startNodeId, activeNodeId);
    if (!found) return;

    let cursor = activeNodeId;
    const nodePath = [graph.getNodeAttribute(activeNodeId, "resourceId")];
    const edgePath = [];
    while (cursor !== startNodeId) {
      const edgeId = prevEdge.get(cursor);
      const fromNode = prevNode.get(cursor);
      if (edgeId === undefined || fromNode === undefined) break;
      graph.setEdgeAttribute(edgeId, "pathActive", true);
      const from = graph.source(edgeId);
      const to = graph.target(edgeId);
      edgePath.push({
        id: edgeId,
        from: graph.getNodeAttribute(from, "resourceId"),
        to: graph.getNodeAttribute(to, "resourceId"),
        predicate_id: graph.getEdgeAttribute(edgeId, "predicate_id")
      });
      nodePath.push(graph.getNodeAttribute(fromNode, "resourceId"));
      cursor = fromNode;
    }
    activePathCache = {
      edges: edgePath.reverse(),
      nodes: nodePath.reverse()
    };
  }

  function getActivePath() {
    return activePathCache.edges;
  }

  function getActivePathNodes() {
    return activePathCache.nodes;
  }

  // Compute and flag the active-to-hover path edges.
  // Compute and flag the active-to-hover path edges.
  function updateHoverPathHighlight() {
    clearHoverPathHighlight();
    if (!activeNodeId || !hoverNodeId) return;
    if (activeNodeId === hoverNodeId) return;
    const { found, prevEdge, prevNode } = findWeightedPath(activeNodeId, hoverNodeId);
    if (!found) return;
    let cursor = hoverNodeId;
    while (cursor !== activeNodeId) {
      const edgeId = prevEdge.get(cursor);
      const fromNode = prevNode.get(cursor);
      if (edgeId === undefined || fromNode === undefined) break;
      graph.setEdgeAttribute(edgeId, "pathHover", true);
      cursor = fromNode;
    }
  }

  // Clear path highlight flags from edges.
  function clearPathHighlight() {
    graph.forEachEdge((edgeId) => {
      if (graph.getEdgeAttribute(edgeId, "pathActive")) {
        graph.setEdgeAttribute(edgeId, "pathActive", false);
      }
    });
    activePathCache = { edges: [], nodes: [] };
  }

  // Clear hover-path highlight flags from edges.
  function clearHoverPathHighlight() {
    graph.forEachEdge((edgeId) => {
      if (graph.getEdgeAttribute(edgeId, "pathHover")) {
        graph.setEdgeAttribute(edgeId, "pathHover", false);
      }
    });
  }

  // Find a shortest path in the undirected view of the graph.
  // Find a lowest-cost path where outgoing edges cost 1 and incoming cost 10.
  function findWeightedPath(fromNode, toNode) {
    const dist = new Map();
    const prevEdge = new Map();
    const prevNode = new Map();
    const visited = new Set();
    const queue = [{ node: fromNode, cost: 0 }];
    dist.set(fromNode, 0);

    while (queue.length) {
      queue.sort((a, b) => a.cost - b.cost);
      const { node: current, cost } = queue.shift();
      if (visited.has(current)) continue;
      visited.add(current);
      if (current === toNode) break;

      graph.forEachEdge(current, (edgeId, attrs, source, target) => {
        const neighbor = source === current ? target : source;
        const stepCost = source === current ? pathWeights.outgoing : pathWeights.incoming;
        const nextCost = cost + stepCost;
        if (!dist.has(neighbor) || nextCost < dist.get(neighbor)) {
          dist.set(neighbor, nextCost);
          prevNode.set(neighbor, current);
          prevEdge.set(neighbor, edgeId);
          queue.push({ node: neighbor, cost: nextCost });
        }
      });
    }

    return { found: dist.has(toNode), prevEdge, prevNode };
  }

  // Compute focus sets for dimming/hiding behavior.
  function updateFocusSets() {
    focusNodes.clear();
    focusEdges.clear();
    const activeAnchor = activeNodeId;
    const hoverAnchor = hoverNodeId;

    if (hoverAnchor) {
      if (!pathOnlyMode) {
        focusNodes.add(hoverAnchor);
        if (activeAnchor) focusNodes.add(activeAnchor);
        if (startNodeId) focusNodes.add(startNodeId);
        graph.forEachEdge(hoverAnchor, (edgeId, attrs, source, target) => {
          focusEdges.add(edgeId);
          focusNodes.add(source);
          focusNodes.add(target);
        });
        graph.forEachEdge((edgeId) => {
          if (graph.getEdgeAttribute(edgeId, "pathActive") || graph.getEdgeAttribute(edgeId, "pathHover")) {
            focusEdges.add(edgeId);
            const source = graph.source(edgeId);
            const target = graph.target(edgeId);
            focusNodes.add(source);
            focusNodes.add(target);
          }
        });
        return;
      }
      focusNodes.add(hoverAnchor);
      if (startNodeId) focusNodes.add(startNodeId);
      if (showActiveNeighborsInPathOnly) {
        graph.forEachEdge(hoverAnchor, (edgeId, attrs, source, target) => {
          focusEdges.add(edgeId);
          focusNodes.add(source);
          focusNodes.add(target);
        });
      }
      graph.forEachEdge((edgeId) => {
        if (graph.getEdgeAttribute(edgeId, "pathActive") || graph.getEdgeAttribute(edgeId, "pathHover")) {
          focusEdges.add(edgeId);
          const source = graph.source(edgeId);
          const target = graph.target(edgeId);
          focusNodes.add(source);
          focusNodes.add(target);
        }
      });
      return;
    }

    const anchor = activeAnchor;
    if (!anchor) return;

    focusNodes.add(anchor);
    if (startNodeId) focusNodes.add(startNodeId);

    if (!pathOnlyMode || showActiveNeighborsInPathOnly) {
    graph.forEachEdge(anchor, (edgeId, attrs, source, target) => {
      if (!pathOnlyMode && !showActiveConnectionsInNormal) {
        return;
      }
      focusEdges.add(edgeId);
      focusNodes.add(source);
      focusNodes.add(target);
    });
    }

    graph.forEachEdge((edgeId) => {
      if (graph.getEdgeAttribute(edgeId, "pathActive")) {
        focusEdges.add(edgeId);
        const source = graph.source(edgeId);
        const target = graph.target(edgeId);
        focusNodes.add(source);
        focusNodes.add(target);
      }
    });
  }

  // Clear focus sets so everything renders normally.
  function clearFocusSets() {
    focusNodes.clear();
    focusEdges.clear();
  }

  // Toggle whether only path-related elements are visible.
  function setPathOnlyMode(enabled) {
    pathOnlyMode = Boolean(enabled);
    if (pathOnlyMode) {
      showActiveNeighborsInPathOnly = false;
    }
    updateFocusSets();
    sigma.refresh();
  }

  // Show or hide nodes by category id.
  function setCategoryVisibility(categoryId, visible) {
    if (categoryId === undefined || categoryId === null) return;
    const key = Number(categoryId);
    if (!Number.isFinite(key)) return;
    resourceCategoryVisibility.set(key, Boolean(visible));
    applyVisibility();
  }

  // Convenience wrapper to hide a category.
  function hideCategory(categoryId) {
    setCategoryVisibility(categoryId, false);
  }

  // Convenience wrapper to show a category.
  function showCategory(categoryId) {
    setCategoryVisibility(categoryId, true);
  }

  // Return the list of currently hidden categories.
  function getHiddenCategories() {
    const hidden = [];
    resourceCategoryVisibility.forEach((visible, key) => {
      if (visible === false) hidden.push(key);
    });
    return hidden.sort((a, b) => a - b);
  }

  // Replace the hidden-category list with a new set of ids.
  function setHiddenCategories(categoryIds) {
    resourceCategoryVisibility.clear();
    if (Array.isArray(categoryIds)) {
      categoryIds.forEach((id) => {
        const key = Number(id);
        if (Number.isFinite(key)) {
          resourceCategoryVisibility.set(key, false);
        }
      });
    }
    applyVisibility();
  }

  // Show or hide edges by predicate id.
  function setPredicateVisibility(predicateId, visible) {
    if (predicateId === undefined || predicateId === null) return;
    const key = Number(predicateId);
    if (!Number.isFinite(key)) return;
    predicateVisibility.set(key, Boolean(visible));
    applyVisibility();
  }

  // Convenience wrapper to hide a predicate.
  function hidePredicate(predicateId) {
    setPredicateVisibility(predicateId, false);
  }

  // Convenience wrapper to show a predicate.
  function showPredicate(predicateId) {
    setPredicateVisibility(predicateId, true);
  }

  // Return the list of currently hidden predicates.
  function getHiddenPredicates() {
    const hidden = [];
    predicateVisibility.forEach((visible, key) => {
      if (visible === false) hidden.push(key);
    });
    return hidden.sort((a, b) => a - b);
  }

  // Replace the hidden-predicate list with a new set of ids.
  function setHiddenPredicates(predicateIds) {
    predicateVisibility.clear();
    if (Array.isArray(predicateIds)) {
      predicateIds.forEach((id) => {
        const key = Number(id);
        if (Number.isFinite(key)) {
          predicateVisibility.set(key, false);
        }
      });
    }
    applyVisibility();
  }

  // Apply category/predicate visibility to nodes and edges.
  function applyVisibility() {
    graph.forEachNode((nodeId, attrs) => {
      const categoryId =
        attrs.category_id !== undefined && attrs.category_id !== null
          ? Number(attrs.category_id)
          : null;
      const isVisible =
        categoryId !== null ? resourceCategoryVisibility.get(categoryId) !== false : true;
      graph.setNodeAttribute(nodeId, "hidden", !isVisible);
    });

    graph.forEachEdge((edgeId) => {
      const sourceId = graph.source(edgeId);
      const targetId = graph.target(edgeId);
      const sourceHidden = graph.getNodeAttribute(sourceId, "hidden");
      const targetHidden = graph.getNodeAttribute(targetId, "hidden");
      const predicateId =
        graph.getEdgeAttribute(edgeId, "predicate_id") !== undefined &&
        graph.getEdgeAttribute(edgeId, "predicate_id") !== null
          ? Number(graph.getEdgeAttribute(edgeId, "predicate_id"))
          : null;
      const predicateHidden =
        predicateId !== null ? predicateVisibility.get(predicateId) === false : false;
      graph.setEdgeAttribute(edgeId, "hidden", Boolean(sourceHidden || targetHidden || predicateHidden));
    });

    graph.forEachNode((nodeId) => {
      const hasAnyEdge = graph.degree(nodeId) > 0;
      if (!hasAnyEdge) return;
      let hasVisibleEdge = false;
      graph.forEachEdge(nodeId, (edgeId, attrs) => {
        if (!attrs.hidden) {
          hasVisibleEdge = true;
        }
      });
      if (!hasVisibleEdge) {
        graph.setNodeAttribute(nodeId, "hidden", true);
      }
    });
    sigma.refresh();
  }

  // Undo the last batch insertion while preserving the base graph.
  function stepBack() {
    const batch = batchHistory.pop();
    if (!batch) return false;
    graph.forEachNode((nodeId) => {
      graph.setNodeAttribute(nodeId, "edgesLoaded", false);
      graph.setNodeAttribute(nodeId, "edgesLoading", false);
      clearEdgesLoadingTimeout(nodeId);
    });
    batch.edges.forEach((edgeId) => {
      if (graph.hasEdge(edgeId)) {
        graph.dropEdge(edgeId);
      }
    });
    batch.nodes.forEach((nodeId) => {
      if (graph.hasNode(nodeId)) {
        graph.dropNode(nodeId);
      }
    });
    activeNodeId = null;
    updateDetailPanel(null);
    sigma.refresh();
    runForceAtlas2();
    return true;
  }

  // Queue batched inserts to keep rendering responsive.
  function enqueueBatch(payload) {
    if (!batchQueue) {
      batchQueue = {
        nodes: [],
        edges: [],
        runLayout: false,
        resetHistory: false,
        createdNodes: new Set(),
        createdEdges: new Set(),
        nearResourceId: null,
        useWorker: defaultUseWorker
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
    if (payload.resetHistory) {
      batchQueue.resetHistory = true;
    }
    if (payload.useWorker !== undefined && payload.useWorker !== null) {
      batchQueue.useWorker = Boolean(payload.useWorker);
    }
    if (payload.nearResourceId !== undefined) {
      batchQueue.nearResourceId = payload.nearResourceId;
    }
    if (!batchQueue.scheduled) {
      batchQueue.scheduled = true;
      requestAnimationFrame(processBatchQueue);
    }
  }

  // Process queued batches and trigger layout when done.
  function processBatchQueue() {
    if (!batchQueue) return;
    const { nodes, edges } = batchQueue;
    const createdNodes = batchQueue.createdNodes;
    const createdEdges = batchQueue.createdEdges;
    const nodesChunk = nodes.splice(0, batchSettings.nodesPerBatch);
    const edgesChunk = edges.splice(0, batchSettings.edgesPerBatch);

    if (nodesChunk.length) addResources(nodesChunk, createdNodes, batchQueue.nearResourceId);
    if (edgesChunk.length) addEdges(edgesChunk, createdEdges, createdNodes);
    sigma.refresh();

    if (pendingEdgeHints.size) {
      pendingEdgeHints.clear();
    }

    if (nodes.length || edges.length) {
      requestAnimationFrame(processBatchQueue);
      return;
    }

    const shouldRunLayout = batchQueue.runLayout;
    const useWorker = batchQueue.useWorker;
    const activateStart = batchQueue.activateStart;
    if (batchQueue.resetHistory) {
      batchHistory.length = 0;
      baseGraphSaved = false;
    }
    if (createdNodes.size || createdEdges.size) {
      if (!baseGraphSaved) {
        baseGraphSaved = true;
      } else {
        batchHistory.push({
          nodes: Array.from(createdNodes),
          edges: Array.from(createdEdges)
        });
      }
    }
    batchQueue = null;
    if (shouldRunLayout) {
      runForceAtlas2({ useWorker });
    } else {
      fitCamera();
    }
    if (activateStart && startNodeId) {
      setActiveResource(startNodeId);
    }
  }

  function scheduleActiveStart(startId) {
    if (!batchQueue) {
      batchQueue = {
        nodes: [],
        edges: [],
        runLayout: false,
        resetHistory: false,
        createdNodes: new Set(),
        createdEdges: new Set(),
        nearResourceId: null,
        useWorker: defaultUseWorker,
        activateStart: true
      };
      if (!batchQueue.scheduled) {
        batchQueue.scheduled = true;
        requestAnimationFrame(processBatchQueue);
      }
    } else {
      batchQueue.activateStart = true;
    }
    startNodeId = String(startId);
    suppressInitialFocus = true;
  }

  // Clamp labels to a reasonable length for display.
  function truncateLabel(text, maxLength) {
    if (!text) return "";
    const normalized = String(text);
    if (normalized.length <= maxLength) return normalized;
    return `${normalized.slice(0, Math.max(0, maxLength - 1))}…`;
  }

  // Programmatically activate a resource and update UI state.
  function setActiveResource(resourceId) {
    const nodeId = String(resourceId);
    if (!graph.hasNode(nodeId)) return false;
    activeNodeId = nodeId;
    updateDetailPanel(nodeId);
    if (suppressInitialFocus) {
      clearFocusSets();
      suppressInitialFocus = false;
    } else {
      updateFocusSets();
    }
    sigma.refresh();
    updatePathHighlight();
    dispatchActiveResourceChange(nodeId);
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

  // Emit an event when the active resource changes.
  function dispatchActiveResourceChange(nodeId) {
    const resourceId = graph.getNodeAttribute(nodeId, "resourceId");
    const event = new CustomEvent("resource:active-change", {
      detail: {
        id: resourceId,
        nodeId: nodeId,
        attributes: graph.getNodeAttributes(nodeId),
        path: getActivePath(),
        pathNodes: getActivePathNodes()
      }
    });
    window.dispatchEvent(event);
    if (typeof window.onResourceActiveChange === "function") {
      window.onResourceActiveChange(resourceId, graph.getNodeAttributes(nodeId), getActivePath());
    }
  }

  // Return rich info for a node, including connected edges.
  function getNodeInfo(resourceId) {
    const nodeId = String(resourceId);
    if (!graph.hasNode(nodeId)) return null;
    const attrs = graph.getNodeAttributes(nodeId);
    const inEdges = [];
    const outEdges = [];
    graph.forEachInEdge(nodeId, (edgeId, edgeAttrs, source, target, sourceAttrs, targetAttrs) => {
      inEdges.push({
        id: edgeId,
        from: source,
        to: target,
        attributes: edgeAttrs
      });
    });
    graph.forEachOutEdge(
      nodeId,
      (edgeId, edgeAttrs, source, target, sourceAttrs, targetAttrs) => {
        outEdges.push({
          id: edgeId,
          from: source,
          to: target,
          attributes: edgeAttrs
        });
      }
    );
    return {
      id: nodeId,
      attributes: attrs,
      incomingEdges: inEdges,
      outgoingEdges: outEdges
    };
  }

  // Return rich info for an edge and its endpoints.
  function getEdgeInfo(edgeId) {
    const edgeKey = Number(edgeId);
    if (!Number.isFinite(edgeKey)) return null;
    if (!graph.hasEdge(edgeKey)) return null;
    const attrs = graph.getEdgeAttributes(edgeKey);
    const source = graph.source(edgeKey);
    const target = graph.target(edgeKey);
    return {
      id: edgeKey,
      from: source,
      to: target,
      attributes: attrs
    };
  }

  // Reload CSS-driven colors and refresh the renderer.
  function refreshColors() {
    COLORS = resolveColors();
    if (sigma) {
      sigma.setSettings({
        defaultNodeColor: COLORS.nodeDefault,
        defaultEdgeColor: COLORS.edgeDefault
      });
      sigma.refresh();
    }
  }

  function setDefaultUseWorker(enabled) {
    defaultUseWorker = Boolean(enabled);
  }

  function setPathWeights({ outgoing, incoming }) {
    if (Number.isFinite(outgoing)) {
      pathWeights.outgoing = outgoing;
    }
    if (Number.isFinite(incoming)) {
      pathWeights.incoming = incoming;
    }
    updatePathHighlight();
    updateHoverPathHighlight();
    updateFocusSets();
    if (sigma) sigma.refresh();
  }

  ensureSigma();

  if (resetButton) {
    resetButton.addEventListener("click", () => resetGraph());
  }
  if (stepBackButton) {
    stepBackButton.addEventListener("click", () => stepBack());
  }
  if (zoomInButton) {
    zoomInButton.addEventListener("click", () => zoomBy(1.2));
  }
  if (zoomOutButton) {
    zoomOutButton.addEventListener("click", () => zoomBy(0.8));
  }
  if (togglePathOnlyButton) {
    togglePathOnlyButton.addEventListener("click", () => {
      setPathOnlyMode(!pathOnlyMode);
      togglePathOnlyButton.classList.toggle("is-active", pathOnlyMode);
    });
    togglePathOnlyButton.classList.toggle("is-active", pathOnlyMode);
  }

  // Adjust the camera zoom with sensible fallbacks.
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
    stepBack,
    setCategoryVisibility,
    hideCategory,
    showCategory,
    getHiddenCategories,
    setHiddenCategories,
    setPredicateVisibility,
    hidePredicate,
    showPredicate,
    getHiddenPredicates,
    setHiddenPredicates,
    markEdgesLoaded,
    markEdgesLoading,
    setEdgesLoadingTimeout,
    runLayout: runForceAtlas2,
    setActiveResource,
    setStartNode,
    setPathOnlyMode,
    getNodeInfo,
    getEdgeInfo,
    refreshColors,
    setDefaultUseWorker,
    setPathWeights,
    fitCamera,
    refresh() {
      if (sigma) sigma.refresh();
    }
  };
})();
