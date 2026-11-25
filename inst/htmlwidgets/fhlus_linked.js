/*
 * fhlus_linked: an FHLUS curve (plotly.js) linked to a map (MapLibre GL).
 *
 * Every geography has an index i (its row in the segment table, ordered by
 * rank). The map's GeoJSON features carry only `i` (promoted to the feature
 * id), so hover/pin styling is done with MapLibre feature-state and never
 * re-uploads data. The curve is highlighted with plotly layout shapes,
 * redrawn at most once per animation frame.
 */
(function () {
  'use strict';

  // Keyless basemaps (Esri). Each has a base layer drawn under the polygons
  // and, where available, a labels layer drawn over them.
  var ESRI = 'https://server.arcgisonline.com/ArcGIS/rest/services/';
  var BASEMAPS = {
    light: {
      label: 'Light',
      under: [ESRI + 'Canvas/World_Light_Gray_Base/MapServer/tile/{z}/{y}/{x}'],
      over: [ESRI + 'Canvas/World_Light_Gray_Reference/MapServer/tile/{z}/{y}/{x}'],
      maxzoom: 16,
      attribution: 'Tiles &copy; Esri &mdash; Esri, HERE, Garmin, &copy; OpenStreetMap contributors, and the GIS user community'
    },
    streets: {
      label: 'Streets',
      under: [ESRI + 'World_Street_Map/MapServer/tile/{z}/{y}/{x}'],
      over: null,
      maxzoom: 19,
      attribution: 'Tiles &copy; Esri &mdash; Esri, HERE, Garmin, USGS, &copy; OpenStreetMap contributors, and the GIS User Community'
    },
    aerial: {
      label: 'Aerial',
      under: [ESRI + 'World_Imagery/MapServer/tile/{z}/{y}/{x}'],
      over: [ESRI + 'Reference/World_Boundaries_and_Places/MapServer/tile/{z}/{y}/{x}'],
      maxzoom: 19,
      attribution: 'Imagery &copy; Esri, Maxar, Earthstar Geographics, and the GIS User Community'
    }
  };
  var BASEMAP_ORDER = ['light', 'streets', 'aerial', 'none'];

  function h(tag, cls, parent, html) {
    var e = document.createElement(tag);
    if (cls) e.className = cls;
    if (html !== undefined) e.innerHTML = html;
    if (parent) parent.appendChild(e);
    return e;
  }

  function segmented(parent, label, options, current, onPick) {
    var group = h('div', 'fhl-group', parent);
    h('span', 'fhl-label', group, label);
    var seg = h('div', 'fhl-seg', group);
    seg.setAttribute('role', 'radiogroup');
    seg.setAttribute('aria-label', label);
    var buttons = {};
    options.forEach(function (o) {
      var b = h('button', 'fhl-btn', seg, o.label);
      b.type = 'button';
      b.setAttribute('role', 'radio');
      buttons[o.key] = b;
      b.addEventListener('click', function () { set(o.key); onPick(o.key); });
    });
    function set(key) {
      Object.keys(buttons).forEach(function (k) {
        var on = k === key;
        buttons[k].classList.toggle('fhl-on', on);
        buttons[k].setAttribute('aria-checked', on ? 'true' : 'false');
      });
    }
    set(current);
    return set;
  }

  function build(el, x) {
    var fig = typeof x.fig === 'string' ? JSON.parse(x.fig) : x.fig;
    var gj = typeof x.geojson === 'string' ? JSON.parse(x.geojson) : x.geojson;
    var n = x.ids.length;
    var idIndex = {};
    for (var k = 0; k < n; k++) idIndex[String(x.ids[k])] = k;

    var layerByKey = {};
    x.layers.forEach(function (L) { layerByKey[L.key] = L; });

    // Per-feature fill colors and bounding boxes, keyed by index
    var hasGeom = new Array(n);
    var bbox = new Array(n);
    gj.features.forEach(function (f) {
      var i = f.properties.i;
      hasGeom[i] = true;
      x.layers.forEach(function (L) { f.properties['c_' + L.key] = L.colors[i]; });
      var b = [Infinity, Infinity, -Infinity, -Infinity];
      var polys = f.geometry.type === 'Polygon' ? [f.geometry.coordinates] : f.geometry.coordinates;
      polys.forEach(function (poly) {
        poly[0].forEach(function (c) {
          if (c[0] < b[0]) b[0] = c[0];
          if (c[1] < b[1]) b[1] = c[1];
          if (c[0] > b[2]) b[2] = c[0];
          if (c[1] > b[3]) b[3] = c[1];
        });
      });
      bbox[i] = b;
    });

    var state = {
      hovered: null,
      pinned: null,
      fillBy: layerByKey[x.fill_by] || x.fill_by === 'none' ? x.fill_by : 'rank',
      basemap: x.basemap,
      opacity: x.fill_opacity,
      mapReady: false
    };

    // ---- DOM ----------------------------------------------------------
    el.innerHTML = '';
    el.classList.add('fhl-root');
    var bar = h('div', 'fhl-toolbar', el);
    var body = h('div', 'fhl-body fhl-' + x.layout, el);
    var plotDiv = h('div', 'fhl-plot', body);
    var mapWrap = h('div', 'fhl-mapwrap', body);
    var mapDiv = h('div', 'fhl-map', mapWrap);
    var legend = h('div', 'fhl-legend', mapWrap);
    var tip = h('div', 'fhl-tip', mapWrap);
    var card = h('div', 'fhl-card', mapWrap);
    var cardClose = h('button', 'fhl-card-close', card, '&times;');
    cardClose.type = 'button';
    cardClose.setAttribute('aria-label', 'Unpin');
    var cardBody = h('div', 'fhl-card-body', card);
    if (x.n_unmapped > 0) {
      h('div', 'fhl-note', mapWrap, x.n_unmapped + ' geograph' +
        (x.n_unmapped === 1 ? 'y has' : 'ies have') + ' no polygon');
    }

    var colorOptions = x.layers.map(function (L) { return {key: L.key, label: L.label}; });
    colorOptions.push({key: 'none', label: 'Outlines'});
    segmented(bar, 'Map color', colorOptions, state.fillBy, setFillBy);
    segmented(bar, 'Basemap', BASEMAP_ORDER.map(function (k) {
      return {key: k, label: k === 'none' ? 'None' : BASEMAPS[k].label};
    }), state.basemap, setBasemap);

    var opGroup = h('label', 'fhl-group', bar);
    h('span', 'fhl-label', opGroup, 'Opacity');
    var slider = h('input', 'fhl-range', opGroup);
    slider.type = 'range'; slider.min = 0; slider.max = 100; slider.step = 5;
    slider.value = Math.round(state.opacity * 100);
    slider.addEventListener('input', function () {
      state.opacity = slider.value / 100;
      if (state.mapReady) applyOpacity();
    });

    var resetBtn = h('button', 'fhl-btn fhl-reset', bar, 'Reset view');
    resetBtn.type = 'button';
    resetBtn.addEventListener('click', function () { fitAll(true); });
    h('span', 'fhl-hint', bar, 'Hover to link &middot; click to pin');

    // ---- Plot ---------------------------------------------------------
    var layout = fig.layout || {};
    delete layout.width; delete layout.height;
    layout.autosize = true;
    var config = fig.config || {};
    config.responsive = false;
    // Hover-layer tooltips are sent once (x.tips) and expanded here
    fig.data.forEach(function (tr) {
      if (tr.customdata && !tr.text) {
        var cd = Array.isArray(tr.customdata) ? tr.customdata : [tr.customdata];
        tr.text = cd.map(function (id) { return x.tips[idIndex[String(id)]]; });
      }
    });
    Plotly.newPlot(plotDiv, fig.data, layout, config);

    // Highlights are drawn in an SVG overlay positioned with plotly's axis
    // transforms. Editing layout.shapes would make plotly redraw every trace
    // on each hover, which is slow with thousands of segments.
    var SVGNS = 'http://www.w3.org/2000/svg';
    var overlay = document.createElementNS(SVGNS, 'svg');
    overlay.setAttribute('class', 'fhl-overlay');
    function svg(tag, attrs) {
      var e = document.createElementNS(SVGNS, tag);
      Object.keys(attrs).forEach(function (k) { e.setAttribute(k, attrs[k]); });
      overlay.appendChild(e);
      return e;
    }
    function drawSegment(i, pinned) {
      var fl = plotDiv._fullLayout;
      if (!fl || !fl.xaxis || !fl.yaxis) return;
      var xa = fl.xaxis, ya = fl.yaxis;
      var X = function (v) { return xa._offset + xa.l2p(v); };
      var Y = function (v) { return ya._offset + ya.l2p(v); };
      var px0 = X(x.x0[i]), px1 = X(x.x1[i]);
      var py0 = Y(x.y0[i]), py1 = Y(x.y1[i]);
      // Keep very narrow segments visible: at least 3px of band
      var bw = Math.max(3, px1 - px0), bx = (px0 + px1) / 2 - bw / 2;
      var top = ya._offset, h = ya._length;
      var left = Math.max(bx, xa._offset), right = Math.min(bx + bw, xa._offset + xa._length);
      if (right > left) {
        svg('rect', {x: left, y: top, width: right - left, height: h,
                     fill: pinned ? '#3a3a3a' : '#8a8a8a', 'fill-opacity': pinned ? 0.13 : 0.16});
      }
      svg('line', {x1: px0, y1: py0, x2: px1, y2: py1, stroke: x.color_var,
                   'stroke-width': 7, 'stroke-opacity': 0.5, 'stroke-linecap': 'round'});
      svg('circle', {cx: (px0 + px1) / 2, cy: (py0 + py1) / 2, r: 5.5,
                     fill: 'rgba(255,255,255,0.92)', stroke: pinned ? '#111111' : x.color_var,
                     'stroke-width': 2});
    }

    var plotFrame = null;
    function schedulePlot() {
      if (plotFrame !== null) return;
      plotFrame = requestAnimationFrame(function () {
        plotFrame = null;
        while (overlay.firstChild) overlay.removeChild(overlay.firstChild);
        var fl = plotDiv._fullLayout;
        if (fl) {
          overlay.setAttribute('width', fl.width);
          overlay.setAttribute('height', fl.height);
        }
        if (state.pinned !== null) drawSegment(state.pinned, true);
        if (state.hovered !== null && state.hovered !== state.pinned) drawSegment(state.hovered, false);
      });
    }
    // Zooming, panning and resizing move the axes, so redraw the overlay
    // Sit under plotly's top layer (which holds the hover label), not over it
    var svgBox = plotDiv.querySelector('.svg-container');
    if (svgBox) {
      var tops = svgBox.querySelectorAll(':scope > svg.main-svg');
      svgBox.insertBefore(overlay, tops.length > 1 ? tops[tops.length - 1] : null);
    }
    plotDiv.on('plotly_afterplot', schedulePlot);
    plotDiv.on('plotly_relayout', schedulePlot);

    function idFromEvent(ev) {
      for (var j = 0; j < ev.points.length; j++) {
        var cd = ev.points[j].customdata;
        if (cd !== undefined && cd !== null && idIndex[String(cd)] !== undefined) {
          return idIndex[String(cd)];
        }
      }
      return null;
    }
    plotDiv.on('plotly_hover', function (ev) {
      var i = idFromEvent(ev);
      if (i !== null) setHover(i);
    });
    plotDiv.on('plotly_unhover', function () { setHover(null); });
    plotDiv.on('plotly_click', function (ev) {
      var i = idFromEvent(ev);
      if (i !== null) setPin(state.pinned === i ? null : i, true);
    });

    // ---- Map ----------------------------------------------------------
    var sources = {}, layers = [
      {id: 'fhl-bg', type: 'background', paint: {'background-color': '#f2f1ee'}}
    ];
    Object.keys(BASEMAPS).forEach(function (k) {
      var b = BASEMAPS[k];
      sources['bm-' + k] = {type: 'raster', tiles: b.under, tileSize: 256,
                            maxzoom: b.maxzoom, attribution: b.attribution};
      layers.push({id: 'bm-' + k, type: 'raster', source: 'bm-' + k,
                   layout: {visibility: k === state.basemap ? 'visible' : 'none'}});
    });

    var map = new maplibregl.Map({
      container: mapDiv,
      style: {version: 8, sources: sources, layers: layers},
      bounds: x.bounds,
      fitBoundsOptions: {padding: 24},
      attributionControl: {compact: true},
      dragRotate: false,
      pitchWithRotate: false,
      maxPitch: 0,
      fadeDuration: 0
    });
    map.touchZoomRotate.disableRotation();
    map.keyboard.disableRotation();
    map.addControl(new maplibregl.NavigationControl({showCompass: false}), 'top-right');
    // Until the user pans or zooms, keep the whole area in view when the
    // panel changes size (e.g. when the layout switches to stacked)
    var userMoved = false;
    map.on('movestart', function (e) { if (e.originalEvent) userMoved = true; });

    var HOV = ['boolean', ['feature-state', 'hover'], false];
    var PIN = ['boolean', ['feature-state', 'pin'], false];

    map.on('load', function () {
      map.addSource('fhl', {type: 'geojson', data: gj, promoteId: 'i'});
      map.addLayer({id: 'fhl-fill', type: 'fill', source: 'fhl',
                    paint: {'fill-color': fillColor(), 'fill-antialias': false}});
      map.addLayer({id: 'fhl-edge', type: 'line', source: 'fhl', paint: edgePaint()});
      // Basemap labels sit above the fill so place names stay readable
      Object.keys(BASEMAPS).forEach(function (k) {
        var b = BASEMAPS[k];
        if (!b.over) return;
        map.addSource('bml-' + k, {type: 'raster', tiles: b.over, tileSize: 256,
                                    maxzoom: b.maxzoom});
        map.addLayer({id: 'bml-' + k, type: 'raster', source: 'bml-' + k,
                      layout: {visibility: k === state.basemap ? 'visible' : 'none'}});
      });
      map.addLayer({id: 'fhl-halo', type: 'line', source: 'fhl',
                    layout: {'line-join': 'round'},
                    paint: {'line-color': '#ffffff', 'line-opacity': 0.95,
                            'line-width': ['case', ['any', HOV, PIN], 6, 0]}});
      map.addLayer({id: 'fhl-hl', type: 'line', source: 'fhl',
                    layout: {'line-join': 'round'},
                    paint: {'line-color': ['case', PIN, '#111111', x.color_var],
                            'line-width': ['case', HOV, 3, PIN, 2.5, 0]}});
      state.mapReady = true;
      applyOpacity();
      if (state.hovered !== null) map.setFeatureState({source: 'fhl', id: state.hovered}, {hover: true});
      if (state.pinned !== null) map.setFeatureState({source: 'fhl', id: state.pinned}, {pin: true});
    });

    map.on('mousemove', 'fhl-fill', function (e) {
      if (!e.features.length) return;
      var i = e.features[0].id;
      map.getCanvas().style.cursor = 'pointer';
      setHover(i);
      showTip(i, e.point);
    });
    map.on('mouseleave', 'fhl-fill', function () {
      map.getCanvas().style.cursor = '';
      setHover(null);
      tip.style.display = 'none';
    });
    map.on('click', function (e) {
      var fs = state.mapReady ? map.queryRenderedFeatures(e.point, {layers: ['fhl-fill']}) : [];
      if (fs.length) {
        var i = fs[0].id;
        setPin(state.pinned === i ? null : i, false);
      } else {
        setPin(null, false);
      }
    });

    function fillColor() {
      return state.fillBy === 'none' ? 'rgba(0,0,0,0)' : ['get', 'c_' + state.fillBy];
    }
    function edgePaint() {
      var outline = state.fillBy === 'none';
      var dark = state.basemap === 'aerial';
      return {
        'line-color': outline ? (dark ? '#ffffff' : '#333333') : '#ffffff',
        'line-width': outline ? 1.2 : 0.6,
        'line-opacity': outline ? 0.9 : 0.75
      };
    }
    function applyOpacity() {
      var o = state.fillBy === 'none' ? 0 : state.opacity;
      // Hovered and pinned polygons get a little more ink so they stand out
      map.setPaintProperty('fhl-fill', 'fill-opacity',
        ['case', ['any', HOV, PIN], Math.min(1, o + 0.2), o]);
    }
    function applyEdges() {
      var p = edgePaint();
      Object.keys(p).forEach(function (k) { map.setPaintProperty('fhl-edge', k, p[k]); });
    }

    function setFillBy(key) {
      state.fillBy = key;
      renderLegend();
      if (!state.mapReady) return;
      map.setPaintProperty('fhl-fill', 'fill-color', fillColor());
      applyOpacity();
      applyEdges();
    }
    function setBasemap(key) {
      state.basemap = key;
      if (!state.mapReady) return;
      Object.keys(BASEMAPS).forEach(function (k) {
        var vis = k === key ? 'visible' : 'none';
        map.setLayoutProperty('bm-' + k, 'visibility', vis);
        if (BASEMAPS[k].over) map.setLayoutProperty('bml-' + k, 'visibility', vis);
      });
      applyEdges();
    }

    function fitAll(animate) {
      if (animate) userMoved = false;
      map.fitBounds(x.bounds, {padding: 24, animate: !!animate, duration: 500});
    }
    function ensureVisible(i) {
      if (!hasGeom[i]) return;
      var b = bbox[i], v = map.getBounds();
      var inside = b[0] >= v.getWest() && b[2] <= v.getEast() &&
                   b[1] >= v.getSouth() && b[3] <= v.getNorth();
      if (!inside) {
        map.fitBounds([[b[0], b[1]], [b[2], b[3]]],
                      {padding: 60, maxZoom: Math.max(map.getZoom(), 12), duration: 600});
      }
    }

    // ---- Tooltip, pin card and legend ----------------------------------
    function showTip(i, pt) {
      tip.innerHTML = x.tips[i];
      tip.style.display = 'block';
      var W = mapWrap.clientWidth, H = mapWrap.clientHeight;
      var w = tip.offsetWidth, hh = tip.offsetHeight;
      var left = pt.x + 14, top = pt.y + 14;
      if (left + w > W - 6) left = Math.max(6, pt.x - w - 14);
      if (top + hh > H - 6) top = Math.max(6, H - hh - 6);
      tip.style.transform = 'translate(' + left + 'px,' + top + 'px)';
    }

    function renderCard() {
      if (state.pinned === null) {
        card.style.display = 'none';
        return;
      }
      cardBody.innerHTML = x.tips[state.pinned];
      card.style.display = 'block';
    }
    cardClose.addEventListener('click', function () { setPin(null, false); });

    function renderLegend() {
      var L = layerByKey[state.fillBy];
      if (!L) { legend.style.display = 'none'; return; }
      legend.style.display = 'block';
      var g = L.legend, out = '<div class="fhl-legend-title">' + g.title + '</div>';
      if (g.type === 'gradient') {
        out += '<div class="fhl-legend-bar" style="background:linear-gradient(to right,' +
               g.colors.join(',') + ')"></div>' +
               '<div class="fhl-legend-ends"><span>' + g.labels[0] + '</span><span>' +
               g.labels[1] + '</span></div>';
      } else {
        g.items.forEach(function (it) {
          out += '<div class="fhl-legend-row"><span class="fhl-swatch" style="background:' +
                 it.color + '"></span>' + it.label + '</div>';
        });
      }
      legend.innerHTML = out;
    }
    renderLegend();

    // ---- Linking --------------------------------------------------------
    function setHover(i) {
      if (i === state.hovered) return;
      if (state.mapReady && state.hovered !== null && hasGeom[state.hovered]) {
        map.setFeatureState({source: 'fhl', id: state.hovered}, {hover: false});
      }
      state.hovered = i;
      if (state.mapReady && i !== null && hasGeom[i]) {
        map.setFeatureState({source: 'fhl', id: i}, {hover: true});
      }
      schedulePlot();
      sendShiny('hover', i);
    }

    function setPin(i, fromPlot) {
      if (i === state.pinned) return;
      if (state.mapReady && state.pinned !== null && hasGeom[state.pinned]) {
        map.setFeatureState({source: 'fhl', id: state.pinned}, {pin: false});
      }
      state.pinned = i;
      if (state.mapReady && i !== null && hasGeom[i]) {
        map.setFeatureState({source: 'fhl', id: i}, {pin: true});
        if (fromPlot) ensureVisible(i);
      }
      renderCard();
      schedulePlot();
      sendShiny('selected', i);
    }

    function sendShiny(what, i) {
      if (!(window.HTMLWidgets && HTMLWidgets.shinyMode && window.Shiny && el.id)) return;
      Shiny.setInputValue(el.id + '_' + what, i === null ? null : String(x.ids[i]));
    }

    function onKey(e) {
      if (e.key === 'Escape' && state.pinned !== null) setPin(null, false);
    }
    el.tabIndex = el.tabIndex >= 0 ? el.tabIndex : 0;
    el.addEventListener('keydown', onKey);

    // ---- Layout ---------------------------------------------------------
    var resizeFrame = null;
    function resize() {
      if (resizeFrame !== null) return;
      resizeFrame = requestAnimationFrame(function () {
        resizeFrame = null;
        setNarrow(el.clientWidth < 820);
        Plotly.Plots.resize(plotDiv);
        map.resize();
        if (!userMoved) fitAll(false);
      });
    }
    // Stacked panels are short: hide the plot legend (it would collide with
    // the axis title) and use tighter margins
    var narrow = null, showLegend = layout.showlegend !== false;
    var wideMargin = JSON.parse(JSON.stringify(layout.margin || {}));
    function setNarrow(v) {
      if (v === narrow) return;
      narrow = v;
      body.classList.toggle('fhl-narrow', v);
      var stacked = v || x.layout === 'stacked';
      var upd = {margin: stacked ? {t: 56, r: 16, b: 72, l: 56} : wideMargin};
      if (showLegend) upd.showlegend = !stacked;
      Plotly.relayout(plotDiv, upd);
    }
    setNarrow(el.clientWidth < 820);
    requestAnimationFrame(function () { map.resize(); if (!userMoved) fitAll(false); });
    var ro = window.ResizeObserver ? new ResizeObserver(resize) : null;
    if (ro) ro.observe(el);

    return {
      resize: resize,
      map: map,
      plot: plotDiv,
      highlight: function (id) { setHover(id === null ? null : idIndex[String(id)]); },
      pin: function (id) { setPin(id === null ? null : idIndex[String(id)], true); },
      destroy: function () {
        if (ro) ro.disconnect();
        el.removeEventListener('keydown', onKey);
        map.remove();
        Plotly.purge(plotDiv);
      }
    };
  }

  HTMLWidgets.widget({
    name: 'fhlus_linked',
    type: 'output',
    factory: function (el, width, height) {
      var inst = null;
      return {
        renderValue: function (x) {
          if (inst) inst.destroy();
          inst = build(el, x);
        },
        resize: function () { if (inst) inst.resize(); },
        getInstance: function () { return inst; }
      };
    }
  });
})();
