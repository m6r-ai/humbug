#!/usr/bin/env python3
"""
gantt.py
--------
Reads a planner JSON file and writes a self-contained HTML Gantt chart with
drill-down, expand/collapse, milestones, tooltips, zoom levels, critical-path
highlighting, and status colouring.

Usage:
    python gantt.py project.json
    python gantt.py project.json output.html   # custom output path
"""

import json
import sys
import os
from pathlib import Path
from typing import Any


def build_tree(project: dict) -> dict:
    """
    Convert the flat planner task list into a nested tree compatible with the
    Gantt renderer.

    The renderer expects a root node with a ``children`` list.  Each node has:

    ``id``              planner task id
    ``name``            display name
    ``start``           ISO date string or None
    ``finish``          ISO date string or None
    ``status``          "not-started" | "in-progress" | "complete"
    ``progress``        float 0-1
    ``is_milestone``    bool
    ``is_summary``      bool
    ``on_critical``     bool
    ``slack_days``      float or None
    ``owner``           str or None
    ``team``            str or None
    ``wbs``             str or None
    ``duration_days``   float or None
    ``dep_ids``         list of predecessor task ids
    ``children``        list of child nodes (recursive)
    """
    tasks_by_id: dict[str, dict] = {}
    for task in project.get("tasks", []):
        tid = task["id"]
        critical_ids = set(
            (project.get("calculated") or {}).get("critical-path") or []
        )

        start = task.get("start-date")
        finish = task.get("end-date")

        # Prefer CPM-computed dates when available
        schedule = project.get("schedule") or {}
        project_start = schedule.get("project-start-datetime") or schedule.get("start-date")
        if project_start and task.get("earliest-start") is not None:
            # Dates may have been computed; use start-date/end-date as they are
            # populated by the importer from the CPM offsets.
            pass

        node = {
            "id": tid,
            "name": task.get("name", "(unnamed)"),
            "start": start,
            "finish": finish,
            "status": task.get("status", "not-started"),
            "progress": task.get("progress", 0.0),
            "is_milestone": bool(task.get("is-milestone", task.get("type") == "milestone")),
            "is_summary": bool(task.get("is-summary", task.get("type") == "summary")),
            "on_critical": tid in critical_ids,
            "slack_days": task.get("slack-days"),
            "owner": task.get("owner"),
            "team": task.get("team"),
            "wbs": task.get("wbs"),
            "description": task.get("description"),
            "duration_days": task.get("duration-days"),
            "dep_ids": [],
            "children": [],
        }
        tasks_by_id[tid] = node

    # Wire up predecessor ids from the dependency list
    for dep in project.get("dependencies", []):
        to_id = dep.get("to-task")
        from_id = dep.get("from-task")
        if to_id in tasks_by_id and from_id in tasks_by_id:
            tasks_by_id[to_id]["dep_ids"].append(from_id)

    # Build hierarchy using parent-id / children fields
    roots: list[dict] = []
    for task in project.get("tasks", []):
        tid = task["id"]
        parent_id = task.get("parent-id")
        if parent_id and parent_id in tasks_by_id:
            tasks_by_id[parent_id]["children"].append(tasks_by_id[tid])
        else:
            roots.append(tasks_by_id[tid])

    # Also incorporate standalone milestones that are not already in the task list
    task_ids = {t["id"] for t in project.get("tasks", [])}
    for ms in project.get("milestones", []):
        mid = ms.get("id", "")
        if mid in task_ids:
            continue
        node = {
            "id": mid,
            "name": ms.get("name", "(unnamed)"),
            "start": ms.get("target-date") or ms.get("actual-date"),
            "finish": ms.get("target-date") or ms.get("actual-date"),
            "status": ms.get("status", "not-started"),
            "progress": 1.0 if ms.get("status") == "complete" else 0.0,
            "is_milestone": True,
            "is_summary": False,
            "on_critical": False,
            "slack_days": None,
            "owner": None,
            "team": None,
            "wbs": None,
            "duration_days": 0.0,
            "dep_ids": [],
            "children": [],
        }
        roots.append(node)

    root = {
        "id": "__root__",
        "name": project.get("name", "Project"),
        "start": (project.get("schedule") or {}).get("start-date"),
        "finish": (project.get("schedule") or {}).get("finish-date"),
        "status": "not-started",
        "progress": 0.0,
        "is_milestone": False,
        "is_summary": True,
        "on_critical": False,
        "slack_days": None,
        "owner": None,
        "team": None,
        "wbs": None,
        "duration_days": None,
        "dep_ids": [],
        "children": roots,
    }
    return root


def assign_ids(node: dict, counter: list) -> None:
    """Assign a unique numeric ``_id`` to every node (in-place)."""
    node["_id"] = counter[0]
    counter[0] += 1
    for child in node.get("children", []):
        assign_ids(child, counter)


HTML_TEMPLATE = r"""<!DOCTYPE html>
<html lang="en">
<head>
<meta charset="UTF-8">
<meta name="viewport" content="width=device-width, initial-scale=1.0">
<title>Project Gantt Chart</title>
<link rel="preconnect" href="https://fonts.googleapis.com">
<link rel="preconnect" href="https://fonts.gstatic.com" crossorigin>
<link href="https://fonts.googleapis.com/css2?family=DM+Sans:wght@300;400;500&family=DM+Mono:wght@400;500&display=swap" rel="stylesheet">
<style>
*, *::before, *::after { box-sizing: border-box; margin: 0; padding: 0; }
:root {
  --sans: 'DM Sans', system-ui, sans-serif;
  --mono: 'DM Mono', monospace;
  --bg:          #0f1117;
  --surface:     #161b27;
  --surface2:    #1d2436;
  --border:      rgba(255,255,255,.08);
  --border2:     rgba(255,255,255,.14);
  --text:        #e8eaf2;
  --text-muted:  #7a8299;
  --text-dim:    #4a5068;
  --blue:        #4f8ef7;
  --blue-dim:    #1e3a6e;
  --green:       #3ecf8e;
  --green-dim:   #0d3d26;
  --amber:       #f5a623;
  --amber-dim:   #4a3000;
  --red:         #f76f6f;
  --red-dim:     #4a0000;
  --slate:       #5a6480;
  --slate-dim:   #1e2335;
  --critical:    #e85555;
  --critical-dim:#3a0a0a;
  --row-h: 38px;
  --bar-h: 20px;
  --label-w: 300px;
  --header-h: 52px;
  --radius: 6px;
}
html, body { height: 100%; }
body { font-family: var(--sans); background: var(--bg); color: var(--text); font-size: 13px; line-height: 1.5; }

#app { display: flex; flex-direction: column; height: 100vh; overflow: hidden; }

#topbar {
  display: flex; align-items: center; gap: 12px;
  padding: 10px 18px;
  background: var(--surface);
  border-bottom: 1px solid var(--border);
  flex-shrink: 0; flex-wrap: wrap;
}
#topbar h1 { font-size: 15px; font-weight: 500; color: var(--text); letter-spacing:.01em; flex-shrink:0; }
.sep { width:1px; height:20px; background:var(--border2); flex-shrink:0; }

#breadcrumb {
  display:flex; align-items:center; gap:4px;
  font-size:12px; color:var(--text-muted);
  flex-wrap:wrap; flex:1;
}
#breadcrumb .bc-item { cursor:pointer; color:var(--blue); padding:2px 4px; border-radius:4px; transition:background .1s; }
#breadcrumb .bc-item:hover { background:var(--blue-dim); }
#breadcrumb .bc-crumb { color:var(--text-dim); font-size:11px; }

.ctrl-group { display:flex; align-items:center; gap:6px; }
label.ctrl-label { font-size:11px; color:var(--text-muted); white-space:nowrap; }

select, button {
  font-family:var(--sans); font-size:12px;
  background:var(--surface2); color:var(--text);
  border:1px solid var(--border2); border-radius:var(--radius);
  padding:5px 10px; cursor:pointer;
  transition:border-color .15s, background .15s;
}
select:hover, button:hover { border-color:var(--blue); }
button:active { transform:scale(.97); }

#gantt-wrap { flex:1; overflow:hidden; display:flex; flex-direction:column; }
#gantt-scroll { flex:1; overflow:auto; position:relative; }
.gantt-inner { min-width:100%; position:relative; }

.g-header {
  display:flex; position:sticky; top:0; z-index:20;
  height:var(--header-h);
  background:var(--surface);
  border-bottom:1px solid var(--border2);
}
.g-header-label {
  width:var(--label-w); min-width:var(--label-w);
  border-right:1px solid var(--border2);
  display:flex; align-items:flex-end;
  padding:6px 12px;
  font-size:11px; font-weight:500; color:var(--text-muted);
  letter-spacing:.05em; text-transform:uppercase;
  position:sticky; left:0; background:var(--surface); z-index:21;
}
.g-header-tl { flex:1; position:relative; overflow:hidden; }

.tick-label { position:absolute; bottom:7px; font-size:11px; color:var(--text-muted); white-space:nowrap; pointer-events:none; font-family:var(--mono); }
.tick-line  { position:absolute; top:0; bottom:0; width:1px; pointer-events:none; }
.tick-line.major { background:var(--border2); }
.tick-line.minor { background:var(--border); }
.today-line { position:absolute; top:0; bottom:0; width:1.5px; background:#f5a62366; pointer-events:none; z-index:3; }
.today-dot  { position:absolute; top:0; width:7px; height:7px; border-radius:50%; background:var(--amber); margin-left:-3px; }

.g-row { display:flex; height:var(--row-h); border-bottom:1px solid var(--border); transition:background .1s; }
.g-row:hover { background:rgba(79,142,247,.04); }
.g-row.summary-row { background:rgba(255,255,255,.015); }
.g-row.critical-row { background:rgba(232,85,85,.04); }

.g-row-label {
  width:var(--label-w); min-width:var(--label-w);
  display:flex; align-items:center;
  padding:0 8px 0 6px;
  border-right:1px solid var(--border);
  position:sticky; left:0;
  background:var(--bg); z-index:5;
  gap:3px; overflow:hidden;
}
.g-row.summary-row .g-row-label { background:#13161f; }
.g-row.critical-row .g-row-label { background:#130e0e; }
.g-row:hover .g-row-label { background:#12182c; }
.g-row.critical-row:hover .g-row-label { background:#1a0f0f; }

.expand-btn {
  width:16px; height:16px; flex-shrink:0;
  border:1px solid var(--border2); border-radius:4px;
  background:var(--surface2); color:var(--text-muted);
  font-size:10px; display:flex; align-items:center; justify-content:center;
  cursor:pointer; user-select:none; transition:border-color .1s, color .1s;
}
.expand-btn:hover { border-color:var(--blue); color:var(--blue); }
.placeholder-btn  { width:16px; flex-shrink:0; }

.task-name { font-size:12px; white-space:nowrap; overflow:hidden; text-overflow:ellipsis; color:var(--text); }
.task-name.is-summary   { font-weight:500; color:#c5d0f0; }
.task-name.is-milestone { color:var(--amber); }
.task-name.is-critical  { color:#f0a0a0; }

.g-row-tl { flex:1; position:relative; overflow:hidden; }

.bar {
  position:absolute; top:50%; transform:translateY(-50%);
  height:var(--bar-h); border-radius:var(--radius);
  display:flex; align-items:center; overflow:hidden;
  padding:0 6px; font-size:11px; color:#fff;
  font-family:var(--mono); white-space:nowrap;
  min-width:4px; cursor:default; transition:filter .1s;
}
.bar:hover { filter:brightness(1.2); }
.bar.clickable { cursor:pointer; }
.bar.summary     { background:var(--blue-dim);     border:1px solid var(--blue);     height:16px; border-radius:3px; }
.bar.complete    { background:var(--green-dim);    border:1px solid var(--green); }
.bar.in-progress { background:var(--amber-dim);    border:1px solid var(--amber); }
.bar.not-started { background:var(--slate-dim);    border:1px solid var(--slate); }
.bar.critical    { background:var(--critical-dim); border:1px solid var(--critical); }
.bar.summary::before { content:''; position:absolute; left:0; top:0; bottom:0; width:3px; background:var(--blue); border-radius:3px 0 0 3px; }
.bar-progress { position:absolute; left:0; top:0; bottom:0; border-radius:inherit; opacity:.35; }
.complete    .bar-progress { background:var(--green); }
.in-progress .bar-progress { background:var(--amber); }
.not-started .bar-progress { background:var(--slate); }
.critical    .bar-progress { background:var(--critical); }

.milestone {
  position:absolute; top:50%; transform:translateY(-50%) rotate(45deg);
  width:13px; height:13px; background:var(--amber); border:2px solid #8b5e00;
  cursor:default; z-index:4; transition:filter .1s;
}
.milestone:hover { filter:brightness(1.3); }

#tooltip {
  position:fixed; z-index:9999;
  background:var(--surface2); border:1px solid var(--border2);
  border-radius:var(--radius); padding:10px 14px;
  font-size:12px; pointer-events:none; display:none;
  max-width:300px; line-height:1.6; color:var(--text);
  box-shadow:0 8px 24px rgba(0,0,0,.5);
}
#tooltip .tt-title { font-weight:500; font-size:13px; margin-bottom:5px; }
#tooltip .tt-row   { color:var(--text-muted); }
#tooltip .tt-row span { color:var(--text); font-family:var(--mono); font-size:11px; }
.badge { display:inline-block; padding:1px 7px; border-radius:3px; font-size:11px; font-weight:500; font-family:var(--mono); }
.badge.complete    { background:var(--green-dim);    color:var(--green);   border:1px solid #1f7050; }
.badge.in-progress { background:var(--amber-dim);    color:var(--amber);   border:1px solid #7a5000; }
.badge.not-started { background:var(--slate-dim);    color:#8a96b0;        border:1px solid #3a4060; }
.badge.milestone-b { background:#4a3000;             color:var(--amber);   border:1px solid #7a5000; }
.badge.critical-b  { background:var(--critical-dim); color:var(--critical);border:1px solid #7a2020; }

#legend {
  display:flex; gap:18px; flex-wrap:wrap;
  padding:8px 18px; border-top:1px solid var(--border);
  background:var(--surface); flex-shrink:0;
}
.lg-item   { display:flex; align-items:center; gap:6px; font-size:11px; color:var(--text-muted); }
.lg-swatch { width:12px; height:12px; border-radius:3px; }
.lg-diamond{ width:9px; height:9px; transform:rotate(45deg); background:var(--amber); }
.lg-summary{ width:12px; height:10px; background:var(--blue-dim); border:1px solid var(--blue); border-radius:2px; }
.empty     { padding:3rem; text-align:center; color:var(--text-muted); }
</style>
</head>
<body>
<div id="app">
  <div id="topbar">
    <h1>&#9783; Project Gantt</h1>
    <div class="sep"></div>
    <div id="breadcrumb"></div>
    <div class="ctrl-group">
      <label class="ctrl-label">Zoom</label>
      <select id="zoom-sel">
        <option value="week">Weekly</option>
        <option value="month" selected>Monthly</option>
        <option value="quarter">Quarterly</option>
      </select>
    </div>
    <div class="ctrl-group">
      <button id="btn-expand">Expand all</button>
      <button id="btn-collapse">Collapse all</button>
      <button id="btn-reset">&#8617; Root</button>
    </div>
  </div>

  <div id="gantt-wrap">
    <div id="gantt-scroll">
      <div class="gantt-inner" id="gantt-inner">
        <div class="g-header">
          <div class="g-header-label">Task</div>
          <div class="g-header-tl" id="hdr-tl"></div>
        </div>
        <div id="gantt-rows"></div>
      </div>
    </div>
  </div>

  <div id="legend">
    <div class="lg-item"><div class="lg-summary"></div> Summary</div>
    <div class="lg-item"><div class="lg-swatch" style="background:var(--green-dim);border:1px solid var(--green)"></div> Complete</div>
    <div class="lg-item"><div class="lg-swatch" style="background:var(--amber-dim);border:1px solid var(--amber)"></div> In progress</div>
    <div class="lg-item"><div class="lg-swatch" style="background:var(--slate-dim);border:1px solid var(--slate)"></div> Not started</div>
    <div class="lg-item"><div class="lg-swatch" style="background:var(--critical-dim);border:1px solid var(--critical)"></div> Critical path</div>
    <div class="lg-item"><div class="lg-diamond"></div> Milestone</div>
  </div>
</div>

<div id="tooltip"></div>

<script>
const PROJECT = __PROJECT_DATA__;

let currentNode = PROJECT;
let navStack    = [];
let expanded    = {};
let zoom        = 'month';

const LABEL_W = 300;

function parseDate(s) {
  if (!s) return null;
  // Accept "YYYY-MM-DD" and full ISO strings
  const d = new Date(s.length === 10 ? s + 'T00:00:00' : s);
  return isNaN(d) ? null : d;
}
function addDays(d, n) { const r = new Date(d); r.setDate(r.getDate() + n); return r; }

function allDates(node) {
  const out = [];
  function walk(n) {
    if (parseDate(n.start))  out.push(parseDate(n.start));
    if (parseDate(n.finish)) out.push(parseDate(n.finish));
    (n.children || []).forEach(walk);
  }
  walk(node);
  return out;
}

function flatVisible(node) {
  const rows = [];
  function walk(n, depth) {
    if (!n.children) return;
    n.children.forEach(child => {
      const hasCh = !!(child.children && child.children.length);
      rows.push({ node: child, depth, hasCh });
      if (hasCh && expanded[child._id]) walk(child, depth + 1);
    });
  }
  walk(node, 0);
  return rows;
}

function setAllExpand(node, val) {
  if (node.children && node.children.length) {
    expanded[node._id] = val;
    node.children.forEach(c => setAllExpand(c, val));
  }
}

function statusClass(s) {
  if (!s) return 'not-started';
  const l = s.toLowerCase();
  if (l === 'complete' || l === 'completed') return 'complete';
  if (l.includes('progress')) return 'in-progress';
  return 'not-started';
}
function statusLabel(s) {
  if (!s) return 'Not started';
  return s.charAt(0).toUpperCase() + s.slice(1).replace(/-/g, ' ');
}

function buildTicks(minD, maxD) {
  const ticks = []; let cur;
  if (zoom === 'week') {
    cur = new Date(minD); cur.setDate(cur.getDate() - cur.getDay());
    while (cur <= maxD) {
      ticks.push({ date: new Date(cur), label: 'W' + getWeek(cur), major: cur.getDate() <= 7 });
      cur.setDate(cur.getDate() + 7);
    }
  } else if (zoom === 'month') {
    cur = new Date(minD.getFullYear(), minD.getMonth(), 1);
    while (cur <= maxD) {
      ticks.push({ date: new Date(cur), label: cur.toLocaleString('default', { month: 'short', year: '2-digit' }), major: cur.getMonth() === 0 });
      cur.setMonth(cur.getMonth() + 1);
    }
  } else {
    cur = new Date(minD.getFullYear(), Math.floor(minD.getMonth() / 3) * 3, 1);
    while (cur <= maxD) {
      const q = Math.floor(cur.getMonth() / 3) + 1;
      ticks.push({ date: new Date(cur), label: `Q${q} ${cur.getFullYear()}`, major: q === 1 });
      cur.setMonth(cur.getMonth() + 3);
    }
  }
  return ticks;
}
function getWeek(d) {
  const j = new Date(d.getFullYear(), 0, 1);
  return Math.ceil(((d - j) / 86400000 + j.getDay() + 1) / 7);
}
function dateX(d, minD, tlW, totalDays) {
  return ((d - minD) / 86400000) / totalDays * tlW;
}

function render() {
  renderBreadcrumb();
  const rows = flatVisible(currentNode);
  const rowsEl = document.getElementById('gantt-rows');
  if (!rows.length) { rowsEl.innerHTML = '<div class="empty">No tasks to display.</div>'; return; }
  const dates = allDates(currentNode);
  if (!dates.length) { rowsEl.innerHTML = '<div class="empty">No date data found.</div>'; return; }

  const minD = addDays(new Date(Math.min(...dates)), -7);
  const maxD = addDays(new Date(Math.max(...dates)), 14);
  const totalDays = (maxD - minD) / 86400000;

  const scrollEl = document.getElementById('gantt-scroll');
  const tlW = Math.max(scrollEl.offsetWidth - LABEL_W - 2, 500);
  document.getElementById('gantt-inner').style.minWidth = (LABEL_W + tlW) + 'px';

  const ticks = buildTicks(minD, maxD);
  const today = new Date();

  /* header */
  const hdrTl = document.getElementById('hdr-tl');
  hdrTl.style.width = tlW + 'px';
  let hHtml = '';
  ticks.forEach(t => {
    const x = dateX(t.date, minD, tlW, totalDays);
    hHtml += `<div class="tick-line ${t.major ? 'major' : 'minor'}" style="left:${x}px"></div>`;
    hHtml += `<div class="tick-label" style="left:${x + 3}px">${t.label}</div>`;
  });
  if (today >= minD && today <= maxD) {
    const tx = dateX(today, minD, tlW, totalDays);
    hHtml += `<div class="today-line" style="left:${tx}px"><div class="today-dot"></div></div>`;
  }
  hdrTl.innerHTML = hHtml;

  /* rows */
  let rHtml = '';
  rows.forEach(({ node, depth, hasCh }) => {
    const isMilestone = node.is_milestone;
    const isSummary   = hasCh || node.is_summary;
    const isCritical  = node.on_critical && !isSummary;
    const sc = statusClass(node.status);
    const start  = parseDate(node.start);
    const finish = parseDate(node.finish);
    const indentPx = depth * 18 + 6;
    const exp = expanded[node._id];
    const pct = Math.round((node.progress || 0) * 100);

    let bHtml = '';
    ticks.forEach(t => {
      const x = dateX(t.date, minD, tlW, totalDays);
      bHtml += `<div class="tick-line ${t.major ? 'major' : 'minor'}" style="left:${x}px"></div>`;
    });
    if (today >= minD && today <= maxD) {
      const tx = dateX(today, minD, tlW, totalDays);
      bHtml += `<div class="today-line" style="left:${tx}px"></div>`;
    }

    if (isMilestone && start) {
      const mx = dateX(start, minD, tlW, totalDays);
      bHtml += `<div class="milestone" style="left:${mx - 6}px" ${encN(node)}></div>`;
    } else if (start && finish) {
      const x1 = dateX(start, minD, tlW, totalDays);
      const x2 = dateX(finish, minD, tlW, totalDays);
      const bw = Math.max(x2 - x1, 4);
      const bc = isSummary ? 'summary' : (isCritical ? 'critical' : sc);
      const click = isSummary ? 'clickable' : '';
      bHtml += `<div class="bar ${bc} ${click}" style="left:${x1}px;width:${bw}px" ${encN(node)} ${isSummary ? `onclick="drillDown(${node._id})"` : ''}>`;
      if (!isSummary) bHtml += `<div class="bar-progress" style="width:${pct}%"></div>`;
      if (bw > 40) {
        bHtml += `<span style="position:relative;z-index:1;overflow:hidden;text-overflow:ellipsis;max-width:${bw - 12}px">${esc(node.name)}</span>`;
      }
      bHtml += `</div>`;
    }

    const sumCls      = isSummary  ? 'summary-row'  : '';
    const critCls     = isCritical ? 'critical-row' : '';
    const nameSumCls  = isSummary  ? 'is-summary'   : '';
    const nameMilCls  = isMilestone ? 'is-milestone' : '';
    const nameCritCls = isCritical  ? 'is-critical'  : '';

    rHtml += `<div class="g-row ${sumCls} ${critCls}">
      <div class="g-row-label" style="padding-left:${indentPx}px">
        ${hasCh
          ? `<div class="expand-btn" onclick="toggleExpand(${node._id})">${exp ? '&minus;' : '+'}</div>`
          : `<div class="placeholder-btn"></div>`}
        <span class="task-name ${nameSumCls} ${nameMilCls} ${nameCritCls}"
          title="${esc(node.name)}"
          ${isSummary ? `style="cursor:pointer" onclick="drillDown(${node._id})"` : ''}>
          ${esc(node.name || '(unnamed)')}
        </span>
      </div>
      <div class="g-row-tl" style="width:${tlW}px">${bHtml}</div>
    </div>`;
  });
  rowsEl.innerHTML = rHtml;
  rowsEl.querySelectorAll('[data-node]').forEach(el => {
    el.addEventListener('mouseenter', showTip);
    el.addEventListener('mousemove',  moveTip);
    el.addEventListener('mouseleave', hideTip);
  });
}

/* navigation */
const nodeMap = {};
function buildMap(n) { nodeMap[n._id] = n; (n.children || []).forEach(buildMap); }
buildMap(PROJECT);

function drillDown(id) {
  const n = nodeMap[id];
  if (!n || !n.children || !n.children.length) return;
  navStack.push({ node: currentNode, label: currentNode.name || 'Root' });
  currentNode = n;
  render();
}
document.getElementById('btn-reset').addEventListener('click', () => { currentNode = PROJECT; navStack = []; render(); });
function toggleExpand(id) { expanded[id] = !expanded[id]; render(); }
document.getElementById('btn-expand').addEventListener('click',   () => { setAllExpand(currentNode, true);  render(); });
document.getElementById('btn-collapse').addEventListener('click', () => { setAllExpand(currentNode, false); render(); });
document.getElementById('zoom-sel').addEventListener('change', e => { zoom = e.target.value; render(); });

function renderBreadcrumb() {
  const bc = document.getElementById('breadcrumb'); let h = '';
  navStack.forEach((item, i) => {
    h += `<span class="bc-item" onclick="navTo(${i})">${esc(item.label)}</span><span class="bc-crumb">›</span>`;
  });
  h += `<span style="color:var(--text)">${esc(currentNode.name || 'Root')}</span>`;
  bc.innerHTML = h;
}
function navTo(idx) { currentNode = navStack[idx].node; navStack = navStack.slice(0, idx); render(); }

function encN(n) {
  const payload = {
    name: n.name, start: n.start, finish: n.finish,
    status: n.status, progress: n.progress,
    is_milestone: n.is_milestone, is_summary: n.is_summary,
    on_critical: n.on_critical, slack_days: n.slack_days,
    owner: n.owner, team: n.team, wbs: n.wbs, duration_days: n.duration_days,
    description: n.description,
    dep_ids: n.dep_ids
  };
  return `data-node="${encodeURIComponent(JSON.stringify(payload))}"`;
}

function showTip(e) {
  const raw = e.currentTarget.dataset.node; if (!raw) return;
  const n = JSON.parse(decodeURIComponent(raw));
  const sc = statusClass(n.status);
  const isMil = n.is_milestone;
  const isCrit = n.on_critical;
  const pct = Math.round((n.progress || 0) * 100);
  const tt = document.getElementById('tooltip');
  let html = `<div class="tt-title">${esc(n.name || '')}</div>`;
  if (n.description) html += `<div class="tt-row" style="margin-bottom:4px;font-size:11px;color:var(--text-muted)">${esc(n.description)}</div>`;
  if (n.owner) html += `<div class="tt-row">Owner &nbsp;<span>${esc(n.owner)}</span>${n.team ? ` <span style="color:var(--text-dim)">/ ${esc(n.team)}</span>` : ''}</div>`;
  if (n.wbs)   html += `<div class="tt-row">WBS &nbsp;&nbsp;&nbsp;<span>${esc(n.wbs)}</span></div>`;
  if (n.start) html += `<div class="tt-row">Start &nbsp;&nbsp;<span>${n.start}</span></div>`;
  if (n.finish)html += `<div class="tt-row">Finish &nbsp;<span>${n.finish}</span></div>`;
  if (n.duration_days != null) html += `<div class="tt-row">Duration <span>${n.duration_days}d</span></div>`;
  if (isMil) {
    html += `<div class="tt-row"><span class="badge milestone-b">Milestone</span></div>`;
  } else {
    html += `<div class="tt-row"><span class="badge ${sc}">${statusLabel(n.status)}</span>`;
    if (isCrit) html += ` <span class="badge critical-b">Critical</span>`;
    html += `</div>`;
  }
  if (!isMil && n.progress != null) html += `<div class="tt-row">Progress <span>${pct}%</span></div>`;
  if (n.slack_days != null) html += `<div class="tt-row">Slack &nbsp;&nbsp;&nbsp;<span>${n.slack_days}d</span></div>`;
  if (n.dep_ids && n.dep_ids.length) html += `<div class="tt-row">Depends on <span>${n.dep_ids.join(', ')}</span></div>`;
  tt.innerHTML = html;
  tt.style.display = 'block';
  moveTip(e);
}
function moveTip(e) {
  const tt = document.getElementById('tooltip');
  let x = e.clientX + 14, y = e.clientY + 14;
  if (x + 310 > window.innerWidth)  x = e.clientX - 310;
  if (y + 200 > window.innerHeight) y = e.clientY - 180;
  tt.style.left = x + 'px'; tt.style.top = y + 'px';
}
function hideTip() { document.getElementById('tooltip').style.display = 'none'; }
function esc(s) {
  return String(s || '').replace(/&/g,'&amp;').replace(/</g,'&lt;').replace(/>/g,'&gt;').replace(/"/g,'&quot;');
}

window.addEventListener('resize', render);
render();
</script>
</body>
</html>
"""


def generate_gantt(json_path: str, output_path: str | None = None) -> str:
    """
    Read a planner JSON file and write a self-contained HTML Gantt chart.

    Returns the path of the generated HTML file.
    """
    with open(json_path, "r", encoding="utf-8") as f:
        project = json.load(f)

    root = build_tree(project)
    assign_ids(root, [0])

    js_tree = json.dumps(root, ensure_ascii=False, default=str)
    html = HTML_TEMPLATE.replace("__PROJECT_DATA__", js_tree)

    if output_path is None:
        stem = Path(json_path).stem
        output_path = str(Path(json_path).parent / f"{stem}_gantt.html")

    with open(output_path, "w", encoding="utf-8") as f:
        f.write(html)

    print(f"Gantt chart written to: {output_path}")
    return output_path


if __name__ == "__main__":
    if len(sys.argv) < 2:
        print("Usage: python gantt.py project.json [output.html]")
        sys.exit(1)

    in_json = sys.argv[1]
    out_html = sys.argv[2] if len(sys.argv) > 2 else None

    if not os.path.exists(in_json):
        print(f"Error: file not found — {in_json}")
        sys.exit(1)

    generate_gantt(in_json, out_html)
