#!/usr/bin/env python3
"""
gantt.py
--------
Reads a planner JSON file and writes a self-contained HTML Gantt chart with
drill-down, expand/collapse, milestones, tooltips, zoom levels, dependency
arrows, violation highlighting, critical-path overlay, and status colouring.

Usage:
    python gantt.py project.json
    python gantt.py project.json output.html   # custom output path
"""

import json
import sys
import os
from pathlib import Path


# Planner dependency type strings -> short codes used by violation detection
_DEP_TYPE_CODES = {
    "finish-to-start":  "FS",
    "start-to-start":   "SS",
    "finish-to-finish": "FF",
    "start-to-finish":  "SF",
}


def build_tree(project: dict) -> dict:
    """
    Convert the flat planner task list into a nested tree for the Gantt renderer.

    Each node carries:
      id, name, start, finish, status, progress, is_milestone, is_summary,
      on_critical, slack_days, owner, team, wbs, duration_days, description,
      predecessors (list of {id, type, lag_days}), children
    """
    critical_ids: set[str] = set(
        (project.get("calculated") or {}).get("critical-path") or []
    )

    tasks_by_id: dict[str, dict] = {}
    for task in project.get("tasks", []):
        tid = task["id"]
        node: dict = {
            "id": tid,
            "name": task.get("name", "(unnamed)"),
            "description": task.get("description"),
            "start": task.get("start-date"),
            "finish": task.get("end-date"),
            "status": task.get("status", "not-started"),
            "progress": task.get("progress", 0.0),
            "is_milestone": bool(task.get("is-milestone", task.get("type") == "milestone")),
            "is_summary": bool(task.get("is-summary", task.get("type") == "summary")),
            "on_critical": tid in critical_ids,
            "slack_days": task.get("slack-days"),
            "owner": task.get("owner"),
            "team": task.get("team"),
            "wbs": task.get("wbs"),
            "duration_days": task.get("duration-days"),
            "predecessors": [],
            "children": [],
        }
        tasks_by_id[tid] = node

    # Build structured predecessor list from the dependency table
    for dep in project.get("dependencies", []):
        to_id = dep.get("to-task")
        from_id = dep.get("from-task")
        if to_id in tasks_by_id and from_id in tasks_by_id:
            tasks_by_id[to_id]["predecessors"].append({
                "id": from_id,
                "type": _DEP_TYPE_CODES.get(dep.get("type", "finish-to-start"), "FS"),
                "lag_days": dep.get("lag-days", 0.0),
            })

    # Reconstruct parent/child hierarchy
    roots: list[dict] = []
    for task in project.get("tasks", []):
        tid = task["id"]
        parent_id = task.get("parent-id")
        if parent_id and parent_id in tasks_by_id:
            tasks_by_id[parent_id]["children"].append(tasks_by_id[tid])
        else:
            roots.append(tasks_by_id[tid])

    # Fold in standalone milestones not already present as tasks
    task_ids = {t["id"] for t in project.get("tasks", [])}
    for ms in project.get("milestones", []):
        mid = ms.get("id", "")
        if mid in task_ids:
            continue
        node = {
            "id": mid,
            "name": ms.get("name", "(unnamed)"),
            "description": None,
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
            "predecessors": [],
            "children": [],
        }
        roots.append(node)

    return {
        "id": "__root__",
        "name": project.get("name", "Project"),
        "description": None,
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
        "predecessors": [],
        "children": roots,
    }


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
<link href="https://fonts.googleapis.com/css2?family=DM+Sans:wght@300;400;500&family=DM+Mono:wght@400;500&display=swap" rel="stylesheet">
<style>
*,*::before,*::after{box-sizing:border-box;margin:0;padding:0}
:root{
  --sans:'DM Sans',system-ui,sans-serif;
  --mono:'DM Mono',monospace;
  --bg:#0f1117;--surface:#161b27;--surface2:#1d2436;
  --border:rgba(255,255,255,.08);--border2:rgba(255,255,255,.14);
  --text:#e8eaf2;--text-muted:#7a8299;--text-dim:#4a5068;
  --blue:#4f8ef7;--blue-dim:#1e3a6e;
  --green:#3ecf8e;--green-dim:#0d3d26;
  --amber:#f5a623;--amber-dim:#4a3000;
  --red:#f76f6f;--red-dim:#4a1010;--red-border:#8b2020;
  --slate:#5a6480;--slate-dim:#1e2335;
  --purple:#a78bfa;--purple-dim:#2d1f5e;
  --row-h:38px;--bar-h:20px;--label-w:300px;--hdr-h:52px;--radius:6px;
}
html,body{height:100%}
body{font-family:var(--sans);background:var(--bg);color:var(--text);font-size:13px;line-height:1.5}
#app{display:flex;flex-direction:column;height:100vh;overflow:hidden}

#topbar{display:flex;align-items:center;gap:10px;padding:8px 16px;background:var(--surface);border-bottom:1px solid var(--border);flex-shrink:0;flex-wrap:wrap}
#topbar h1{font-size:15px;font-weight:500;flex-shrink:0}
.sep{width:1px;height:20px;background:var(--border2);flex-shrink:0}
#breadcrumb{display:flex;align-items:center;gap:4px;font-size:12px;color:var(--text-muted);flex-wrap:wrap;flex:1}
.bc-item{cursor:pointer;color:var(--blue);padding:2px 4px;border-radius:4px}
.bc-item:hover{background:var(--blue-dim)}
.bc-crumb{color:var(--text-dim);font-size:11px}
.ctrl-grp{display:flex;align-items:center;gap:5px}
label.cl{font-size:11px;color:var(--text-muted);white-space:nowrap}
select,button{font-family:var(--sans);font-size:12px;background:var(--surface2);color:var(--text);border:1px solid var(--border2);border-radius:var(--radius);padding:4px 9px;cursor:pointer;transition:border-color .15s}
select:hover,button:hover{border-color:var(--blue)}
button.active{background:var(--blue-dim);border-color:var(--blue);color:var(--blue)}
button:active{transform:scale(.97)}

#statsbar{display:flex;gap:16px;padding:6px 16px;background:var(--surface);border-bottom:1px solid var(--border);flex-shrink:0;flex-wrap:wrap}
.stat{display:flex;align-items:center;gap:6px;font-size:11px;color:var(--text-muted)}
.stat-dot{width:8px;height:8px;border-radius:50%;flex-shrink:0}
.stat-val{font-weight:500;color:var(--text);font-family:var(--mono)}

#gantt-wrap{flex:1;overflow:hidden;display:flex;flex-direction:column}
#gantt-scroll{flex:1;overflow:auto;position:relative}
.gantt-inner{min-width:100%;position:relative}

.g-header{display:flex;position:sticky;top:0;z-index:20;height:var(--hdr-h);background:var(--surface);border-bottom:1px solid var(--border2);min-width:max-content}
.g-header-label{width:var(--label-w);min-width:var(--label-w);border-right:1px solid var(--border2);display:flex;align-items:flex-end;padding:6px 12px;font-size:11px;font-weight:500;color:var(--text-muted);letter-spacing:.05em;text-transform:uppercase;position:sticky;left:0;background:var(--surface);z-index:21}
.g-header-tl{flex:1;position:relative;overflow:visible;min-width:0}

.tick-label{position:absolute;bottom:7px;font-size:11px;color:var(--text-muted);white-space:nowrap;pointer-events:none;font-family:var(--mono)}
.tick-line{position:absolute;top:0;bottom:0;width:1px;pointer-events:none}
.tick-line.major{background:var(--border2)}
.tick-line.minor{background:var(--border)}
.today-line{position:absolute;top:0;bottom:0;width:1.5px;background:#f5a62355;pointer-events:none;z-index:3}
.today-dot{position:absolute;top:0;width:7px;height:7px;border-radius:50%;background:var(--amber);margin-left:-3px}

.g-row{display:flex;height:var(--row-h);border-bottom:1px solid var(--border);transition:background .1s;position:relative}
.g-row:hover{background:rgba(79,142,247,.04)}
.g-row.summary-row{background:rgba(255,255,255,.015)}
.g-row.violation-row{background:rgba(247,111,111,.06)!important}
.g-row.critical-row{background:rgba(167,139,250,.04)!important}

.g-row-label{width:var(--label-w);min-width:var(--label-w);display:flex;align-items:center;padding:0 8px 0 6px;border-right:1px solid var(--border);position:sticky;left:0;background:var(--bg);z-index:5;gap:3px;overflow:hidden}
.g-row.summary-row .g-row-label{background:#13161f}
.g-row.violation-row .g-row-label{background:#1a0e0e!important}
.g-row.critical-row .g-row-label{background:#130e1f!important}
.g-row:hover .g-row-label{background:#12182c}

.expand-btn{width:16px;height:16px;flex-shrink:0;border:1px solid var(--border2);border-radius:4px;background:var(--surface2);color:var(--text-muted);font-size:10px;display:flex;align-items:center;justify-content:center;cursor:pointer;user-select:none;transition:border-color .1s,color .1s}
.expand-btn:hover{border-color:var(--blue);color:var(--blue)}
.placeholder-btn{width:16px;flex-shrink:0}

.task-name{font-size:12px;white-space:nowrap;overflow:hidden;text-overflow:ellipsis;color:var(--text);flex:1;min-width:0}
.task-name.is-summary{font-weight:500;color:#c5d0f0}
.task-name.is-milestone{color:var(--amber)}
.task-name.is-violation{color:var(--red)!important}
.task-name.is-critical{color:var(--purple)!important}

.violation-icon,.critical-icon{font-size:11px;flex-shrink:0}

.g-row-tl{flex:1;position:relative;overflow:visible;min-width:0}

.bar{position:absolute;top:50%;transform:translateY(-50%);height:var(--bar-h);border-radius:var(--radius);display:flex;align-items:center;overflow:hidden;padding:0 6px;font-size:11px;color:#fff;font-family:var(--mono);white-space:nowrap;min-width:4px;cursor:default;transition:filter .1s,box-shadow .1s}
.bar:hover{filter:brightness(1.2)}
.bar.clickable{cursor:pointer}
.bar.summary{background:var(--blue-dim);border:1px solid var(--blue);height:16px;border-radius:3px}
.bar.complete{background:var(--green-dim);border:1px solid var(--green)}
.bar.in-progress{background:var(--amber-dim);border:1px solid var(--amber)}
.bar.not-started{background:var(--slate-dim);border:1px solid var(--slate)}
.bar.violation{background:var(--red-dim)!important;border:1px solid var(--red)!important;box-shadow:0 0 0 2px rgba(247,111,111,.2)}
.bar.critical-path{box-shadow:0 0 0 2px rgba(167,139,250,.5)!important}
.bar.summary::before{content:'';position:absolute;left:0;top:0;bottom:0;width:3px;background:var(--blue);border-radius:3px 0 0 3px}
.bar-progress{position:absolute;left:0;top:0;bottom:0;border-radius:inherit;opacity:.35}
.complete .bar-progress{background:var(--green)}
.in-progress .bar-progress{background:var(--amber)}
.not-started .bar-progress{background:var(--slate)}
.violation .bar-progress{background:var(--red)}

.milestone{position:absolute;top:50%;transform:translateY(-50%) rotate(45deg);width:13px;height:13px;background:var(--amber);border:2px solid #8b5e00;cursor:default;z-index:4;transition:filter .1s}
.milestone:hover{filter:brightness(1.3)}
.milestone.violation{background:var(--red)!important;border-color:var(--red-border)!important}

#dep-svg{position:absolute;top:0;left:0;pointer-events:none;z-index:8;overflow:visible}
.dep-arrow{stroke:#4f8ef766;stroke-width:1.5;fill:none;marker-end:url(#arrowhead)}
.dep-arrow.violation{stroke:#f76f6f99;stroke-width:2}
.dep-arrow.critical-path-arrow{stroke:#a78bfa99;stroke-width:1.5}

#tooltip{position:fixed;z-index:9999;background:var(--surface2);border:1px solid var(--border2);border-radius:var(--radius);padding:10px 14px;font-size:12px;pointer-events:none;display:none;max-width:300px;line-height:1.6;color:var(--text);box-shadow:0 8px 24px rgba(0,0,0,.5)}
#tooltip .tt-title{font-weight:500;font-size:13px;margin-bottom:5px}
#tooltip .tt-desc{font-size:11px;color:var(--text-muted);margin-bottom:6px;padding-bottom:6px;border-bottom:1px solid var(--border2);line-height:1.5;max-width:260px;white-space:normal}
#tooltip .tt-row{color:var(--text-muted)}
#tooltip .tt-row span{color:var(--text);font-family:var(--mono);font-size:11px}
#tooltip .tt-section{margin-top:6px;padding-top:6px;border-top:1px solid var(--border2)}
.badge{display:inline-block;padding:1px 7px;border-radius:3px;font-size:11px;font-weight:500;font-family:var(--mono)}
.badge.complete{background:var(--green-dim);color:var(--green);border:1px solid #1f7050}
.badge.in-progress{background:var(--amber-dim);color:var(--amber);border:1px solid #7a5000}
.badge.not-started{background:var(--slate-dim);color:#8a96b0;border:1px solid #3a4060}
.badge.milestone-b{background:#4a3000;color:var(--amber);border:1px solid #7a5000}
.badge.violation-b{background:var(--red-dim);color:var(--red);border:1px solid var(--red-border)}
.badge.critical-b{background:var(--purple-dim);color:var(--purple);border:1px solid #5b3ea0}

#legend{display:flex;gap:14px;flex-wrap:wrap;padding:7px 16px;border-top:1px solid var(--border);background:var(--surface);flex-shrink:0}
.lg-item{display:flex;align-items:center;gap:5px;font-size:11px;color:var(--text-muted)}
.lg-sw{width:12px;height:12px;border-radius:3px}
.lg-diamond{width:9px;height:9px;transform:rotate(45deg);background:var(--amber)}
.lg-summary{width:12px;height:10px;background:var(--blue-dim);border:1px solid var(--blue);border-radius:2px}
.empty{padding:3rem;text-align:center;color:var(--text-muted)}
</style>
</head>
<body>
<div id="app">

<div id="topbar">
  <h1>&#9783; Project Gantt</h1>
  <div class="sep"></div>
  <div id="breadcrumb"></div>
  <div class="ctrl-grp">
    <label class="cl">Zoom</label>
    <select id="zoom-sel">
      <option value="week">Weekly</option>
      <option value="month" selected>Monthly</option>
      <option value="quarter">Quarterly</option>
    </select>
  </div>
  <div class="sep"></div>
  <div class="ctrl-grp">
    <button id="btn-expand">Expand all</button>
    <button id="btn-collapse">Collapse all</button>
    <button id="btn-reset">&#8617; Root</button>
  </div>
  <div class="sep"></div>
  <div class="ctrl-grp">
    <button id="btn-arrows" title="Show/hide dependency arrows">Arrows</button>
    <button id="btn-critical" title="Highlight critical path">Critical path</button>
  </div>
</div>

<div id="statsbar">
  <div class="stat"><div class="stat-dot" style="background:var(--text-muted)"></div>Tasks <span class="stat-val" id="st-total">-</span></div>
  <div class="stat"><div class="stat-dot" style="background:var(--green)"></div>Complete <span class="stat-val" id="st-done">-</span></div>
  <div class="stat"><div class="stat-dot" style="background:var(--amber)"></div>In progress <span class="stat-val" id="st-prog">-</span></div>
  <div class="stat"><div class="stat-dot" style="background:var(--slate)"></div>Not started <span class="stat-val" id="st-ns">-</span></div>
  <div class="stat"><div class="stat-dot" style="background:var(--red)"></div>Violations <span class="stat-val" id="st-viol">-</span></div>
  <div class="stat"><div class="stat-dot" style="background:var(--purple)"></div>Critical path tasks <span class="stat-val" id="st-crit">-</span></div>
</div>

<div id="gantt-wrap">
  <div id="gantt-scroll">
    <div class="gantt-inner" id="gantt-inner">
      <div class="g-header">
        <div class="g-header-label">Task</div>
        <div class="g-header-tl" id="hdr-tl"></div>
      </div>
      <div id="gantt-rows"></div>
      <svg id="dep-svg"><defs>
        <marker id="arrowhead" markerWidth="7" markerHeight="5" refX="6" refY="2.5" orient="auto">
          <polygon points="0 0, 7 2.5, 0 5" fill="#4f8ef766"/>
        </marker>
        <marker id="arrowhead-viol" markerWidth="7" markerHeight="5" refX="6" refY="2.5" orient="auto">
          <polygon points="0 0, 7 2.5, 0 5" fill="#f76f6f99"/>
        </marker>
        <marker id="arrowhead-crit" markerWidth="7" markerHeight="5" refX="6" refY="2.5" orient="auto">
          <polygon points="0 0, 7 2.5, 0 5" fill="#a78bfa99"/>
        </marker>
      </defs></svg>
    </div>
  </div>
</div>

<div id="legend">
  <div class="lg-item"><div class="lg-summary"></div> Summary</div>
  <div class="lg-item"><div class="lg-sw" style="background:var(--green-dim);border:1px solid var(--green)"></div> Complete</div>
  <div class="lg-item"><div class="lg-sw" style="background:var(--amber-dim);border:1px solid var(--amber)"></div> In progress</div>
  <div class="lg-item"><div class="lg-sw" style="background:var(--slate-dim);border:1px solid var(--slate)"></div> Not started</div>
  <div class="lg-item"><div class="lg-sw" style="background:var(--red-dim);border:1px solid var(--red)"></div> &#x26a0; Violation</div>
  <div class="lg-item"><div class="lg-sw" style="background:var(--purple-dim);border:2px solid var(--purple)"></div> Critical path</div>
  <div class="lg-item"><div class="lg-diamond"></div> Milestone</div>
</div>
</div>

<div id="tooltip"></div>

<script>
const PROJECT = __PROJECT_DATA__;

let currentNode  = PROJECT;
let navStack     = [];
let expanded     = {};
let zoom         = 'month';
let showArrows   = false;
let showCritical = false;

const LABEL_W = 300;
const ROW_H   = 38;

/* -- build id->node map -- */
const nodeById = {};

function buildMap(n) {
  nodeById[n.id] = n;
  (n.children || []).forEach(buildMap);
}
buildMap(PROJECT);

/* -- violation detection -- */
function detectViolations(rootNode) {
  const violations = {};
  function walk(n) {
    if (n.predecessors && n.predecessors.length && n.start) {
      const taskStart  = parseDate(n.start);
      const taskFinish = parseDate(n.finish);
      if (taskStart) {
        n.predecessors.forEach(function(p) {
          const pred = nodeById[p.id];
          if (!pred) return;
          let predDate = null;
          if (p.type === 'FS' || p.type === 'FF') predDate = parseDate(pred.finish);
          else                                     predDate = parseDate(pred.start);
          if (!predDate) return;
          const required   = addDays(predDate, p.lag_days || 0);
          const compareDate = (p.type === 'FF' || p.type === 'SF') ? taskFinish : taskStart;
          if (compareDate && compareDate < required) {
            if (!violations[n.id]) violations[n.id] = [];
            violations[n.id].push({
              pred_id:      p.id,
              pred_name:    pred.name,
              type:         p.type,
              lag_days:     p.lag_days || 0,
              required:     required,
              actual:       compareDate,
              overlap_days: Math.round((required - compareDate) / 86400000),
            });
          }
        });
      }
    }
    (n.children || []).forEach(walk);
  }
  walk(rootNode);
  return violations;
}

/* -- critical path -- use CPM flag from planner data where available,
      fall back to a client-side longest-path approximation -- */
function buildCriticalSet(rootNode) {
  const fromData = new Set();
  function walk(n) {
    if (n.on_critical) fromData.add(n.id);
    (n.children || []).forEach(walk);
  }
  walk(rootNode);
  if (fromData.size > 0) return fromData;

  // Fallback: simple forward/backward pass on leaf tasks
  const tasks = [];
  function collect(n) {
    if (!n.children || !n.children.length) {
      if (n.start && n.finish) tasks.push(n);
    }
    (n.children || []).forEach(collect);
  }
  collect(rootNode);
  if (!tasks.length) return fromData;

  const ef = {};
  tasks.forEach(function(t) {
    const dur = (parseDate(t.finish) - parseDate(t.start)) / 86400000;
    let es = parseDate(t.start).getTime();
    (t.predecessors || []).forEach(function(p) {
      const pef = ef[p.id];
      if (pef && pef > es) es = pef;
    });
    ef[t.id] = es + dur * 86400000;
  });

  const maxEF = Math.max(...Object.values(ef));
  const ls = {};
  const succs = {};
  tasks.forEach(function(t) {
    (t.predecessors || []).forEach(function(p) {
      if (!succs[p.id]) succs[p.id] = [];
      succs[p.id].push(t.id);
    });
  });
  [...tasks].reverse().forEach(function(t) {
    const dur = (parseDate(t.finish) - parseDate(t.start)) / 86400000;
    let lf = maxEF;
    (succs[t.id] || []).forEach(function(sid) { if (ls[sid] !== undefined && ls[sid] < lf) lf = ls[sid]; });
    ls[t.id] = lf - dur * 86400000;
  });

  const result = new Set();
  tasks.forEach(function(t) {
    const slack = (ls[t.id] || 0) - parseDate(t.start).getTime();
    if (Math.abs(slack) < 86400000 * 1.5) result.add(t.id);
  });
  return result;
}

let violations   = {};
let criticalIds  = new Set();

function refreshAnalysis() {
  violations  = detectViolations(PROJECT);
  criticalIds = buildCriticalSet(PROJECT);
  updateStats();
}
refreshAnalysis();

/* -- stats bar -- */
function updateStats() {
  let total = 0, done = 0, prog = 0, ns = 0;
  function walk(n) {
    if (!n.children || !n.children.length) {
      if (n.start) {
        total++;
        const sc = statusClass(n.status);
        if (sc === 'complete')    done++;
        else if (sc === 'in-progress') prog++;
        else                      ns++;
      }
    }
    (n.children || []).forEach(walk);
  }
  walk(PROJECT);
  document.getElementById('st-total').textContent = total;
  document.getElementById('st-done').textContent  = done;
  document.getElementById('st-prog').textContent  = prog;
  document.getElementById('st-ns').textContent    = ns;
  document.getElementById('st-viol').textContent  = Object.keys(violations).length;
  document.getElementById('st-crit').textContent  = criticalIds.size;
}

/* -- utilities -- */
function parseDate(s) {
  if (!s) return null;
  const d = new Date(s.length === 10 ? s + 'T00:00:00' : s);
  return isNaN(d) ? null : d;
}
function addDays(d, n) {
  if (!d) return null;
  const r = new Date(d); r.setDate(r.getDate() + n); return r;
}

function allDates(node) {
  const out = [];
  function walk(n) {
    const s = parseDate(n.start), f = parseDate(n.finish);
    if (s) out.push(s); if (f) out.push(f);
    (n.children || []).forEach(walk);
  }
  walk(node); return out;
}

function flatVisible(node) {
  const rows = [];
  function walk(n, depth) {
    if (!n.children) return;
    n.children.forEach(function(child) {
      const hasCh = !!(child.children && child.children.length);
      rows.push({ node: child, depth, hasCh });
      if (hasCh && expanded[child._id]) walk(child, depth + 1);
    });
  }
  walk(node, 0); return rows;
}

function setAllExpand(node, val) {
  if (node.children && node.children.length) {
    expanded[node._id] = val;
    node.children.forEach(function(c) { setAllExpand(c, val); });
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
    while (cur <= maxD) { ticks.push({ date: new Date(cur), label: 'W' + getWeek(cur), major: cur.getDate() <= 7 }); cur.setDate(cur.getDate() + 7); }
  } else if (zoom === 'month') {
    cur = new Date(minD.getFullYear(), minD.getMonth(), 1);
    while (cur <= maxD) { ticks.push({ date: new Date(cur), label: cur.toLocaleString('default', { month: 'short', year: '2-digit' }), major: cur.getMonth() === 0 }); cur.setMonth(cur.getMonth() + 1); }
  } else {
    cur = new Date(minD.getFullYear(), Math.floor(minD.getMonth() / 3) * 3, 1);
    while (cur <= maxD) { const q = Math.floor(cur.getMonth() / 3) + 1; ticks.push({ date: new Date(cur), label: 'Q' + q + ' ' + cur.getFullYear(), major: q === 1 }); cur.setMonth(cur.getMonth() + 3); }
  }
  return ticks;
}
function getWeek(d) { const j = new Date(d.getFullYear(), 0, 1); return Math.ceil(((d - j) / 86400000 + j.getDay() + 1) / 7); }
function dX(d, minD, tlW, totalDays) { return ((d - minD) / 86400000) / totalDays * tlW; }

/* -- row position map for arrow drawing -- */
let rowPosMap = {};   // id -> { y, barLeft, barRight }

/* -- main render -- */
function render() {
  renderBreadcrumb();
  rowPosMap = {};

  const rows = flatVisible(currentNode);
  const rowsEl = document.getElementById('gantt-rows');
  if (!rows.length) { rowsEl.innerHTML = '<div class="empty">No tasks to display.</div>'; return; }

  // Use only dates from visible rows so the axis fits the current view
  const visibleDates = [];
  rows.forEach(function(r) {
    const s = parseDate(r.node.start), f = parseDate(r.node.finish);
    if (s) visibleDates.push(s); if (f) visibleDates.push(f);
  });
  const dates = visibleDates.length ? visibleDates : allDates(currentNode);
  if (!dates.length) { rowsEl.innerHTML = '<div class="empty">No date data found.</div>'; return; }

  const minD = addDays(new Date(Math.min(...dates)), -7);
  const maxD = addDays(new Date(Math.max(...dates)), 14);
  const totalDays = (maxD - minD) / 86400000;

  const scrollEl = document.getElementById('gantt-scroll');
  const visibleW = scrollEl.clientWidth - LABEL_W - 2;
  const minPxPerDay = zoom === 'week' ? 20 : zoom === 'month' ? 4 : 1.5;
  const tlW = Math.max(visibleW, Math.ceil(totalDays * minPxPerDay), 500);
  document.getElementById('gantt-inner').style.minWidth = (LABEL_W + tlW) + 'px';
  document.getElementById('gantt-inner').style.width = 'max-content';

  const ticks = buildTicks(minD, maxD);
  const today = new Date();

  /* header */
  const hdrTl = document.getElementById('hdr-tl');
  hdrTl.style.width = tlW + 'px';
  let hHtml = '';
  ticks.forEach(function(t) {
    const x = dX(t.date, minD, tlW, totalDays);
    hHtml += '<div class="tick-line ' + (t.major ? 'major' : 'minor') + '" style="left:' + x + 'px"></div>';
    hHtml += '<div class="tick-label" style="left:' + (x + 3) + 'px">' + t.label + '</div>';
  });
  if (today >= minD && today <= maxD) {
    const tx = dX(today, minD, tlW, totalDays);
    hHtml += '<div class="today-line" style="left:' + tx + 'px"><div class="today-dot"></div></div>';
  }
  hdrTl.innerHTML = hHtml;

  /* rows */
  let rHtml = '';
  rows.forEach(function({ node, depth, hasCh }, rowIdx) {
    const isMilestone = node.is_milestone;
    const isSummary   = hasCh || node.is_summary;
    const sc          = statusClass(node.status);
    const start       = parseDate(node.start);
    const finish      = parseDate(node.finish);
    const indentPx    = depth * 18 + 6;
    const exp         = expanded[node._id];
    const pct         = Math.round((node.progress || 0) * 100);
    const isViol      = !!(violations[node.id] && violations[node.id].length);
    const isCrit      = showCritical && criticalIds.has(node.id);

    let barLeft = null, barRight = null;

    let bHtml = '';
    ticks.forEach(function(t) {
      const x = dX(t.date, minD, tlW, totalDays);
      bHtml += '<div class="tick-line ' + (t.major ? 'major' : 'minor') + '" style="left:' + x + 'px"></div>';
    });
    if (today >= minD && today <= maxD) {
      const tx = dX(today, minD, tlW, totalDays);
      bHtml += '<div class="today-line" style="left:' + tx + 'px"></div>';
    }

    if (isMilestone && start) {
      const mx = dX(start, minD, tlW, totalDays);
      barLeft = barRight = mx;
      const violCls = isViol ? 'violation' : '';
      bHtml += '<div class="milestone ' + violCls + '" style="left:' + (mx - 6) + 'px" ' + encN(node) + '></div>';
    } else if (start && finish) {
      const x1 = dX(start, minD, tlW, totalDays);
      const x2 = dX(finish, minD, tlW, totalDays);
      const bw = Math.max(x2 - x1, 4);
      barLeft = x1; barRight = x1 + bw;
      const bc       = isSummary ? 'summary' : sc;
      const violCls  = isViol && !isSummary ? 'violation' : '';
      const critCls  = isCrit && !isSummary ? 'critical-path' : '';
      const clickCls = isSummary ? 'clickable' : '';
      const onclick  = isSummary ? ' onclick="drillDown(' + node._id + ')"' : '';
      bHtml += '<div class="bar ' + bc + ' ' + violCls + ' ' + critCls + ' ' + clickCls + '"'
        + ' style="left:' + x1 + 'px;width:' + bw + 'px"'
        + ' ' + encN(node) + onclick + '>';
      if (!isSummary) bHtml += '<div class="bar-progress" style="width:' + pct + '%"></div>';
      if (isSummary && bw > 16) bHtml += '<span style="position:absolute;right:5px;top:50%;transform:translateY(-50%);font-size:9px;opacity:0.7;pointer-events:none">&#x25b6;</span>';
      if (bw > 40) bHtml += '<span style="position:relative;z-index:1;overflow:hidden;text-overflow:ellipsis;max-width:' + (bw - 12) + 'px">' + taskLabel(node.id, node.name) + '</span>';
      bHtml += '</div>';
    }

    if (barLeft !== null) rowPosMap[node.id] = { y: rowIdx * ROW_H + ROW_H / 2, barLeft, barRight };

    const rowCls = [
      isSummary ? 'summary-row' : '',
      isViol && !isSummary ? 'violation-row' : '',
      isCrit && !isSummary ? 'critical-row' : '',
    ].join(' ');

    const expandBtn = hasCh
      ? '<div class="expand-btn" onclick="toggleExpand(' + node._id + ')">' + (exp ? '&minus;' : '+') + '</div>'
      : '<div class="placeholder-btn"></div>';

    const violIcon = isViol && !isSummary
      ? '<span class="violation-icon" title="Scheduling violation">&#x26a0;</span>' : '';
    const critIcon = isCrit && !isSummary
      ? '<span class="critical-icon" title="Critical path" style="color:var(--purple)">&#x25c6;</span>' : '';

    const nameCls = 'task-name'
      + (isSummary    ? ' is-summary'   : '')
      + (isMilestone  ? ' is-milestone' : '')
      + (isViol && !isSummary ? ' is-violation' : '')
      + (isCrit && !isSummary ? ' is-critical'  : '');
    const nameStyle  = isSummary ? ' style="cursor:pointer"' : '';
    const nameOnclick = isSummary ? ' onclick="drillDown(' + node._id + ')"' : '';
    const summaryPfx = isSummary ? '&#x25b8; ' : '';

    rHtml += '<div class="g-row ' + rowCls + '">'
      + '<div class="g-row-label" style="padding-left:' + indentPx + 'px">'
      +   expandBtn + violIcon + critIcon
      +   '<span class="' + nameCls + '" ' + encN(node) + nameStyle + nameOnclick + '>'
      +     summaryPfx + taskLabel(node.id, node.name || '(unnamed)')
      +   '</span>'
      + '</div>'
      + '<div class="g-row-tl" style="width:' + tlW + 'px">' + bHtml + '</div>'
      + '</div>';
  });
  rowsEl.innerHTML = rHtml;

  rowsEl.querySelectorAll('[data-node]').forEach(function(el) {
    el.addEventListener('mouseenter', showTip);
    el.addEventListener('mousemove',  moveTip);
    el.addEventListener('mouseleave', hideTip);
  });

  requestAnimationFrame(function() { drawArrows(rows, tlW); });
}

/* -- dependency arrows -- */
function drawArrows(rows, tlW) {
  const svg = document.getElementById('dep-svg');
  const totalH = rows.length * ROW_H;
  svg.setAttribute('width',  LABEL_W + tlW);
  svg.setAttribute('height', totalH);
  svg.style.width  = (LABEL_W + tlW) + 'px';
  svg.style.height = totalH + 'px';

  [...svg.querySelectorAll('path,line')].forEach(function(e) { e.remove(); });

  if (!showArrows && !showCritical) return;

  rows.forEach(function({ node }, rowIdx) {
    if (!node.predecessors || !node.predecessors.length) return;
    const toPos = rowPosMap[node.id];
    if (!toPos) return;
    const toX = LABEL_W + toPos.barLeft - 2;
    const toY = rowIdx * ROW_H + ROW_H / 2;

    node.predecessors.forEach(function(p) {
      const fromPos = rowPosMap[p.id];
      if (!fromPos) return;

      const isViolPair = !!(violations[node.id] && violations[node.id].find(function(v) { return v.pred_id === p.id; }));
      const isCritPair = showCritical && criticalIds.has(node.id) && criticalIds.has(p.id);

      if (!showArrows && !isCritPair) return;

      const fromRowIdx = rows.findIndex(function(r) { return r.node.id === p.id; });
      if (fromRowIdx < 0) return;
      const fromY = fromRowIdx * ROW_H + ROW_H / 2;
      const fromX = LABEL_W + fromPos.barRight + 2;

      const marker = isViolPair ? 'url(#arrowhead-viol)' : isCritPair ? 'url(#arrowhead-crit)' : 'url(#arrowhead)';
      const cls    = isViolPair ? 'dep-arrow violation' : isCritPair ? 'dep-arrow critical-path-arrow' : 'dep-arrow';

      const midX = fromX + Math.max((toX - fromX) / 2, 10);
      const path = document.createElementNS('http://www.w3.org/2000/svg', 'path');
      path.setAttribute('class', cls);
      path.setAttribute('marker-end', marker);
      path.setAttribute('d', 'M' + fromX + ',' + fromY + ' C' + midX + ',' + fromY + ' ' + midX + ',' + toY + ' ' + toX + ',' + toY);
      svg.appendChild(path);
    });
  });
}

/* -- navigation -- */
function drillDown(id) {
  const n = nodeById[Object.keys(nodeById).find(function(k) { return nodeById[k]._id === id; })];
  if (!n || !n.children || !n.children.length) return;
  navStack.push({ node: currentNode, label: taskLabel(currentNode.id, currentNode.name || 'Root') });
  currentNode = n; render();
}

document.getElementById('btn-reset').addEventListener('click', function() { currentNode = PROJECT; navStack = []; render(); });
function toggleExpand(id) { expanded[id] = !expanded[id]; render(); }
document.getElementById('btn-expand').addEventListener('click',   function() { setAllExpand(currentNode, true);  render(); });
document.getElementById('btn-collapse').addEventListener('click', function() { setAllExpand(currentNode, false); render(); });
document.getElementById('zoom-sel').addEventListener('change', function(e) { zoom = e.target.value; render(); });
document.getElementById('btn-arrows').addEventListener('click', function() {
  showArrows = !showArrows; this.classList.toggle('active', showArrows); render();
});
document.getElementById('btn-critical').addEventListener('click', function() {
  showCritical = !showCritical; this.classList.toggle('active', showCritical); render();
});

/* -- breadcrumb -- */
function renderBreadcrumb() {
  const bc = document.getElementById('breadcrumb'); let h = '';
  navStack.forEach(function(item, i) {
    h += '<span class="bc-item" onclick="navTo(' + i + ')">' + esc(item.label) + '</span><span class="bc-crumb">&#x203a;</span>';
  });
  h += '<span style="color:var(--text)">' + taskLabel(currentNode.id, currentNode.name || 'Root') + '</span>';
  bc.innerHTML = h;
}
function navTo(idx) { currentNode = navStack[idx].node; navStack = navStack.slice(0, idx); render(); }

/* -- task label helper -- */
function taskLabel(id, name) {
  if (!id || id === '__root__') return esc(name);
  return '<span style="color:var(--text-muted);font-family:var(--mono);font-size:10px;margin-right:5px">' + esc(id) + '</span>' + esc(name);
}

/* -- tooltip data encoding -- */
function encN(n) {
  const viols = violations[n.id] || [];
  const payload = {
    id: n.id, name: n.name, description: n.description,
    start: n.start, finish: n.finish, status: n.status, progress: n.progress,
    is_milestone: n.is_milestone, is_summary: n.is_summary,
    on_critical: n.on_critical, slack_days: n.slack_days,
    owner: n.owner, team: n.team, wbs: n.wbs, duration_days: n.duration_days,
    predecessors: n.predecessors || [],
    violations: viols,
    isCritical: criticalIds.has(n.id),
  };
  return 'data-node="' + encodeURIComponent(JSON.stringify(payload)) + '"';
}

/* -- tooltip display -- */
function showTip(e) {
  const raw = e.currentTarget.dataset.node; if (!raw) return;
  let n; try { n = JSON.parse(decodeURIComponent(raw)); } catch(err) { return; }
  const sc     = statusClass(n.status);
  const isMil  = n.is_milestone;
  const isCrit = n.isCritical;
  const pct    = Math.round((n.progress || 0) * 100);
  const tt     = document.getElementById('tooltip');

  let html = '<div class="tt-title">' + taskLabel(n.id, n.name || '') + '</div>';
  if (n.description) html += '<div class="tt-desc">' + esc(n.description) + '</div>';
  if (n.owner) html += '<div class="tt-row">Owner &nbsp;&nbsp;<span>' + esc(n.owner) + (n.team ? ' / ' + esc(n.team) : '') + '</span></div>';
  if (n.wbs)   html += '<div class="tt-row">WBS &nbsp;&nbsp;&nbsp;&nbsp;<span>' + esc(n.wbs) + '</span></div>';
  if (n.start) html += '<div class="tt-row">Start &nbsp;&nbsp;&nbsp;<span>' + n.start + '</span></div>';
  if (n.finish)html += '<div class="tt-row">Finish &nbsp;&nbsp;<span>' + n.finish + '</span></div>';
  if (n.start && n.finish && !isMil) {
    const s = parseDate(n.start), f = parseDate(n.finish);
    if (s && f) {
      const cal = Math.round((f - s) / 86400000) + 1;
      let wd = 0; const cur = new Date(s);
      while (cur <= f) { if (cur.getDay() !== 0 && cur.getDay() !== 6) wd++; cur.setDate(cur.getDate() + 1); }
      html += '<div class="tt-row">Duration &nbsp;<span>' + wd + 'd working / ' + cal + 'd calendar</span></div>';
    }
  }
  if (n.duration_days != null) html += '<div class="tt-row">Sched days <span>' + n.duration_days + 'd</span></div>';
  if (!isMil) html += '<div class="tt-row">Progress &nbsp;<span>' + pct + '%</span></div>';
  if (n.slack_days != null) html += '<div class="tt-row">Slack &nbsp;&nbsp;&nbsp;&nbsp;<span>' + n.slack_days + 'd</span></div>';

  const badge     = isMil ? '<span class="badge milestone-b">Milestone</span>' : '<span class="badge ' + sc + '">' + statusLabel(n.status) + '</span>';
  const violBadge = n.violations && n.violations.length ? ' <span class="badge violation-b">&#x26a0; Violation</span>' : '';
  const critBadge = isCrit ? ' <span class="badge critical-b">&#x25c6; Critical</span>' : '';
  html += '<div class="tt-row">' + badge + violBadge + critBadge + '</div>';

  if (n.predecessors && n.predecessors.length) {
    html += '<div class="tt-section"><div style="font-size:11px;color:var(--text-muted);margin-bottom:3px">Predecessors</div>';
    n.predecessors.forEach(function(p) {
      const pred = nodeById[p.id];
      const pname = pred ? pred.name : p.id;
      const lag = p.lag_days ? ' +' + p.lag_days + 'd' : '';
      html += '<div class="tt-row" style="font-size:11px">' + esc(pname) + ' <span style="color:var(--text-dim)">[' + p.type + lag + ']</span></div>';
    });
    html += '</div>';
  }

  if (n.violations && n.violations.length) {
    html += '<div class="tt-section">';
    n.violations.forEach(function(v) {
      html += '<div class="tt-row" style="font-size:11px;color:var(--red)">'
        + '&#x26a0; Starts ' + v.overlap_days + 'd early vs <em>' + esc(v.pred_name || '') + '</em>'
        + ' <span style="color:var(--text-dim)">[' + v.type + ']</span></div>';
    });
    html += '</div>';
  }

  if (isCrit) html += '<div class="tt-section"><span class="badge critical-b">&#x25c6; On critical path</span></div>';

  tt.innerHTML = html;
  tt.style.display = 'block';
  moveTip(e);
}

function moveTip(e) {
  const tt = document.getElementById('tooltip');
  let x = e.clientX + 14, y = e.clientY + 14;
  if (x + 310 > window.innerWidth)  x = e.clientX - 310;
  if (y + 260 > window.innerHeight) y = e.clientY - 260;
  tt.style.left = x + 'px'; tt.style.top = y + 'px';
}
function hideTip() { document.getElementById('tooltip').style.display = 'none'; }
function esc(s) { return String(s || '').replace(/&/g,'&amp;').replace(/</g,'&lt;').replace(/>/g,'&gt;').replace(/"/g,'&quot;'); }

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
