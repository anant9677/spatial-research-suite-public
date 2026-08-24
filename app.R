# =========================================================================
# APP.R - MAIN ENTRY POINT  (Spatial Research Suite - open research build)
# =========================================================================
# Public, open-source build: no accounts, no login, no payments. Every module
# and every export is available immediately from a boundary + date range.
# The modules in R/ (mod_extractor, mod_lulc, mod_gee, mod_carto, ...) are
# auto-loaded by Shiny; global.R initialises the GEE session and shared
# analysis functions. Earth Engine credentials are per-user and never shipped.
# =========================================================================
source("global.R")
shared_head <- tags$head(
  tags$title("Spatial Research Suite"),
  tags$link(rel = "preconnect", href = "https://fonts.googleapis.com"),
  tags$link(rel = "preconnect", href = "https://fonts.gstatic.com", crossorigin = ""),
  tags$link(href = "https://fonts.googleapis.com/css2?family=IBM+Plex+Mono:wght@400;500;600;700&family=IBM+Plex+Sans:wght@400;500;600;700&display=swap", rel = "stylesheet"),
  tags$link(rel = "icon", type = "image/svg+xml", href = "data:image/svg+xml,%3Csvg xmlns='http://www.w3.org/2000/svg' viewBox='0 0 100 100'%3E%3Ccircle cx='50' cy='50' r='46' fill='%2326333e'/%3E%3Ccircle cx='50' cy='50' r='16' fill='%2345936f'/%3E%3C/svg%3E"),
  tags$meta(name = "viewport", content = "width=device-width, initial-scale=1"),
  tags$meta(property = "og:url", content = "https://gisforus.com"),
  tags$meta(name = "twitter:card", content = "summary"),
  tags$meta(property = "og:title", content = "Spatial Research Suite - Cloud Analytics"),
  tags$meta(property = "og:description", content = "Perform LULC and spatial analytics directly from your browser."),
  tags$meta(property = "og:type", content = "website"),
  tags$meta(name = "description", content = "Advanced Spatial Research Suite for LULC analysis, change detection, and cloud analytics using Google Earth Engine and R."),
  tags$script(HTML("
    (function() {
      var retried = {};
      window.addEventListener('error', function(e) {
        var el = e.target;
        if (!el || (el.tagName !== 'SCRIPT' && el.tagName !== 'LINK')) return;
        var url = el.src || el.href;
        if (!url || retried[url]) return;
        retried[url] = true;
        setTimeout(function() {
          if (el.tagName === 'SCRIPT') {
            var s = document.createElement('script'); s.src = url; s.async = false; document.head.appendChild(s);
          } else {
            var l = document.createElement('link'); l.rel = 'stylesheet'; l.href = url; document.head.appendChild(l);
          }
        }, 1000);
      }, true);
    })();
  ")),
  tags$script(HTML("
    var __sessionStartTime = new Date().getTime();
    var __lastHeartbeatTime = new Date().getTime();
    var __lastActivityTime = new Date().getTime();
    setInterval(function() {
      Shiny.setInputValue('keep_alive', new Date().getTime(), {priority: 'event'});
      __lastHeartbeatTime = new Date().getTime();
    }, 25000);
    ['mousemove', 'keydown', 'click', 'scroll', 'touchstart'].forEach(function(evt) {
      document.addEventListener(evt, function() { __lastActivityTime = new Date().getTime(); }, {passive: true});
    });
    var IDLE_WARNING_SECONDS = 20 * 60;
    function __updateSessionTimer() {
      var el = document.getElementById('session-timer-badge');
      if (!el) return;
      var now = new Date().getTime();
      var idleSec = Math.floor((now - __lastActivityTime) / 1000);
      if (idleSec >= IDLE_WARNING_SECONDS) {
        var idleMin = Math.floor(idleSec / 60);
        el.innerHTML = '<span style=\"display:inline-block;width:7px;height:7px;border-radius:50%;background:#c1683b;margin-right:6px;\"></span>Idle ' + idleMin + 'm \\u2014 refresh if unresponsive';
        el.title = 'This tab has been idle a while. If the app seems unresponsive, refreshing the page will give you a fresh, healthy connection.';
        return;
      }
      var elapsedSec = Math.floor((now - __sessionStartTime) / 1000);
      var hh = Math.floor(elapsedSec / 3600);
      var mm = Math.floor((elapsedSec % 3600) / 60);
      var ss = elapsedSec % 60;
      var timeStr = (hh > 0 ? hh + ':' : '') + String(mm).padStart(2, '0') + ':' + String(ss).padStart(2, '0');
      var sinceHeartbeat = (now - __lastHeartbeatTime) / 1000;
      var dotColor = sinceHeartbeat < 40 ? '#45936f' : (sinceHeartbeat < 90 ? '#c1683b' : '#8b3a2b');
      el.innerHTML = '<span style=\"display:inline-block;width:7px;height:7px;border-radius:50%;background:' + dotColor + ';margin-right:6px;\"></span>Session ' + timeStr;
      el.title = 'Time since this tab was loaded. The dot shows connection health \\u2014 green is healthy; if it turns amber or red, try refreshing the page.';
    }
    setInterval(__updateSessionTimer, 1000);
  ")),
  # 🚀 MOBILE SLEEP/DISCONNECT FIX (Task A): branded 'Session Paused' fallback.
  # Replaces Shiny's permanent grey #shiny-disconnected-overlay (the 'endless grey
  # screen' users hit after their phone sleeps or Cloud Run idles the socket) with a
  # dark-navy/green branded overlay carrying a tappable Reconnect button. Works WITH
  # session$allowReconnect(TRUE): if Shiny silently reconnects we hide the overlay
  # automatically; if it can't, the user taps Reconnect to restore the session.
  tags$style(HTML("
    /* Suppress Shiny's default grey fade — we render our own branded overlay instead. */
    #shiny-disconnected-overlay { display: none !important; }
    #gf-reconnect-overlay {
      position: fixed; inset: 0; z-index: 100000;
      display: none; align-items: center; justify-content: center;
      background: rgba(38,51,62,0.92); backdrop-filter: blur(3px);
      -webkit-backdrop-filter: blur(3px);
      font-family: 'Inter','Segoe UI',system-ui,sans-serif;
    }
    #gf-reconnect-overlay.gf-show { display: flex; }
    .gf-reconnect-card {
      background: #26333e; border: 1px solid #45936f; border-radius: 14px;
      padding: 34px 30px; max-width: 340px; width: calc(100% - 48px);
      text-align: center; box-shadow: 0 18px 50px rgba(0,0,0,0.45);
    }
    .gf-reconnect-dot {
      width: 46px; height: 46px; margin: 0 auto 16px; border-radius: 50%;
      background: #45936f; position: relative;
      box-shadow: 0 0 0 0 rgba(69,147,111,0.7);
      animation: gf-pulse 1.8s infinite;
    }
    @keyframes gf-pulse {
      0%   { box-shadow: 0 0 0 0 rgba(69,147,111,0.55); }
      70%  { box-shadow: 0 0 0 18px rgba(69,147,111,0); }
      100% { box-shadow: 0 0 0 0 rgba(69,147,111,0); }
    }
    .gf-reconnect-card h3 { color: #ffffff; font-size: 19px; font-weight: 700; margin: 0 0 8px; }
    .gf-reconnect-card p  { color: #b7c2c9; font-size: 13.5px; line-height: 1.55; margin: 0 0 20px; }
    #gf-reconnect-btn {
      display: inline-block; width: 100%; border: none; cursor: pointer;
      background: #45936f; color: #ffffff; font-size: 15px; font-weight: 700;
      padding: 13px 18px; border-radius: 9px; min-height: 46px;
      transition: background 0.15s ease;
    }
    #gf-reconnect-btn:hover { background: #3c8261; }
    #gf-reconnect-btn:active { background: #34714f; }
    #gf-reconnect-status { color: #8ea3ad; font-size: 11.5px; margin-top: 14px; min-height: 14px; }
  ")),
  tags$script(HTML("
    (function() {
      function buildOverlay() {
        if (document.getElementById('gf-reconnect-overlay')) return;
        var ov = document.createElement('div');
        ov.id = 'gf-reconnect-overlay';
        ov.innerHTML =
          '<div class=\"gf-reconnect-card\" role=\"alertdialog\" aria-live=\"assertive\">' +
            '<div class=\"gf-reconnect-dot\"></div>' +
            '<h3>Session Paused</h3>' +
            '<p>Your connection dropped \\u2014 this usually happens after your device sleeps or the tab is in the background. Your work is safe.</p>' +
            '<button id=\"gf-reconnect-btn\" type=\"button\">Reconnect</button>' +
            '<div id=\"gf-reconnect-status\"></div>' +
          '</div>';
        document.body.appendChild(ov);
        document.getElementById('gf-reconnect-btn').addEventListener('click', function() {
          var s = document.getElementById('gf-reconnect-status');
          if (s) s.textContent = 'Reconnecting\\u2026';
          try {
            if (window.Shiny && Shiny.shinyapp && typeof Shiny.shinyapp.reconnect === 'function') {
              Shiny.shinyapp.reconnect();
              // If the silent reconnect can't restore state within a few seconds, hard reload.
              setTimeout(function() {
                if (document.getElementById('gf-reconnect-overlay').classList.contains('gf-show')) {
                  window.location.reload();
                }
              }, 6000);
            } else {
              window.location.reload();
            }
          } catch (err) { window.location.reload(); }
        });
      }
      function showOverlay() {
        buildOverlay();
        var ov = document.getElementById('gf-reconnect-overlay');
        var s = document.getElementById('gf-reconnect-status');
        if (s) s.textContent = '';
        if (ov) ov.classList.add('gf-show');
      }
      function hideOverlay() {
        var ov = document.getElementById('gf-reconnect-overlay');
        if (ov) ov.classList.remove('gf-show');
      }
      // Shiny fires these DOM events on socket state changes.
      document.addEventListener('shiny:disconnected', showOverlay);
      document.addEventListener('shiny:connected', hideOverlay);

      // ---- BUG 1: mid-analysis sleep => zombie spinner watchdog ----
      // If the phone sleeps WHILE an analysis is running, the websocket drops mid
      // computation; the result never reaches the client and shinycssloaders /
      // withProgress spinners spin forever. Shiny's own 'shiny:disconnected' may not
      // fire promptly for a suspended tab. So on wake (visibilitychange -> visible)
      // we nudge a keep-alive and then VERIFY the socket actually recovers; if it's
      // still dead after ~5s we surface the branded 'Session Paused' overlay (which
      // covers any hung spinner) so the user can cleanly reload into a fresh session
      // instead of staring at an infinite loader. Module-agnostic: works for GEE,
      // multivariate, correlation, batch — every analysis, because it's socket-level.
      function socketHealthy() {
        try {
          var s = Shiny.shinyapp && Shiny.shinyapp.$socket;
          return !!s && s.readyState === 1; // WebSocket.OPEN
        } catch (e) { return false; }
      }
      var __wakeWatch = null;
      document.addEventListener('visibilitychange', function() {
        if (document.hidden) return;
        if (window.Shiny && Shiny.setInputValue) {
          try { Shiny.setInputValue('keep_alive', new Date().getTime(), {priority: 'event'}); } catch (e) {}
        }
        if (__wakeWatch) { clearInterval(__wakeWatch); }
        var tries = 0;
        __wakeWatch = setInterval(function() {
          tries++;
          if (socketHealthy()) { clearInterval(__wakeWatch); __wakeWatch = null; return; }
          if (tries >= 5) { clearInterval(__wakeWatch); __wakeWatch = null; showOverlay(); }
        }, 1000);
      });

      // ---- BUG 2: orphaned Bootstrap modal-backdrop => dead buttons ----
      // Several flows (boundary picker, error dialogs, Cite/Request modals) call
      // showModal()/removeModal(). On BS5 a removeModal() that races the show
      // animation can leave a transparent '.modal-backdrop' (z-index 1040+) and
      // 'body.modal-open' behind — an INVISIBLE full-screen mask that swallows every
      // click, which is exactly why the header utility buttons went dead after an
      // analysis. Sweep any orphan backdrop whenever no modal is actually open.
      // A modal is only *really* open if a .modal element is actually visible; a
      // leftover .modal.show that is display:none (offsetParent null) still counts as
      // closed. This catches the Extractor 'Save Workspace' freeze where a prior modal's
      // backdrop lingered even though .modal.show was technically still in the DOM.
      function anyModalVisible() {
        var modals = document.querySelectorAll('.modal');
        for (var i = 0; i < modals.length; i++) {
          var m = modals[i];
          if (m.offsetParent !== null && getComputedStyle(m).display !== 'none') return true;
        }
        return false;
      }
      function sweepBackdrops() {
        if (anyModalVisible()) return;
        var bd = document.querySelectorAll('.modal-backdrop');
        if (bd.length) { bd.forEach(function(b) { if (b.parentNode) b.parentNode.removeChild(b); }); }
        if (document.body.classList.contains('modal-open')) {
          document.body.classList.remove('modal-open');
          document.body.style.removeProperty('padding-right');
          document.body.style.removeProperty('overflow');
        }
      }
      // Immediate cleanup the instant any modal hides (BS5 'hidden.bs.modal' + Shiny's
      // modal removal), plus a slow safety poll for anything the events miss.
      try { $(document).on('hidden.bs.modal', function(){ setTimeout(sweepBackdrops, 30); }); } catch (e) {}
      document.addEventListener('shiny:modalhidden', function(){ setTimeout(sweepBackdrops, 30); });
      setInterval(sweepBackdrops, 800);
    })();
  ")),
  # =====================================================================
  # STATE PERSISTENCE recovery key + 55-MIN "Session Expiring Soon" modal
  # =====================================================================
  tags$style(HTML("
    #gf-expiry-overlay { position: fixed; inset: 0; z-index: 100050; display: none;
      align-items: center; justify-content: center; background: rgba(38,51,62,0.86);
      backdrop-filter: blur(3px); -webkit-backdrop-filter: blur(3px);
      font-family: 'Inter','Segoe UI',system-ui,sans-serif; }
    #gf-expiry-overlay.gf-show { display: flex; }
    .gf-expiry-card { background: #26333e; border: 1px solid #c1683b; border-radius: 14px;
      padding: 30px 28px; max-width: 360px; width: calc(100% - 48px); text-align: center;
      box-shadow: 0 18px 50px rgba(0,0,0,0.45); }
    .gf-expiry-ico { font-size: 40px; line-height: 1; margin-bottom: 10px; }
    .gf-expiry-card h3 { color: #fff; font-size: 19px; font-weight: 700; margin: 0 0 8px; }
    .gf-expiry-card p { color: #b7c2c9; font-size: 13.5px; line-height: 1.55; margin: 0 0 20px; }
    #gf-expiry-extend { display: inline-block; width: 100%; border: none; cursor: pointer;
      background: #45936f; color: #fff; font-size: 15px; font-weight: 700; padding: 13px 18px;
      border-radius: 9px; min-height: 46px; transition: background .15s ease; }
    #gf-expiry-extend:hover { background: #3c8261; }
    #gf-expiry-dismiss { color: #8ea3ad; font-size: 12.5px; margin-top: 14px; cursor: pointer; }
  ")),
  tags$script(HTML("
    (function(){
      // ---- per-browser recovery key (survives reload), drives server-side restore ----
      var RK;
      try {
        RK = localStorage.getItem('gf_recovery_key');
        if (!RK) { RK = 'r' + new Date().getTime().toString(36) + Math.random().toString(36).slice(2,10); localStorage.setItem('gf_recovery_key', RK); }
      } catch(e) { RK = 'r' + new Date().getTime().toString(36); }
      function sendKey(){ try { if (window.Shiny && Shiny.setInputValue) Shiny.setInputValue('gf_recovery_key', RK, {priority:'event'}); } catch(e){} }
      $(document).on('shiny:connected', sendKey);
      setTimeout(sendKey, 1500);

      // ---- 55-minute 'Session Expiring Soon' warning ----
      var WARN_MS = 55 * 60 * 1000, expTimer = null;
      function buildExpiry(){
        if (document.getElementById('gf-expiry-overlay')) return;
        var ov = document.createElement('div'); ov.id = 'gf-expiry-overlay';
        ov.innerHTML = '<div class=\"gf-expiry-card\">' +
          '<div class=\"gf-expiry-ico\">\\u23F3</div>' +
          '<h3>Session Expiring Soon</h3>' +
          '<p>This session has been active for nearly an hour and may disconnect shortly. Your work is saved automatically \\u2014 tap Extend to keep it alive.</p>' +
          '<button id=\"gf-expiry-extend\" type=\"button\">Extend Session</button>' +
          '<div id=\"gf-expiry-dismiss\">Dismiss</div>' +
        '</div>';
        document.body.appendChild(ov);
        document.getElementById('gf-expiry-extend').addEventListener('click', function(){
          try { Shiny.setInputValue('keep_alive', new Date().getTime(), {priority:'event'}); } catch(e){}
          try { Shiny.setInputValue('gf_extend_session', new Date().getTime(), {priority:'event'}); } catch(e){}
          hideExpiry(); armExpiry();
        });
        document.getElementById('gf-expiry-dismiss').addEventListener('click', function(){ hideExpiry(); armExpiry(); });
      }
      function showExpiry(){ buildExpiry(); var o = document.getElementById('gf-expiry-overlay'); if (o) o.classList.add('gf-show'); }
      function hideExpiry(){ var o = document.getElementById('gf-expiry-overlay'); if (o) o.classList.remove('gf-show'); }
      function armExpiry(){ if (expTimer) clearTimeout(expTimer); expTimer = setTimeout(showExpiry, WARN_MS); }
      $(document).on('shiny:connected', armExpiry);
      setTimeout(armExpiry, 2000);
    })();
  ")),
  tags$script(HTML("
    document.addEventListener('contextmenu', function(e) {
      if (e.target.tagName === 'IMG' || e.target.tagName === 'CANVAS' || e.target.tagName === 'SVG' ||
          e.target.closest('.leaflet-container') || e.target.closest('.protect-wrap') || e.target.closest('.shiny-plot-output')) {
        e.preventDefault();
      }
    });
  ")),
  tags$style(HTML("
    :root {
      --ink: #26333e; --ink-soft: #3d4f5c; --paper: #f7f6f2; --paper-raised: #ffffff;
      --rule: #d8d4c8; --forest: #45936f; --forest-soft: #e8f0ea; --clay: #c1683b; --danger-deep: #8b3a2b;
      --slate: #5c6b73; --mono: 'IBM Plex Mono', ui-monospace, monospace;
      --sans: 'IBM Plex Sans', system-ui, -apple-system, 'Segoe UI', Roboto, sans-serif;
    }
    body { background-color: var(--paper); } 
    @keyframes fadeInSlideUp { from { opacity: 0; transform: translateY(15px); } to { opacity: 1; transform: translateY(0); } }
    .tab-pane.active { animation: fadeInSlideUp 0.4s cubic-bezier(0.25, 0.8, 0.25, 1) forwards; }
    .navbar { padding: 12px 30px !important; box-shadow: 0 4px 12px rgba(38,51,62,0.08); margin-bottom: 25px; transition: all 0.3s ease; }
    .navbar-brand { display: flex !important; align-items: center; margin-right: 40px !important; padding-top: 4px !important; padding-bottom: 4px !important; }
    .navbar-nav .nav-item .nav-link, .navbar-nav .nav-item .nav-link.active, .navbar-nav .nav-item .nav-link:hover { 
      font-family: var(--mono) !important; font-size: 13px !important; font-weight: 500 !important; letter-spacing: 0.02em;
      margin: 0 10px; padding: 10px 15px !important; position: relative; transition: color 0.3s ease;
      border-bottom: none !important; box-shadow: none !important;
    }
    .navbar-nav .nav-item .nav-link::after {
      content: ''; position: absolute; width: 0; height: 3px; bottom: 0px; left: 50%;
      background-color: var(--forest); transition: all 0.3s cubic-bezier(0.25, 0.8, 0.25, 1); transform: translateX(-50%); border-radius: 2px;
    }
    .navbar-nav .nav-item .nav-link:hover::after, .navbar-nav .nav-item .nav-link.active::after { width: 80%; }
    .sidebar-panel-custom { height: 85vh; overflow-y: auto; background-color: var(--paper-raised); border-right: 1px solid var(--rule); padding: 25px; padding-bottom: 50px; box-shadow: 2px 0 10px rgba(38,51,62,0.03); } 
    .main-panel-custom { height: 85vh; overflow-y: auto; padding: 20px; background-color: var(--paper); } 
    [id$='-ext_map'], [id$='-interactive_map'], [id$='-lulc_pred_map'], [id$='-gee_live_map'], [id$='-change_map'], [id$='-trend_map'] { height: 68vh !important; border-radius: 6px; border: 1px solid var(--rule); box-shadow: 0 4px 12px rgba(38,51,62,0.05); } 
    .step-card { background: var(--paper-raised); border: 1px solid var(--rule); border-radius: 6px; padding: 20px; margin-bottom: 20px; transition: all 0.3s cubic-bezier(0.25, 0.8, 0.25, 1); box-shadow: 0 2px 5px rgba(38,51,62,0.02); } 
    .step-card:hover { box-shadow: 0 8px 20px rgba(38,51,62,0.08); transform: translateY(-3px); }
    .step-title { margin-top: 0; color: var(--ink); font-family: var(--mono); font-size: 13px; font-weight: 600; letter-spacing: 0.02em; border-bottom-width: 2px !important; border-bottom-style: solid !important; border-bottom-color: var(--rule); text-decoration: none !important; box-shadow: none !important; padding-bottom: 8px; margin-bottom: 15px; display: flex; align-items: center; gap: 8px; } 
    .info-icon { cursor: pointer; color: #4a83c4; margin-left: auto; position: relative; display: inline-block; font-size: 13px; transition: color 0.2s;} 
    .info-icon:hover { color: var(--ink); }
    .info-icon .tooltip-text { visibility: hidden; width: 280px; background-color: var(--ink); color: #e8ecee; text-align: left; border-radius: 6px; padding: 10px; font-size: 11px; position: absolute; z-index: 9999; top: 150%; right: -5px; opacity: 0; transition: opacity 0.2s ease-in-out, transform 0.2s ease; transform: translateY(-5px); box-shadow: 0 4px 12px rgba(38,51,62,0.2);} 
    .info-icon:hover .tooltip-text { visibility: visible; opacity: 1; transform: translateY(0); } 
    .btn { transition: all 0.2s cubic-bezier(0.25, 0.8, 0.25, 1); font-weight: 500; font-family: var(--mono); border-radius: 3px; letter-spacing: 0.01em; }
    .btn:hover { transform: translateY(-2px); box-shadow: 0 6px 12px rgba(38,51,62,0.15); }
    .btn:active { transform: translateY(1px) !important; box-shadow: 0 2px 4px rgba(38,51,62,0.1) !important; }
    .btn-primary:hover { box-shadow: 0 6px 15px rgba(28, 43, 54, 0.3); }
    .btn-success:hover { box-shadow: 0 6px 15px rgba(69, 147, 111, 0.3); }
    .btn-info:hover    { box-shadow: 0 6px 15px rgba(74, 131, 196, 0.3); }
    .btn-warning:hover { box-shadow: 0 6px 15px rgba(193, 104, 59, 0.3); }
    .btn-danger:hover  { box-shadow: 0 6px 15px rgba(139, 58, 43, 0.3); }
    .btn-custom { width: 100%; margin-bottom: 10px; padding: 10px; }
    /* ===== BUG 3: GLOBAL BUTTON UNIFORMITY =====
       Every action button (sidebar 'Run …' primaries, the map-view controls, the
       header utility buttons, Save/Export, etc.) adopts the SAME geometry,
       typography, hover and active feel as the primary module buttons —
       regardless of any inline style= set at its call site. Semantic accent
       COLOURS (navy primary, green success, blue info, amber warning) are kept
       deliberately as affordances; only shape / font / weight / motion are
       unified. !important is required because many buttons still carry legacy
       inline overrides (border-radius:6px/20px, font-weight:bold, default font). */
    .btn, .btn.btn-custom, .action-button {
      font-family: var(--mono) !important;
      font-weight: 600 !important;
      border-radius: 3px !important;
      letter-spacing: 0.01em !important;
      transition: all 0.2s cubic-bezier(0.25,0.8,0.25,1) !important;
    }
    .btn:hover, .action-button:hover {
      transform: translateY(-2px) !important;
      box-shadow: 0 6px 14px rgba(38,51,62,0.18) !important;
    }
    .btn:active, .action-button:active {
      transform: translateY(1px) !important;
      box-shadow: 0 2px 4px rgba(38,51,62,0.12) !important;
    }
    table.dataTable tbody tr td input { color: var(--ink) !important; background-color: var(--paper-raised) !important; border: 1px solid #4a83c4 !important; font-weight: 500 !important; padding: 2px 5px !important; border-radius: 4px; transition: border-color 0.2s; }
    table.dataTable tbody tr td input:focus { outline: none; border-color: var(--forest) !important; box-shadow: 0 0 5px rgba(69, 147, 111, 0.4); }
    .protect-wrap::after, .leaflet-container::after, .shiny-plot-output::after, .secure-content::after {
      content: ''; position: absolute; top: 0; left: 0; right: 0; bottom: 0; z-index: 650; pointer-events: none;
      background-image: url(\"data:image/svg+xml,%3Csvg xmlns='http://www.w3.org/2000/svg' width='300' height='300'%3E%3Cg transform='rotate(-30 150 150)'%3E%3Ccircle cx='92' cy='156' r='8' fill='%2326333e' opacity='0.11'/%3E%3Ccircle cx='92' cy='156' r='3' fill='%2345936f' opacity='0.14'/%3E%3Ctext x='108' y='163' fill='%2326333e' opacity='0.11' font-size='21' font-weight='700' font-family='IBM Plex Sans, Arial, sans-serif'%3EGisforus%3C/text%3E%3C/g%3E%3C/svg%3E\");
      background-repeat: repeat;
    }
    .shiny-plot-output, .leaflet-container, .protect-wrap, .secure-content { position: relative !important; }
    .glass-overlay { display: none !important; }
    .map-title { background: rgba(255,255,255,0.92); backdrop-filter: blur(6px); padding: 5px 13px; border: 1px solid var(--rule); border-radius: 20px; font-family: var(--sans); font-weight: 600; font-size: 11.5px; color: var(--ink); box-shadow: 0 3px 10px rgba(38,51,62,0.12); letter-spacing: 0.01em; }
    .north-arrow { width: 32px; height: 32px; background: rgba(255,255,255,0.92); border-radius: 50%; padding: 2px; border: 1px solid var(--rule); box-shadow: 0 3px 8px rgba(38,51,62,0.12); }
    img, svg, canvas { -webkit-user-drag: none; -khtml-user-drag: none; -moz-user-drag: none; -o-user-drag: none; }
    .custom-popup .leaflet-popup-content-wrapper { background-color: var(--paper-raised); border-radius: 6px; box-shadow: 0 6px 20px rgba(38,51,62,0.2); padding: 0; overflow: hidden; }
    .custom-popup .leaflet-popup-content { margin: 0; line-height: 1.2; }
    .custom-popup .leaflet-popup-close-button { color: #fff !important; font-weight: bold; margin-top: 4px; margin-right: 4px; }
    pre.flowchart-box { background-color: var(--ink); color: #8fb5a0; padding: 15px; border-radius: 6px; font-family: var(--mono); font-size: 12px; overflow-x: auto; line-height: 1.4; margin-top:10px; box-shadow: inset 0 2px 5px rgba(0,0,0,0.5); }
    .login-wrap { max-width: 380px; margin: 80px auto; background: var(--paper-raised, #ffffff); border: 1px solid var(--rule); border-radius: 10px; padding: 32px; box-shadow: 0 8px 24px rgba(38,51,62,0.08); }
    .login-wrap h3 { font-family: var(--mono); font-weight: 700; color: var(--ink); font-size: 19px; margin: 0 0 4px; text-align: center; }
    .login-wrap .login-subtitle { text-align: center; color: #5c6b73; font-size: 12.5px; margin-bottom: 22px; }
    .login-wrap input { width: 100%; padding: 11px 13px; margin-bottom: 10px; border: 1px solid var(--rule); border-radius: 6px; box-sizing: border-box; font-family: var(--mono); font-size: 13.5px; background: var(--paper, #f7f6f2); transition: border-color 0.2s; }
    .login-wrap input:focus { outline: none; border-color: var(--forest); }
    .login-wrap button { width: 100%; padding: 11px; margin-bottom: 10px; border: none; border-radius: 6px; font-weight: 600; cursor: pointer; font-family: var(--mono); font-size: 13px; letter-spacing: 0.01em; transition: all 0.2s; }
    .login-wrap button:hover { transform: translateY(-1px); box-shadow: 0 4px 10px rgba(38,51,62,0.15); }
    .btn-login-email { background: var(--ink); color: white; }
    .btn-login-google { background: white; color: var(--ink); border: 1px solid var(--rule) !important; display:flex; align-items:center; justify-content:center; gap:8px; }
    .btn-login-secondary { background: transparent; color: var(--forest); text-decoration: none; font-size: 12px !important; padding: 6px !important; margin-bottom: 4px !important; }
    .login-wrap hr { border: none; border-top: 1px solid var(--rule); margin: 16px 0; }
    #login-error { color: var(--danger-deep); font-size: 13px; min-height: 18px; margin-bottom: 8px; }

    /* =========================================================================
       🚀 MOBILE-FRIENDLY LAYOUT — everything below only applies at <=767px
       (Bootstrap's own 'xs/sm' breakpoint), so desktop/tablet layout is
       completely untouched. Goals: sidebar+map stack vertically instead of
       side-by-side, fixed-height panels become natural-height (no awkward
       nested-scrollbars), fixed-corner badges don't overlap each other or the
       header, buttons/inputs get real touch-target sizing, and wide tables
       scroll horizontally instead of overflowing the screen.
       ========================================================================= */
    @media (max-width: 767px) {
      /* Sidebar and map/results panel: stop forcing a fixed 85vh height with
         internal scroll (that pattern works for a wide desktop layout with
         two side-by-side columns, but on a single narrow mobile column it
         creates a confusing scroll-within-a-scroll). Let each panel size to
         its own content instead, with a sane minimum so it's not cramped. */
      .sidebar-panel-custom, .main-panel-custom {
        height: auto !important;
        max-height: none !important;
        overflow-y: visible !important;
        padding: 14px !important;
        border-right: none !important;
      }

      /* Leaflet maps: 68vh was tuned for a desktop 8/12-width column: on a
         full-width mobile column that's often taller than the available
         viewport once the header/nav are accounted for. 50vh keeps the map
         genuinely usable (enough room to pan/zoom with a thumb) without
         pushing the rest of the page too far down. */
      [id$='-ext_map'], [id$='-interactive_map'], [id$='-lulc_pred_map'], [id$='-gee_live_map'], [id$='-change_map'], [id$='-trend_map'] {
        height: 50vh !important;
      }

      /* Step-cards: less padding so more usable width remains for inputs on
         a narrow screen; hover-lift animation (translateY) is a desktop-only
         mouse-hover affordance that doesn't apply to touch, and disabling it
         avoids a slightly janky tap-flash on some touch browsers. */
      .step-card { padding: 14px !important; margin-bottom: 14px !important; }
      .step-card:hover { transform: none !important; }

      /* Every text input / select / button gets a real touch target (Apple/
         Google guidance is ~44px minimum) — the desktop sizing (10-11px
         padding on a 13px font) comes out closer to 32-34px, comfortable
         with a mouse cursor but fiddly with a fingertip. */
      .btn, .form-control, .selectize-input, input[type='text'], input[type='email'],
      input[type='password'], input[type='number'], select {
        min-height: 44px !important;
        font-size: 15px !important;
      }
      .btn-sm { min-height: 38px !important; }

      /* Header bar: the GEE-status / Data-Clipboard / action-buttons row is a
         3-way flex layout tuned for a wide desktop navbar — on a phone it
         needs to WRAP onto multiple lines rather than squishing every button
         into an unreadable sliver. */
      .navbar > .container-fluid > div[style*='background-color: #26333e'] {
        flex-direction: column !important;
        align-items: stretch !important;
        gap: 10px !important;
        padding: 14px !important;
      }
      .navbar > .container-fluid > div[style*='background-color: #26333e'] > div {
        flex: none !important;
        justify-content: center !important;
        flex-wrap: wrap !important;
        gap: 8px !important;
      }
      .navbar > .container-fluid > div[style*='background-color: #26333e'] .btn {
        flex: 1 1 auto !important;
        min-width: 120px !important;
      }

      /* Fixed-position corner badges (version tag, session timer, signed-in
         email): on desktop these safely stack in the top-right corner above
         the header. On a narrow phone screen, stacking them at fixed pixel
         offsets can overlap the navbar-brand/title or each other — instead,
         let them flow as a single compact row UNDER the brand, and hide the
         least essential one (the 'v3.0' version tag) below a certain width
         to keep the row from wrapping awkwardly. */
      #corner-badges { bottom: 6px !important; gap: 6px !important; }
      #version-badge { display: none !important; }

      /* Login form: fixed max-width + large auto margins were sized for a
         desktop modal; on mobile the modal itself is already full-width, so
         let the form fill it rather than leaving cramped side-gutters. */
      .login-wrap { max-width: 100% !important; margin: 10px !important; padding: 20px !important; }

      /* DataTables: force horizontal scrolling instead of letting a wide
         table overflow the screen edge (many result tables in this app have
         5+ columns that don't fit a phone's width no matter the font size). */
      .dataTables_wrapper { overflow-x: auto !important; }

      /* Modals: use nearly the full viewport width/height on mobile instead
         of Bootstrap's fixed desktop modal widths (size='l'/'m'), which can
         otherwise leave the dialog narrower than useful or force its own
         horizontal scrollbar. */
      .modal-dialog { width: 94vw !important; max-width: 94vw !important; margin: 8px auto !important; }
      .modal-body { max-height: 70vh !important; overflow-y: auto !important; }

      /* Plot/table export card grids inside modals (Export Manager, e.g.)
         often use fluidRow/column side-by-side buttons — stack them for
         easier tapping. */
      .modal-body .row > div[class*='col-'] { width: 100% !important; }

      /* Tab labels in the top navbar can wrap or truncate awkwardly at small
         sizes — tighten spacing and allow smaller (but still legible) text
         so all 5 module tabs stay reachable without horizontal scrolling. */
      .navbar-nav .nav-item .nav-link { padding: 8px 10px !important; font-size: 12px !important; margin: 0 2px !important; }
    }
  ")),
  # =====================================================================
  # MOBILE ENHANCEMENTS v2 — full column stacking, un-clipped dropdowns,
  # responsive charts, horizontally-scrollable nav, and a map "tap to
  # interact" overlay that stops the map trapping page-scroll on touch.
  # Additive to the media block above; touches nothing on desktop.
  # =====================================================================
  tags$style(HTML("
    /* Map tap-to-interact overlay + lock chip (base; only shown on mobile below).
       While the overlay is present the map can't pan, so a downward swipe scrolls
       the PAGE instead of getting trapped; tapping wakes the map; the chip re-arms it. */
    .gf-map-tap { position: absolute; inset: 0; z-index: 1200; display: none;
      align-items: flex-end; justify-content: center; padding-bottom: 14px;
      background: transparent; pointer-events: auto; cursor: pointer; }
    .gf-map-tap span { background: rgba(38,51,62,0.88); color: #fff; font-family: var(--sans);
      font-size: 12.5px; font-weight: 600; padding: 7px 15px; border-radius: 20px;
      box-shadow: 0 3px 12px rgba(0,0,0,0.28); }
    .gf-map-lock { position: absolute; top: 10px; left: 50%; transform: translateX(-50%);
      z-index: 1250; display: none; background: rgba(38,51,62,0.9); color: #fff; border: none;
      border-radius: 16px; padding: 6px 14px; font-size: 12px; font-weight: 600;
      font-family: var(--sans); box-shadow: 0 3px 10px rgba(0,0,0,0.25); min-height: 34px; }

    @media (max-width: 767.98px) {
      /* 1. FULL COLUMN STACKING — force sidebar + main panels to 100% width so they
         stack on ALL phones/tablet-portrait (col-sm-* alone only stacks below 576px). */
      .sidebar-panel-custom, .main-panel-custom {
        width: 100% !important; max-width: 100% !important; flex: 0 0 100% !important;
      }
      .container-fluid > .row, .tab-pane > .row { margin-left: 0 !important; margin-right: 0 !important; }

      /* 2. UN-CLIP DROPDOWNS — the 38+ index multi-select and other selectize menus
         must float above content and never be clipped by a scroll container. */
      .selectize-dropdown { z-index: 3000 !important; }
      .selectize-input { flex-wrap: wrap !important; }
      .shiny-input-container, .selectize-control { width: 100% !important; }

      /* 3. RESPONSIVE CHARTS/IMAGES — plots shrink to their container width. */
      .shiny-plot-output img, .shiny-image-output img { max-width: 100% !important; height: auto !important; }
      .plotly, .js-plotly-plot { width: 100% !important; }

      /* 4. MAIN NAV — the native BS5 collapse/toggler is unreliable inside this navbarPage,
         so on mobile we HIDE the native tab strip and drive the REAL (hidden) tab links from
         a custom, always-reliable hamburger + slide-in menu (built by the script below).
         A menu tap programmatically clicks the genuine Shiny nav-link, so tab switching uses
         the real mechanism — no dependence on Bootstrap's collapse JS. */
      .navbar-nav { display: none !important; }
      .navbar-toggler { display: none !important; }

      /* 5. MAP tap-to-interact overlay active only on mobile. */
      .gf-map-shell .gf-map-tap { display: flex; }
      .gf-map-shell.gf-map-live .gf-map-tap { display: none; }
      .gf-map-shell.gf-map-live .gf-map-lock { display: block; }

      /* 6. Keep the Insights drawer from covering the smaller mobile map. */
      .gf-ins-body { max-height: calc(50vh - 24px) !important; width: 200px !important; }

      /* 7. UTILITY / ACTION BAR — was overflowing the viewport (no wrap, forcing a
         zoom-out). Target it ROBUSTLY by its inline style (the earlier navbar>container
         path never matched) and let it WRAP onto centred rows. */
      div[style*='background-color: #26333e'][style*='justify-content: space-between'] {
        flex-wrap: wrap !important; flex-direction: row !important; justify-content: center !important;
        align-items: center !important; gap: 6px !important; row-gap: 8px !important;
        padding: 8px !important; margin-bottom: 12px !important;
      }
      div[style*='background-color: #26333e'][style*='justify-content: space-between'] > div {
        flex: 0 1 auto !important; justify-content: center !important; flex-wrap: wrap !important; gap: 6px !important;
      }
      /* 8. THINNER utility buttons on mobile — retain their distinct background colours
         (we only touch padding / size), overriding the global 44px min-height for these. */
      div[style*='background-color: #26333e'][style*='justify-content: space-between'] .btn {
        padding: 5px 10px !important; font-size: 11px !important; min-height: 0 !important;
        min-width: 0 !important; width: auto !important; border-radius: 6px !important;
        line-height: 1.2 !important; margin: 0 !important;
      }

      /* 9. Safety net — nothing may force the page itself wider than the screen (which is
         what triggered the zoom-out). Inner scrollers (tables, nav strip) keep their own
         overflow-x, so this only kills PAGE-level horizontal scroll. */
      body { overflow-x: hidden !important; }
    }
  ")),
  tags$script(HTML("
    (function(){
      function isMobile(){ return window.matchMedia('(max-width: 767.98px)').matches; }
      function armShell(shell){
        if (shell.getAttribute('data-gf-armed')) return;
        shell.setAttribute('data-gf-armed','1');
        var ov = document.createElement('div');
        ov.className = 'gf-map-tap';
        ov.innerHTML = '<span>Tap map to interact</span>';
        var lock = document.createElement('button');
        lock.type = 'button'; lock.className = 'gf-map-lock'; lock.textContent = 'Lock map';
        shell.appendChild(ov); shell.appendChild(lock);
        var wake = function(){ shell.classList.add('gf-map-live'); };
        ov.addEventListener('click', wake);
        ov.addEventListener('touchend', function(){ wake(); }, {passive:true});
        lock.addEventListener('click', function(e){ e.stopPropagation(); shell.classList.remove('gf-map-live'); });
      }
      function scan(){
        if (!isMobile()) return;
        document.querySelectorAll('.gf-map-shell').forEach(armShell);
        // Collapse the Insights drawer once on mobile so it doesn't cover the map.
        document.querySelectorAll('.gf-ins-toggle').forEach(function(t){
          if (!t.getAttribute('data-gf-mobinit')) { t.setAttribute('data-gf-mobinit','1'); t.checked = false; }
        });
      }
      document.addEventListener('shiny:value', function(){ setTimeout(scan, 250); });
      $(document).on('shiny:connected', function(){ setTimeout(scan, 500); });
      window.addEventListener('resize', function(){ setTimeout(scan, 200); });
      setInterval(scan, 2500);
      scan();
    })();
  ")),
  # =====================================================================
  # MOBILE NAV (custom hamburger + slide-in menu) + SIDEBAR ACCORDION.
  # Reliable, self-contained: the menu drives the REAL Shiny tab links, and
  # each sidebar step-card collapses to save vertical space on phones.
  # =====================================================================
  tags$style(HTML("
    /* Custom hamburger + slide-in menu (shown only on mobile via the @media rule). */
    .gf-hamburger { display: none; position: fixed; top: 12px; right: 12px; z-index: 100001;
      width: 44px; height: 44px; border: none; border-radius: 9px; background: rgba(255,255,255,0.14);
      color: #fff; font-size: 20px; align-items: center; justify-content: center; box-shadow: 0 2px 8px rgba(0,0,0,0.25); }
    .gf-navmenu { display: none; position: fixed; top: 0; left: 0; height: 100%; width: 76%; max-width: 320px;
      background: var(--ink); z-index: 100002; transform: translateX(-100%); transition: transform .25s ease;
      box-shadow: 4px 0 24px rgba(0,0,0,0.4); flex-direction: column; padding: 16px 0 24px; overflow-y: auto; }
    .gf-navmenu.gf-open { transform: translateX(0); }
    .gf-navmenu-head { color:#fff; font-family: var(--mono); font-weight:700; font-size:14px; letter-spacing:0.04em;
      padding: 6px 20px 14px; border-bottom:1px solid rgba(255,255,255,0.12); margin-bottom:8px; }
    .gf-navmenu a.gf-navitem { display:block; color:#e8ecee; text-decoration:none; font-family: var(--sans);
      font-size:15px; font-weight:600; padding: 14px 20px; border-left: 3px solid transparent; }
    .gf-navmenu a.gf-navitem.gf-active { background: rgba(69,147,111,0.18); border-left-color: var(--forest); color:#fff; }
    .gf-navbackdrop { display:none; position:fixed; inset:0; background: rgba(0,0,0,0.45); z-index:100000; }
    .gf-navbackdrop.gf-open { display:block; }

    @media (max-width: 767.98px) {
      .gf-hamburger { display: flex; }
      .gf-navmenu { display: flex; }
      /* Sidebar accordion — tap a step title to expand/collapse (first card starts open). */
      .sidebar-panel-custom .step-card > .step-title { cursor: pointer; }
      .sidebar-panel-custom .step-card > .step-title::before {
        content: '\\25BE'; margin-right: 8px; font-size: 11px; color: var(--slate);
        display: inline-block; transition: transform .2s ease; }
      .sidebar-panel-custom .step-card.gf-collapsed > .step-title::before { transform: rotate(-90deg); }
      .sidebar-panel-custom .step-card.gf-collapsed > *:not(.step-title) { display: none !important; }
    }
  ")),
  tags$script(HTML("
    (function(){
      // ---- Custom hamburger nav: mirror the real (hidden) tab links ----
      function buildNav(){
        if (document.getElementById('gf-hamburger')) return true;
        var links = document.querySelectorAll('.navbar-nav .nav-link');
        if (!links.length) return false;
        var hb = document.createElement('button');
        hb.id='gf-hamburger'; hb.className='gf-hamburger'; hb.type='button';
        hb.setAttribute('aria-label','Menu'); hb.innerHTML='&#9776;';
        var bd = document.createElement('div'); bd.className='gf-navbackdrop';
        var menu = document.createElement('div'); menu.className='gf-navmenu';
        menu.innerHTML='<div class=\"gf-navmenu-head\">GISFORUS — Menu</div>';
        document.body.appendChild(hb); document.body.appendChild(bd); document.body.appendChild(menu);
        function close(){ menu.classList.remove('gf-open'); bd.classList.remove('gf-open'); }
        function rebuild(){
          menu.querySelectorAll('a.gf-navitem').forEach(function(a){ a.remove(); });
          document.querySelectorAll('.navbar-nav .nav-link').forEach(function(link){
            var a = document.createElement('a'); a.className='gf-navitem'; a.href='#';
            a.textContent=(link.textContent||'').trim();
            if (link.classList.contains('active')) a.classList.add('gf-active');
            a.addEventListener('click', function(e){ e.preventDefault(); link.click(); close(); });
            menu.appendChild(a);
          });
        }
        hb.addEventListener('click', function(){ rebuild(); menu.classList.add('gf-open'); bd.classList.add('gf-open'); });
        bd.addEventListener('click', close);
        rebuild();
        return true;
      }
      var t=0, iv=setInterval(function(){ t++; if (buildNav() || t>50) clearInterval(iv); }, 300);
      $(document).on('shiny:connected', buildNav);

      // ---- Sidebar accordion (mobile only) ----
      function isMobile(){ return window.matchMedia('(max-width: 767.98px)').matches; }
      function initAccordion(){
        if (!isMobile()) return;
        document.querySelectorAll('.sidebar-panel-custom').forEach(function(sb){
          var cards = sb.querySelectorAll(':scope > .step-card');
          cards.forEach(function(card, i){
            if (card.getAttribute('data-gf-acc')) return;
            card.setAttribute('data-gf-acc','1');
            if (i > 0) card.classList.add('gf-collapsed');
            var title = card.querySelector(':scope > .step-title');
            if (title) title.addEventListener('click', function(e){
              if (e.target.closest('.info-icon')) return;   // let the tooltip icon work
              card.classList.toggle('gf-collapsed');
            });
          });
        });
      }
      $(document).on('shiny:connected', function(){ setTimeout(initAccordion, 600); });
      document.addEventListener('shiny:value', function(){ setTimeout(initAccordion, 300); });
      window.addEventListener('resize', function(){ setTimeout(initAccordion, 200); });
      setInterval(initAccordion, 3000);
    })();
  ")),
  # =====================================================================
  # MOBILE FIX PACK v3 — (a) brand + scroll the bslib stats accordion so no
  # panel is hidden, (b) make Leaflet map legends collapsible, (c) wrap charts
  # in a horizontal scroller so axis labels aren't crushed on narrow screens.
  # All additive; desktop is untouched (chart wrap + collapse default are
  # gated on the mobile media query / matchMedia).
  # =====================================================================
  tags$style(HTML("
    /* (a) bslib accordion (stats sidebar): brand the header bars so every panel
       is clearly visible and comfortably tappable — the previous 'blank panels'
       were collapsed bodies that never expanded; now all open by default. */
    .accordion-button { font-weight: 600; color: #26333e; padding: 12px 14px; }
    .accordion-button:not(.collapsed) { color: #26333e; background-color: #eef2f0; box-shadow: none; }
    .accordion-button:focus { box-shadow: none; border-color: rgba(69,147,111,0.45); }
    .accordion-item { border-color: var(--rule); }

    /* (b) collapsible Leaflet legend */
    .gf-legend-toggle { cursor: pointer; font-weight: 700; font-size: 11px; color: #26333e;
      font-family: var(--sans); user-select: none; display: flex; align-items: center; gap: 5px; }
    .gf-legend-toggle::before { content: '\\25BE'; font-size: 9px; transition: transform .2s ease; }
    .info.legend.gf-legend-collapsed .gf-legend-toggle::before { transform: rotate(-90deg); }
    .info.legend.gf-legend-collapsed .gf-legend-body { display: none !important; }
    .info.legend.gf-legend-collapsed { padding: 6px 10px !important; }

    /* (c) chart horizontal scroller (base; min-width applied on mobile only) */
    .gf-chart-scroll { max-width: 100%; }

    @media (max-width: 767.98px) {
      /* off-canvas sidebar drawer must scroll so all 5 accordion panels are reachable */
      .bslib-sidebar-layout > .sidebar .sidebar-content { max-height: 100%; overflow-y: auto; }
      .accordion-button { min-height: 46px; }

      /* legends: cap size and collapse by default so they don't cover the map */
      .info.legend, .leaflet-control.legend { max-width: 62vw; max-height: 42vh; overflow: auto; }

      /* charts: give ggplot a legible render width and let the user swipe.
         min-width is set on the scroller's IMMEDIATE CHILD (the spinner container
         / plot wrapper), NOT on .shiny-plot-output itself — so Shiny measures a
         concrete 560px parent and renders the plot at 100% of it (a real, non-zero
         dimension) instead of us fighting its size calculator. */
      .gf-chart-scroll { overflow-x: auto; -webkit-overflow-scrolling: touch;
        border: 1px solid var(--rule); border-radius: 8px; }
      .gf-chart-scroll > * { min-width: 560px; }
      .gf-chart-scroll::after { content: 'swipe chart \\2194'; display: block; text-align: center;
        font-size: 10px; color: var(--slate); padding: 2px 0 4px; font-family: var(--sans); }
    }
  ")),
  tags$script(HTML("
    (function(){
      function isMobile(){ return window.matchMedia('(max-width: 767.98px)').matches; }
      // ---- (b) Collapsible Leaflet legends (any map, app-wide) ----
      function collapsibleLegends(){
        document.querySelectorAll('.info.legend, .leaflet-control.legend').forEach(function(lg){
          if (lg.getAttribute('data-gf-legend')) return;
          lg.setAttribute('data-gf-legend','1');
          var body = document.createElement('div'); body.className='gf-legend-body';
          while (lg.firstChild) body.appendChild(lg.firstChild);
          var tog = document.createElement('div'); tog.className='gf-legend-toggle'; tog.textContent='Legend';
          lg.appendChild(tog); lg.appendChild(body);
          tog.addEventListener('click', function(e){ e.stopPropagation(); lg.classList.toggle('gf-legend-collapsed'); });
          // BUG 4: collapsibles are collapsed by DEFAULT on initial render, without
          // exception (desktop and mobile alike). The user taps 'Legend' to expand.
          lg.classList.add('gf-legend-collapsed');
        });
      }
      // ---- (c) Chart horizontal scroller is now a NATIVE server-rendered div
      //          (gf-chart-scroll) placed around each plotOutput in the UI. No JS
      //          DOM manipulation: moving a bound plotOutput at runtime (and the
      //          resize dispatch that followed) is what broke Shiny's output binding
      //          / dimension calc and left the plots stuck 'loading' with no data. ----
      function tick(){ try { collapsibleLegends(); } catch(e){} }
      $(document).on('shiny:value', function(){ setTimeout(tick, 250); });
      $(document).on('shiny:connected', function(){ setTimeout(tick, 800); });
      window.addEventListener('resize', function(){ setTimeout(collapsibleLegends, 200); });
      setInterval(tick, 2500);
    })();
  ")),
  tags$div(id = "loading-splash", style = "position:fixed; top:0; left:0; width:100%; height:100%; background:#26333e; z-index:99999; display:flex; align-items:center; justify-content:center; flex-direction:column;",
           tags$div(style = "border:4px solid rgba(255,255,255,0.2); border-top:4px solid #45936f; border-radius:50%; width:50px; height:50px; animation: splash-spin 1s linear infinite;"),
           tags$p("Loading Spatial Research Suite...", style = "color:white; margin-top:20px; font-family:'IBM Plex Mono',monospace; font-size:13px;")
  ),
  tags$style(HTML("@keyframes splash-spin { to { transform: rotate(360deg); } }")),
  tags$script(HTML("
    $(document).on('shiny:connected', function() {
      var splash = document.getElementById('loading-splash');
      if (splash) { splash.style.display = 'none'; }
    });
  ")),
  # =====================================================================
  # PREMIUM MAP PANE + DYNAMIC INSIGHTS DASHBOARD  (uniform, app-wide)
  # One block, injected once, standardises every map pane into a premium
  # GIS-dashboard surface and styles the floating Insights drawer
  # (mod_insights.R). Purely presentational — no R logic depends on it.
  # =====================================================================
  tags$style(HTML("
    /* ---- 1. UNIFORM PREMIUM MAP PANE (every leaflet map, app-wide) ---- */
    .gf-map-shell { position: relative; }
    .leaflet-container {
      border-radius: 10px !important;
      border: 1px solid var(--rule) !important;
      box-shadow: 0 10px 30px rgba(38,51,62,0.12), inset 0 0 0 1px rgba(255,255,255,0.5) !important;
      background: #eef1f0 !important;
      font-family: var(--mono) !important;
    }
    /* the app already sizes specific maps by id; keep that, just refine chrome */
    [id$='-ext_map'], [id$='-interactive_map'], [id$='-lulc_pred_map'],
    [id$='-gee_live_map'], [id$='-change_map'], [id$='-trend_map'] {
      border-radius: 10px; border: 1px solid var(--rule);
      box-shadow: 0 10px 30px rgba(38,51,62,0.12);
    }
    .leaflet-control-zoom a {
      border-radius: 6px !important; color: var(--ink) !important;
      border: 1px solid var(--rule) !important; box-shadow: 0 2px 6px rgba(38,51,62,0.12) !important;
      font-weight: 600 !important; transition: all .15s ease;
    }
    .leaflet-control-zoom a:hover { background: var(--forest-soft) !important; color: var(--forest) !important; }
    .leaflet-bar { border: none !important; box-shadow: none !important; }
    .leaflet-control-attribution {
      background: rgba(255,255,255,0.82) !important; backdrop-filter: blur(4px);
      border-radius: 6px 0 0 0 !important; font-size: 9.5px !important; color: var(--slate) !important;
      padding: 2px 7px !important;
    }
    .leaflet-popup-content-wrapper {
      border-radius: 8px !important; box-shadow: 0 8px 24px rgba(38,51,62,0.22) !important;
      border: 1px solid var(--rule) !important;
    }
    .leaflet-control-layers {
      border-radius: 8px !important; border: 1px solid var(--rule) !important;
      box-shadow: 0 6px 18px rgba(38,51,62,0.15) !important; font-family: var(--mono) !important;
      font-size: 12px !important; color: var(--ink) !important;
    }
    .info.legend, .leaflet-control .legend {
      background: rgba(255,255,255,0.94) !important; backdrop-filter: blur(6px);
      border-radius: 8px !important; border: 1px solid var(--rule) !important;
      box-shadow: 0 6px 18px rgba(38,51,62,0.15) !important; font-family: var(--mono) !important;
      color: var(--ink) !important; padding: 10px 12px !important; line-height: 1.5 !important;
    }
    /* Bottom-left stack: scale bar + layer toggle stack cleanly with even spacing, and the
       live coordinate readout sits at the very bottom clear of both — no more mashed-together
       controls. One consistent layout across EVERY map view (not scoped to a single pane). */
    .leaflet-bottom.leaflet-left {
      display: flex; flex-direction: column-reverse; align-items: flex-start;
      gap: 7px; margin-left: 4px; margin-bottom: 36px;
    }
    .leaflet-bottom.leaflet-left .leaflet-control { margin: 0 !important; float: none !important; clear: none !important; }
    .gf-coord-box {
      position: absolute; left: 8px; bottom: 8px; z-index: 700;
      background: rgba(255,255,255,0.92); backdrop-filter: blur(5px);
      padding: 4px 10px; border: 1px solid var(--rule); border-radius: 8px;
      box-shadow: 0 3px 10px rgba(38,51,62,0.12);
      font-family: var(--sans); font-size: 10.5px; font-weight: 600; color: var(--ink);
      letter-spacing: 0.02em; white-space: nowrap; pointer-events: none;
    }

    /* ---- 2. INSIGHTS DASHBOARD — sleek collapsible widget (mod_insights.R) ----
       Sits top-right at a high z-index; native leaflet controls are pushed to the
       other corners (layers+scalebar bottom-left, north-arrow+attribution bottom-
       right) via R, so the panel never overlaps them. Sans-serif throughout. */
    .gf-insights { position: absolute; top: 12px; right: 12px; z-index: 10000;
      font-family: var(--sans); pointer-events: none; }
    .gf-insights > * { pointer-events: auto; }
    .gf-ins-toggle { position: absolute; opacity: 0; width: 0; height: 0; pointer-events: none; }
    .gf-ins-tab { display: none; align-items: center; gap: 7px; cursor: pointer;
      background: rgba(38,51,62,0.95); backdrop-filter: blur(8px); color: #fff;
      padding: 8px 13px; border-radius: 20px; font-size: 12px; font-weight: 600;
      box-shadow: 0 6px 16px rgba(38,51,62,0.28); border: 1px solid rgba(255,255,255,0.12);
      transition: transform .15s ease, box-shadow .15s ease; }
    .gf-ins-tab:hover { transform: translateY(-1px); box-shadow: 0 10px 22px rgba(38,51,62,0.35); }
    .gf-ins-tab-ico { color: var(--forest); font-size: 11px; }
    .gf-ins-toggle:not(:checked) ~ .gf-ins-tab { display: inline-flex; }
    .gf-ins-toggle:checked ~ .gf-ins-body { display: flex; }
    .gf-ins-toggle:not(:checked) ~ .gf-ins-body { display: none; }
    /* border matches the map pane exactly (10px radius, 1px var(--rule)); height is
       capped to the map pane's own height (68vh) so it never runs longer than the map,
       and the inner scroll area takes the overflow. */
    .gf-ins-body { width: 236px; max-width: 34vw; max-height: calc(68vh - 28px);
      background: rgba(255,255,255,0.95); backdrop-filter: blur(16px) saturate(1.1);
      border: 1px solid var(--rule); border-radius: 10px;
      box-shadow: 0 10px 30px rgba(38,51,62,0.18); overflow: hidden;
      flex-direction: column; animation: gfInsIn .22s cubic-bezier(.25,.8,.25,1); }
    @keyframes gfInsIn { from { opacity: 0; transform: translateY(-6px); } to { opacity: 1; transform: none; } }
    .gf-ins-head { flex: 0 0 auto; display: flex; align-items: center; gap: 8px; padding: 11px 13px;
      background: linear-gradient(135deg, var(--ink) 0%, #33454f 100%); color: #fff; }
    .gf-ins-title { font-size: 12.5px; font-weight: 700; letter-spacing: 0.01em; flex: 1; font-family: var(--sans); }
    .gf-ins-eng { font-size: 8.5px; font-weight: 700; color: #fff; padding: 2px 7px;
      border-radius: 10px; letter-spacing: 0.04em; text-transform: uppercase; white-space: nowrap; }
    .gf-ins-close { cursor: pointer; color: rgba(255,255,255,0.75); font-size: 19px;
      line-height: 1; margin: 0; padding: 0 2px; font-weight: 400; transition: color .15s; }
    .gf-ins-close:hover { color: #fff; }
    .gf-ins-scroll { flex: 1 1 auto; min-height: 0; overflow-y: auto; padding: 11px; display: flex; flex-direction: column; gap: 10px; }
    .gf-ins-scroll::-webkit-scrollbar { width: 6px; }
    .gf-ins-scroll::-webkit-scrollbar-thumb { background: rgba(92,107,115,0.33); border-radius: 4px; }
    .gf-ins-card { background: #fff; border: 1px solid var(--rule); border-radius: 10px;
      padding: 11px 12px; box-shadow: 0 1px 4px rgba(38,51,62,0.05); }
    .gf-ins-card-h { font-size: 9.5px; font-weight: 700; letter-spacing: 0.08em; text-transform: uppercase;
      color: var(--slate); margin-bottom: 8px; padding-bottom: 6px; border-bottom: 1px solid var(--paper);
      font-family: var(--sans); }
    .gf-ins-empty { font-size: 11.5px; color: var(--slate); line-height: 1.5; }
    .gf-ins-hero { text-align: left; }
    .gf-ins-hero-num { font-size: 25px; font-weight: 700; color: var(--ink); line-height: 1.05; letter-spacing: -0.02em; font-variant-numeric: tabular-nums; }
    .gf-ins-hero-lab { font-size: 10.5px; color: var(--slate); margin-top: 1px; }
    .gf-ins-sub { font-size: 10.5px; color: var(--forest); font-weight: 600; margin: 7px 0 9px; }
    .gf-ins-rows { display: flex; flex-direction: column; gap: 5px; }
    .gf-ins-row { display: flex; align-items: center; gap: 8px; font-size: 11px; }
    .gf-ins-chip { width: 11px; height: 11px; border-radius: 3px; flex-shrink: 0;
      box-shadow: inset 0 0 0 1px rgba(0,0,0,0.12); }
    .gf-ins-row-name { color: var(--ink); flex: 1; white-space: nowrap; overflow: hidden; text-overflow: ellipsis; }
    .gf-ins-row-val { color: var(--slate); font-weight: 600; white-space: nowrap; font-variant-numeric: tabular-nums; }
    .gf-ins-grid { display: grid; grid-template-columns: 1fr 1fr; gap: 7px; margin-top: 9px; }
    .gf-ins-cell { background: var(--paper); border-radius: 8px; padding: 7px 8px; text-align: center; }
    .gf-ins-cell-v { font-size: 14px; font-weight: 700; color: var(--ink); font-variant-numeric: tabular-nums; }
    .gf-ins-cell-l { font-size: 8.5px; color: var(--slate); text-transform: uppercase; letter-spacing: 0.05em; margin-top: 1px; }
    .gf-ins-ctx-rows { display: flex; flex-direction: column; gap: 5px; }
    .gf-ins-ctx-row { display: flex; align-items: baseline; justify-content: space-between; gap: 8px; font-size: 11px; }
    .gf-ins-ctx-lab { color: var(--slate); text-transform: uppercase; letter-spacing: 0.05em; font-size: 8.5px; font-weight: 700; }
    .gf-ins-ctx-val { color: var(--ink); font-weight: 600; text-align: right; font-variant-numeric: tabular-nums; }
    .gf-ins-bbox { margin-top: 10px; padding-top: 9px; border-top: 1px solid var(--paper); }
    .gf-ins-bbox-h { font-size: 8.5px; font-weight: 700; letter-spacing: 0.05em; text-transform: uppercase; color: var(--slate); margin-bottom: 6px; }
    .gf-ins-bbox-grid { display: grid; grid-template-columns: 1fr 1fr; gap: 6px; }
    .gf-ins-bbox-cell { display: flex; align-items: baseline; gap: 6px; background: var(--paper); border-radius: 6px; padding: 5px 8px; }
    .gf-ins-bbox-dir { font-size: 9px; font-weight: 700; color: var(--forest); width: 10px; }
    .gf-ins-bbox-deg { font-size: 11px; color: var(--ink); font-weight: 600; font-variant-numeric: tabular-nums; }
    @media (max-width: 900px) {
      .gf-ins-body { width: 210px; max-width: 62vw; max-height: calc(50vh - 24px); }
      .gf-insights { top: 10px; right: 10px; }
    }

    /* ---- 3. MODULE TAB BAR — clean horizontal scrollable segmented control ----
       Scoped to .main-panel-custom so the top navbar tabs are untouched. Replaces
       the wrapping green-text default with a single non-wrapping row + underline. */
    .main-panel-custom .nav-tabs {
      flex-wrap: nowrap; overflow-x: auto; overflow-y: hidden;
      border-bottom: 1px solid var(--rule); gap: 2px; padding-bottom: 0;
      scrollbar-width: thin; -webkit-overflow-scrolling: touch;
    }
    .main-panel-custom .nav-tabs::-webkit-scrollbar { height: 5px; }
    .main-panel-custom .nav-tabs::-webkit-scrollbar-thumb { background: rgba(92,107,115,0.3); border-radius: 3px; }
    .main-panel-custom .nav-tabs .nav-item { flex: 0 0 auto; margin-bottom: 0; }
    .main-panel-custom .nav-tabs .nav-link {
      white-space: nowrap; border: none !important; border-radius: 8px 8px 0 0;
      font-family: var(--sans) !important; font-size: 13px !important; font-weight: 600 !important;
      color: var(--slate) !important; padding: 9px 15px !important; margin: 0 !important; letter-spacing: 0;
      border-bottom: 2px solid transparent !important; transition: all .15s ease; box-shadow: none !important;
    }
    .main-panel-custom .nav-tabs .nav-link::after { display: none !important; }
    .main-panel-custom .nav-tabs .nav-link:hover { color: var(--ink) !important; background: var(--forest-soft); }
    .main-panel-custom .nav-tabs .nav-link.active {
      color: var(--forest) !important; background: transparent !important;
      border-bottom: 2px solid var(--forest) !important;
    }

    /* ---- 4. FIT & FINISH — consistent spacing, sizing, alignment ---- */
    /* Header toolbar: vertically-centre the three flex sections and give every
       action button the same height + centred label for an even, aligned row. */
    .navbar div[style*='background-color: #26333e'] > div { align-items: center; }
    .navbar div[style*='background-color: #26333e'] .btn {
      min-height: 38px; display: inline-flex; align-items: center; justify-content: center; gap: 6px;
    }
    /* Sidebar action buttons: uniform height + centred multi-line labels. */
    .sidebar-panel-custom .btn-custom {
      min-height: 42px; display: inline-flex; align-items: center; justify-content: center;
      text-align: center; line-height: 1.25;
    }
    /* Even vertical rhythm for step-cards and form controls. */
    .step-card:last-child { margin-bottom: 0; }
    .sidebar-panel-custom .form-group { margin-bottom: 13px; }
    /* Tighter, symmetric gutters for side-by-side inputs inside cards. */
    .step-card .row { margin-left: -6px; margin-right: -6px; }
    .step-card .row > [class*='col-'] { padding-left: 6px; padding-right: 6px; }
    /* Result tab panels: uniform top padding so every tab's content starts level. */
    .main-panel-custom .tab-content > .tab-pane { padding-top: 4px; }
    /* Analysis section headers (h4 inside result panels) unified to one size / weight /
       family across EVERY module, matching the app's base UI typography — no more
       disjointed 16px-vs-14px header sizes between panes. */
    .main-panel-custom h4, .main-panel-custom .h4 {
      font-family: var(--sans) !important; font-size: 15px !important; font-weight: 600 !important;
      color: var(--ink) !important; letter-spacing: 0.01em; line-height: 1.35;
    }
  "))
)
main_navbar_ui <- navbarPage(
  title = tags$div(
    style = "display:flex; align-items:center; gap:11px;",
    tags$div(style = "width:30px; height:30px; border-radius:50%; background:#f7f6f2; position:relative; flex-shrink:0;",
             tags$div(style = "position:absolute; top:50%; left:50%; width:10px; height:10px; border-radius:50%; background:#45936f; transform:translate(-50%,-50%);")
    ),
    tags$div(
      tags$div("SPATIAL SUITE", style = "font-family:'IBM Plex Mono',monospace; font-weight:700; font-size:19px; line-height:1.15; letter-spacing:0.02em; color:#f7f6f2;"),
      tags$div("Geospatial & Remote-Sensing Suite", style = "font-family:'IBM Plex Mono',monospace; font-weight:400; font-size:10px; line-height:1.2; color:#b9c2c8; letter-spacing:0.03em;")
    )
  ), id = "main_nav",
  theme = bs_theme(
    version = 5, preset = "flatly",
    primary = "#26333e", success = "#45936f", info = "#4a83c4", warning = "#c1683b", danger = "#8b3a2b",
    bg = "#f7f6f2", fg = "#26333e"
  ),
  header = tagList(
    div(id = "corner-badges", style = "position:fixed; bottom:10px; left:50%; transform:translateX(-50%); z-index:9998; display:flex; gap:8px; align-items:center; pointer-events:none;",
        div(id = "version-badge", style = "background:rgba(38,51,62,0.78); color:#dfe5e8; font-family:'IBM Plex Sans',sans-serif; font-size:9.5px; font-weight:600; padding:3px 11px; border-radius:11px; letter-spacing:0.4px; box-shadow:0 2px 8px rgba(38,51,62,0.18); backdrop-filter:blur(4px); pointer-events:auto;",
            "v3.0"),
        div(id = "session-timer-badge", title = "Time since this tab was loaded.", style = "background:rgba(38,51,62,0.78); color:#dfe5e8; font-family:'IBM Plex Sans',sans-serif; font-size:9.5px; font-weight:600; padding:3px 11px; border-radius:11px; letter-spacing:0.3px; box-shadow:0 2px 8px rgba(38,51,62,0.18); backdrop-filter:blur(4px); cursor:help; pointer-events:auto;",
            "Session 00:00")
    ),
    tags$style(HTML("
      @keyframes busy-pulse { 0% { opacity: 1; } 50% { opacity: 0.6; } 100% { opacity: 1; } }
      .busy-banner-spinner { display:inline-block; width:14px; height:14px; border:2px solid rgba(255,255,255,0.4); border-top-color:#fff; border-radius:50%; animation: busy-spin 0.8s linear infinite; margin-right:8px; vertical-align:middle; }
      @keyframes busy-spin { to { transform: rotate(360deg); } }
      @keyframes gee-dot-blink { 0%, 100% { opacity: 1; box-shadow: 0 0 0 0 rgba(69,147,111,0.5); } 50% { opacity: 0.5; box-shadow: 0 0 0 4px rgba(69,147,111,0); } }
    ")),
    tags$script(HTML("
      Shiny.addCustomMessageHandler('showBusyBanner', function(message) {
        var banner = document.getElementById('global_busy_banner_static');
        if (banner) { banner.innerHTML = '<span class=\"busy-banner-spinner\"></span>' + message.text; banner.style.display = 'block'; }
      });
      Shiny.addCustomMessageHandler('hideBusyBanner', function(message) {
        var banner = document.getElementById('global_busy_banner_static');
        if (banner) { banner.style.display = 'none'; }
      });
    ")),
    div(id = "global_busy_banner_static", style = "display:none; background-color:#c1683b; color:white; padding:8px 20px; text-align:center; font-weight:bold; font-size:13px; border-radius:6px; margin-bottom:10px; animation: busy-pulse 2s ease-in-out infinite;"),
    div(style = "background-color: #26333e; color: #ffffff; padding: 12px 25px; display: flex; justify-content: space-between; align-items: center; width: 100%; border-radius: 8px; margin-bottom: 20px; box-shadow: 0 4px 10px rgba(38, 51, 62, 0.2); transition: all 0.3s ease;",
        div(style = "flex: 1; display: flex; align-items: center; gap: 8px; font-size: 13px; font-weight: 600;", 
            tags$span(style = "width:8px; height:8px; border-radius:50%; background:#45936f; display:inline-block; animation: gee-dot-blink 2s ease-in-out infinite;"),
            tags$span("GEE Engine Connected")),
        div(style = "flex: 1; display: flex; justify-content: center;", 
            actionButton("view_floating_btn", "Data Clipboard (0)", style="background-color: #f7f6f2; color: #26333e; border: 1px solid #d8d4c8; border-radius: 20px; font-size: 14px; font-weight: bold; padding: 6px 25px; box-shadow: 0 2px 5px rgba(0,0,0,0.1);")),
        div(style = "flex: 1; display: flex; justify-content: flex-end; gap: 10px;", 
            actionButton("cite_btn", "Cite Methods", icon = icon("book"), class="btn-warning", style="font-size: 13px; font-weight: bold; padding: 8px 15px; border-radius: 6px; border: none; color: white;"),
            actionButton("req_feature_btn", "Request Feature", icon = icon("lightbulb"), class="btn-info", style="font-size: 13px; font-weight: bold; padding: 8px 15px; border-radius: 6px; border: none; color: white;"),
            actionButton("view_cart_btn", "Export Manager (0)", class="btn-success", style="font-size: 14px; font-weight: bold; padding: 8px 20px; border-radius: 6px; border: none;")
        )
    )
  ),
  tabPanel("Shapefile Extractor", mod_extractor_ui("extractor_1")),
  tabPanel("LULC Engine", mod_lulc_ui("lulc_1")),
  tabPanel("GEE Cloud Analytics", mod_gee_ui("gee_1")),
  tabPanel("Publication Maps", mod_carto_ui("carto_1")),
  footer = tags$footer(
    style = "background-color:#26333e; color:#b9c2c8; padding:20px 30px; margin-top:10px; font-size:12px; text-align:center; font-family:'IBM Plex Mono',monospace;",
    tags$div(
      actionLink("about_link", "About", style = "color:#b9c2c8; margin:0 12px;"),
      tags$span("|", style = "color:#3d4f5c;"),
      tags$a("Source (GitHub)", href = "https://github.com/anant9677/spatial-research-suite-public", target = "_blank", style = "color:#b9c2c8; margin:0 12px; text-decoration:none;")
    ),
    tags$div(style = "margin-top:8px; color:#7c8b96;", "Spatial Research Suite \u00b7 MIT Licensed \u00b7 Research & educational screening tools, not field-validated.")
  )
)
ui <- tagList(shared_head, main_navbar_ui)
server <- function(input, output, session) {

  # 🚀 MOBILE SLEEP/DISCONNECT FIX (Task A) — corrected mode.
  # allowReconnect(TRUE), NOT "force". On Cloud Run (non-sticky, autoscaled, no
  # guaranteed same-instance affinity) "force" tells the browser to re-attach to a
  # session the server no longer has — a ZOMBIE session: the UI looks alive (no grey
  # overlay) but no output ever updates, so an in-flight render (e.g. the GEE live
  # map) spins forever. TRUE only restores the session if the SAME R process still
  # holds it (a genuine transient blip on one instance); if the session is truly
  # gone, the branded "Session Paused" overlay appears and its Reconnect button does
  # a clean reload — a fresh, healthy session instead of a stuck one.
  session$allowReconnect(TRUE)

  rv <- reactiveValues( 
    gee_landsat_img = NULL, mask_vect = NULL, train_data = NULL, 
    class_labels = data.frame(Class_ID = integer(), Class_Name = character(), Class_Color = character(), stringsAsFactors = FALSE), 
    drawn_points = data.frame(lon = numeric(), lat = numeric(), class_id = numeric(), class_name = character(), color = character()), 
    stats_df = NULL, temporal_stats_df = NULL, acc_df = NULL, train_vect = NULL, train_col = NULL, trained_classifier = NULL, 
    training_bands = NULL, temp_coords = NULL, logs = "=== SYSTEM INITIALIZED ===\nReady for input.\n", 
    acc_text = "Run classification to view detailed accuracy metrics.", model_accuracy = 0, 
    last_id = 1, last_name = "Water", last_color = "#1E90FF", table_update = 0,
    available_maps = c(), lulc_grid_urls = list(), sankey_data = NULL,
    is_premium = TRUE  # Open build: all exports unlocked (no subscription gating)
  )
  


  gee_rv <- reactiveValues(zonal_data=NULL, ts_data=NULL, hist_data=NULL, grid_urls=list(), current_feature=NULL, mean_val=NULL, med_val=NULL, saved_images=list(), saved_vis=list(), sensor_used=list(), temporal_feature=NULL, temporal_month=NULL)  
  carto_rv <- reactiveValues(plot_preview = NULL, plot_clean = NULL, raster = NULL, plot = NULL)
  floating_rv <- reactiveValues(files = list())
  cart_rv <- reactiveValues(items = data.frame(ID = character(), Item_Name = character(), Price = numeric(), stringsAsFactors = FALSE))
  stats_rv <- reactiveValues(corr_plot = NULL, corr_stats = NULL, ci_df = NULL, lulc_class_stats = NULL, lulc_class_stats_meta = NULL)
  # Cross-dataset registry: LULC Temporal runs publish their class-area (hectares)
  # time series here so the Statistical Analysis module can correlate two of them
  # (e.g. Built-up ha vs Live-Coral ha) with no CSV round-trip.
  analysis_registry <- reactiveValues(sets = list())

  # =========================================================================
  # STATE PERSISTENCE (reactiveValues snapshot cache)
  # Persists the SERIALIZABLE analytical state (boundaries, tables, GEE map
  # recipes, generated plots, workspace cart) to a per-browser file, so a
  # dropped/reloaded session restores the user's work instead of starting from
  # zero. Live Earth Engine objects (trained classifier, EE images) and terra
  # rasters are intentionally EXCLUDED — they are bound to this R process and
  # invalid once it ends; maps are re-derivable from their saved recipes via
  # resolve_saved_image(). Every read/write is wrapped in try(): persistence
  # must NEVER be able to break a live session.
  #
  # Scope note: snapshots live on this instance's local disk, so they reliably
  # restore a reconnect/reload that lands on the same warm Cloud Run instance
  # (the common phone-sleep case). Cross-instance durability would need GCS/
  # Firestore and is a deliberate follow-up, not part of this cache.
  # =========================================================================
  gf_snap_dir <- file.path(tempdir(), "gf_state_snapshots")
  try(dir.create(gf_snap_dir, showWarnings = FALSE, recursive = TRUE), silent = TRUE)
  gf_recovery_key <- reactiveVal(NULL)
  gf_restored <- reactiveVal(FALSE)
  gf_snap_path <- function(key) file.path(gf_snap_dir, paste0("snap_", key, ".rds"))

  # The browser sends a stable per-browser recovery key (localStorage) on connect.
  observeEvent(input$gf_recovery_key, {
    k <- input$gf_recovery_key
    if (is.character(k) && nzchar(k)) gf_recovery_key(gsub("[^A-Za-z0-9_-]", "", k))
  })

  # Assemble the whitelist of serializable state. Anything not listed here is
  # deliberately NOT persisted (EE handles, classifier, rasters, auth, is_premium).
  gf_collect_state <- reactive({
    list(
      v = 1L, ts = Sys.time(),
      rv = list(
        mask_vect = rv$mask_vect, class_labels = rv$class_labels, drawn_points = rv$drawn_points,
        stats_df = rv$stats_df, temporal_stats_df = rv$temporal_stats_df, acc_df = rv$acc_df,
        train_vect = rv$train_vect, train_col = rv$train_col, training_bands = rv$training_bands,
        logs = rv$logs, acc_text = rv$acc_text, model_accuracy = rv$model_accuracy,
        available_maps = rv$available_maps, lulc_grid_urls = rv$lulc_grid_urls,
        sankey_data = rv$sankey_data, batch_results_df = rv$batch_results_df,
        stats_outlier_df = rv$stats_outlier_df, lulc_boundary_used = rv$lulc_boundary_used,
        temporal_grid_years = rv$temporal_grid_years, change_significance = rv$change_significance
      ),
      gee = list(
        zonal_data = gee_rv$zonal_data, ts_data = gee_rv$ts_data, hist_data = gee_rv$hist_data,
        grid_urls = gee_rv$grid_urls, current_feature = gee_rv$current_feature,
        mean_val = gee_rv$mean_val, med_val = gee_rv$med_val, saved_images = gee_rv$saved_images,
        saved_vis = gee_rv$saved_vis, sensor_used = gee_rv$sensor_used,
        temporal_feature = gee_rv$temporal_feature, temporal_month = gee_rv$temporal_month
      ),
      stats = list(
        corr_plot = stats_rv$corr_plot, corr_stats = stats_rv$corr_stats, ci_df = stats_rv$ci_df,
        lulc_class_stats = stats_rv$lulc_class_stats, lulc_class_stats_meta = stats_rv$lulc_class_stats_meta,
        mv_res = stats_rv$mv_res
      ),
      cart = cart_rv$items,
      floating = floating_rv$files
    )
  })

  # Atomic write (temp file -> rename) so a partial/failed serialization never
  # replaces a good snapshot. Skips saving until there's something worth keeping.
  gf_write_snapshot <- function(payload, key) {
    if (is.null(key) || is.null(payload)) return(invisible())
    if (is.null(payload$rv$mask_vect) && length(payload$gee$saved_images) == 0 &&
        (is.null(payload$cart) || nrow(payload$cart) == 0)) return(invisible())
    try({
      tmp <- tempfile(tmpdir = gf_snap_dir, fileext = ".rds")
      saveRDS(payload, tmp)
      file.rename(tmp, gf_snap_path(key))
    }, silent = TRUE)
  }

  # Debounced autosave: coalesce rapid reactive changes into one write.
  gf_state_debounced <- debounce(gf_collect_state, 4000)
  observe({
    key <- gf_recovery_key()
    if (is.null(key)) return()
    gf_write_snapshot(gf_state_debounced(), key)
  })

  # Restore once, on a fresh session, as soon as the recovery key arrives.
  # (A same-process reconnect keeps its reactiveValues in memory and needs no
  # restore; this covers the reload / fresh-process case.)
  observeEvent(gf_recovery_key(), {
    if (isTRUE(gf_restored())) return()
    key <- gf_recovery_key(); if (is.null(key)) return()
    p <- gf_snap_path(key); if (!file.exists(p)) return()
    snap <- tryCatch(readRDS(p), error = function(e) NULL)
    if (is.null(snap) || is.null(snap$ts)) return()
    if (as.numeric(difftime(Sys.time(), snap$ts, units = "hours")) > 6) return()  # ignore stale
    gf_restored(TRUE)
    try({
      for (nm in names(snap$rv))    if (!is.null(snap$rv[[nm]]))    rv[[nm]]      <- snap$rv[[nm]]
      for (nm in names(snap$gee))   if (!is.null(snap$gee[[nm]]))   gee_rv[[nm]]  <- snap$gee[[nm]]
      for (nm in names(snap$stats)) if (!is.null(snap$stats[[nm]])) stats_rv[[nm]] <- snap$stats[[nm]]
      if (!is.null(snap$cart))     cart_rv$items    <- snap$cart
      if (!is.null(snap$floating)) floating_rv$files <- snap$floating
      updateActionButton(session, "view_cart_btn", label = sprintf("Export Manager (%d)", nrow(cart_rv$items)))
      showNotification(HTML("<b>Previous session restored</b> — your boundaries, tables and results are back. Note: a trained LULC classifier is tied to the compute engine and must be re-run."),
                       type = "message", duration = 12)
    }, silent = TRUE)
  }, ignoreInit = FALSE)

  # "Extend Session" from the 55-minute warning modal: force an immediate save so
  # nothing is lost even if the hard timeout hits, and keep the socket warm.
  observeEvent(input$gf_extend_session, {
    gf_write_snapshot(isolate(gf_collect_state()), isolate(gf_recovery_key()))
    showNotification("Session extended — your work is saved.", type = "message", duration = 6)
  })

  log_msg <- function(msg) { rv$logs <- paste0(rv$logs, "[", format(Sys.time(), "%H:%M:%S"), "] INFO: ", msg, "\n") }
  
  add_to_workspace <- function(item_id, name, ignored_price = NULL) { 
    fixed_price <- 0.50
    if(item_id %in% cart_rv$items$ID) { 
      showNotification("Item updated in workspace.", type = "warning")
      cart_rv$items <- cart_rv$items[cart_rv$items$ID != item_id, ] 
    }
    cart_rv$items <- rbind(cart_rv$items, data.frame(ID = item_id, Item_Name = name, Price = fixed_price, stringsAsFactors = FALSE))
    updateActionButton(session, "view_cart_btn", label = sprintf("Export Manager (%d)", nrow(cart_rv$items)))
    showNotification(paste("Added to Export Manager:", name), type = "message")
  }
  
  mod_extractor_server("extractor_1", ext_rv=rv, floating_rv=floating_rv, cart_rv=cart_rv, add_to_workspace=add_to_workspace)
  mod_lulc_server("lulc_1", rv=rv, floating_rv=floating_rv, cart_rv=cart_rv, log_msg=log_msg, add_to_workspace=add_to_workspace, analysis_registry=analysis_registry)
  mod_gee_server("gee_1", rv=rv, gee_rv=gee_rv, floating_rv=floating_rv, cart_rv=cart_rv, add_to_workspace=add_to_workspace)
  mod_carto_server("carto_1", rv=rv, gee_rv=gee_rv, carto_rv=carto_rv, cart_rv=cart_rv, add_to_workspace=add_to_workspace, stats_rv=stats_rv)
  
  observe({
    req(!is.null(floating_rv$files))
    updateActionButton(session, "view_floating_btn", label = sprintf("Data Clipboard (%d)", length(floating_rv$files)))
  })
  
  observeEvent(input$view_floating_btn, {
    showModal(modalDialog(
      title = HTML("<h4 style='color:#26333e; margin:0; font-weight: bold;'>Data Clipboard</h4>"),
      uiOutput("clipboard_ui"),
      size = "m", easyClose = TRUE, 
      footer = tagList(
        if(length(floating_rv$files) > 0) actionButton("clear_floating_btn", "Clear All", class="btn-danger"), 
        modalButton("Close")
      )
    ))
  })
  
  output$clipboard_ui <- renderUI({
    if(length(floating_rv$files) == 0) {
      return(p("Clipboard is empty. Send items here to use them across modules without downloading.", style="color:#5c6b73; margin-top: 15px; font-size:13px; text-align:center;"))
    }
    items <- lapply(names(floating_rv$files), function(file_id) {
      f <- floating_rv$files[[file_id]]
      tags$li(style="font-size:13px; margin-bottom:10px; padding:10px 15px; background:#f7f6f2; border:1px solid #eee; border-radius:4px; display: flex; align-items: center; justify-content: space-between; transition: all 0.2s; box-shadow: 0 1px 3px rgba(0,0,0,0.05);", 
              div(tags$b(f$name, style="color:#26333e;")), 
              div(style="display:flex; align-items:center; gap:10px;",
                  tags$span(style="background-color:#e8f0ea; padding: 3px 8px; border-radius: 4px; color:#5c6b73; font-size:10px; font-weight: bold;", toupper(f$type)),
                  actionButton(paste0("del_", file_id), "✖", style="background:transparent; border:none; box-shadow:none; color:#8b3a2b; font-size:18px; font-weight:bold; padding: 0px 5px; line-height: 1;", onclick=sprintf("Shiny.setInputValue('delete_clipboard_item', '%s', {priority: 'event'});", file_id))
              )
      )
    })
    do.call(tags$ul, c(list(style="padding-left: 0; list-style: none; margin-top: 15px;"), items))
  })
  
  observeEvent(input$delete_clipboard_item, {
    item_id <- input$delete_clipboard_item
    floating_rv$files[[item_id]] <- NULL
    showNotification("File removed from Clipboard.", type="warning")
  })
  
  observeEvent(input$clear_floating_btn, { 
    floating_rv$files <- list()
    removeModal()
    showNotification("Clipboard cleared completely.", type="message") 
  })
  
  observeEvent(input$cite_btn, {
    flows <- get_flowcharts()
    showModal(modalDialog(
      title = HTML("<h4 style='color:#26333e; font-weight:bold; margin:0;'>Methodology & Citations</h4>"),
      tabsetPanel(
        tabPanel(tagList(icon("book-open"), "Citations"), 
                 div(style="background:#f7f6f2; padding: 15px; border-radius: 6px; font-size: 13px; color: #3d4f5c; max-height: 400px; overflow-y: auto; margin-top:15px;",
                     HTML("<b>Software Platform:</b><br>Pathak, A. K. (2026). Spatial Research Suite [Web Application].<br><br>
                    <b>Cloud Processing Engine:</b><br>Gorelick, N., Hancher, M., Dixon, M., Ilyushchenko, S., Thau, D., & Moore, R. (2017). Google Earth Engine: Planetary-scale geospatial analysis for everyone. <i>Remote sensing of Environment</i>, 19(18), 3573.<br><br>
                    <b>Machine Learning:</b><br>Breiman, L. (2001). Random Forests. <i>Machine Learning</i>, 45(1), 5-32.<br><br>
                    <b>Satellite Datasets:</b><br>
                    • <i>Landsat 8/9:</i> USGS (2026). Landsat 8/9 Collection 2 Tier 1 Level 2.<br>
                    • <i>Precipitation:</i> Funk, C., et al. (2015). The climate hazards infrared precipitation with stations—a new environmental record for monitoring extremes. <i>Scientific Data</i>.<br>
                    • <i>Elevation:</i> Farr, T. G., et al. (2007). The Shuttle Radar Topography Mission. <i>Reviews of Geophysics</i>.<br>
                    • <i>Population:</i> WorldPop (2026). Global High Resolution Population Denominators.<br>")
                 )
        ),
        tabPanel(tagList(icon("diagram-project"), "Flowcharts"),
                 div(style="max-height: 400px; overflow-y: auto; margin-top:15px;",
                     h5("LULC Machine Learning Workflow", style="color:#26333e; font-weight:bold;"),
                     tags$pre(class="flowchart-box", flows$lulc),
                     h5("Surface Temperature (LST) Workflow", style="color:#26333e; font-weight:bold; margin-top:20px;"),
                     tags$pre(class="flowchart-box", flows$lst),
                     h5("Precipitation (CHIRPS) Workflow", style="color:#26333e; font-weight:bold; margin-top:20px;"),
                     tags$pre(class="flowchart-box", flows$chirps),
                     h5("Optical Indices (NDVI/NDWI) Workflow", style="color:#26333e; font-weight:bold; margin-top:20px;"),
                     tags$pre(class="flowchart-box", flows$optical)
                 )
        )
      ),
      footer = modalButton("Close"),
      size = "m"
    ))
  })
  
  observeEvent(input$req_feature_btn, {
    showModal(modalDialog(
      title = HTML("<h4 style='color:#26333e; font-weight:bold; margin:0;'>Request a Feature</h4>"),
      p("Have an idea to improve this Spatial Suite? Drop it below. (Completely Anonymous)", style="color:#5c6b73; font-size:13px;"),
      textAreaInput("fb_msg", "Your Idea / Feedback:", rows = 5, placeholder = "I would love to see a feature that..."),
      footer = tagList(
        actionButton("send_fb_btn", "Submit Request", class="btn-success"),
        modalButton("Cancel")
      )
    ))
  })
  
  observeEvent(input$send_fb_btn, {
    req(input$fb_msg)
    showNotification("Sending your request...", type="message")
    tryCatch({
      res <- httr::POST(url = "https://formspree.io/f/xbdvgyvp", body = list(message = input$fb_msg, submission_type = "Anonymous Feature Request"), encode = "json")
      removeModal()
      showNotification("Thank you! Your idea has been securely sent.", type="message", duration = 5)
    }, error = function(e) { showNotification("Failed to send request via API. Please try again later.", type="error") })
  })
  
  
  observeEvent(input$about_link, {
    showModal(modalDialog(
      title = "About Spatial Research Suite", size = "m", easyClose = TRUE, footer = modalButton("Close"),
      div(style = "font-size:13px; line-height:1.6; color:#3d4f5c;",
          p("Spatial Research Suite is an open R / Shiny + Google Earth Engine pipeline for reproducible, publishable remote-sensing research \u2014 land-cover & change analysis, spectral-index libraries, trend/correlation statistics, and a marine/coral module."),
          p("Features include: administrative boundary extraction, Random Forest / CART / SVM land-cover classification with per-pixel confidence mapping, statistically-validated multi-year trend detection (Mann-Kendall), batch zonal statistics, correlation analysis, and publication-ready cartography."),
          p(tags$b("Built by:"), " Anant Kumar Pathak"),
          p(tags$b("Contact:"), " ", tags$a(href = "mailto:anant4infinity@gmail.com", "anant4infinity@gmail.com"))
      )
    ))
  })
  
  # -----------------------------------------------------------------------
  # Export Manager: lists saved assets and lets the user download them (ZIP of
  # data/plots + an optional PDF summary report). Open build - all unlocked.
  # -----------------------------------------------------------------------
  observeEvent(input$view_cart_btn, {
    showModal(modalDialog(
      title = HTML("<div style='text-align: center;'><h4 style='color:#26333e; margin:0; font-weight: 800; font-size: 22px;'>Export Manager</h4></div>"),
      if (nrow(cart_rv$items) == 0) {
        p("Manager is empty. Process data and save assets to download.", style = "color:#5c6b73; margin-top: 15px; font-size: 14px; text-align: center;")
      } else {
        tagList(
          div(style = "background-color: #e8f0ea; padding: 18px; border-radius: 10px; border: 1px solid #9bc4ab; margin-top: 15px; margin-bottom: 20px; text-align: center;",
              p("All outputs are free to download \u2014 this is an open research build.", style = "margin: 0; font-size: 14px; color: #26333e; font-weight: 500;")
          ),
          div(style = "border: 1px solid #d8d4c8; border-radius: 8px; overflow: hidden;", DTOutput("cart_table"))
        )
      },
      size = "l",
      footer = tagList(
        if (nrow(cart_rv$items) > 0) actionButton("clear_cart_btn", "Clear List", class = "btn-danger"),
        if (nrow(cart_rv$items) > 0) downloadButton("download_workspace_assets", "Download My Assets", class = "btn-success", style = "font-weight:bold; font-size:15px; padding:8px 20px;"),
        if (nrow(cart_rv$items) > 0) downloadButton("download_pdf_report", "PDF Summary Report", class = "btn-info", style = "font-weight:bold; font-size:15px; padding:8px 20px; color:white;"),
        modalButton("Close")
      )
    ))
  })
  
  output$cart_table <- renderDT({ 
    req(nrow(cart_rv$items) > 0)
    display_df <- cart_rv$items[, c("ID", "Item_Name", "Price")]
    display_df$Price <- "Included"
    display_df$Remove <- sprintf(
      '<button class="btn btn-danger btn-xs" onclick="Shiny.setInputValue(\'remove_cart_item\', \'%s\', {priority: \'event\'})" style="padding:2px 10px; font-size:12px; font-weight:bold;" title="Remove this item">✕</button>',
      display_df$ID
    )
    display_df$ID <- NULL
    datatable(display_df, colnames = c("Asset Name", "Status", ""), options = list(dom = 't', paging = FALSE), rownames = FALSE, selection = "none", escape = FALSE) %>% formatStyle('Price', color = '#45936f', fontWeight = 'bold')
  }, server = FALSE)
  
  observeEvent(input$remove_cart_item, {
    req(input$remove_cart_item)
    cart_rv$items <- cart_rv$items[cart_rv$items$ID != input$remove_cart_item, ]
    updateActionButton(session, "view_cart_btn", label = sprintf("Export Manager (%d)", nrow(cart_rv$items)))
    showNotification("Item removed from Export Manager.", type = "message")
    if (nrow(cart_rv$items) == 0) removeModal()
  })
  
  observeEvent(input$clear_cart_btn, { 
    cart_rv$items <- data.frame(ID = character(), Item_Name = character(), Price = numeric(), stringsAsFactors = FALSE) 
    updateActionButton(session, "view_cart_btn", label = "Export Manager (0)") 
    removeModal() 
    showNotification("Manager cleared.", type="message") 
  })
  
  # CORE DOWNLOAD ENGINE - assembles a ZIP of the saved assets on demand.
  output$download_workspace_assets <- downloadHandler(
    filename = function() { 
      region_tag <- if (!is.null(rv$mask_vect)) get_region_label(rv$mask_vect) else "UnknownRegion"
      paste0("Spatial_Research_Assets_", region_tag, "_", format(Sys.Date(), "%Y%m%d"), ".zip") 
    },
    content = function(file) {
      temp_dir <- tempdir(); files_to_zip <- c()
      cite_path <- file.path(temp_dir, "Methodology_and_Citations.md")
      flows <- get_flowcharts()
      cite_text <- generate_methodology_text(cart_rv, rv, gee_rv, flows)
      writeLines(cite_text, cite_path); files_to_zip <- c(files_to_zip, cite_path)
      
      if("ext_shp" %in% cart_rv$items$ID && !is.null(rv$ext_shp_export)) {
        shp_dir <- file.path(temp_dir, "Extracted_Boundary")
        dir.create(shp_dir, showWarnings = FALSE)
        shp_region_tag <- get_region_label(rv$ext_shp_export)
        sf::st_write(rv$ext_shp_export, file.path(shp_dir, paste0(shp_region_tag, "_Extracted_Boundary.shp")), delete_layer = TRUE, quiet = TRUE)
        files_to_zip <- c(files_to_zip, list.files(shp_dir, full.names = TRUE))
      }
      
      if("drawn_roi" %in% cart_rv$items$ID && !is.null(rv$drawn_roi_export)) {
        roi_dir <- file.path(temp_dir, "Custom_ROI")
        dir.create(roi_dir, showWarnings = FALSE)
        roi_region_tag <- get_region_label(rv$drawn_roi_export)
        sf::st_write(rv$drawn_roi_export, file.path(roi_dir, paste0(roi_region_tag, "_Custom_ROI.shp")), delete_layer = TRUE, quiet = TRUE)
        files_to_zip <- c(files_to_zip, list.files(roi_dir, full.names = TRUE))
      }
      
      if("sankey_csv" %in% cart_rv$items$ID && !is.null(rv$sankey_data)) {
        csv_path <- file.path(temp_dir, "LULC_Change_Matrix.csv")
        write.csv(rv$sankey_data, csv_path, row.names = FALSE)
        files_to_zip <- c(files_to_zip, csv_path)
        xlsx_path <- file.path(temp_dir, "LULC_Change_Matrix.xlsx")
        writexl::write_xlsx(rv$sankey_data, xlsx_path)
        files_to_zip <- c(files_to_zip, xlsx_path)
      }
      
      if("lulc_csv" %in% cart_rv$items$ID && !is.null(rv$stats_df)) {
        csv_path <- file.path(temp_dir, "LULC_Area_Statistics.csv")
        write.csv(rv$stats_df, csv_path, row.names = FALSE)
        files_to_zip <- c(files_to_zip, csv_path)
        xlsx_path <- file.path(temp_dir, "LULC_Area_Statistics.xlsx")
        writexl::write_xlsx(rv$stats_df, xlsx_path)
        files_to_zip <- c(files_to_zip, xlsx_path)
      }
      
      if("lulc_plot" %in% cart_rv$items$ID && !is.null(rv$stats_df)) {
        chart_path <- file.path(temp_dir, "LULC_Area_Chart.png")
        clean_colors <- trimws(rv$stats_df$Class_Color); names(clean_colors) <- rv$stats_df$Class_Name
        p <- ggplot(rv$stats_df, aes(x = reorder(Class_Name, Area_km2), y = Area_km2, fill = Class_Name)) + geom_bar(stat = "identity", color = "black") + scale_fill_manual(values = clean_colors) + coord_flip() + theme_minimal(base_family="sans") + labs(x="LULC Classification Categories", y="Total Area (in Square Kilometers)", title="Total Area Coverage by Class") + theme(legend.position="none", plot.title=element_text(face="bold", size=16, color="#26333e"), axis.text=element_text(size=12, face="bold"), axis.title=element_text(size=14, face="bold"))
        ggsave(chart_path, plot = p, width = 8, height = 6, dpi = 150) # 🚀 COST FIX: 150 DPI is print/document-optimal, ~4x fewer pixels to render than 300 DPI, with no visible quality loss at normal reading distance in a report or PDF
        files_to_zip <- c(files_to_zip, chart_path)
      }
      
      if("acc_report" %in% cart_rv$items$ID && rv$acc_text != "") {
        txt_path <- file.path(temp_dir, "ML_Accuracy_Report.txt")
        full_acc <- paste0(rv$acc_text)
        writeLines(full_acc, txt_path)
        files_to_zip <- c(files_to_zip, txt_path)
      }
      
      if("lulc_temp_csv" %in% cart_rv$items$ID && !is.null(rv$temporal_stats_df)) {
        wide_stats <- rv$temporal_stats_df %>% select(Class_Name, Year, Area_km2) %>% tidyr::pivot_wider(names_from = Year, values_from = Area_km2, names_prefix = "Year_")
        csv_path <- file.path(temp_dir, "LULC_Temporal_Statistics.csv")
        write.csv(wide_stats, csv_path, row.names=FALSE)
        files_to_zip <- c(files_to_zip, csv_path)
        xlsx_path <- file.path(temp_dir, "LULC_Temporal_Statistics.xlsx")
        writexl::write_xlsx(wide_stats, xlsx_path)
        files_to_zip <- c(files_to_zip, xlsx_path)
      }
      
      if("lulc_temp_plot" %in% cart_rv$items$ID && !is.null(rv$temporal_stats_df)) {
        chart_path <- file.path(temp_dir, "LULC_Temporal_Area_Chart.png")
        df <- rv$temporal_stats_df
        pal <- setNames(trimws(rv$class_labels$Class_Color), rv$class_labels$Class_Name)
        p <- ggplot(df, aes(x=factor(Year), y=Area_km2, fill=Class_Name)) + geom_bar(stat="identity", position="stack", color="black", linewidth=0.3) + scale_fill_manual(values=pal) + theme_minimal(base_family="sans") + labs(x="Year", y="Total Area (Sq.Km)", title="Spatiotemporal LULC Transitions Over Time") + theme(legend.position="right", plot.title=element_text(face="bold", size=16, color="#26333e"), axis.text=element_text(size=12, face="bold"), axis.title=element_text(size=14, face="bold"))
        ggsave(chart_path, plot = p, width = 8, height = 6, dpi = 150) # 🚀 COST FIX: 150 DPI is print/document-optimal, ~4x fewer pixels to render than 300 DPI, with no visible quality loss at normal reading distance in a report or PDF
        files_to_zip <- c(files_to_zip, chart_path)
      }
      
      if(any(grepl("^lulc_gallery_", cart_rv$items$ID)) && length(rv$lulc_grid_urls) > 0) {
        gal_dir <- file.path(temp_dir, "LULC_Temporal_Gallery")
        dir.create(gal_dir, showWarnings = FALSE)
        for(yr in names(rv$lulc_grid_urls)) { try({ download.file(rv$lulc_grid_urls[[yr]], file.path(gal_dir, paste0("LULC_Map_", yr, ".png")), mode="wb", quiet=TRUE) }, silent=TRUE) }
        files_to_zip <- c(files_to_zip, list.files(gal_dir, full.names = TRUE))
      }
      
      if(any(grepl("^carto_png_", cart_rv$items$ID)) && !is.null(carto_rv$plot_clean)) {
        png_path <- file.path(temp_dir, "Publication_Map.png")
        ggsave(png_path, plot = carto_rv$plot_clean, width = 10, height = 8, dpi = 150) # 🚀 COST FIX: 150 DPI is print/document-optimal, ~4x fewer pixels to render than 300 DPI, with no visible quality loss at normal reading distance in a report or PDF
        files_to_zip <- c(files_to_zip, png_path)
      }

      # Publication Maps modular-canvas composition (Phase 2)
      if(any(grepl("^carto_canvas_", cart_rv$items$ID)) && !is.null(carto_rv$canvas_composed)) {
        canvas_path <- file.path(temp_dir, "Publication_Canvas.png")
        try(ggsave(canvas_path, plot = carto_rv$canvas_composed, width = 11, height = 8.5, dpi = 200, bg = "white", limitsize = FALSE), silent = TRUE)
        if (file.exists(canvas_path)) files_to_zip <- c(files_to_zip, canvas_path)
      }
      
      if(any(grepl("^carto_tif_", cart_rv$items$ID)) && !is.null(carto_rv$raster)) {
        tif_path <- file.path(temp_dir, "Publication_Raster.tif")
        writeRaster(carto_rv$raster, tif_path, filetype="GTiff", overwrite=TRUE)
        files_to_zip <- c(files_to_zip, tif_path)
      }
      
      zonal_ids <- cart_rv$items$ID[grepl("^gee_zonal_", cart_rv$items$ID)]
      if(length(zonal_ids) > 0 && !is.null(gee_rv$zonal_data)) {
        z_path <- file.path(temp_dir, "Zonal_Statistics.csv")
        write.csv(gee_rv$zonal_data, z_path, row.names = FALSE)
        files_to_zip <- c(files_to_zip, z_path)
        z_xlsx_path <- file.path(temp_dir, "Zonal_Statistics.xlsx")
        writexl::write_xlsx(gee_rv$zonal_data, z_xlsx_path)
        files_to_zip <- c(files_to_zip, z_xlsx_path)
      }
      
      corr_ids <- cart_rv$items$ID[grepl("^stats_corr_", cart_rv$items$ID)]
      if (length(corr_ids) > 0 && !is.null(stats_rv$corr_plot)) {
        chart_path <- file.path(temp_dir, "Correlation_Analysis_Chart.png")
        ggsave(chart_path, plot = stats_rv$corr_plot, width = 8, height = 6, dpi = 150) # 🚀 COST FIX: 150 DPI is print/document-optimal, ~4x fewer pixels to render than 300 DPI, with no visible quality loss at normal reading distance in a report or PDF
        files_to_zip <- c(files_to_zip, chart_path)
      }
      
      ci_ids <- cart_rv$items$ID[grepl("^stats_ci_", cart_rv$items$ID)]
      if (length(ci_ids) > 0 && !is.null(stats_rv$ci_df)) {
        ci_csv_path <- file.path(temp_dir, "Confidence_Interval.csv")
        write.csv(stats_rv$ci_df, ci_csv_path, row.names = FALSE)
        files_to_zip <- c(files_to_zip, ci_csv_path)
        ci_xlsx_path <- file.path(temp_dir, "Confidence_Interval.xlsx")
        writexl::write_xlsx(stats_rv$ci_df, ci_xlsx_path)
        files_to_zip <- c(files_to_zip, ci_xlsx_path)
      }
      
      lulc_stats_ids <- cart_rv$items$ID[grepl("^stats_lulc_", cart_rv$items$ID)]
      if (length(lulc_stats_ids) > 0 && !is.null(stats_rv$lulc_class_stats)) {
        ls_csv_path <- file.path(temp_dir, "Statistics_by_Land_Cover_Class.csv")
        write.csv(stats_rv$lulc_class_stats, ls_csv_path, row.names = FALSE)
        files_to_zip <- c(files_to_zip, ls_csv_path)
        ls_xlsx_path <- file.path(temp_dir, "Statistics_by_Land_Cover_Class.xlsx")
        writexl::write_xlsx(stats_rv$lulc_class_stats, ls_xlsx_path)
        files_to_zip <- c(files_to_zip, ls_xlsx_path)
      }
      
      ts_ids <- cart_rv$items$ID[grepl("^gee_ts_", cart_rv$items$ID)]
      if(length(ts_ids) > 0 && !is.null(gee_rv$ts_data)) {
        df_plot <- gee_rv$ts_data[!is.na(gee_rv$ts_data$Value), ]
        p1 <- ggplot(df_plot, aes(x=Month, y=Value, group=1)) + geom_line(color="#8e44ad", linewidth=1.5) + geom_point(size=4, color="#26333e") + scale_x_discrete(drop = FALSE) + scale_y_continuous(expand = expansion(mult = c(0.1, 0.2))) + theme_minimal(base_family="sans") + labs(title=paste("Temporal Trend:", gee_rv$current_feature), subtitle="Real 12-month variation.", y=gee_rv$current_feature, x="Month") + theme(plot.title=element_text(face="bold", size=16, color="#26333e"), plot.subtitle=element_text(color="#5c6b73", face="italic", size=12), axis.text=element_text(size=10, face="bold", angle=45, hjust=1), axis.title=element_text(size=14, face="bold"))
        if(nrow(df_plot) > 2 && !gee_rv$current_feature %in% c("Elevation (DEM)", "Terrain Slope")) { p1 <- p1 + geom_smooth(method="loess", se=FALSE, color="gray50", linetype="dashed", linewidth=1) }
        ts_path <- file.path(temp_dir, "Time_Series_Trend.png")
        ggsave(ts_path, plot = p1, width = 8, height = 5, dpi = 150) # 🚀 COST FIX: 150 DPI is print/document-optimal, ~4x fewer pixels to render than 300 DPI, with no visible quality loss at normal reading distance in a report or PDF
        files_to_zip <- c(files_to_zip, ts_path)
      }
      
      hist_ids <- cart_rv$items$ID[grepl("^gee_hist_", cart_rv$items$ID)]
      if(length(hist_ids) > 0 && !is.null(gee_rv$hist_data)) {
        p2 <- ggplot(gee_rv$hist_data, aes(x=Bin, y=Count)) + geom_col(fill="#4a83c4", color="black", alpha=0.8) + theme_minimal(base_family="sans") + labs(title=paste("Pixel Distribution:", gee_rv$current_feature), x="Value Range", y="Pixel Count") + theme(plot.title=element_text(face="bold", size=16, color="#26333e"), plot.subtitle=element_text(color="#5c6b73", face="italic", size=12), axis.text=element_text(size=12, face="bold"), axis.title=element_text(size=14, face="bold"), legend.position="bottom")
        if(!is.null(gee_rv$mean_val) && !is.na(gee_rv$mean_val)) { p2 <- p2 + geom_vline(aes(xintercept=gee_rv$mean_val, color="Mean"), linetype="dashed", linewidth=1) }
        if(!is.null(gee_rv$med_val) && !is.na(gee_rv$med_val)) { p2 <- p2 + geom_vline(aes(xintercept=gee_rv$med_val, color="Median"), linetype="solid", linewidth=1) }
        p2 <- p2 + scale_color_manual(name="Statistics", values=c("Mean"="#e74c3c", "Median"="#f1c40f"))
        hist_path <- file.path(temp_dir, "Area_Distribution_Histogram.png")
        ggsave(hist_path, plot = p2, width = 8, height = 5, dpi = 150) # 🚀 COST FIX: 150 DPI is print/document-optimal, ~4x fewer pixels to render than 300 DPI, with no visible quality loss at normal reading distance in a report or PDF
        files_to_zip <- c(files_to_zip, hist_path)
      }
      
      if(any(grepl("^gee_gallery_", cart_rv$items$ID)) && length(gee_rv$grid_urls) > 0) {
        gal_dir <- file.path(temp_dir, "Temporal_Gallery")
        dir.create(gal_dir, showWarnings = FALSE)
        for(yr in names(gee_rv$grid_urls)) { try({ download.file(gee_rv$grid_urls[[yr]], file.path(gal_dir, paste0("Grid_Map_", yr, ".png")), mode="wb", quiet=TRUE) }, silent=TRUE) }
        files_to_zip <- c(files_to_zip, list.files(gal_dir, full.names = TRUE))
      }

      # ---- Sequential Pipeline outputs (individual Add-to-Workspace items) ----
      # Georeference a visualized map thumbnail (RGB PNG over the boundary bbox) into a GeoTIFF
      # that opens correctly-positioned in QGIS/ArcGIS. Best-effort — falls back to PNG-only.
      png_url_to_tif <- function(url, bbox, out) tryCatch({
        if (is.null(url) || !nzchar(url) || is.null(bbox)) return(FALSE)
        tmp <- tempfile(fileext = ".png"); download.file(url, tmp, mode = "wb", quiet = TRUE)
        arr <- png::readPNG(tmp); if (length(dim(arr)) == 2) arr <- array(arr, c(dim(arr), 1))
        r <- terra::rast(arr)
        terra::ext(r) <- c(as.numeric(bbox["xmin"]), as.numeric(bbox["xmax"]), as.numeric(bbox["ymin"]), as.numeric(bbox["ymax"]))
        terra::crs(r) <- "EPSG:4326"; r <- round(r * 255)
        terra::writeRaster(r, out, datatype = "INT1U", overwrite = TRUE, filetype = "GTiff")
        file.exists(out)
      }, error = function(e) FALSE)

      if (any(grepl("^pipe_cmap_", cart_rv$items$ID)) && !is.null(gee_rv$pipeline_final_plot)) {
        p <- file.path(temp_dir, "Pipeline_Composite_Map.png")
        try(ggsave(p, plot = gee_rv$pipeline_final_plot, width = 8, height = 7, dpi = 150, bg = "white"), silent = TRUE)
        if (file.exists(p)) files_to_zip <- c(files_to_zip, p)
        tif <- file.path(temp_dir, "Pipeline_Composite_Map.tif")   # georeferenced GeoTIFF
        if (isTRUE(png_url_to_tif(gee_rv$pipeline_final_url, gee_rv$pipeline_final_bbox, tif))) files_to_zip <- c(files_to_zip, tif)
      }
      smap_ids <- cart_rv$items$ID[grepl("^pipe_smap_", cart_rv$items$ID)]
      if (length(smap_ids) > 0 && !is.null(gee_rv$pipeline_steps)) {
        sdir <- file.path(temp_dir, "Pipeline_Layer_Maps"); dir.create(sdir, showWarnings = FALSE)
        for (id in smap_ids) {
          i  <- suppressWarnings(as.integer(sub("^pipe_smap_(\\d+)_.*$", "\\1", id)))
          st <- if (!is.na(i)) gee_rv$pipeline_steps[[i]] else NULL
          if (!is.null(st) && !is.null(st$url) && nzchar(st$url)) {
            nm <- gsub("[^A-Za-z0-9]+", "_", st$label %||% paste0("Step_", i))
            try(download.file(st$url, file.path(sdir, sprintf("%02d_%s.png", i, nm)), mode = "wb", quiet = TRUE), silent = TRUE)
            png_url_to_tif(st$url, gee_rv$pipeline_final_bbox, file.path(sdir, sprintf("%02d_%s.tif", i, nm)))  # + GeoTIFF
          }
        }
        files_to_zip <- c(files_to_zip, list.files(sdir, full.names = TRUE))
      }
      if (any(grepl("^pipe_tbl_classarea_", cart_rv$items$ID)) && !is.null(gee_rv$gee_class_insights$classes)) {
        p <- file.path(temp_dir, "Pipeline_Area_by_Class.csv"); write.csv(gee_rv$gee_class_insights$classes, p, row.names = FALSE); files_to_zip <- c(files_to_zip, p)
      }
      if (any(grepl("^pipe_tbl_trend_", cart_rv$items$ID)) && !is.null(gee_rv$trend_summary)) {
        ts <- gee_rv$trend_summary
        if (!is.null(ts$series)) { p <- file.path(temp_dir, "Pipeline_Trend_Series.csv"); write.csv(ts$series, p, row.names = FALSE); files_to_zip <- c(files_to_zip, p) }
      }
      if (any(grepl("^pipe_tbl_correl_", cart_rv$items$ID)) && !is.null(gee_rv$correl_series$corr)) {
        rmat <- as.data.frame(round(gee_rv$correl_series$corr$r, 3)); rmat <- cbind(Indicator = rownames(rmat), rmat)
        p <- file.path(temp_dir, "Pipeline_Correlation_Matrix.csv"); write.csv(rmat, p, row.names = FALSE); files_to_zip <- c(files_to_zip, p)
      }
      if (any(grepl("^pipe_tbl_aca_", cart_rv$items$ID)) && !is.null(gee_rv$aca_summary$classes)) {
        p <- file.path(temp_dir, "Pipeline_ACA_Habitat.csv"); write.csv(gee_rv$aca_summary$classes, p, row.names = FALSE); files_to_zip <- c(files_to_zip, p)
      }
      if (any(grepl("^pipe_tbl_prov_", cart_rv$items$ID)) && !is.null(gee_rv$pipeline_provenance)) {
        p <- file.path(temp_dir, "Pipeline_Reproducibility.csv"); write.csv(gee_rv$pipeline_provenance, p, row.names = FALSE); files_to_zip <- c(files_to_zip, p)
      }
      if (any(grepl("^pipe_chart_trend_", cart_rv$items$ID)) && !is.null(gee_rv$trend_summary$series)) {
        tp <- try(gf_trend_line_plot(gee_rv$trend_summary$series, label = gee_rv$trend_summary$feature %||% "Value", units = ""), silent = TRUE)
        if (!inherits(tp, "try-error") && !is.null(tp)) { p <- file.path(temp_dir, "Pipeline_Trend_Chart.png"); try(ggsave(p, plot = tp, width = 7, height = 3.6, dpi = 150, bg = "white"), silent = TRUE); if (file.exists(p)) files_to_zip <- c(files_to_zip, p) }
      }
      if (any(grepl("^pipe_chart_correl_", cart_rv$items$ID)) && !is.null(gee_rv$correl_series$series)) {
        cc <- gee_rv$correl_series
        sp <- try(gf_correlation_scatter_plot(cc$series, cc$years, cc$response), silent = TRUE)
        if (!inherits(sp, "try-error") && !is.null(sp)) { p <- file.path(temp_dir, "Pipeline_Correlation_Scatter.png"); try(ggsave(p, plot = sp, width = 7, height = 4.3, dpi = 150, bg = "white"), silent = TRUE); if (file.exists(p)) files_to_zip <- c(files_to_zip, p) }
      }

      zip::zipr(zipfile = file, files = files_to_zip)
    }
  )
  
  output$download_pdf_report <- downloadHandler(
    filename = function() {
      region_tag <- if (!is.null(rv$mask_vect)) get_region_label(rv$mask_vect) else "UnknownRegion"
      paste0("Spatial_Research_Report_", region_tag, "_", format(Sys.Date(), "%Y%m%d"), ".pdf")
    },
    content = function(file) {
      flows <- get_flowcharts()
      report_text <- generate_methodology_text(cart_rv, rv, gee_rv, flows)
      region_tag <- if (!is.null(rv$mask_vect)) gsub("_", " ", get_region_label(rv$mask_vect)) else "Unknown Region"
      
      pdf(file, width = 8.5, height = 11)
      plot.new()
      text(0.5, 0.82, "Spatial Research Suite", cex = 2.2, font = 2)
      text(0.5, 0.75, "Methodology & Analysis Summary Report", cex = 1.3)
      text(0.5, 0.65, format(Sys.Date(), "%B %d, %Y"), cex = 1)
      text(0.5, 0.58, paste("Study Area:", region_tag), cex = 1.1, font = 3, col = "#26333e")
      text(0.5, 0.15, "Generated by Spatial Research Suite \u2014 Google Earth Engine Cloud Analytics", cex = 0.7, col = "#5c6b73")
      
      lines_vec <- strsplit(report_text, "\n")[[1]]
      lines_per_page <- 58
      n_pages <- max(1, ceiling(length(lines_vec) / lines_per_page))
      for (p in seq_len(n_pages)) {
        plot.new()
        idx_start <- (p - 1) * lines_per_page + 1
        idx_end <- min(p * lines_per_page, length(lines_vec))
        page_text <- paste(lines_vec[idx_start:idx_end], collapse = "\n")
        text(0.02, 0.98, page_text, adj = c(0, 1), cex = 0.62, family = "mono")
        mtext(sprintf("Page %d of %d", p + 1, n_pages + 1), side = 1, line = -1, cex = 0.6, col = "#5c6b73")
      }
      
      render_df_as_pdf_page <- function(df, title) {
        if (is.null(df) || nrow(df) == 0) return(invisible(NULL))
        df_fmt <- as.data.frame(lapply(df, function(col) {
          if (is.numeric(col)) format(round(col, 3), nsmall = 0, trim = TRUE) else as.character(col)
        }), stringsAsFactors = FALSE)
        col_widths <- pmax(nchar(names(df_fmt)), sapply(df_fmt, function(col) max(nchar(col), na.rm = TRUE))) + 2
        header_line <- paste(mapply(function(nm, w) formatC(nm, width = -w), names(df_fmt), col_widths), collapse = "")
        sep_line <- paste(rep("-", sum(col_widths)), collapse = "")
        body_lines <- apply(df_fmt, 1, function(row) paste(mapply(function(val, w) formatC(val, width = -w), row, col_widths), collapse = ""))
        rows_per_page <- 50
        n_sub_pages <- max(1, ceiling(length(body_lines) / rows_per_page))
        for (sp in seq_len(n_sub_pages)) {
          plot.new()
          text(0.02, 0.97, title, adj = c(0, 1), cex = 0.95, font = 2, col = "#26333e")
          idx_s <- (sp - 1) * rows_per_page + 1
          idx_e <- min(sp * rows_per_page, length(body_lines))
          table_text <- paste(c(header_line, sep_line, body_lines[idx_s:idx_e]), collapse = "\n")
          text(0.02, 0.90, table_text, adj = c(0, 1), cex = 0.55, family = "mono")
        }
      }
      
      render_df_as_pdf_page(rv$batch_results_df, "Batch Zonal Statistics Results")
      render_df_as_pdf_page(rv$acc_df, "LULC Classification Accuracy (Producer's / User's Accuracy by Class)")
      render_df_as_pdf_page(rv$stats_df, "LULC Area Statistics by Class")
      render_df_as_pdf_page(gee_rv$zonal_data, "GEE Cloud Analytics \u2014 Zonal Statistics")
      
      dev.off()
    }
  )
}
shinyApp(ui, server)