let Grafana =
        env:DHALL_GRAFANA
      ? https://raw.githubusercontent.com/weeezes/dhall-grafana/d5630dc55deacf5100a99802a4df1d9680b263b3/package.dhall
          sha256:93f7d53fae8d9b9a74d8f54929fc320c80579de7cc503f72c8fe42a5cfe985f9

let datasource = Some (env:GRAFANA_DATASOURCE as Text ? "prometheus")

let scope = "{job=\"weeder\"}"

let target =
      \(refId : Text) ->
      \(expr : Text) ->
      \(title : Text) ->
        Grafana.MetricsTargets.PrometheusTarget
          Grafana.PrometheusTarget::{
          , refId
          , expr
          , legendFormat = Some "{{ job }} : ${title}"
          }

let panel =
      \(y : Natural) ->
      \(title : Text) ->
      \(targets : List Grafana.MetricsTargets) ->
        Grafana.Panels.mkGraphPanel
          Grafana.GraphPanel::{
          , title
          , gridPos = { x = 0, y, w = 24, h = 6 }
          , datasource
          , targets
          , fill = 0
          , linewidth = 2
          }

let panels =
      [ panel
          0
          "Activity"
          [ target "A" "increase(http_request${scope}[5m])" "Request count" ]
      , panel
          8
          "Memory"
          [ target
              "B"
              "increase(ghc_gcdetails_live_bytes${scope}[5m])"
              "Total amount of live data in the heap."
          ]
      , panel
          16
          "CPU"
          [ target
              "C"
              "increase(ghc_cpu_seconds_total${scope}[5m])"
              "CPU time in seconds"
          ]
      ]

in  Grafana.Dashboard::{
    , title = "Zuul Weeder"
    , editable = True
    , panels = Grafana.Utils.generateIds panels
    , links =
      [ Grafana.Link.Type.Link
          Grafana.LinkExternal::{
          , title = "Zuul Weeder"
          , url =
              "https://github.com/softwarefactory-project/zuul-weeder#readme"
          , tooltip = "softwarefactory-project/zuul-weeder"
          }
      ]
    }
