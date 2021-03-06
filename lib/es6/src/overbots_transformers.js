// Generated by BUCKLESCRIPT VERSION 1.7.5, PLEASE EDIT WITH CARE
'use strict';

import * as List              from "bs-platform/lib/es6/list.js";
import * as Block             from "bs-platform/lib/es6/block.js";
import * as Curry             from "bs-platform/lib/es6/curry.js";
import * as Tea_cmd           from "bucklescript-tea/lib/es6/src/tea_cmd.js";
import * as Pervasives        from "bs-platform/lib/es6/pervasives.js";
import * as Overbots_flags    from "./overbots_flags.js";
import * as Overbots_types    from "./overbots_types.js";
import * as Overbots_resource from "./overbots_resource.js";

function name() {
  return "Sunlight";
}

function enabled() {
  return /* true */1;
}

function transformers(model) {
  return /* :: */[
          /* Generate */Block.__(0, [
              /* Energy */0,
              Overbots_flags.float_flag_value(/* BasicSolarPanelSelfGeneration */0, model)
            ]),
          /* [] */0
        ];
}

var BaseSolarGeneration = /* module */[
  /* name */name,
  /* enabled */enabled,
  /* transformers */transformers
];

function name$1() {
  return "Internal Drilling";
}

function enabled$1(model) {
  return Overbots_flags.bool_flag_exists(/* DrillDeployed */3, model);
}

function transformers$1() {
  return /* :: */[
          /* Consume */Block.__(1, [
              /* Energy */0,
              0.5
            ]),
          /* :: */[
            /* Generate */Block.__(0, [
                /* IronOxide */1,
                0.2
              ]),
            /* :: */[
              /* Generate */Block.__(0, [
                  /* RawSilicon */2,
                  0.1
                ]),
              /* [] */0
            ]
          ]
        ];
}

var DrillEnabled = /* module */[
  /* name */name$1,
  /* enabled */enabled$1,
  /* transformers */transformers$1
];

var all_transformers_001 = /* :: */[
  DrillEnabled,
  /* [] */0
];

var all_transformers = /* :: */[
  BaseSolarGeneration,
  all_transformers_001
];

function enabled_transformers(model) {
  return List.filter((function (T) {
                  return Curry._1(T[/* enabled */1], model);
                }))(all_transformers);
}

function transformer_delta(param) {
  if (param.tag) {
    return /* tuple */[
            param[0],
            -param[1]
          ];
  } else {
    return /* tuple */[
            param[0],
            param[1]
          ];
  }
}

function calculate_resource_delta(model, map, T) {
  return List.fold_left((function (map, transformation) {
                var match = transformer_delta(transformation);
                var rid = match[0];
                var delta = match[1] + Curry._2(Overbots_types.ResourceMap[/* find */21], rid, map);
                return Curry._3(Overbots_types.ResourceMap[/* add */3], rid, delta, map);
              }), map, Curry._1(T[/* transformers */2], model));
}

function calculate_resource_deltas(model, transformers) {
  return List.fold_left((function (param, param$1) {
                return calculate_resource_delta(model, param, param$1);
              }), Overbots_resource.init_resources_values, transformers);
}

function calculate_delta_to_next_filled(model, rid, delta, old_time) {
  if (delta === 0.0) {
    return old_time;
  } else {
    var value = Curry._2(Overbots_types.ResourceMap[/* find */21], rid, model[/* resource_values */4]);
    var R = Overbots_resource.get_resource_module(rid);
    var match = Curry._1(R[/* get_value_range */2], model);
    var rmax = match[1];
    var rmin = match[0];
    if (value >= rmax || value <= rmin) {
      return old_time;
    } else {
      var at_time = delta > 0.0 ? (rmax - value) / delta : (value - rmin) / delta;
      if (at_time > 0.0 && at_time < old_time) {
        return at_time;
      } else {
        return old_time;
      }
    }
  }
}

function calculate_deltas_to_next_filled(model, resource_deltas) {
  return Curry._3(Overbots_types.ResourceMap[/* fold */10], (function (param, param$1, param$2) {
                return calculate_delta_to_next_filled(model, param, param$1, param$2);
              }), resource_deltas, Pervasives.max_float);
}

function apply_resource_deltas(model, resource_deltas, cur_time) {
  var time_delta = cur_time - model[/* gametime */2];
  return Curry._3(Overbots_types.ResourceMap[/* fold */10], (function (rid, delta, model) {
                var delta$1 = delta * time_delta;
                var match = Overbots_resource.add_resource_value(rid, delta$1, model);
                if (typeof match === "number") {
                  return model;
                } else {
                  return match[0];
                }
              }), resource_deltas, model);
}

function update_transformations(_model, new_time) {
  while(true) {
    var model = _model;
    var match;
    if (model[/* cache */8][/* transformers */0]) {
      match = /* tuple */[
        model[/* cache */8][/* transformers */0],
        model[/* cache */8][/* resource_deltas */1]
      ];
    } else {
      var transformers = enabled_transformers(model);
      match = /* tuple */[
        transformers,
        calculate_resource_deltas(model, transformers)
      ];
    }
    var resource_deltas = match[1];
    var time_to_next_filled = model[/* gametime */2] + calculate_deltas_to_next_filled(model, resource_deltas);
    var time_slice = Pervasives.min(time_to_next_filled, new_time);
    var model$1 = apply_resource_deltas(model, resource_deltas, time_slice);
    if (time_slice >= new_time) {
      var cache_000 = /* transformers */match[0];
      var cache = /* record */[
        cache_000,
        /* resource_deltas */resource_deltas
      ];
      var newrecord = model$1.slice();
      newrecord[/* gametime */2] = new_time;
      newrecord[/* cache */8] = cache;
      return /* tuple */[
              newrecord,
              Tea_cmd.none
            ];
    } else {
      var cache$1 = /* record */[
        /* transformers : [] */0,
        /* resource_deltas */Overbots_resource.init_resources_values
      ];
      var newrecord$1 = model$1.slice();
      newrecord$1[/* gametime */2] = time_slice;
      newrecord$1[/* cache */8] = cache$1;
      _model = newrecord$1;
      continue ;
      
    }
  };
}

export {
  BaseSolarGeneration             ,
  DrillEnabled                    ,
  all_transformers                ,
  enabled_transformers            ,
  transformer_delta               ,
  calculate_resource_delta        ,
  calculate_resource_deltas       ,
  calculate_delta_to_next_filled  ,
  calculate_deltas_to_next_filled ,
  apply_resource_deltas           ,
  update_transformations          ,
  
}
/* Overbots_flags Not a pure module */
