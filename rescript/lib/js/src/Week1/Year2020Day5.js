// Generated by ReScript, PLEASE EDIT WITH CARE
'use strict';

var Fs = require("fs");
var Caml = require("rescript/lib/js/caml.js");
var Belt_List = require("rescript/lib/js/belt_List.js");
var Belt_Array = require("rescript/lib/js/belt_Array.js");
var Belt_SortArray = require("rescript/lib/js/belt_SortArray.js");

var input = Fs.readFileSync("input/Week1/Year2020Day5.txt", "utf8");

var bPass = Belt_Array.keep(input.split("\n"), (function (b) {
        return b !== "";
      }));

var seatIds = Belt_Array.map(bPass, (function (p) {
        return Belt_Array.reduce(p.split(""), 0, (function (acc, c) {
                      if (c === "B" || c === "R") {
                        return (acc << 1) + 1 | 0;
                      } else {
                        return (acc << 1);
                      }
                    }));
      }));

console.log(Belt_Array.reduce(seatIds, 0, (function (acc, id) {
            if (acc > id) {
              return acc;
            } else {
              return id;
            }
          })));

function sum(ids) {
  return Belt_Array.reduce(ids, 0, (function (acc, i) {
                return acc + i | 0;
              }));
}

console.log(seatIds.length);

var list = Belt_List.fromArray(Belt_SortArray.stableSortBy(seatIds, Caml.caml_int_compare));

function slidingWindow(list) {
  var loop = function (list, acc) {
    var c = Belt_List.take(list, 2);
    var x = Belt_List.tail(list);
    if (c !== undefined && x !== undefined) {
      return {
              hd: c,
              tl: loop(x, acc)
            };
    } else {
      return acc;
    }
  };
  return loop(list, /* [] */0);
}

console.log(slidingWindow(list));

exports.input = input;
exports.bPass = bPass;
exports.seatIds = seatIds;
exports.sum = sum;
exports.list = list;
exports.slidingWindow = slidingWindow;
/* input Not a pure module */