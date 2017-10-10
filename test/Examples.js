"use strict";

exports.setTimeout = function (ms) {
  return function (eff) {
    return function () {
      setTimeout(eff, ms);
    };
  };
};