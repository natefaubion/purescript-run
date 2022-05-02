"use strict";

export function setTimeout(ms) {
  return function (eff) {
    return function () {
      setTimeout(eff, ms);
    };
  };
}
