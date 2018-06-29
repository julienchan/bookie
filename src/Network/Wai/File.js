'use strict';

var fs = require('fs');

exports.createReadStreamRangeImpl = function (path, start, end) {
  return function () {
    return fs.createReadStream(path, {
      start: start,
      end: end
    });
  }
}
