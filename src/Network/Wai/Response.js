exports.bufferByteLength = function (buff) {
  return function () {
    return Buffer.byteLength(buff)
  }
}

exports.pipeNoEnd = function (read) {
  return function (write) {
    return function () {
      return read.pipe(write, { end : false});
    };
  };
}

exports.clearHeaders = function (res) {
  return function () {
    var headers = getHeaderNames(res)

    for (var i = 0; i < headers.length; i++) {
      res.removeHeader(headers[i])
    }
  };
}

function getHeaderNames (res) {
  return typeof res.getHeaderNames !== 'function'
    ? Object.keys(res._headers || {})
    : res.getHeaderNames()
}
