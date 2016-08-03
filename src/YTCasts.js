var cheerio = require('cheerio');
var exec = require('child_process').exec;

exports.parseConfig = function (string) {
  var config = JSON.parse(string);
  if (!config.targets) {
    throw new Error('missing targets field in config');
  } else {
    return config;
  }
}

exports.getCasts = function(string) {
  var $ = cheerio.load(string);
  var casts = [];
  $('.channels-browse-content-grid a.spf-link').each(function () {
    var $this = $(this);
    casts.push({
      title: $this.text(),
      link : 'https://www.youtube.com' + $this.attr('href')
    });
  });
  return casts;
}

exports._runDownload = function (url, cb) {
  return function () {
    console.log('downloading', url);
    exec('youtube-dl -o downloads/%\\(title\\)s.%\\(ext\\)s -x --audio-format mp3' + url, function (err) {
      if (err) {
        throw err;
      } else {
        cb()();
      }
    });
  }
}
