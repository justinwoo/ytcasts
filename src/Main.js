var cheerio = require('cheerio');
var sqlite3 = require('sqlite3');
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

exports._newDB = function (filename, cb) {
  return function () {
    cb(new sqlite3.Database(filename))();
  }
}

exports._closeDB = function (db, cb) {
  return function () {
    db.close();
    cb()();
  }
}

exports._queryDB = function (db, query, params, cb) {
  return function () {
    var args = [query].concat(params.concat(function (err, rows) {
      if (err) {
        throw err;
      } else {
        cb(rows)();
      }
    }));
    // the hell API is this that it breaks with null context:
    db.all.apply(db, args);
  }
}

exports.parseRow = function (row) {
  return {
    title: row.title,
    link : row.link,
    path: row.path,
    created: row.created
  };
}

exports._runDownload = function (url, cb) {
  return function () {
    console.log('downloading ', url);
    exec('youtube-dl -o downloads/%\\(title\\)s.%\\(ext\\)s -x ' + url, function (err) {
      if (err) {
        throw err;
      } else {
        cb()();
      }
    });
  }
}
