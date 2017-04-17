var cheerio = require('cheerio');

exports._getCasts = function(string) {
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
};
