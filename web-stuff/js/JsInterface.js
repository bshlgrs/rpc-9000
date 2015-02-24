(function() {
  window.JsInterface = {
    getBody: function () {
      return editor.val();
    },
    jsonTree: function(stuff) {
      return JSONTree.create(stuff);
    },
    parse: Parser.parse.bind(Parser)
  };
})();
