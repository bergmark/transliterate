$(document).ready(function () {

  var H = Strict.Translit.Hangeul;

  var inp = jQuery("#inp");
  var out = jQuery("#out");

  inp.keyup(function (_) {
    out.val(H.toHangeul(inp.val()));
  });
  out.keyup(function (_) {
    inp.val(H.fromHangeul(out.val()));
  });

});
