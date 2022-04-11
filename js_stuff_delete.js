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

/*
  aaa
document.onclick = function(){
  Shiny.onInputChange('document_clicked', Math.random());
}

// get date
var elements = document.getElementsByClassName('tui-full-calendar-weekday-grid-line  tui-full-calendar-near-month-day');
for (var i = 0; i < elements.length; i++) {
  elements[i].addEventListener('click', function(el) {
    var target = el.target;
    Shiny.onInputChange('clicked_data', {val: target.textContent, rand: Math.random()});
  });
}

// get month
var elements = document.getElementsByClassName('tui-full-calendar-month-week-item');
for (var i = 0; i < elements.length; i++) {
  elements[i].addEventListener('click', function(el) {
    var target = el.target;
    while (target.className != 'tui-full-calendar-month-week-item') {
      target = target.parentElement
    }
    Shiny.onInputChange('row_idx', [...target.parentElement.children].indexOf(target))
  });
}
*/
  