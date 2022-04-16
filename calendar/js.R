js_mark_dates <- function(date_nr, color = "green"){paste0("
  var elements = document.getElementsByClassName('tui-full-calendar-weekday-grid-line  tui-full-calendar-near-month-day');
  var elements2 = document.getElementsByClassName('tui-full-calendar-weekday-grid-header');
  
  for (var nr = 0; nr < elements.length; nr++) {
    console.log(nr)
    const nrr = nr;
    elements[nr].addEventListener('click', function(el) {
      var target = el.target;
      Shiny.onInputChange('clicked_data', {val: target.textContent, nr: nrr, rand: Math.random()});
    });
    
    if(nr == ", date_nr,"){
      elements[nr].style['background-color'] = '", color,"'
      
      container = elements2[nr].getElementsByTagName('span')[0]
      container.style.textAlign= 'center';
      container.style.width = '240px'
      var div = document.createElement('div');
      var htmlString = '<i class=\"fa fa-sun\" role=\"presentation\" aria-label=\"sun icon\"></i>'
      //var htmlString = '<i class=\"fa fa-cloud\" role=\"presentation\" aria-label=\"cloud icon\"></i>'
      div.innerHTML = htmlString.trim();
      div.style.float = 'right'
      div.style.marginRight = '5px';
      div.style.marginTop = '5px';
      container.appendChild(div);
      
    }
    console.log(2)
  }
  
  var elementx = document.getElementById('my_calendar_menu_navi');
  elementx.addEventListener('click', function(el) {
      var target = el.target;
      Shiny.onInputChange('updated_calendar', {rand: Math.random()});
    });

")}


js_current_date <- "
var elements_title = document.getElementsByClassName('tui-full-calendar-weekday-schedule-title');
for (var i = 0; i < elements_title.length; i++) {
  container = elements_title[i].parentNode
  container.style.textAlign= 'center';
  elements_title[i].style.float = 'left'
  var div = document.createElement('div');
  var htmlString = '<i class=\"fa fa-bicycle\" role=\"presentation\" aria-label=\"bicycle icon\"></i>'
  
  div.innerHTML = htmlString.trim();
  div.style.float = 'left'
  div.style.marginLeft = '2px';
  container.appendChild(div);
};

// get date
var elements = document.getElementsByClassName('tui-full-calendar-weekday-grid-line  tui-full-calendar-near-month-day');
for (var i = 0; i < elements.length; i++) {
  const nrr = i
  elements[i].addEventListener('click', function(el) {
    var target = el.target;
    Shiny.onInputChange('clicked_data', {val: target.textContent, nr: nrr; rand: Math.random()});
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
"
