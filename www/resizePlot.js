
const checkElement = async selector => {
  while ( document.querySelector(selector) === null) {
    await new Promise( resolve =>  requestAnimationFrame(resolve) )
  }
  return document.querySelector("selector"); 
};


// timer to prevent resize window to trigger multiple events
function debounce(func){
  var timer;
  return function(event){
    if(timer) clearTimeout(timer);
    timer = setTimeout(func,250,event);
  };
}


$(document).on("shiny:connected", function() {
  
  
  // fade out intro page
    function removeFadeOut( el, speed ) {
        var seconds = speed/1000;
        el.style.transition = "opacity "+seconds+"s ease";

        el.style.opacity = 0;
        setTimeout(function() {
            el.parentNode.removeChild(el);
        }, speed);
    }

    // speedy way to remove intor page: just press enter
    window.addEventListener('keydown', function(e){
        if (e.keyCode == 13) {
            document.querySelector("#close_intro").click()
        }
      });

    // remove intro and show main when start is clicked
    document.querySelector("#close_intro").addEventListener("click", () => {
        removeFadeOut(document.querySelector('#intro_page'), 1000);
    });
  
  
  
  
  
  
checkElement('#menu').then((selector) => {
  
  // resize plot output to fit initial window size
  var w = window.innerHeight;
  var menu_box = document.querySelector('#menu').offsetHeight;
  var value = window.innerHeight - menu_box - 20;
  Shiny.setInputValue('set_plot_height', value);
  
  // activate all tooltips
  var tooltipTriggerList = [].slice.call(document.querySelectorAll('[data-bs-toggle="tooltip"]'))
  var tooltipList = tooltipTriggerList.map(function (tooltipTriggerEl) {
    return new bootstrap.Tooltip(tooltipTriggerEl)
  })
  
  
  // modal functionality: close modal when click outside
  var modal = document.querySelector('#info_modal');
  //window.onclick = function(event) {
  //  if(!modal.classList.contains('hidden')){
  //    if (event.target.id != "info_modal" & event.target.id != "info_modalheader") {
  //       modal.classList.add('hidden')
  //     }
  //  }
  //}
  
  // close modal when esc keypress
  document.addEventListener('keydown', (event) => {
    if (event.key === 'Escape') {
      if(!modal.classList.contains('hidden')){
      modal.classList.add('hidden')
      }
  }
})


// modal draggable
dragElement(document.getElementById("info_modal"));

function dragElement(elmnt) {
  var pos1 = 0, pos2 = 0, pos3 = 0, pos4 = 0;
  if (document.getElementById(elmnt.id + "header")) {
    // if present, the header is where you move the DIV from:
    document.getElementById(elmnt.id + "header").onmousedown = dragMouseDown;
  } else {
    // otherwise, move the DIV from anywhere inside the DIV:
    elmnt.onmousedown = dragMouseDown;
  }

  function dragMouseDown(e) {
    e = e || window.event;
    e.preventDefault();
    // get the mouse cursor position at startup:
    pos3 = e.clientX;
    pos4 = e.clientY;
    document.onmouseup = closeDragElement;
    // call a function whenever the cursor moves:
    document.onmousemove = elementDrag;
  }

  function elementDrag(e) {
    e = e || window.event;
    e.preventDefault();
    // calculate the new cursor position:
    pos1 = pos3 - e.clientX;
    pos2 = pos4 - e.clientY;
    pos3 = e.clientX;
    pos4 = e.clientY;
    // set the element's new position:
    elmnt.style.top = (elmnt.offsetTop - pos2) + "px";
    elmnt.style.left = (elmnt.offsetLeft - pos1) + "px";

  }

  function closeDragElement() {
    // stop moving when mouse button is released:
    document.onmouseup = null;
    document.onmousemove = null;
  }
}

  window.addEventListener('resize', debounce(function() {
    
    var w = window.innerHeight;
    var menu_box = document.querySelector('#menu').offsetHeight;
    var value = window.innerHeight - menu_box - 20;
    Shiny.setInputValue('set_plot_height', value);
    })
      );
});

});





