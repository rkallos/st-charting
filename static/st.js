$( document ).ready(function() {
    // Editable stuff
    $('.step #show-edit').on('click', function(event) {
        var ele = $(this).closest('.step').find('#edit-form');
        ele.slideToggle();
    });

    // Confirm delete form
    $('.step .delete').on('click', function(event) {
        var ele = $(this).closest('.step').find('#delete-form');
        ele.slideToggle();
    });

    // Toggling edit form
    $('.title #show-edit, .real #show-edit, .ideal #show-edit').on('click', function(event) {
        var ele = $(this).closest('.title, .ideal, .real');
        ele.find('p').slideToggle();
        ele.find('#edit-form').slideToggle();
    });

    // Datepicker
    $(function() {
	$('.datepicker').datepicker({dateFormat: 'yy-mm-dd'});
    });

    // Sortable steps
    $('#step-list').sortable({
        handle : '#step-move',
        placeholder : "sorting"
    });
    
    console.log( 'DOM ready!' );
});

// Taken from PureCSS
(function (window, document) {

    var layout   = document.getElementById('layout'),
        menu     = document.getElementById('menu'),
        menuLink = document.getElementById('menuLink');

    function toggleClass(element, className) {
        var classes = element.className.split(/\s+/),
            length = classes.length,
            i = 0;

        for(; i < length; i++) {
          if (classes[i] === className) {
            classes.splice(i, 1);
            break;
          }
        }
        // The className is not found
        if (length === classes.length) {
            classes.push(className);
        }

        element.className = classes.join(' ');
    }

    menuLink.onclick = function (e) {
        var active = 'active';

        e.preventDefault();
        toggleClass(layout, active);
        toggleClass(menu, active);
        toggleClass(menuLink, active);
    };

}(this, this.document));
